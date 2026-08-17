(ns build
  (:refer-clojure :exclude [test])
  (:require
   [clojure.tools.build.api :as b]
   [clojure.string :as string])
  (:import
   [java.io File]))

(def lib        'org.clojars.loud/usbsid-configtool)
(def version    (string/trim-newline (slurp "resources/.version")))
(def main       'usbsid.core)
(def app-name   "USBSID-Configtool")
(def class-dir  "target/classes")
(def uber-file  (format "target/%s-%s.jar" (name lib) version))

(def ^:private driver-group    "usbsid")
(def ^:private driver-artifact "usbsid-usb-driver-library-java")
(def ^:private driver-version  "1.1")

(defn- ensure-driver!
  "Install the USBSID driver JAR/POM into the local Maven repo from assets/driver/ if absent."
  []
  (let [sep        File/separator
        m2-repo    (str (System/getProperty "user.home") sep ".m2" sep "repository")
        target-dir (File. (str m2-repo sep driver-group sep driver-artifact sep driver-version))
        jar-name   (str driver-artifact "-" driver-version ".jar")
        pom-name   (str driver-artifact "-" driver-version ".pom")
        target-jar (File. target-dir jar-name)]
    (when-not (.exists target-jar)
      (let [src-jar (File. (str "assets" sep "driver" sep jar-name))
            src-pom (File. (str "assets" sep "driver" sep pom-name))]
        (when-not (.exists src-jar)
          (println "Driver JAR not found in local Maven repo or assets/driver/")
          (println {:looked-for (.getAbsolutePath src-jar)}))
        (println "\nDriver not in local Maven repo - installing from assets/driver/...")
        (.mkdirs target-dir)
        (b/copy-file {:src (.getPath src-jar) :target (.getPath (File. target-dir jar-name))})
        (when (.exists src-pom)
          (b/copy-file {:src (.getPath src-pom) :target (.getPath (File. target-dir pom-name))}))
        (println (str "  Installed " jar-name " -> " (.getAbsolutePath target-dir)))))))

; JVM args for JavaFX 11+ running from a fat jar (non-modular classpath mode)
(def jfx-opts
  ["--add-opens=javafx.graphics/com.sun.javafx.application=ALL-UNNAMED"
   "--add-opens=javafx.base/com.sun.javafx.runtime=ALL-UNNAMED"
   "--add-opens=javafx.controls/com.sun.javafx.scene.control=ALL-UNNAMED"])
(def java-opts
  ["--enable-native-access=ALL-UNNAMED"
   "--sun-misc-unsafe-memory-access=allow"]) ; JVM 22+
(def compile-opts
  (conj java-opts "-Dcljfx.skip-javafx-initialization=true"))
(def jfx-jvm-args
  (string/join " " (into jfx-opts java-opts)))
(def jpackage-opts
  (into jfx-opts java-opts))

; Platform / arch detection for selecting matching JavaFX classifier alias.
; macOS uberjar would otherwise pack BOTH $mac and $mac-aarch64 dylibs at
; identical JAR-root paths → uber merge drops one → arch mismatch at runtime.

(defn- host-jfx-alias
  "Returns deps.edn alias keyword for the host's mac classifier, or nil on non-mac."
  []
  (let [os   (string/lower-case (System/getProperty "os.name"))
        arch (string/lower-case (System/getProperty "os.arch"))]
    (when (string/includes? os "mac")
      (if (or (= arch "aarch64") (= arch "arm64"))
        :jfx-mac-arm
        :jfx-mac-x64))))

(defn- with-jfx-aliases
  "Appends host JavaFX alias (if any) to the supplied alias vec."
  [aliases]
  (if-let [a (host-jfx-alias)]
    (conj (vec aliases) a)
    (vec aliases)))


;;; unittests

(defn test
  "Run all tests via cognitect test-runner."
  [opts]
  (ensure-driver!)
  (let [basis    (b/create-basis {:aliases (with-jfx-aliases [:test])})
        cmds     (b/java-command
                  {:basis     basis
                   :main      'clojure.main
                   :main-args ["-m" "cognitect.test-runner"]})
        {:keys [exit]} (b/process cmds)]
    (when-not (zero? exit) (throw (ex-info "Tests failed" {}))))
  opts)


;;; uberjar

(defn uber
  "Build a self-contained fat JAR including all deps + JavaFX platform JARs.
   On macOS hosts, picks exactly one mac classifier ($mac or $mac-aarch64) via
   `host-jfx-alias` to avoid the libprism_*.dylib collision."
  [opts]
  (ensure-driver!)
  (b/delete {:path "target"})
  (let [basis (b/create-basis {:aliases (with-jfx-aliases [])})]
    (println "\nCopying sources and resources...")
    (b/copy-dir {:src-dirs ["src" "resources"] :target-dir class-dir})
    (println (str "\nCompiling " main "..."))
    (b/compile-clj {:basis      basis
                    :src-dirs   ["src"]
                    :class-dir  class-dir
                    :ns-compile [main]
                    :java-opts  compile-opts})
    (println "\nBuilding uberjar...")
    (let [skip (fn [_] nil)]  ; skip duplicate - return nil = no write
      (b/uber {:class-dir class-dir
               :uber-file uber-file
               :basis     basis
               :main      main
               :conflict-handlers
               {"^module-info.class$"    skip
                "^META-INF/MANIFEST.MF$" skip
                "^META-INF/maven/.*"     skip
                "^META-INF/services/.*"  :append
                "^META-INF/.*"           skip}}))
    (println (str "\nBuilt: " uber-file " ("
                  (int (/ (.length (File. uber-file)) 1024 1024))
                  " MB)")))
  opts)


;;; launcher scripts

(defn- write-launchers []
  (let [jar   (str (name lib) "-" version ".jar")
        sh    (str "#!/bin/sh\n"
                   "DIR=\"$(cd \"$(dirname \"$0\")\" && pwd)\"\n"
                   "exec java " jfx-jvm-args " -jar \"$DIR/" jar "\" \"$@\"\n")
        bat   (str "@echo off\r\n"
                   "set DIR=%~dp0\r\n"
                   "java " (string/replace jfx-jvm-args "\n" " ") " -jar \"%DIR%" jar "\" %*\r\n")]
    (spit "target/run.sh"  sh)
    (spit "target/run.bat" bat)
    (.setExecutable (File. "target/run.sh") true)
    (println "  Wrote target/run.sh and target/run.bat")))


;;; jpackage

(defn- detect-pkg-type []
  (let [os (string/lower-case (System/getProperty "os.name"))]
    (cond
      (string/includes? os "linux")   "app-image"
      (string/includes? os "windows") "msi"
      (string/includes? os "mac")     "dmg"
      :else                           "app-image")))

(defn- jpackage!
  "Invoke jpackage with the given arg vector. Throws on non-zero exit."
  [args]
  (let [{:keys [exit]} (b/process {:command-args args})]
    (when-not (zero? exit)
      (throw (ex-info "jpackage failed" {:exit exit :args args})))))

(defn- codesign-app!
  "Ad-hoc deep-sign the .app with hardened runtime + entitlements.

  jpackage's default ad-hoc signature only covers the launcher binary; the
  embedded JVM libraries and runtime-extracted JNI dylibs (libusb4java) stay
  unsigned. On Apple Silicon macOS 14+, entitlements (`device.usb`,
  `disable-library-validation`) bind only when the *whole* bundle carries a
  consistent signature, so an unsigned libusb4java.dylib leaves `libusb_open`
  blocked by TCC with no error - the connect call hangs forever.

  `--sign -` ad-hoc identity is enough to make the entitlements bind; proper
  Developer ID signing is still the long-term answer for distribution +
  notarization, but ad-hoc unblocks local + community Apple Silicon use today."
  [app-path entitlements]
  (println (str "Deep-signing " app-path " (ad-hoc, with entitlements)"))
  (let [{:keys [exit]}
        (b/process
         {:command-args ["codesign"
                         "--force"
                         "--deep"
                         "--options"       "runtime"
                         "--entitlements"  entitlements
                         "--sign"          "-"
                         app-path]})]
    (when-not (zero? exit)
      (throw (ex-info "codesign failed" {:exit exit :app app-path})))))

(defn package
  "Create a platform-native package using jpackage.
  On Linux: app-image (self-contained directory).
  On Windows: msi installer (requires WiX toolset), per-user install scope.
  On macOS: app-image → ad-hoc deep codesign → dmg (two-step so the
  resigned .app is what lands in the dmg).
  NOTE: cross-compilation not supported - run on each target platform."
  [opts]
  (when-not (.exists (File. uber-file))
    (uber opts))
  (let [pkg-type  (or (:type opts) (detect-pkg-type))
        jar-name  (str (name lib) "-" version ".jar")
        input-dir "target/jpackage-input"
        dest      "target/package"
        ; macOS jpackage requires integers only, first component > 0.
        ; On macOS only: bump leading 0 to 1 (0.1.0 → 1.1.0) in package metadata.
        ver       (let [v     (string/replace version "-SNAPSHOT" "")
                        macos (string/includes? (string/lower-case (System/getProperty "os.name")) "mac")
                        parts (string/split v #"\." 3)]
                    (if (and macos (= "0" (first parts)))
                      (string/join "." (cons "1" (rest parts)))
                      v))]
    ; Copy only the JAR into a clean input dir (avoids jpackage recursing into dest)
    (b/delete {:path input-dir})
    (b/delete {:path dest})
    (.mkdirs (File. input-dir))
    (.mkdirs (File. dest))
    (b/copy-file {:src uber-file :target (str input-dir "/" jar-name)})
    (let [os           (string/lower-case (System/getProperty "os.name"))
          win?         (string/includes? os "windows")
          mac?         (string/includes? os "mac")
          icon         (cond
                         win?                          "resources/usbsid-configtool-icon.ico"
                         (string/includes? os "linux") "resources/usbsid-configtool-icon-flat.png"
                         mac?                          "resources/usbsid-configtool-icon.icns"
                         :else                         nil)
          ; macOS hardened-runtime (jpackage default) blocks System.load of unsigned
          ; libusb4java.dylib extracted from classifier JAR. Entitlements disable
          ; library validation so the JNI native loads. Without these the app hangs at
          ; UsbHostManager.getUsbServices() with a silent UnsatisfiedLinkError.
          entitlements "resources/mac-entitlements.plist"
          common-args  (cond-> (into
                                ["--name"           app-name
                                 "--app-version"    ver
                                 "--input"          input-dir
                                 "--main-jar"       jar-name
                                 "--main-class"     "usbsid.core"
                                 "--vendor"         "LouD"
                                 "--description"    "USBSID-Configuration Tool"
                                 "--copyright"      "Copyright 2024-2026 LouD, GPLv2"]
                                (mapcat #(vector "--java-options" %) jpackage-opts))
                         (and icon (.exists (File. ^String icon))) (into ["--icon" icon])
                         (and mac? (.exists (File. ^String entitlements)))
                         (into ["--mac-entitlements" entitlements])
                         win? (into ["--win-menu"
                                     "--win-menu-group"    "USBSID-Pico"
                                     "--win-shortcut"
                                     "--win-dir-chooser"
                                     "--win-per-user-install"
                                     "--win-upgrade-uuid"  "b2e8c4f1-3a5d-4e6b-9c7d-0f1e2a3b4c5d"]))]
      (if mac?
        ; Two-step macOS flow: app-image → deep codesign → dmg wrapping the resigned app.
        (let [app-dir  (str dest "/app-image")
              app-path (str app-dir "/" app-name ".app")]
          (.mkdirs (File. app-dir))
          (println (str "\nBuilding macOS app-image -> " app-dir "/..."))
          (jpackage! (into ["jpackage" "--type" "app-image" "--dest" app-dir] common-args))
          (when (and (.exists (File. ^String entitlements))
                     (.exists (File. ^String app-path)))
            (codesign-app! app-path entitlements))
          (println (str "\nWrapping signed app into dmg -> " dest "/..."))
          (jpackage! ["jpackage"
                      "--type"          "dmg"
                      "--app-image"     app-path
                      "--name"          app-name
                      "--app-version"   ver
                      "--dest"          dest
                      "--vendor"        "LouD"
                      "--description"   "USBSID-Configuration Tool"
                      "--copyright"     "Copyright 2024-2026 LouD, GPLv2"])
          (println (str "Package created in " dest "/")))
        ; Non-mac: single jpackage call as before.
        (do
          (println (str "\nPackaging as " pkg-type " -> " dest "/..."))
          (jpackage! (into ["jpackage" "--type" pkg-type "--dest" dest] common-args))
          (println (str "Package created in " dest "/"))))))
  opts)

; ci

(defn tests
  "Run the unittests only"
  [opts]
  (test opts)
  opts)

(defn ci
  "Run full CI pipeline: tests + uberjar + launcher scripts."
  [opts]
  (test opts)
  (uber opts)
  (write-launchers)
  opts)

(defn release
  "Full release build: CI pipeline + platform package."
  [opts]
  (ci opts)
  (package opts)
  opts)
