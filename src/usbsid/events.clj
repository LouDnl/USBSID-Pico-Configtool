(ns usbsid.events
  "It's a party up in here!"
  (:require
   [usbsid.config-model :as model]
   [usbsid.state :as state]
   [usbsid.driver :as driver]
   [usbsid.ini-io :as ini-io]
   [usbsid.logging :refer [severe]])
  (:import
   [javafx.stage FileChooser FileChooser$ExtensionFilter]))


;;; Moar then two!

(defmulti handle :event/type)

(defmethod handle :default [event]
  (println "Unhandled event:" event))

(defmethod handle :navigate [{:keys [section]}]
  (state/set-section! section))

(defmethod handle :refresh [_]
  (try
    (swap! state/*state update :app-nonce (fnil inc 0))
    (catch Exception e
      (severe (str "Exception during dev/refresh-app: " (.getMessage e)) e))))

(defmethod handle :config-changed [{:keys [path value]}]
  (when
   (and
    (not= path [:last-preset])
    (not= (get-in @state/*state [:connection :fw-line]) :unknown)
    (not= (get-in @state/*state [:connection :fw-line]) :legacy)
    (= (get-in @state/*state [:connection :status]) :connected))
    (driver/set-config! path value))
  (state/set-config-value! path value))

(defmethod handle :connect [{:keys [key]}]
  (when key (handle {:event/type :popup-hide :key key}))
  (driver/connect!))

(defmethod handle :disconnect [{:keys [key]}]
  (when key (handle {:event/type :popup-hide :key key}))
  (driver/disconnect!))

(defmethod handle :read-config [{:keys [key]}]
  (when key (handle {:event/type :popup-hide :key key}))
  (driver/read-config!))

(defmethod handle :write-config [{:keys [key]}]
  (when key (handle {:event/type :popup-hide :key key}))
  (driver/write-config!))

(defmethod handle :save-config [{:keys [key]}]
  (when key (handle {:event/type :popup-hide :key key}))
  (driver/save-reboot!))

(defmethod handle :reset-config [{:keys [key]}]
  (when key (handle {:event/type :popup-hide :key key}))
  (driver/reset-config!))

(defmethod handle :detect-sids [_]
  (driver/detect-sids!))

(defmethod handle :apply-preset [{:keys [preset]}]
  (if (driver/connected?)
    (driver/apply-preset! preset)
    (state/log!
     (str "Preset " (name preset) " selected (not connected - connect board to apply)"))))

(defmethod handle :apply-config [{:keys [key]}]
  (when key (handle {:event/type :popup-hide :key key}))
  (driver/apply-config!))

(defmethod handle :save-noreset [{:keys [key]}]
  (when key (handle {:event/type :popup-hide :key key}))
  (let [saved (driver/save-config!)]
    (deref saved)
    (handle {:event/type :read-config})))

(defmethod handle :reload-flash [{:keys [key]}]
  (when key (handle {:event/type :popup-hide :key key}))
  (driver/reload-config!))

(defmethod handle :auto-detect [_]
  (driver/auto-detect!))

(defmethod handle :detect-clones [_]
  (driver/detect-clones!))

(defmethod handle :test-all-sids [_]
  (driver/test-all-sids!))

(defmethod handle :test-sid [{:keys [n]}]
  (driver/test-sid! n))

(defmethod handle :stop-tests [_]
  (driver/stop-tests!))

(defmethod handle :restart-bus [_]
  (driver/restart-bus!))

(defmethod handle :restart-clk [_]
  (driver/restart-clk!))

(defmethod handle :sync-pios [_]
  (driver/sync-pios!))

(defmethod handle :reset-sids [_]
  (driver/reset-sids!))

(defmethod handle :reset-mcu [_]
  (driver/reset-mcu!))

(defmethod handle :bootloader [_]
  (driver/bootloader!))

(defmethod handle :popup-show [{:keys [key]}]
  (swap! state/*state assoc :hover-popup key))

(defmethod handle :popup-hide [{:keys [key]}]
  (swap! state/*state
         (fn [s]
           (if (= (:hover-popup s) key)
             (assoc s :hover-popup nil)
             s))))

(defmethod handle :open-url [{:keys [url]}]
  (let [desktop (java.awt.Desktop/getDesktop)]
    (when (.isSupported desktop java.awt.Desktop$Action/BROWSE)
      (.browse desktop (java.net.URI. url)))))

(defmethod handle :confirm-config [_]
  (driver/confirm-config!))

(defn- ini-ext-filter []
  (FileChooser$ExtensionFilter. "INI files" ^"[Ljava.lang.String;" (into-array String ["*.ini"])))

(defmethod handle :export-ini [_]
  (let [s    @state/*state
        fc   (doto (FileChooser.)
               (.setTitle "Save Configuration as INI")
               (.setInitialFileName "USBSID-Pico-cfg.ini")
               (-> .getExtensionFilters (.add (ini-ext-filter))))
        file (.showSaveDialog fc nil)]
    (when file
      (let [ini (ini-io/config->ini
                 (:config s)
                 (get-in s [:connection :fw-version]))]
        (spit file ini)
        (swap! state/*state assoc-in [:connection :config-status] :exportini)
        (state/log! (str "Config exported to " (.getName file)))))))

(defmethod handle :import-ini [_]
  (let [fc   (doto (FileChooser.)
               (.setTitle "Load Configuration from INI")
               (-> .getExtensionFilters (.add (ini-ext-filter))))
        file (.showOpenDialog fc nil)]
    (when file
      (let [ini-str     (slurp file)
            base-cfg    (:config @state/*state)
            new-cfg     (ini-io/ini->config ini-str base-cfg)
            ini-ver     (ini-io/ini->version ini-str)
            ini-line    (state/fw-version->line ini-ver)
            board-line  (get-in @state/*state [:connection :fw-line])]
        (when (and board-line ini-line (not= board-line ini-line))
          (state/log!
           (format "INI source fw %s differs from connected board (%s) - cross-version import"
                   (or ini-ver "?") (name board-line))))
        (when (= board-line :legacy)
          (doseq [sk [:socket-one :socket-two]
                  :let [chip (get-in new-cfg [sk :chiptype])]
                  :when (and chip (not (model/legacy-supports-chip? chip)))]
            (state/log! (format "INI %s chiptype %s has no v0.5/0.6 equivalent - will be dropped on write"
                                (name sk) (name chip)))))
        (swap! state/*state
               #(-> %
                    (assoc :config new-cfg :dirty true)
                    (assoc-in [:connection :config-status] :importini)))
        (state/log! (str "Config imported from " (.getName file)))))))

(defn config-changed!
  [path value]
  (handle {:event/type :config-changed
           :path path
           :value value}))

(defn chiptype-changed!
  [socketkey value]
  (state/set-chiptype! (if (= socketkey :socket-one) :chipone :chiptwo) value))
