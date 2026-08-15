(ns usbsid.config-model
  "USBSID-Pico config options"
  (:require
   [clojure.string :refer [join split]]))


;;; What you doing?

(defn deep-merge
  "Merge submap items with submap items"
  [a b]
  (if (and (map? a) (map? b))
    (merge-with deep-merge a b)
    b))

(defn parse-pcb-version
  "PCB version string to integer"
  [pcbver]
  (if (string? pcbver)
    (Long/parseLong (join "" (split pcbver #"\.")))
    999))

(defn parse-fw-version
  "Firmware version string to integer"
  [fwver]
  (if (string? fwver)
    (-> fwver
        (clojure.string/split  #"-")
        first
        (clojure.string/split  #"\.")
        (#(clojure.string/join "" %))
        (Long/parseLong))
    0))


;;; Fixed things

(def clock-rates
  [{:key :default :label "DEFAULT (1.000 MHz)" :value 1000000 :id 0}
   {:key :pal     :label "PAL     (0.985 MHz)" :value 985248  :id 1}
   {:key :ntsc    :label "NTSC    (1.023 MHz)" :value 1022727 :id 2}
   {:key :drean   :label "DREAN   (1.023 MHz)" :value 1023440 :id 3}
   {:key :ntsc2   :label "NTSC2   (1.023 MHz)" :value 1022730 :id 4}])

(def clock-rate-by-id
  (into {} (map (fn [c] [(:id c) c]) clock-rates)))

(def clock-rate-by-key
  (into {} (map (fn [c] [(:key c) c]) clock-rates)))

(def chip-types
  [{:key :real     :label "Real SID" :id 0}
   {:key :unknown  :label "Unknown"  :id 1}
   {:key :skpico   :label "SKPico"   :id 2}
   {:key :armsid   :label "ARMSID"   :id 3}
   {:key :arm2sid  :label "ARM2SID"  :id 4}
   {:key :fpgasid  :label "FPGASID"  :id 5}
   {:key :redipsid :label "RedipSID" :id 6}
   {:key :pdsid    :label "PDSID"    :id 7}
   {:key :backsid  :label "BackSID"  :id 8}
   {:key :sidemu   :label "SIDEmu"   :id 9}])

(def chip-type-by-id
  (into {} (map (fn [c] [(:id c) c]) chip-types)))

(def chip-type-by-key
  (into {} (map (fn [c] [(:key c) c]) chip-types)))

(comment
  "For reference only"
  "Legacy (v0.5.x / v0.6.x firmware) chiptype + clonetype enums."
  "Firmware split chip class (real/clone/unknown) and clone subtype across"
  "two bytes (config_array bytes 12+13 / 23+24). v0.7+ collapsed them into"
  "the single flat `chip-types` enum above"

  (def legacy-chiptypes
    [{:key :real    :id 0}
     {:key :clone   :id 1}
     {:key :unknown :id 2}])

  (def legacy-clonetypes
    [{:key :disabled :id 0}
     {:key :other    :id 1}
     {:key :skpico   :id 2}
     {:key :armsid   :id 3}
     {:key :fpgasid  :id 4}
     {:key :redipsid :id 5}])

  "Forward map: legacy {chiptype-id, clonetype-id} -> v0.7 flat chip-types :key."
  "Used when reading a config from a legacy board into the unified UI model."
  "- chiptype=0 (real) collapses to :real regardless of clonetype."
  "- chiptype=2 (unknown) -> :unknown."
  "- chiptype=1 (clone) is dispatched by clonetype: disabled/other -> :unknown"
  "(no flat key for \"clone-but-unspecified\") the rest map to their named keys."
  )

(defn legacy->flat-chip
  "Combine legacy {chiptype, clonetype} byte values into a v0.7 flat chip-types :key."
  [chiptype clonetype]
  (case (int chiptype)
    0 :real
    2 :unknown
    1 (case (int clonetype)
        2 :skpico
        3 :armsid
        4 :fpgasid
        5 :redipsid
        :unknown) ; disabled / other / out-of-range
    :unknown))

(comment
  "Reverse map: v0.7 flat :key -> legacy {chiptype, clonetype} bytes."
  "Returns nil for keys that have no legacy equivalent (`:arm2sid` / `:pdsid` /"
  "`:backsid` / `:sidemu`) writer must drop+log those."
  )
(def flat-chip->legacy
  {:real     {:chiptype 0 :clonetype 0}
   :unknown  {:chiptype 2 :clonetype 0}
   :skpico   {:chiptype 1 :clonetype 2}
   :armsid   {:chiptype 1 :clonetype 3}
   :fpgasid  {:chiptype 1 :clonetype 4}
   :redipsid {:chiptype 1 :clonetype 5}})

(defn legacy-supports-chip?
  "True if the given flat chip-types :key has a legacy {chiptype, clonetype} mapping."
  [chip-key]
  (contains? flat-chip->legacy chip-key))

(comment
  "Per-firmware capability set. UI consults this to grey out controls the"
  "connected firmware does not understand"
  )
(def fw-capabilities
  {:legacy #{:clock :sockets :led :rgb :fmopl :stereo :audio-sw :mirrored}
   :v0_7   #{:clock :sockets :led :rgb :fmopl :stereo :audio-sw :mirrored
             :flipped :mixed :need-confirmation :socket_change_detect
             :last-preset :preset_auto_detect}})

(defn fw-supports?
  "True if the given firmware line exposes the given capability keyword."
  [fw-line capability]
  (contains? (get fw-capabilities fw-line (:v0_7 fw-capabilities))
             capability))

(def sid-types
  [{:key :unknown :label "Unknown" :id 0}
   {:key :na      :label "N/A"     :id 1}
   {:key :mos8580 :label "MOS8580" :id 2}
   {:key :mos6581 :label "MOS6581" :id 3}
   {:key :fmopl   :label "FMOpl"   :id 4}])

(def sid-type-by-id
  (into {} (map (fn [s] [(:id s) s]) sid-types)))

(def sid-type-by-key
  (into {} (map (fn [s] [(:key s) s]) sid-types)))

(def fmopl-sid-options
  [{:key 0   :label "Unknown"}
   {:key 1   :label "SID 1"}
   {:key 2   :label "SID 2"}
   {:key 3   :label "SID 3"}
   {:key 4   :label "SID 4"}
   {:key 255 :label "Disabled"}])

(def sid-to-use-options
  [{:key -1 :label "Off"}
   {:key  1 :label "SID 1"}
   {:key  2 :label "SID 2"}
   {:key  3 :label "SID 3"}
   {:key  4 :label "SID 4"}])

(def voltages
  [{:mos6581  "12v"}
   {:mos8580   "9v"}
   {:unknown   "9v"}
   {:skpico    "9v"}
   {:armsid    "9v"}
   {:arm2sid   "9v"}
   {:fpgasid   "9v"}
   {:redipsid  "9v"}
   {:pdsid     "9v"}
   {:backsid   "9v"}
   {:sidemu    "9v"}])

(def voltage-by-type
  (apply merge voltages))

(defn chipvoltage
  [chipkey sidkey]
  (or (if (= chipkey :real)
        (get voltage-by-type sidkey)
        (get voltage-by-type chipkey))
      "N/A"))

(def presets
  [{:key :single-s1     :label "SINGLE SID (Socket 1)"        :id  0}
   {:key :single-s2     :label "SINGLE SID (Socket 2)"        :id  1}
   {:key :dual-both     :label "DUAL SID (S1 + S2)"           :id  2}
   {:key :dual-s1       :label "DUAL SID (Socket 1 only)"     :id  3}
   {:key :dual-s2       :label "DUAL SID (Socket 2 only)"     :id  4}
   {:key :triple-s1     :label "TRIPLE (Dual S1 + Single S2)" :id  5}
   {:key :triple-s2     :label "TRIPLE (Single S1 + Dual S2)" :id  6}
   {:key :quad          :label "QUAD SID (4 chips)"           :id  7}
   {:key :mirrored      :label "MIRRORED (S2 = S1)"           :id  8}
   {:key :mirrored-dual :label "MIRRORED DUAL"                :id  9}
   {:key :dual-flipped  :label "DUAL FLIPPED"                 :id 10}
   {:key :quad-flipped  :label "QUAD FLIPPED"                 :id 11}
   {:key :quad-mixed    :label "QUAD MIXED"                   :id 12}
   {:key :quad-flipmix  :label "QUAD FLIP+MIX"                :id 13}])

(def preset-by-id
  (into {} (map (fn [p] [(:id p) p]) presets)))

(def preset-by-key
  (into {} (map (fn [p] [(:key p) p]) presets)))

(def initial-config
  {:clock-rate           :pal
   :socket-one           {:chiptype :unknown
                          :voltage  :unknown
                          :sid1     {:id 0    :addr 0x00 :type :unknown}
                          :sid2     {:id 0xFF :addr 0xFF :type :na}
                          :enabled  true
                          :dualsid  false}
   :socket-two           {:chiptype :unknown
                          :voltage  :unknown
                          :sid1     {:id 1    :addr 0x20 :type :unknown}
                          :sid2     {:id 0xFF :addr 0xFF :type :na}
                          :enabled  true
                          :dualsid  false}
   :led                  {:enabled true :idle-breathe false}
   :rgbled               {:enabled false :idle-breathe false :brightness 127 :sid-to-use -1}
   :cdc                  {:enabled true}
   :webusb               {:enabled true}
   :asid                 {:enabled true}
   :midi                 {:enabled true}
   :fmopl                {:sidno 0 :enabled false}
   :external-clock       false
   :lock-clockrate       false
   :stereo-en            false
   :lock-audio-sw        false
   :mirrored             false
   :flipped              false
   :mixed                false
   :need-confirmation    false
   :socket_change_detect true
   :preset_auto_detect   true
   :last-preset          :dual-both ; defaults to 2 dual-both
   :raw-config           nil}) ; save the raw config bytes just in case we need to verify things
