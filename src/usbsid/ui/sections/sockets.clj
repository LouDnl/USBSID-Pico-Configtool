(ns usbsid.ui.sections.sockets
  "Aren't I pretty?"
  (:require
   [usbsid.config-model :as model]
   [usbsid.events :as events]
   [usbsid.ui.sections.common :as common]
   [usbsid.ui.widgets :as w]))


;;; It's in there somewhere!

(defn socket-panel
  "Well, it's me, you caught me, plug me in already!"
  [{:keys [socket-key socket fw-line connected? hover-popup]}]
  (let [label      (if (= socket-key :socket-one) "SOCKET 1" "SOCKET 2")
        chip-types (if (= fw-line :legacy)
                     (filterv (comp model/legacy-supports-chip? :key) model/chip-types)
                     model/chip-types)
        sid-types  model/sid-types]
    {:fx/type     :v-box
     :style-class "c64-section"
     :spacing     6
     :children
     [{:fx/type     :label
       :text        label
       :style-class "c64-section-header"}

      (w/c64-narrow-row "Enabled"
                        (common/toggles
                         :onoff
                         {:fx/type     :toggle-button
                          :text        (if (:enabled socket) "ON " "OFF")
                          :selected    (:enabled socket)
                          :on-action   {:event/type :config-changed
                                        :path       [socket-key :enabled]
                                        :value      (not (:enabled socket))}
                          :style-class "c64-toggle"}
                         {:hover-popup hover-popup
                          :hover-text  "Disable or enable this socket"
                          :popup-key   [socket-key :enabled]}))

      (w/c64-narrow-row "Chiptype"
                        {:fx/type          :combo-box
                         :style-class      ["combo-box" "c64-combo-box"]
                         :items            (mapv :label chip-types)
                         :value            (get-in model/chip-type-by-key [(:chiptype socket) :label])
                         :on-value-changed (fn [v]
                                             (when-let [ct (first (filter (comp #{v} :label) chip-types))]
                                               (try
                                                 (events/config-changed! [socket-key :chiptype] (:key ct))
                                                 (when connected? (events/chiptype-changed! socket-key (:key ct)))
                                                 (catch Exception e e))))})
      (if (and (not= fw-line :legacy) connected?)
        (w/c64-narrow-row "Chip voltage"
                          {:fx/type     :label
                           :text        (model/chipvoltage (:chiptype socket) (get-in socket [:sid1 :type]))
                           :style-class "c64-label-dim"})
        (w/c64-separator))

      (w/c64-narrow-row "Dual SID mode"
                        (common/toggles
                         :onoff
                         {:fx/type     :toggle-button
                          :text        (if (:dualsid socket) "ON " "OFF")
                          :selected    (:dualsid socket)
                          :on-action   {:event/type :config-changed
                                        :path       [socket-key :dualsid]
                                        :value      (not (:dualsid socket))}
                          :style-class "c64-toggle"}
                         {:hover-popup hover-popup
                          :hover-text  "Disable or enable Dual SID (Clone chip only)"
                          :popup-key   [socket-key :dualsid]}))

      (w/c64-narrow-row "SID 1 Type"
                        {:fx/type          :combo-box
                         :style-class      ["combo-box" "c64-combo-box"]
                         :items            (mapv :label sid-types)
                         :value            (get-in model/sid-type-by-key [(get-in socket [:sid1 :type]) :label])
                         :on-value-changed (fn [v]
                                             (when-let [st (first (filter (comp #{v} :label) sid-types))]
                                               (events/config-changed! [socket-key :sid1 :type] (:key st))))})
      (if (and (not= fw-line :legacy) connected?)
        {:fx/type     :h-box
         :style-class "c64-hbox"
         :children
         [{:fx/type     :label
           :text        (format "addr: 0x%02x id: %s"
                                (get-in socket [:sid1 :addr])
                                (if (> (get-in socket [:sid1 :id]) 4)
                                  "n/a"
                                  (get-in socket [:sid1 :id])))
           :style-class ["c64-label-dim" "c64-text-wrap"]
           :min-width   200}]}
        (w/c64-separator))

      (w/c64-narrow-row "SID 2 Type"
                        {:fx/type          :combo-box
                         :style-class      ["combo-box" "c64-combo-box"]
                         :items            (mapv :label sid-types)
                         :value            (get-in model/sid-type-by-key [(get-in socket [:sid2 :type]) :label])
                         :disable          (not (:dualsid socket))
                         :on-value-changed (fn [v]
                                             (when-let [st (first (filter (comp #{v} :label) sid-types))]
                                               (events/config-changed! [socket-key :sid2 :type] (:key st))))})

      (if (and (not= fw-line :legacy) connected?)
        {:fx/type     :h-box
         :style-class "c64-hbox"
         :children
         [{:fx/type     :label
           :text        (format "addr: 0x%02x id: %s"
                                (get-in socket [:sid2 :addr])
                                (if (> (get-in socket [:sid2 :id]) 4)
                                  "n/a"
                                  (get-in socket [:sid2 :id])))
           :style-class ["c64-label-dim" "c64-text-wrap"]
           :min-width   200}]}
        (w/c64-separator))]}))

(defn fmopl-panel
  "I'm good at making noises"
  [config & {:keys [hover-popup]}]
  {:fx/type :h-box
   :spacing 12
   :children
   [{:fx/type     :v-box
     :style-class "c64-section"
     :spacing     6
     :children
     [(w/c64-header "FM Opl")
      {:fx/type     :label
       :text        "(Requires clone SID chip)"
       :style-class "c64-label-dim"}

      (w/c64-narrow-row "FMOpl Enabled"
                        (common/toggles
                         :onoff
                         {:fx/type     :toggle-button
                          :text        (if (get-in config [:fmopl :enabled]) "ON " "OFF")
                          :selected    (get-in config [:fmopl :enabled])
                          :on-action   {:event/type :config-changed
                                        :path       [:fmopl :enabled]
                                        :value      (not (get-in config [:fmopl :enabled]))}
                          :style-class "c64-toggle"}
                         {:hover-popup hover-popup
                          :hover-text  "Disable or enable FMOpl, after saving the SID No. will automatically be detected if needed"
                          :popup-key   [:fmopl :dualsid]}))

      (w/c64-narrow-row "FMOpl SID No."
                        {:fx/type     :label
                         :text        (:label
                                       (first
                                        (filter
                                         (if (get-in config [:fmopl :enabled])
                                           (comp #{(get-in config [:fmopl :sidno])} :key)
                                           (comp #{0} :key))
                                         model/fmopl-sid-options)))
                         :style-class "c64-label-dim"})]}]})

(defn preset-panel
  "If your lazy like, kinda..."
  [config connection]
  (let [fw        (get-in connection [:fw-version])
        pcb       (get-in connection [:pcb-version])
        fwverint  (model/parse-fw-version fw)
        pcbverint (model/parse-pcb-version pcb)]
    {:fx/type :h-box
     :spacing 12
     :children
     [{:fx/type     :v-box
       :style-class "c64-section"
       :spacing     6
       :children
       (cond->
        [{:fx/type     :label
          :text        "(!) Presets are automatically applied and saved to flash."
          :style-class ["c64-warning-text" "c64-text-wrap"]}

         (w/c64-row "Preset"
                    {:fx/type          :combo-box
                     :style-class      ["combo-box" "c64-combo-box-wide"]
                     ; override combo-box width width and force computed size
                     :style            "-fx-pref-width: -1; -fx-min-width: -1; -fx-max-width: -1;"
                     :items            (mapv :label model/presets)
                     :value            (get-in model/preset-by-key [(:last-preset config) :label])
                     :on-value-changed (fn [v]
                                         (prn v)
                                         (when-let [p (first (filter (comp #{v} :label) model/presets))]
                                           (prn p)
                                           (events/config-changed! [:last-preset] (:key p))))})

         {:fx/type     :label
          :text        "During apply an auto detection will run as validation"
          :style-class ["c64-label-dim" "c64-text-wrap"]}

         {:fx/type     :h-box
          :style-class "c64-hbox"
          :spacing     8
          :children
          [(w/c64-button "APPLY PRESET"
                         {:event/type :apply-preset
                          :preset     (:last-preset config)})]}]
         (and
          (>= fwverint 70)
          (>= pcbverint 15))
         (conj
          {:fx/type     :label
          :text        "(!v1.5+) Presets will only persist reboots if you disable 'automatic socket change detection' and 'preset silent auto detect'!"
          :style-class ["c64-error-text" "c64-text-wrap"]}))}]}))

(defn sockets-section
  "I hold the one true specials!"
  [{:keys [config connection hover-popup]}]
  (let [fw-line    (or (:fw-line connection) :v0_7)
        connected? (= :connected (:status connection))]
    {:fx/type     :v-box
     :style-class "c64-vbox"
     :spacing     12
     :padding     {:top    8
                   :right  8
                   :bottom 8
                   :left   8}
     :children
     [(w/c64-header "SOCKET CONFIGURATION")

      {:fx/type :h-box
       :spacing 12
       :children
       [(socket-panel {:socket-key  :socket-one
                       :socket      (:socket-one config)
                       :connected?  connected?
                       :fw-line     fw-line
                       :hover-popup hover-popup})
        (socket-panel {:socket-key  :socket-two
                       :socket      (:socket-two config)
                       :connected?  connected?
                       :fw-line     fw-line
                       :hover-popup hover-popup})]}

      (w/c64-separator)
      (fmopl-panel config {:hover-popup hover-popup})

      (w/c64-separator)
      (w/c64-header "SOCKET PRESET")
      (preset-panel config connection)]}))
