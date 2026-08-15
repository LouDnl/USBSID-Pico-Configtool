(ns usbsid.driver
  "Will drive you to the chiptune garden!"
  (:require
   [clojure.string :as string]
   [usbsid.logging :as logging]
   [usbsid.state :as state]
   [usbsid.config-model :as model])
  (:import
   [usbsid USBSID USBSIDDevice Config$Cfg Config$CLK Cmd]))


;;; Only just this once, I promise!

(defonce ^:private drv-atom (atom nil))


;;; Byte helpers

(defn b->int [b] (Byte/toUnsignedInt b))

(defn ba-get [^bytes arr i] (b->int (aget arr i)))

(defn cfg-byte [v] (unchecked-byte (bit-and v 0xff)))

(defn- sid-addr [id]
  (case (int id) 0 0x00, 1 0x20, 2 0x40, 3 0x60, 0xFF))


;;; Parsers

(defmulti parse-config-bytes*
  "Parse the 64-byte config response into a config map. Dispatches on the
   firmware-line keyword (`:v0_7` / `:legacy`). See PROJECT.md §9.0.2."
  (fn [fw-line _buf] fw-line))

(defmethod parse-config-bytes* :v0_7
  [_ ^bytes buf]
  (let [g         (fn [i] (ba-get buf i)) ; get byte from buffer
        clock-val (bit-or (bit-shift-left (g 7) 16)
                          (bit-shift-left (g 8) 8)
                          (g 9))
        clk-enum  (Config$CLK/getCLK (int clock-val))
        clk-id    (if clk-enum (Config$CLK/clkID clk-enum) 1)
        s1-id1    (bit-and (g 13) 0xF)
        s1-id2    (bit-shift-right (bit-and (g 13) 0xF0) 4)
        s2-id1    (bit-and (g 24) 0xF)
        s2-id2    (bit-shift-right (bit-and (g 24) 0xF0) 4)]
    {; future use
     :raw-config           buf
     ; states
     :need-confirmation    (pos? (g 2)) ; Need configuration confirmation v1.5+ only
     :socket_change_detect (pos? (g 3)) ; Disable socket change detection v1.5+ only
     :preset_auto_detect   (pos? (bit-and (g 4) 0x80))
     :last-preset          (:key (get model/preset-by-id (bit-and (g 4) 0x7f)))
     ; Clockworx
     :lock-clockrate       (pos? (g 5))
     :external-clock       (pos? (g 6))
     :clock-rate           (:key (get model/clock-rate-by-id clk-id {:key :pal}))
     ; SocketOne
     :socket-one           {:enabled  (pos? (g 10))
                            :dualsid  (pos? (g 11))
                            :chiptype (:key (get model/chip-type-by-id (g 12) {:key :unknown}))
                            :sid1     {:id   s1-id1
                                       :addr (sid-addr s1-id1)
                                       :type (:key (get model/sid-type-by-id (g 14) {:key :unknown}))}
                            :sid2     {:id   s1-id2
                                       :addr (sid-addr s1-id2)
                                       :type (:key (get model/sid-type-by-id (g 15) {:key :unknown}))}}
     ; SocketTwo
     :socket-two           {:enabled  (pos? (g 20))
                            :dualsid  (pos? (g 21))
                            :chiptype (:key (get model/chip-type-by-id (g 23) {:key :unknown}))
                            :sid1     {:id   s2-id1
                                       :addr (sid-addr s2-id1)
                                       :type (:key (get model/sid-type-by-id (g 25) {:key :unknown}))}
                            :sid2     {:id   s2-id2
                                       :addr (sid-addr s2-id2)
                                       :type (:key (get model/sid-type-by-id (g 26) {:key :unknown}))}}
     ; Bright light, bright light!!
     :led                  {:enabled      (pos? (g 30))
                            :idle-breathe (pos? (g 31))}
     ; I can see colors!
     :rgbled               {:enabled      (pos? (g 40))
                            :idle-breathe (pos? (g 41))
                            :brightness   (g 42)
                            :sid-to-use   (let [v (g 43)] (if (or (zero? v) (= v 0xFF)) -1 v))}
     ; Stuffs
     :cdc                  {:enabled (pos? (g 51))}
     :webusb               {:enabled (pos? (g 52))}
     :asid                 {:enabled (pos? (g 53))}
     :midi                 {:enabled (pos? (g 54))}
     ; Stuff for keyboard people
     :fmopl                {:enabled (pos? (g 55))
                            :sidno   (g 56)}
     ; Headphonez on!
     :stereo-en            (pos? (g 57))
     :lock-audio-sw        (pos? (g 58))
     :mirrored             (pos? (bit-and (g 60) 0x1))
     :flipped              (pos? (bit-and (g 60) 0x2))
     :mixed                (pos? (bit-and (g 60) 0x4))}))

(defmethod parse-config-bytes* :legacy
  [_ ^bytes buf]
  ; v0.5.0 / v0.6.0 / v0.6.4 firmware byte layout. Differences vs v0.7.x:
  ; - byte 2, 3, 60 are unused (need-confirmation / socket_change_detect / mirror-flip-mix don't exist).
  ; - byte 4 is unused (preset_auto_detect and last_preset don't exist)
  ; - bytes 13 + 24 store clonetype (NOT packed SID-id nibbles). SID ids are implicit (0..3).
  ; - byte 22 stores the (global) mirrored flag.
  ; - chiptype byte 12/23 is the 3-value legacy enum (real/clone/unknown); combine with clonetype
  ;   byte to map onto the unified flat chip-types model via model/legacy->flat-chip.
  (let [g         (fn [i] (ba-get buf i))
        clock-val (bit-or (bit-shift-left (g 7) 16)
                          (bit-shift-left (g 8) 8)
                          (g 9))
        clk-enum  (Config$CLK/getCLK (int clock-val))
        clk-id    (if clk-enum (Config$CLK/clkID clk-enum) 1)]
    {:raw-config           buf
     :need-confirmation    false
     :socket_change_detect false
     :lock-clockrate       (pos? (g 5))
     :external-clock       (pos? (g 6))
     :clock-rate           (:key (get model/clock-rate-by-id clk-id {:key :pal}))
     :socket-one           {:enabled  (pos? (g 10))
                            :dualsid  (pos? (g 11))
                            :chiptype (model/legacy->flat-chip (g 12) (g 13))
                            :sid1     {:id   0
                                       :addr (sid-addr 0)
                                       :type (:key (get model/sid-type-by-id (g 14) {:key :unknown}))}
                            :sid2     {:id   1
                                       :addr (sid-addr 1)
                                       :type (:key (get model/sid-type-by-id (g 15) {:key :unknown}))}}
     :socket-two           {:enabled  (pos? (g 20))
                            :dualsid  (pos? (g 21))
                            :chiptype (model/legacy->flat-chip (g 23) (g 24))
                            :sid1     {:id   2
                                       :addr (sid-addr 2)
                                       :type (:key (get model/sid-type-by-id (g 25) {:key :unknown}))}
                            :sid2     {:id   3
                                       :addr (sid-addr 3)
                                       :type (:key (get model/sid-type-by-id (g 26) {:key :unknown}))}}
     :led                  {:enabled      (pos? (g 30))
                            :idle-breathe (pos? (g 31))}
     :rgbled               {:enabled      (pos? (g 40))
                            :idle-breathe (pos? (g 41))
                            :brightness   (g 42)
                            :sid-to-use   (let [v (g 43)] (if (or (zero? v) (= v 0xFF)) -1 v))}
     :cdc                  {:enabled (pos? (g 51))}
     :webusb               {:enabled (pos? (g 52))}
     :asid                 {:enabled (pos? (g 53))}
     :midi                 {:enabled (pos? (g 54))}
     :fmopl                {:enabled (pos? (g 55))
                            :sidno   (g 56)}
     :stereo-en            (pos? (g 57))
     :lock-audio-sw        (pos? (g 58))
     :mirrored             (pos? (g 22))
     :flipped              false
     :mixed                false}))

(defn parse-config-bytes
  "Parse the 64-byte config response. Single-arg form (legacy callsites + tests)
   assumes the current `:v0_7` byte layout. Pass `fw-line` to dispatch on a
   different firmware schema (`:legacy` for v0.5.x / v0.6.x)."
  ([buf]         (parse-config-bytes* :v0_7 buf))
  ([fw-line buf] (parse-config-bytes* fw-line buf)))


;;; Write config helpers

(def config-path->command-id ; gets config->commands location id in list (fw v0.7.0+ only!)
  {[:clock-rate]             0
   [:lock-clockrate]         0
   [:socket-one :enabled]    1
   [:socket-one :dualsid]    2
   [:socket-one :chiptype]   3
   [:socket-one :sid1 :type] 4
   [:socket-one :sid2 :type] 5
   [:socket-two :enabled]    6
   [:socket-two :dualsid]    7
   [:socket-two :chiptype]   8
   [:socket-two :sid1 :type] 9
   [:socket-two :sid2 :type] 10
   [:led :enabled]           11
   [:led :idle-breathe]      12
   [:rgbled :enabled]        13
   [:rgbled :idle-breathe]   14
   [:rgbled :brightness]     15
   [:rgbled :sid-to-use]     16
   [:asid :enabled]          17
   [:midi :enabled]          18
   [:fmopl :enabled]         19
   [:stereo-en]              20
   [:lock-audio-sw]          21
   [:mirrored]               22
   [:flipped]                23
   [:mixed]                  24
   [:socket_change_detect]   25
   [:preset_auto_detect]     26})

(defmulti config->commands*
  "Convert a config map to a seq of [section item value] triples for SET_CONFIG.
   Dispatch on firmware-line keyword (`:v0_7` / `:legacy`). See PROJECT.md §9.0.2."
  (fn [fw-line _cfg] fw-line))

(defmethod config->commands* :v0_7
  [_ {:keys [singles] :as cfg}]
  (let [cfg (if singles
              (model/deep-merge model/initial-config cfg)
              cfg)
        s1  (:socket-one cfg)
        s2  (:socket-two cfg)
        led (:led cfg)
        rgb (:rgbled cfg)]
    [[0x0 (:id (get model/clock-rate-by-key (:clock-rate cfg) {:id 1}))
      (if (:lock-clockrate cfg) 1 0)] ; 0
     [0x1 0x0 (if (:enabled s1) 1 0)] ; 1
     [0x1 0x1 (if (:dualsid s1) 1 0)] ; 2
     [0x1 0x2 (:id (get model/chip-type-by-key (:chiptype s1) {:id 1}))] ; 3
     [0x1 0x4 (:id (get model/sid-type-by-key (get-in s1 [:sid1 :type]) {:id 0}))] ; 4
     [0x1 0x5 (:id (get model/sid-type-by-key (get-in s1 [:sid2 :type]) {:id 0}))] ; 5
     [0x2 0x0 (if (:enabled s2) 1 0)] ; 6
     [0x2 0x1 (if (:dualsid s2) 1 0)] ; 7
     [0x2 0x2 (:id (get model/chip-type-by-key (:chiptype s2) {:id 1}))] ; 8
     [0x2 0x4 (:id (get model/sid-type-by-key (get-in s2 [:sid1 :type]) {:id 0}))] ; 9
     [0x2 0x5 (:id (get model/sid-type-by-key (get-in s2 [:sid2 :type]) {:id 0}))] ; 10
     [0x3 0x0 (if (:enabled led) 1 0)] ; 11
     [0x3 0x1 (if (:idle-breathe led) 1 0)] ; 12
     [0x4 0x0 (if (:enabled rgb) 1 0)] ; 13
     [0x4 0x1 (if (:idle-breathe rgb) 1 0)] ; 14
     [0x4 0x2 (:brightness rgb)] ; 15
     [0x4 0x3 (max 0 (:sid-to-use rgb))] ; 16
     [0x7 (if (:asid cfg) 1 0) 0] ; 17
     [0x8 (if (:midi cfg) 1 0) 0] ; 18
     [0x9 (if (get-in cfg [:fmopl :enabled]) 1 0) 0] ; 19
     [0xA (if (:stereo-en cfg) 1 0) 0] ; 20
     [0xB (if (:lock-audio-sw cfg) 1 0) 0] ; 21
     [0xC (if (:mirrored cfg) 1 0) 0] ; 22
     [0xD (if (:flipped cfg) 1 0) 0] ; 23
     [0xE (if (:mixed cfg) 1 0) 0] ; 24
     [0xF (if (:socket_change_detect cfg) 1 0) 0] ; 25
     [0x10 (if (:preset_auto_detect cfg) 1 0) 0]])) ; 26

(defn- legacy-socket-cmds
  "SET_CONFIG triples for one legacy-firmware socket.
   `section` = 0x1 (s1) or 0x2 (s2). Drops chip writes whose flat key has
   no legacy {chiptype, clonetype} mapping (`:arm2sid`, `:pdsid`, `:backsid`,
   `:sidemu`) - those would silently no-op on the firmware. Caller logs the
   drop via the writer-level guard in `send-config!`."
  [section sock]
  (let [chip-key (:chiptype sock)
        legacy   (model/flat-chip->legacy chip-key)]
    (cond-> [[section 0x0 (if (:enabled sock) 1 0)]
             [section 0x1 (if (:dualsid sock) 1 0)]]
      legacy (conj [section 0x2 (:chiptype legacy)]
                   [section 0x3 (:clonetype legacy)])
      true   (conj [section 0x4 (:id (get model/sid-type-by-key (get-in sock [:sid1 :type]) {:id 0}))]
                   [section 0x5 (:id (get model/sid-type-by-key (get-in sock [:sid2 :type]) {:id 0}))]))))

(defmethod config->commands* :legacy
  [_ cfg]
  ; v0.5.0 / v0.6.0 / v0.6.4 SET_CONFIG grid (config.c:467-633). Differences vs v0.7:
  ; - chip class needs the split `chiptype` (0x_/0x2) + `clonetype` (0x_/0x3) writes.
  ; - global `mirrored` goes through `[0x2 0x6 v]`, NOT `[0xC v 0]`.
  ; - sections 0xD (flipped) and 0xE (mixed) don't exist; omit the writes.
  (let [s1  (:socket-one cfg)
        s2  (:socket-two cfg)
        led (:led cfg)
        rgb (:rgbled cfg)]
    (concat
     [[0x0 (:id (get model/clock-rate-by-key (:clock-rate cfg) {:id 1}))
       (if (:lock-clockrate cfg) 1 0)]]
     (legacy-socket-cmds 0x1 s1)
     (legacy-socket-cmds 0x2 s2)
     [[0x2 0x6 (if (:mirrored cfg) 1 0)]
      [0x3 0x0 (if (:enabled led) 1 0)]
      [0x3 0x1 (if (:idle-breathe led) 1 0)]
      [0x4 0x0 (if (:enabled rgb) 1 0)]
      [0x4 0x1 (if (:idle-breathe rgb) 1 0)]
      [0x4 0x2 (:brightness rgb)]
      [0x4 0x3 (max 0 (:sid-to-use rgb))]
      [0x9 (if (get-in cfg [:fmopl :enabled]) 1 0) 0]
      [0xA (if (:stereo-en cfg) 1 0) 0]
      [0xB (if (:lock-audio-sw cfg) 1 0) 0]])))

(defn config->commands
  "Single-arg form (legacy callsites + tests) targets the current `:v0_7`
   firmware. Pass `fw-line` to target the legacy schema (`:legacy`)."
  ([cfg]         (config->commands* :v0_7 cfg))
  ([fw-line cfg] (config->commands* fw-line cfg)))

(defn- set-cfg
  "Wrapper around .USBSID_sendconfigcommand"
  [& {:keys [a b c d]
      :or {a 0 b 0 c 0 d 0}}]
  (.USBSID_sendconfigcommand
   @drv-atom
   (bit-and (.get Config$Cfg/SET_CONFIG) 0xff)
   (into-array Byte [(cfg-byte a) (cfg-byte b) (cfg-byte c) (cfg-byte d)])))

(defn- current-fw-line
  "Read the firmware-line keyword from connection state. Defaults to `:v0_7`
   when no board is connected (preview / dry-run from tests stays on the
   newest schema)."
  []
  (get-in @state/*state [:connection :fw-line] :v0_7))

(defn send-config!
  "Write all settings from the supplied config, 1 write per setting.
   Dispatch on the connected board's firmware-line keyword."
  [cfg]
  (let [fw-line (current-fw-line)]
    (when (= fw-line :legacy)
      (doseq [sk [:socket-one :socket-two]
              :let [chip (get-in cfg [sk :chiptype])]
              :when (and chip (not (model/legacy-supports-chip? chip)))]
        (state/log! (format "send-config! skipping %s chiptype %s - no v0.5/0.6 equivalent"
                            (name sk) (name chip)))))
    (doall
     (doseq [[a b c] (config->commands fw-line cfg)]
       (try
         (set-cfg {:a a :b b :c c})
         (catch Exception e
           (state/log! (format "send-config! error [0x%X 0x%X 0x%X]: %s" a b c (.getMessage e)))
           (throw e)))))))


;;; Internal driver API

(defn- with-driver
  "Throwable catch wrapper around driver functions.
   Catches Error (e.g. UnsatisfiedLinkError on macOS hardened-runtime dylib load fails)
   so the future doesn't die silently."
  [label f]
  (future
    (try
      (f)
      (catch Throwable t
        (logging/severe (str label " error") t)
        (state/log! (str label " error: " (.getName (class t)) " - " (.getMessage t)))))))

(defn- valid-fw-version?
  "FW version looks like '0.6.0-BETA.20250101', starts with 'digit', length > 3."
  [s]
  (and (string? s) (> (count s) 3) (string/includes? s ".") (not (string/includes? s "ERROR"))))

(defn- valid-pcb-version?
  "PCB version looks like '1.5', has a dot, length > 2."
  [s]
  (and (string? s) (> (count s) 2) (string/includes? s ".") (not (string/includes? s "ERROR"))))

(defn- read-with-retry
  "Reads can sometimes return garbage.
   Call read-fn up to max-attempts times, sleeping delay-ms between each,
   until valid-fn returns true. Returns last result regardless."
  [read-fn valid-fn max-attempts delay-ms]
  (loop [n max-attempts]
    (let [result (try (read-fn)
                      (catch Exception e
                        (state/log! (str "Read error: " (.getClass e) " - " (.getMessage e)))
                        nil))]
      (cond
        (valid-fn result) result
        (pos? n)          (do (Thread/sleep delay-ms) (recur (dec n)))
        :else             result))))

(defn- valid-config-response?
  "Firmware sends: result[0]=0x30 (initiator), result[1]=0x7F (verification), result[63]=0xFF (terminator)."
  [^bytes b]
  (and b
       (= (alength b) 64)
       (= (Byte/toUnsignedInt (aget b 0))  0x30)
       (= (Byte/toUnsignedInt (aget b 1))  0x7F)
       (= (Byte/toUnsignedInt (aget b 63)) 0xFF)))

(defn- valid-config-slice?
  "True if the given byte-array slice [off..off+63] is a valid READ_CONFIG
   response: initiator 0x30, verification 0x7F, terminator 0xFF at off+63."
  [^bytes buf off]
  (and (<= (+ off 64) (alength buf))
       (= (Byte/toUnsignedInt (aget buf off))         0x30)
       (= (Byte/toUnsignedInt (aget buf (+ off 1)))   0x7F)
       (= (Byte/toUnsignedInt (aget buf (+ off 63)))  0xFF)))

(defn- do-read-config
  "Read the complete configuration and return a 64-byte Byte array.
   v0.7+ firmware sends a single 64-byte packet. v0.5/v0.6 firmware
   (`config.c` `case READ_CONFIG`) loops `write_back_data(64)` four times,
   sending 4 × 64-byte packets back-to-back. The valid header is in the
   FIRST packet but the host's IN endpoint can deliver them in any order
   depending on URB queueing - if the host reads only one 64-byte URB it
   may land on the trailing zero packet and the parsed config silently
   reads as all zeroes. Drain the full 4-packet window on legacy boards
   and scan 64-byte slices for the valid `[0x30 0x7F ... 0xFF]` header."
  [fw-line]
  (let [read-len (if (= fw-line :legacy) 256 64)
        result   (.USBSID_rwconfigcommand
                  @drv-atom
                  (bit-and (.get Config$Cfg/READ_CONFIG) 0xff)
                  (int read-len)
                  (into-array Byte [(byte 0)]))]
    (cond
      (nil? result)           result
      (= (alength result) 64) result
      :else
      (let [off (->> (range 0 (- (alength result) 63) 64)
                     (filter (partial valid-config-slice? result))
                     first)]
        (if off
          (java.util.Arrays/copyOfRange ^bytes result (int off) (int (+ off 64)))
          ; No valid slice found - return first 64 bytes so the
          ; valid-config-response? guard in read-config! reports a clean
          ; "invalid response after retries" instead of a silent zeroed config.
          (java.util.Arrays/copyOfRange ^bytes result 0 64))))))

(defn- do-read-pcbversion
  "Read, verify and return PCB version"
  []
  (let [rawread    (.USBSID_rwconfigcommand
                    @@#'usbsid.driver/drv-atom
                    (bit-and (.get usbsid.Config$Cfg/US_PCB_VERSION) 0xff)
                    (int 64)
                    (into-array Byte [(byte 0)]))
        rawversion (java.util.Arrays/copyOfRange rawread 2 (+ 2 (Byte/toUnsignedInt (aget rawread 1))))]
    (if (= (aget rawread 0) (.get usbsid.Config$Cfg/US_PCB_VERSION))
      (String. rawversion)
      "0.0-ERROR")))

(defn- do-read-fwversion
  "Read, verify and return FW version"
  []
  (let [rawread    (.USBSID_rwconfigcommand
                    @drv-atom
                    (bit-and (.get Config$Cfg/USBSID_VERSION) 0xff)
                    (int 64)
                    (into-array Byte [(byte 0)]))
        rawversion (java.util.Arrays/copyOfRange rawread 2 (+ 2 (Byte/toUnsignedInt (aget rawread 1))))]
    (if (= (aget rawread 0) (.get usbsid.Config$Cfg/USBSID_VERSION))
      (String. rawversion)
      "0.0.0-ERROR")))


;;; Public driver API

(defn connected? [] (USBSIDDevice/isOpen))

(defn read-config-command
  "Read from USBSID config after a write config command"
  [cmd & {:keys [a b c d]
          :or   {a 0 b 0 c 0 d 0}}]
  (let [result (.USBSID_rwconfigcommand
                @drv-atom
                cmd
                (int 64)
                (into-array Byte [(cfg-byte a) (cfg-byte b) (cfg-byte c) (cfg-byte d)]))]
    result
    #_(cond
        (nil? result)           result
        (= (alength result) 64) result
        :else
        (let [off (->> (range 0 (- (alength result) 63) 64)
                       (filter (partial valid-config-slice? result))
                       first)]
          (if off
            (java.util.Arrays/copyOfRange ^bytes result (int off) (int (+ off 64)))
            (java.util.Arrays/copyOfRange ^bytes result 0 64))))))

(defn read-config!
  "Read USBSID-Pico configuration. Dispatch parser on connected board's
   firmware-line so v0.5/v0.6 boards get the legacy byte map (PROJECT.md §9.0)."
  []
  (with-driver "Read config"
    (fn []
      (state/log! "Reading configuration")
      (let [fw-line (current-fw-line)
            result  (read-with-retry #(do-read-config fw-line) valid-config-response? 3 50)]
        (if (valid-config-response? result)
          (let [cfg (parse-config-bytes fw-line result)]
            (swap! state/*state assoc :config cfg :dirty false)
            (swap! state/*state assoc-in [:config :raw-config] result)
            (swap! state/*state assoc-in [:connection :config-status] :loaded)
            (swap! state/*state assoc-in [:connection :chipone] (get-in cfg [:socket-one :chiptype]))
            (swap! state/*state assoc-in [:connection :chiptwo] (get-in cfg [:socket-two :chiptype]))
            ; v1.5-only confirmation flow; legacy firmware never raises it
            ; (parser hard-sets `:need-confirmation false`) so the gate doubles
            ; as a fw capability check.
            (when (and (model/fw-supports? fw-line :need-confirmation)
                       (:need-confirmation cfg))
              (state/set-section! :sockets))
            (state/log! "Configuration loaded."))
          (state/log! "Read config: invalid response after retries"))))))

(defn connect!
  "Connect to USBSID-Pico"
  []
  (with-driver "Connect"
    (fn []
      (reset! drv-atom (USBSID.))
      (state/log! "Connecting to USBSID-Pico")
      (let [backend (if (USBSIDDevice/isWinblows) "libusb-winusb" "usbx")
            _       (USBSIDDevice/setdriver_USBSID backend)
            _       (state/log! (str "Backend set: " backend " - calling USBSID_init"))
            t0      (System/currentTimeMillis)
            result  (.USBSID_init @drv-atom)
            elapsed (- (System/currentTimeMillis) t0)
            _       (state/log! (format "USBSID_init returned %d in %dms" result elapsed))]
        (if (zero? result)
          (let [fw  (read-with-retry #(do-read-fwversion) valid-fw-version? 3 50)
                pcb (read-with-retry #(do-read-pcbversion) valid-pcb-version? 3 50)]
            (state/set-connection! :connected fw pcb)
            (state/log! (format "Connected! FW: v%s PCB: v%s" fw pcb))
            (when (= (current-fw-line) :legacy)
              (state/log!
               "Legacy firmware mode (v0.5/v0.6) - flipped / mixed / socket_change_detect / preset_auto_detect controls disabled"))
            @(read-config!))
          (do (state/set-connection! :disconnected nil nil)
              (state/log! "Connection failed - device not found")))))))

(defn disconnect!
  "Disconnect from USBSID-Pico"
  []
  (with-driver "Disconnect"
    (fn []
      (state/log! "Disconnecting")
      ; Don't care for Exceptions on disconnect, stops flow otherwise
      (try (.USBSID_exit @drv-atom) (catch Exception _ nil))
      (state/set-connection! :disconnected nil nil) ; Will also set config-loaded to nothing
      (reset! drv-atom nil)
      (state/log! "Disconnected."))))

(defn set-config!
  "Set (write) a single config item directly on change fw v0.7+ only"
  [path value]
  (with-driver "Set config"
    (fn []
      (let [cfgitem         (cond-> {}
                              (= (count path) 1)
                              (assoc (first path) value)
                              (= (count path) 2)
                              (assoc-in [(first path)
                                         (second path)]
                                        value)
                              (= (count path) 3)
                              (assoc-in [(first path)
                                         (second path)
                                         (last path)]
                                        value)
                              :always
                              (assoc :singles true))
            commandlocation (get config-path->command-id path)
            cfgvec          (when (integer? commandlocation)
                              (config->commands* :v0_7 cfgitem))
            [a b c]         (when (integer? commandlocation)
                              (nth cfgvec commandlocation))]

        (when (integer? commandlocation)
         (try
          (state/log! (str "Set configuration item: " (dissoc cfgitem :singles)))
          (set-cfg {:a a :b b :c c})
          (catch Exception e
            (state/log! (format "send-config! error [0x%X 0x%X 0x%X]: %s" a b c (.getMessage e)))
            (throw e))))))))

(defn write-config!
  "Write the current configuration to USBSID-Pico"
  []
  (with-driver "Write config"
    (fn []
      (state/log! "Writing configuration")
      ; Change state _before_ writing the config in case of single thrown set_config exception
      (swap! state/*state assoc-in [:connection :config-status] :written)
      (send-config! (:config @state/*state))
      (state/log! "Configuration written."))))

(defn save-config!
  "Send a save to flash command to USBSID-Pico"
  []
  (with-driver "Save config"
    (fn []
      (state/log! "Saving configuration (no reboot)")
      (.USBSID_sendconfigcommand
       @drv-atom
       (bit-and (.get Config$Cfg/SAVE_NORESET) 0xff)
       (into-array Byte [(cfg-byte 0)]))
      (swap! state/*state assoc :dirty false)
      (swap! state/*state assoc-in [:connection :config-status] :saved)
      (state/log! "Configuration saved."))))

(defn save-reboot!
  "Send a save to flash and reboot command to USBSID-Pico"
  []
  (with-driver "Save+reboot"
    (fn []
      (state/log! "Saving and rebooting board")
      (.USBSID_sendconfigcommand
       @drv-atom
       (bit-and (.get Config$Cfg/SAVE_CONFIG) 0xff)
       (into-array Byte [(cfg-byte 0)]))
      (disconnect!)
      (state/log! "Saved. Board rebooting - reconnect when ready."))))

(defn reset-config!
  "Send a reset config to default command to USBSID-Pico"
  []
  (with-driver "Reset config"
    (fn []
      (state/log! "Resetting to default configuration")
      (.USBSID_sendconfigcommand
       @drv-atom
       (bit-and (.get Config$Cfg/RESET_CONFIG) 0xff)
       (into-array Byte [(cfg-byte 0)]))
      (state/log! "Reset to defaults.")
      (Thread/sleep 500) ; Sleep a while, stay forever?
      @(read-config!))))

(defn apply-config!
  "Send apply config command to USBSID-Pico"
  []
  (with-driver "Apply config"
    (fn []
      (state/log! "Applying configuration (no save)")
      (.USBSID_sendconfigcommand
       @drv-atom
       (bit-and (.get Config$Cfg/APPLY_CONFIG) 0xff)
       (into-array Byte [(cfg-byte 0)]))
      (swap! state/*state assoc-in [:connection :config-status] :applied)
      (state/log! "Configuration applied."))))

(defn reload-config!
  []
  (with-driver "Reload config"
    (fn []
      (state/log! "Reloading configuration from flash")
      (.USBSID_sendconfigcommand
       @drv-atom
       (bit-and (.get Config$Cfg/RELOAD_CONFIG) 0xff)
       (into-array Byte [(cfg-byte 0)]))
      (Thread/sleep 200)
      @(read-config!)
      (state/log! "Configuration reloaded from flash."))))

(defn confirm-config!
  "Send CONFIG_ACK (0xFA), enables socket power regulators on v1.5+ boards.
   Clears need_confirmation flag after ACK."
  []
  (with-driver "Confirm config"
    (fn []
      (state/log! "Sending CONFIG_ACK to board")
      (.USBSID_sendconfigcommand
       @drv-atom
       (bit-and (.get Config$Cfg/CONFIG_ACK) 0xff)
       (into-array Byte [(cfg-byte 0)]))
      (state/set-config-value! [:need-confirmation] false)
      (swap! state/*state assoc-in [:connection :config-status] :confirmed)
      (state/log! "Configuration acknowledged. Socket power enabled."))))

(defn auto-detect!
  "Run AUTO_DETECT (0x5B): detects chip + SID types, saves, applies. Doesn't reboot!"
  []
  (with-driver "Auto detect"
    (fn []
      (state/log! "Starting auto-detection (chip + SID types)")
      (.USBSID_sendconfigcommand
       @drv-atom
       (bit-and (.get Config$Cfg/AUTO_DETECT) 0xff)
       (into-array Byte [(cfg-byte 0)]))
      (Thread/sleep 3000)
      (state/log! "Auto-detection complete. Reading updated config")
      @(read-config!))))

(defn detect-sids!
  "Send SID detect command to USBSID-Pico"
  []
  (with-driver "Detect SIDs"
    (fn []
      (state/log! "Detecting SID types (this may take a moment)")
      (.USBSID_sendconfigcommand
       @drv-atom
       (bit-and (.get Config$Cfg/DETECT_SIDS) 0xff)
       (into-array Byte [(cfg-byte 0)]))
      (Thread/sleep 3000)
      (state/log! "Detection complete.")
      @(read-config!))))

(defn detect-clones!
  "Send Clone detect command to USBSID-Pico"
  []
  (with-driver "Detect clones"
    (fn []
      (state/log! "Detecting clone SID types")
      (.USBSID_sendconfigcommand
       @drv-atom
       (bit-and (.get Config$Cfg/DETECT_CLONES) 0xff)
       (into-array Byte [(cfg-byte 0)]))
      (Thread/sleep 2000)
      (state/log! "Clone detection complete.")
      @(read-config!))))

(defn test-all-sids!
  "Send test all SIDs command to USBSID-Pico"
  []
  (with-driver "Test all SIDs"
    (fn []
      (state/log! "Running test on all SIDs (long)")
      (.USBSID_sendconfigcommand
       @drv-atom
       (bit-and (.get Config$Cfg/TEST_ALLSIDS) 0xff)
       (into-array Byte [(cfg-byte 0)])))))

(defn test-sid!
  "Run test on SID n (1-4)."
  [n]
  (with-driver (str "Test SID " n)
    (fn []
      (state/log! (str "Running test on SID " n " (long)"))
      (let [cmd (case n
                  1 Config$Cfg/TEST_SID1
                  2 Config$Cfg/TEST_SID2
                  3 Config$Cfg/TEST_SID3
                  4 Config$Cfg/TEST_SID4
                  Config$Cfg/TEST_SID1)]
        (.USBSID_sendconfigcommand
         @drv-atom
         (bit-and (.get cmd) 0xff)
         (into-array Byte [(cfg-byte 0)]))))))

(defn stop-tests!
  "Send stop tests command to USBSID-Pico"
  []
  (with-driver "Stop tests"
    (fn []
      (state/log! "Stopping all SID tests")
      (.USBSID_sendconfigcommand
       @drv-atom
       (bit-and (.get Config$Cfg/STOP_TESTS) 0xff)
       (into-array Byte [(cfg-byte 0)]))
      (state/log! "Tests stopped."))))

(defn restart-bus!
  "Send restart bus command to USBSID-Pico"
  []
  (with-driver "Restart bus"
    (fn []
      (state/log! "Restarting DMA & PIO bus")
      (.USBSID_sendconfigcommand
       @drv-atom
       (bit-and (.get Config$Cfg/RESTART_BUS) 0xff)
       (into-array Byte [(cfg-byte 0)]))
      (state/log! "Bus restarted."))))

(defn restart-clk!
  "Send restart clock command to USBSID-Pico"
  []
  (with-driver "Restart clock"
    (fn []
      (state/log! "Restarting PIO clocks")
      (.USBSID_sendconfigcommand
       @drv-atom
       (bit-and (.get Config$Cfg/RESTART_BUS_CLK) 0xff)
       (into-array Byte [(cfg-byte 0)]))
      (state/log! "Clock restarted."))))

(defn sync-pios!
  "Send synchronise PIO's command to USBSID-Pico"
  []
  (with-driver "Sync PIOs"
    (fn []
      (state/log! "Syncing PIO clocks")
      (.USBSID_sendconfigcommand
       @drv-atom
       (bit-and (.get Config$Cfg/SYNC_PIOS) 0xff)
       (into-array Byte [(cfg-byte 0)]))
      (state/log! "PIOs synced."))))

(defn reset-sids!
  "Send reset SIDs command to USBSID-Pico"
  []
  (with-driver "Reset SIDs"
    (fn []
      (state/log! "Resetting SID chips")
      (.USBSID_reset @drv-atom (byte 0))
      (state/log! "SIDs reset."))))

(defn reset-mcu!
  "Send reboot command to USBSID-Pico"
  []
  (with-driver "Reset MCU"
    (fn []
      (state/log! "Resetting MCU (USB will disconnect)")
      (.USBSID_sendconfigcommand
       @drv-atom
       (bit-and (.get Config$Cfg/RESET_USBSID) 0xff)
       (into-array Byte [(cfg-byte 0)]))
      (disconnect!)
      (state/log! "MCU reset. Reconnect when ready."))))

(defn bootloader!
  "Send restart too bootloader command to USBSID-Pico"
  []
  (with-driver "Bootloader"
    (fn []
      (state/log! "Entering bootloader mode (USB will disconnect)")
      (USBSIDDevice/sendCommand (.get Cmd/BOOTLOADER) (into-array Byte []))
      (disconnect!)
      (state/log! "Board in bootloader mode. Flash new firmware, then reconnect."))))

(def ^:private preset->cfg
  "Maps preset key to [Config$Cfg cmd-byte arg-byte].
   SINGLE_SID   arg 0=socket1, 1=socket2 (firmware buffer[1]).
   MIRRORED_SID arg 0=single,  1=dual    (firmware buffer[1])."
  {:single-s1     [Config$Cfg/SINGLE_SID     0]
   :single-s2     [Config$Cfg/SINGLE_SID     1]
   :dual-s1       [Config$Cfg/DUAL_SOCKET1   0]
   :dual-s2       [Config$Cfg/DUAL_SOCKET2   0]
   :dual-both     [Config$Cfg/DUAL_SID       0]
   :triple-s1     [Config$Cfg/TRIPLE_SID     0]
   :triple-s2     [Config$Cfg/TRIPLE_SID_TWO 0]
   :quad          [Config$Cfg/QUAD_SID       0]
   :mirrored      [Config$Cfg/MIRRORED_SID   0]
   :mirrored-dual [Config$Cfg/MIRRORED_SID   1]
   :dual-flipped  [Config$Cfg/DUAL_FLIPPED   0]
   :quad-flipped  [Config$Cfg/QUAD_FLIPPED   0]
   :quad-mixed    [Config$Cfg/QUAD_MIXED     0]
   :quad-flipmix  [Config$Cfg/QUAD_FLIPMIX   0]})

(defn apply-preset!
  "Send direct preset command to board, then read config back."
  [preset-key]
  (with-driver "Apply preset"
    (fn []
      (if-let [[cmd arg] (get preset->cfg preset-key)]
        (do
          (state/log! (str "Applying preset: " (name preset-key) ""))
          (.USBSID_sendconfigcommand @drv-atom
                                     (bit-and (.get cmd) 0xff)
                                     (into-array Byte [(cfg-byte arg)]))
          (Thread/sleep 100)
          (state/log! "Preset applied. Reading updated config")
          @(read-config!))
        (state/log! (str "Preset " (name preset-key) " has no direct command"))))))
