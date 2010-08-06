(in-package :sqnc)

;   string gens

(defp *notenames* '("C" "C#" "D" "D#" "E" "F" "F#" "G" "G#" "A" "A#" "B"))

(def pitch-str (kn) (nth (mod kn 12) *notenames*))

(def oct-str (kn) (str-of (trunc kn 12)))

(def note-str (kn) (cat (pitch-str kn) (oct-str kn)))

(def player-str () (if (playing?) "Playing" "Paused"))

(def filename-str () (aif (path (s)) (file-namestring it) "Unsaved"))

(def touched-str () (if (touched (s)) "--:**" "--:--"))

(def swank-str () (aif (c-swank) (str-of "Port" it) "Offline"))

(def meter-str (pat tick)
  (mvbind (meas beat tick) (meter-time pat tick)
    (mkstr "~x:~x:~x" meas beat tick)))

;; (def keymap-str (km)
;;   (w/n (str "")
;;     (mh (cat! str (mkstr "~10A ~A~%" (name (key k)) (fn->cmd v)))
;;      km)))

(def keymaps-str (km) km "FIXME! keymaps-str")

; getter/setter closure gens

(defs ctl-name-fn      (c)       (fn () (name c))
      ctl-name-set-fn  (c)       [ctl-name-set _ c]
      cget-fn          (c)       (fn () (interp c))
      cfset-fn         (c)       [cfctl-set _ c]
      ciset-fn         (c)       [cictl-set _ c]
      yget             (ctl n)   (fn () (y (envpt ctl n)))
      xget             (ctl n)   (fn () (x (envpt ctl n)))
      yset             (ctl n)   [dfctl-set-y _ ctl n]
      xset             (ctl n)   [dfctl-set-x _ ctl n])

; widget drawing utils

(defm spacer-wij (miny minx height width)
  (new-wij 'widget :miny miny :minx minx :height height :width width))

(def slider-knob (wij buf win)
  (attr *acs-bullet* (+ (color-pair w-gr9) (if (eq wij (c-wij buf win)) a_reverse 0))))

(def slider-pos (begin getter ctl size &o reverse)
  (w/ (pos (trunc (rescale (=> getter) (lo ctl) (hi ctl) 0 (1- size))))
    (+ begin (if reverse (- (1- size) pos) pos))))

(def block-amount (ctl width)
  (/ (- (hi ctl) (lo ctl)) width))

(def hilited? (wij buf win)
  (or (and *parent-cur* (hilite wij))
      (eq wij (c-wij buf win))))

(mac w/hilite ((wij buf win) . body)
  `(pn (_wattron ,buf (color-pair (if (hilited? ,wij ,buf ,win)
                                      (hilited-color ,wij)
                                      (color ,wij))))
       ,@body))

; abstract base classes

(classy numeric-widget (widget))

(classy text-widget (widget))

; labelbox

(classy labelbox (text-widget)
  (color gr3-gr9)
  (hilited-color gr9-gr3))

(def-redraw-widget labelbox
  (w/ (ny (miny w) nx (minx w))
    (w/hilite (w buf win)
      (_+str buf ny nx (mkstr "~v@a " (1- (width w)) (=> (getter w)))))))

; bracketbox

(classy bracketbox (text-widget)
  (color         gr3-gr9)
  (hilited-color w-gr9))

(def-redraw-widget bracketbox
  (w/ (ny (miny w) nx (minx w))
    (w/hilite (w buf win)
      (_+ch buf ny nx        (cursch #\<))
      (_+ch buf ny (maxvx w) (cursch #\>))
      (_+str buf ny (1+ nx)  (mkstr " ~v@a " (- (width w) 4) (=> (getter w)))))))

; button

(classy button (widget)
  (action nil))

(def-redraw-widget button
  (w/hilite (w buf win)
    (_+str buf (miny w) (minx w) (mkstr "(~a)" (name w)))))

(defcmd execute-button (&o (w (c-wij)))
  (=> (action w)))

; toggle-button

(classy toggle-button (button)
  (on-fn    nil)
  (on-text  nil)
  (off-text nil))

(def-redraw-widget toggle-button
  (w/hilite (w buf win)
    (_+str buf (miny w) (minx w)
           (mkstr "~v@a" (width w) (if (=> (on-fn w))
                                       (on-text  w)
                                       (off-text w))))))

; intbox

(classy intbox (numeric-widget)
  (color         gr3-gr9)
  (hilited-color w-gr9))

(def-redraw-widget intbox
  (w/hilite (w buf win)
    (_+str buf (miny w) (minx w) (mkstr "~va " (1- (width w)) (=> (getter w))))))

; labeled-intbox

(classy labeled-intbox (intbox)
  (label-width  10)
  (label-color  w-gr8))

(def-redraw-widget labeled-intbox
  (cnm)
  (_wattron buf (color-pair (label-color w)))
  (_+str buf (miny w) (- (minx w) (label-width w) 1)
    (mkstr "~v@a " (label-width w) (name w))))

(def mk-labeled-intbox (name ctl getter setter unit miny minx height iw lw
                        &o (color gr3-gr9) (hcolor gr9-gr3) (label-color w-gr8) (intrac t))
  (new-wij 'labeled-intbox
           :name           name
           :ctl            ctl
           :getter         getter
           :setter         setter
           :unit           unit
           :miny           miny
           :minx           minx
           :height         height
           :width          iw
           :label-width    lw
           :color          color
           :hilited-color  hcolor
           :label-color    label-color
           :intrac         intrac))

; floatbox

(classy floatbox (numeric-widget)
  (color          gr3-gr9)
  (hilited-color  gr8-gr4)
  (decimals       nil))

(def-redraw-widget floatbox
  (w/hilite (w buf win)
    (_+str buf (miny w) (minx w)
           (mkstr "~v,vf " (1- (width w)) (decimals w) (=> (getter w))))))

; boxed-widget

(classy boxed-widget (widget)
  (hilited-box-color w-gr9)
  (regular-box-color gr5-gr9))

(def box-color (wij buf win)
  (color-pair (if (cursor-within-wij (cy buf win) (cx buf win) wij)
                  (hilited-box-color wij)
                  (regular-box-color wij))))

; boxed-textbox

(classy boxed-textbox (textbox boxed-widget))

(defm create-widgets ((w boxed-textbox))
  (ls (new-wij 'bracketbox
               :miny (1- (miny w))
               :minx (+ (minx w) 2)
               :height 1
               :width (+ (ln (name w)) 4)
               :getter (fn () (name w))
               :hilite t)))

(def-redraw-widget boxed-textbox
  (cnm)
  (w/ (ny (miny w) nx (minx w))
    (_wattron buf (box-color w buf win))
    (cbox buf (1- ny) (1- nx) (+ ny (height w)) (+ nx (width w)))))

; listbox

(classy listbox (widget)
  (color         gr3-gr9)
  (lst-fn        nil)
  (printer       nil)
  (origin        0)
  (cur           0))

(def listbox-origin (w)
  (w/ (o (origin w))
    (max 0 (+ o (astray (cur w) o (+ -1 o (height w)))))))

(def-redraw-widget listbox
  (w/* (num  (height w)
        cur  (cur w)
        ori  (listbox-origin w)
        lst  (=> (lst-fn w))
        elt  (nth cur lst)
        lst  (nthcdr ori lst)
        strw (- (width w) 5))
    (=! (origin w) ori)
    (dotimes (n num)
      (w/ (celt (nth n lst))
        (cattr buf (t (color-pair (color w)) (and elt (eq elt celt)) a_reverse)
          (_+str buf (+ n (miny w)) (minx w)
                 (mkstr "~3d. ~v@a" (+ n ori) strw
                        (chop-str (if celt (=> (printer w) celt) "") strw))))))))

; listbox cmds

(def listbox-confine (w)
  (w/* (lst (=> (lst-fn w)) ln (ln lst))
    (when (>= (cur w) ln)
      (=! (cur w) (max 0 (1- ln))))))

(defcmd listbox-next (&o (w (c-wij)) (off 1))
  (listbox-confine w)
  (w/* (lst  (=> (lst-fn w))
        ln   (ln lst)
        ncur (+ off (cur w)))
    (=! (cur w) (max 0 (cif (>= ncur ln) 0
                            (< ncur 0)   (1- ln)
                                         ncur)))
    (touch (c-buf))))

(defcmd listbox-prev (&o (w (c-wij)) (off 1))
  (listbox-next w (* -1 off)))

; boxed-listbox

(classy boxed-listbox (listbox boxed-widget))

(defm create-widgets ((w boxed-listbox))
  (ls (new-wij 'bracketbox
               :getter (fn () (name w))
               :miny   (1- (miny w))
               :minx   (+ 2 (minx w))
               :height 1
               :width  (+ 4 (ln (name w)))
               :hilite t)))

(def-redraw-widget boxed-listbox
  (cnm)
  (w/ (ny (1- (miny w))
       nx (1- (minx w)))
    (_wattron buf (box-color w buf win))
    (cbox buf ny nx (+ ny 1 (height w)) (+ nx 1 (width w)))))

; hslider

(classy hslider (numeric-widget))

(def-redraw-widget hslider
  (w/* (ny  (miny w)
        nx  (minx w)
        pos (slider-pos nx (getter w) (ctl w) (width w)))
    (map0-n [pn (_+ch buf (+ ny _) pos      (slider-knob w buf win))
                (_hln buf (+ ny _) nx       (colorch #\space w-gr4) (- pos nx))
                (_hln buf (+ ny _) (1+ pos) (colorch #\space w-gr6) (- (maxvx w) pos))]
            (1- (height w)))))

; hfader

(classy hfader (widget))

(defm create-widgets ((w hfader))
  (w/* (c            (ctl w)
        ny           (miny  w)
        nx           (minx  w)
        width        (width w)
        w1/5         (trunc (/ width 5))
        slider-width (- width (* 2 w1/5))
        res          nil)
    (push (new-wij 'labelbox
                   :ctl      c
                   :getter   (ctl-name-fn c)
                   :setter   (ctl-name-set-fn c)
                   :miny     ny
                   :minx     nx
                   :height   1
                   :width    w1/5
                   :intrac   t) res)
    (push (new-wij 'hslider
                   :ctl      c
                   :getter   (cget-fn c)
                   :setter   (cfset-fn c)
                   :unit     (block-amount c slider-width)
                   :miny     ny
                   :minx     (+ nx w1/5)
                   :height   1
                   :width    slider-width
                   :intrac   t) res)
    (push (new-wij 'floatbox
                   :ctl      c
                   :getter   (cget-fn c)
                   :setter   (cfset-fn c)
                   :unit     .1
                   :miny     ny
                   :minx     (- (+ width nx) w1/5)
                   :height   1
                   :width    w1/5
                   :decimals 3
                   :intrac   t) res)
    (nrev res)))

(def-redraw-widget hfader)

; henvelope

(classy henvelope (boxed-widget))

(classy henv-floatbox (floatbox)
  (envpt-num 0))

(classy henv-slider (hslider)
  (envpt-num 0))

(defm create-widgets ((h henvelope))
  (w/ (c (ctl h) res nil)
    (push (new-wij 'bracketbox
                   :getter (ctl-name-fn c)
                   :miny   (miny h)
                   :minx   10
                   :height 1
                   :width  (+ 4 (ln (name c)))
                   :hilite t) res)
    (loop with slider-width = 20
       for y from (1+ (miny h))
       for n below (ln (val c))
       do (pn (push (new-wij 'henv-floatbox
                             :ctl       c
                             :getter    (xget c n)
                             :setter    (xset c n)
                             :envpt-num n
                             :unit      0.1
                             :miny      y
                             :minx      1
                             :height    1
                             :width     6
                             :intrac    t
                             :decimals  3) res)
              (push (new-wij 'henv-slider
                             :name      (name c)
                             :ctl       c
                             :getter    (yget c n)
                             :setter    (yset c n)
                             :envpt-num n
                             :unit      (block-amount c slider-width)
                             :miny      y
                             :minx      8
                             :height    1
                             :width     slider-width
                             :intrac    t) res)
              (push (new-wij 'henv-floatbox
                             :ctl       c
                             :getter    (yget c n)
                             :setter    (yset c n)
                             :envpt-num n
                             :unit      .1
                             :miny      y
                             :minx      29
                             :height    1
                             :width     8
                             :intrac    t
                             :decimals  3) res)))
    (nrev res)))

(def-redraw-widget henvelope
  (w/ (h (height w) miny (miny w)
       color (box-color w buf win))
    (_wattron buf color)
    (wijbox w buf)
    (capped-vline buf miny  7 h)
    (capped-vline buf miny 28 h)))

(defcmd add-envpt-cmd (&o (w (c-wij)))
  (add-envpt (ctl w) (envpt-num w)))

(defcmd del-envpt-cmd (&o (w (c-wij)))
  (del-envpt (ctl w) (envpt-num w)))

(defcmd zap-kill-ring-ctl (&o (w (c-wij)))
  (w/ (n (name (ctl w))
       v (val  (ctl w)))
    {dolist (trg *kill-ring*)
      (dolist (sn (slot-names trg))
        (w/ (c (sval trg sn))
          (when (and (ctl-p c) (string= (name c) n))
            (_u (uslotf c 'val (copy v))))))}))

; hctl

(classy hctl (boxed-widget)
  (floatbox-width 8))

(defm create-widgets ((w hctl))
  (w/* (c      (ctl w)
        fbw    (floatbox-width w)
        swidth (- (width w) fbw 3)
        res    nil)
    (push (new-wij 'bracketbox
                   :getter (ctl-name-fn c)
                   :miny   (miny w)
                   :minx   (+ (minx w) 10)
                   :height 1
                   :width  (+ 4 (ln (name c)))
                   :hilite t) res)
    (push (new-wij 'hslider
                   :ctl      c
                   :getter   (cget-fn c)
                   :setter   (cfset-fn c)
                   :unit     (block-amount c swidth)
                   :miny     (1+ (miny w))
                   :minx     (1+ (minx w))
                   :height   1
                   :width    swidth
                   :intrac   t) res)
    (push (new-wij 'floatbox
                   :ctl      c
                   :getter   (cget-fn c)
                   :setter   (cfset-fn c)
                   :unit     .1
                   :miny     (1+ (miny w))
                   :minx     (+ (minx w) 2 swidth)
                   :height   1
                   :width    fbw
                   :intrac   t
                   :decimals 3) res)
    (nrev res)))

(def-redraw-widget hctl
  (_wattron buf (box-color w buf win))
  (wijbox w buf)
  (capped-vline buf (miny w) (+ (minx w) (- (width w) (floatbox-width w) 2)) (height w)))

; channel strip

(classy channel-strip (boxed-widget))

(classy available-effects-listbox (boxed-listbox))

(classy inserted-effects-listbox (boxed-listbox))

(defm create-widgets ((w channel-strip))
  (w/* (ny (miny w) nx (minx w) c (ctl w) sends (sends c) y ny
        fader-width (trunc (width w) 2) res nil)
    (push (new-wij 'bracketbox
                   :getter (fn () (name w))
                   :hilite t
                   :miny   y
                   :minx   (+ nx 3)
                   :height 1
                   :width  (+ 4 (ln (name w)))) res)
    (push (new-wij 'hfader
                   :ctl    (amp c)
                   :miny   (+! y)
                   :minx   (+ nx 1)
                   :height 1
                   :width  fader-width) res)
    (loop for send in sends
       do (push (new-wij 'hfader
                         :ctl    send
                         :miny   (+! y)
                         :minx   (+ nx 1)
                         :height 1
                         :width  fader-width) res))
    (push (new-wij 'available-effects-listbox
                   :name         "Effects"
                   :ctl          c
                   :color        gr2-gr8
                   :lst-fn       (fn () (available-effects))
                   :printer      [mkstr "~(~a~)" _]
                   :miny         (+ 2 ny)
                   :minx         (+ nx 3 fader-width)
                   :height       5
                   :width        20
                   :intrac       t) res)
    (push (new-wij 'inserted-effects-listbox
                   :name         "Inserted"
                   :ctl          c
                   :color        gr2-gr8
                   :lst-fn       (fn () (inserts c))
                   :printer      [mkstr "~a" (name _)]
                   :miny         (+ 2 ny)
                   :minx         (+ nx 26 fader-width)
                   :height       5
                   :width        20
                   :intrac       t) res)
    (push (new-wij 'toggle-button
                   :ctl      (mute c)
                   :color    gr3-gr9
                   :hilited-color gr9-gr3
                   :name     "mute"
                   :on-fn    (fn () (no (zerop (val (mute c)))))
                   :on-text  "on"
                   :off-text "muted"
                   :miny     (+! y)
                   :minx     (+ 1 nx)
                   :height   1
                   :width    8
                   :intrac   t
                   :action   (fn () (toggle-channel-mute c))) res)
    (nrev res)))

(def-redraw-widget channel-strip
  (wijcolor w buf (color w))
  (_wattron buf (box-color w buf win))
  (wijbox w buf))

(defcmd listbox-insert-effect (&o (w (c-wij)))
  (awhen (nth (cur w) (=> (lst-fn w)))
    {_u (uadd (new it) (ctl w))}
    (touch (c-buf))))

(defcmd listbox-delete-effect (&o (w (c-wij)))
  (awhen (nth (cur w) (=> (lst-fn w)))
    {_u (udel it (ctl w))}
    (listbox-confine w)))

; mixer head

(classy mixer-head (boxed-widget))

(defm create-widgets ((w mixer-head))
  (w/* (ny      (miny w)
        nx      (minx w)
        swidth  (- (trunc (width w) 2) 5)
        m       (mixer (s))
        y       ny)
    (ls (new-wij 'bracketbox
                 :getter (fn () (name w))
                 :hilite t
                 :miny   ny
                 :minx   (+ nx 3)
                 :height 1
                 :width  (+ 4 (ln (name w))))
        (new-wij 'hfader
                 :name   "Amp"
                 :ctl    (amp m)
                 :miny   (+! y)
                 :minx   (+ nx 2)
                 :height 1
                 :width  swidth)
        (new-wij 'hfader
                 :name   "Pan"
                 :ctl    (pan m)
                 :miny   (+! y)
                 :minx   (+ nx 2)
                 :height 1
                 :width  swidth)
        )))

(def-redraw-widget mixer-head
  (wijcolor w buf (color w))
  (_wattron buf (box-color w buf win))
  (wijbox w buf))

;   general widget cmds

(defcmd next-wij! (&o (n 1) (b (c-buf)) (w (c-win)))
  (sync-wij-curs (cnext (c-wij b w) (leaf-wijs b) n) b w))

(defcmd prev-wij! (&o (n 1) (b (c-buf)) (w (c-win)))
  (sync-wij-curs (cprev (c-wij b w) (leaf-wijs b) n) b w))

(defcmds next-wij-2! () (next-wij! 2)
         prev-wij-2! () (prev-wij! 2)
         next-wij-4! () (next-wij! 4)
         prev-wij-4! () (prev-wij! 4))

; numeric widget cmds

(def offset-fctl-wij (num &o (w (c-wij)))
  (=> (setter w) (+ (* num (unit w)) (=> (getter w)))))

(defcmds wij-1+   () (offset-fctl-wij  1.0)
         wij-1-   () (offset-fctl-wij -1.0)
         wij-.1+  () (offset-fctl-wij  0.1)
         wij-.1-  () (offset-fctl-wij -0.1)
         wij-.01+ () (offset-fctl-wij  0.01)
         wij-.01- () (offset-fctl-wij -0.01))

(defcmd query-set-numeric-wij (&o (w (c-wij)))
  (awhen (mbq 'number "New value: ")
    (=> (setter w) it)))

; text-widget cmds

(defcmd query-set-text-wij (&o (w (c-wij)))
  (awhen (mbq 'string "New text: ")
    (=> (setter w) it)))

;;; mixer-buffer

(classy mixer-buffer-modeline (modeline))

(classy mixer-buffer (standard-buffer)
  (color w-gr7))

(defm create-widgets ((b mixer-buffer))
  (w/* (channel-width 100 x 2 w nil mixer (mixer (s)) res nil
        head (new-wij 'mixer-head :name "master"
                      :miny 1 :minx 2 :height 4 :width channel-width))
    (push (spacer-wij 0 0 0 106) res)
    (push head res)
    (loop for y = (+ 2 (height head)) then (+ y 1 (height w))
          for ch in (append (channels mixer) (sends mixer))
          for i from 0
          do  (push (=! w (new-wij 'channel-strip
                                   :name     (name ch)
                                   :ctl      ch
                                   :miny     y
                                   :minx     x
                                   :height   (max (+ 3 (ln (sends ch))) 9)
                                   :width    channel-width)) res))
    (nrev res)))

(definit (b mixer-buffer)
  (slotf b 'bottom (new 'mixer-buffer-modeline)))

(defms touch-p ((c chnctl)   (b mixer-buffer)) t
       break-p ((c mixerctl) (b mixer-buffer)) t)

(defredraw mixer-buffer-modeline
  (draw-modeline b (hl? w) " sqnc Mixer "))

(defp *mixer-buffer* nil)

(defcmd toggle-mixer ()
  (if *recursive*
      (done!)
      (full-screen-buffer-excursion 'mixer-buffer)))

;;; inst-buffer

(classy available-insts-listbox (boxed-listbox))

(classy c-insts-listbox (boxed-listbox))

(classy sample-map-listbox (boxed-listbox)
  (instbox nil))

(classy samples-listbox (boxed-listbox))

(classy sample-map-editor (boxed-textbox))

(classy inst-buffer-modeline (modeline))

(classy inst-buffer (standard-buffer)
  (color w-gr7))

;; (def inst-pprinter (inst)
;;   (w/ (code (code inst))
;;     (mkstr "(~(~A~)~{~&  ~(~A~)~})" (car code) (cdr code))))

(def inst-pprinter (inst) inst (mkstr "FIXME"))

(def import-inst-code (i &o (w (c-wij)))
  (=! (str w) (inst-pprinter i))
  (touch (c-buf)))

(def set-inst-code (&o (w (c-wij)) (i (inst (s))))
  (w/error-screen (mkstr "~A" e)
    (in-package :sqnc)
    (w/ (code (read-from-string (str w)))
      (when (and code i)
        {_u (uslotf i 'code code)}
        (mb-msg (mkstr "The code for ~a has been set" (name i)))
        (touch (c-buf))))))

(def import-smap (smap &o (w (c-wij)))
  (=! (str w) (mkstr "~a" smap))
  (touch (c-buf)))

(def define-smap (&o (w (c-wij)))
  (w/error-screen (mkstr "~A" e)
    (in-package :sqnc)
    (eval (read-from-string (str w)))
    (mb-msg "Sample-map defined")
    (touch (c-buf))))s

(def inst-printer (i)
  (if (sample-inst-p i)
      (mkstr "~a:~a" (name i) (aand (sample-map i) (id it)))
      (mkstr "~a" (name i))))

(defm create-widgets ((b inst-buffer))
  (w/* (s (s)
        color gr2-gr8
        insts-listbox (new-wij 'c-insts-listbox
                               :name         "Insts"
                               :color        color
                               :lst-fn       (fn () (insts s))
                               :printer      #'inst-printer
                               :miny         2
                               :minx         29
                               :height       7
                               :width        22
                               :intrac       t)
        smaps-listbox (new-wij 'sample-map-listbox
                               :name         "Sample Maps"
                               :color        color
                               :lst-fn       (fn () (sample-maps s))
                               :printer      [mkstr "~a" (id _)]
                               :miny         2
                               :minx         55
                               :height       7
                               :width        22
                               :intrac       t
                               :instbox      insts-listbox)
        smap-editor   (new-wij 'sample-map-editor
                               :name         "Sample Map Editor"
                               :color        w-gr8
                               :hilited-box-color w-gr9
                               :regular-box-color gr5-gr9
                               :miny         12
                               :minx         3
                               :height       22
                               :width        74
                               :intrac       t))
    (ls (spacer-wij 0 0 (std-height) (std-width))
        (new-wij 'available-insts-listbox
                 :name         "Avail. Insts"
                 :color        color
                 :lst-fn       #'available-insts
                 :printer      [mkstr "~(~a~)" _]
                 :miny         2
                 :minx         3
                 :height       7
                 :width        22
                 :intrac       t)
        insts-listbox
        smaps-listbox
        smap-editor
        (new-wij 'samples-listbox
                 :name         "Samples"
                 :color        color
                 :lst-fn       (fn () (sort (hashkeys (hash (samples s))) #'string<))
                 :printer      [mkstr "~a" _]
                 :miny         2
                 :minx         81
                 :height       32
                 :width        22
                 :intrac       t)
        (new-wij 'button
                 :color gr5-gr9
                 :hilited-color gr9-gr3
                 :name "load inst code"
                 :action (fn () (awhen (nth (cur insts-listbox) (insts s))
                                  (import-inst-code it smap-editor)))
                 :miny 34
                 :minx 6
                 :height 1
                 :width 14
                 :intrac t)
        (new-wij 'button
                 :color gr5-gr9
                 :hilited-color gr9-gr3
                 :name "set inst code"
                 :action (fn () (set-inst-code smap-editor))
                 :miny 34
                 :minx 24
                 :height 1
                 :width 13
                 :intrac t)
        (new-wij 'button
                 :color gr5-gr9
                 :hilited-color gr9-gr3
                 :name "sample map"
                 :action (fn () (import-smap (nth (cur smaps-listbox) (sample-maps s)) smap-editor))
                 :miny 34
                 :minx 41
                 :height 1
                 :width 10
                 :intrac t)
        (new-wij 'button
                 :color gr5-gr9
                 :hilited-color gr9-gr3
                 :name "define sample map"
                 :action (fn () (define-smap smap-editor))
                 :miny 34
                 :minx 55
                 :height 1
                 :width 17
                 :intrac t))))

; inst-buffer cmds

(defcmd listbox-insert-inst (&o (w (c-wij)))
  (awhen (nth (cur w) *available-insts*)
    {_u (uadd (new it) (s))}))

(defcmd listbox-delete-inst (&o (w (c-wij)) (s (s)))
  (awhen (nth (cur w) (insts s))
    {_u (udel it (s))}
    (listbox-confine w)))

(defcmd set-insts-smap (&o (w (c-wij)) (s (s)))
  (w/ (smap (nth (cur w) (sample-maps s))
       inst (nth (cur (instbox w)) (insts s)))
    (when (sample-inst-p inst)
      (=! (sample-map inst) smap)
      (mb-msg (mkstr "Inst ~a's sample map set to ~a" (name inst) (id smap)))
      (touch (c-buf)))))

(defcmd toggle-inst-buffer ()
  (if *recursive*
      (done!)
      (full-screen-buffer-excursion 'inst-buffer)))

; inst-buffer predicates

(defm touch-p ((i inst) (b inst-buffer)) t)

;;; trg-buffer

(classy trg-buffer (standard-buffer)
  (trg             nil)
  (color           w-gr8)
  (border-color    w-gr8))

(classy trg-buffer-modeline (modeline))

(def trg-buf-trg (b &o (s (s)))
  (or (trg@p s) (and (slot-boundp b 'trg) (trg b)) (trg s)))

(definit-before (b trg-buffer)
  (=! (trg b) (trg-buf-trg b)))

(definit (b trg-buffer)
  (=! (bottom b) (new 'trg-buffer-modeline)))

(defm create-hook ((b trg-buffer))
  (fn () (when (neq (trg-buf-trg b) (trg b))
           (break-buffer b))))

(defm break-p ((c ctl) (b trg-buffer))
  (mapp [eq c (ctl _)] (leaf-wijs b)))

(defredraw trg-buffer-modeline
  (w/ (trg (trg (buffer b)))
    (draw-modeline b (hl? w)
                   (mkstr "~a | Start:~5,3fs | Dur:~5,3fs | Keynum:~A | Inst:~A "
                          (touched-str)
                          (aif trg (float (start it)) 0)
                          (aif trg (float (dur trg)) 0)
                          (aif trg (keynum it) 0)
                          (aif trg
                               (string-downcase (symbol-name (type-of it)))
                               "No trg")))))

(defm create-widgets ((b trg-buffer))
  (remove nil
  (loop for y = 0 then (+ y prev)
        for ctl in (aand (trg b) (acc [or (dfctl-p _) (cfctl-p _)] (slot-vals it)))
        for wij = (etypecase ctl
                    (dfctl (new-wij 'henvelope
                                    :name    (name ctl)
                                    :ctl     ctl
                                    :miny    y
                                    :minx    0
                                    :height  (+ 2 (ln (val ctl)))
                                    :width   38
                                    :hilited-box-color w-gr8
                                    :regular-box-color gr5-gr8))
                    (cfctl (new-wij 'hctl
                                    :name   (name ctl)
                                    :ctl    ctl
                                    :miny   y
                                    :minx   0
                                    :height 3
                                    :width  38
                                    :floatbox-width 8
                                    :hilited-box-color w-gr8
                                    :regular-box-color gr5-gr8)))
        for prev = (height wij)
        collect wij)))

;   base buffer

(classy base-widget (widget))

(classy base-buffer-modeline (modeline))

(classy base-buffer (standard-buffer)
  (always-redraw   t)
  (color           w-gr8)
  (border-color    w-gr9)
  (bottom          (new 'base-buffer-modeline :color w-gr9)))

(defm create-widgets ((b base-buffer))
  (create-std-wij 'base-widget))

(defredraw base-buffer-modeline
  (w/ (s1 (touched-str) s2 (filename-str) s3 (player-str))
    (draw-modeline b (hl? w) (str-of s1 s2 s3 " "))))

(defredraw base-buffer
  (w/ (y -1 x 0 n 50 s (s) cs (c-cs))
    (w/fns ((r (str val) (_+str b (+! y) x (mkstr " ~30a ~20a" str val)))
            (div () (_hln b (+! y) x 0 n)))
      (r "Current inst:"           (aif (inst s) (name it) "No insts loaded"))
      (r "Current octave:"         (c-oct))
      (r "edit-step:"              *edit-step*)
      (div)
      (r "Csound:"                 (cs-state-str))
      (r "Sample Rate:"            (aand cs (csoundgetsr     (csi it))))
      (r "Control Rate:"           (aand cs (csoundgetkr     (csi it))))
      (r "Output channels:"        (aand cs (csoundgetnchnls (csi it))))
      (r "0dbfs:"                  (aand cs (csoundget0dbfs  (csi it))))
      (div)
      (r "Number of patterns:"     (ln (patterns s)))
      (r "Number of tracks:"       (ln (tracks s)))
      (r "Number of buffers:"      (ln (m-bl)))
      (r "Number of windows:"      (ln (c-wl)))
      (div)
      (r "Swank:"                  (swank-str))
      (div)
      ;; (r "Codes:"               (mkstr "~{~a ~}" *codes*))
      (r "Last key:"               (last-keyname)))))

(classy message-buffer (standard-text-buffer)
  (always-redraw t))

(classy cs-message-buffer (message-buffer))

(defredraw cs-message-buffer
  (=! (str (c-wij b)) (cs-msgs)))

(def oor-cs-message-buffer ()
  (or *dump-cs-msgs* (toggle-dump-cs-msgs))
  (open-or-raise 'cs-message-buffer))

;   v-tick-line-buffer

(classy v-tick-line (widget))

(classy v-tick-line-buffer (vline-buffer)
  (pattern nil))

(classy left-v-tick-line-buffer (v-tick-line-buffer left-buffer))

(classy right-v-tick-line-buffer (v-tick-line-buffer right-buffer))

(definit-before (b v-tick-line-buffer)
  (=! (height b) (ticks/pat (pget :pattern initargs))))

(defm create-widgets ((b v-tick-line-buffer))
  (ls (new-wij 'v-tick-line :height (height b) :width 1)))

(defredraw v-tick-line-buffer
  (w/slots (measures beats resol) (pattern b)
    (for (i 0 (* measures beats resol))
      (mvbind (beat rem) (trunc i resol)
              (and (eq rem 0)
                   (_+ch b i 0 (char-code (hexch (mod beat beats)))))))))

;   h-octave-line-buffer

(classy h-octave-line (widget))

(classy h-octave-line-buffer (top-buffer)
  (keys nil))

(definit-before (b h-octave-line-buffer)
  (=! (width b) (pget :keys initargs)))

(defm create-widgets ((b h-octave-line-buffer))
  (ls (new-wij 'h-octave-line :height 1 :width (width b))))

(defredraw h-octave-line-buffer
  (for (n 0 (ceiling (/ (width b) 12)))
    (_+str b 0 (* n 12) (str-of n))))

;   piano-roll-buffer

(classy piano-roll-buffer (standard-buffer))

(classy piano-roll-buffer-modeline (modeline))

(classy piano-roll (widget)
  (color            w-gr4)
  (cursor-visible   t))

(defm create-widgets ((b piano-roll-buffer))
  (ls (new-wij 'piano-roll
               :height (aand (pget :pat (props b)) (ticks/pat it))
               :width  (keys (pget :trk (props b)))
               :intrac t)))

(definit (b piano-roll-buffer)
  (w/* (props (pget :props initargs)
        pat   (pget :pat props))
    (slotf* b 'bottom (new 'piano-roll-buffer-modeline :props props) ;; :buffer b)
              'top    (new       'h-octave-line-buffer :keys (keys (pget :trk props)))
              'left   (new    'left-v-tick-line-buffer :pattern pat)
              'right  (new   'right-v-tick-line-buffer :pattern pat))))

(classy root-piano-roll-buffer (piano-roll-buffer))

(mac with-piano-roll (buf . body)
  `(w/ (pat (pget :pat (props ,buf))
        trk (pget :trk (props ,buf)))
     pat trk ,@body))

(defps *black-keys*                              '(1 3 6 8 10)
       *black-key-color*                         w-gr5
       *black-key-char*                          (char-code #\space)
       *trg-color*                               w-r1
       *marked-trg-color*                        w-r4
       *ptrg-color*                              w-b0
       *marked-ptrg-color*                       w-b6
       *muted-trg-color*                         w-gr6
       *marked-muted-trg-color*                  w-gr9
       *measure-line-color*                      gr2-gr4
       *measure-in-loop-line-color*              gr2-gr5
       *beat-line-color*                         gr8-gr4
       *beat-in-loop-line-color*                 gr8-gr5
       *note-head-char*                          *acs-bullet*
       *note-body-char*                          *acs-vline*)

(defp *grab-trgs* nil)

(defcmd toggle-grab-trgs () (toggle *grab-trgs*))

(defm update-current ((b piano-roll-buffer) (w window) (song song))
  (when (eq b (c-buf))
    (with-piano-roll b
      (w/ (new-time (ticks->s (cy b w) (tpm pat))
           new-kn   (cx b w))
        (awhen (and *grab-trgs* (or (c-mtrgs) (aand (trg-in-ctick song) (ls it))))
          {pn (_u (uoffset-slots it 'start  (- new-time (time@pt pat))))
              (_u (uoffset-slots it 'keynum (- new-kn (keynum trk))))})
        (=! (pattern song) pat
            (track   pat)  trk
            (time@pt pat)  new-time
            (keynum  trk)  new-kn)))))

; modeline

(defredraw piano-roll-buffer-modeline
  (with-piano-roll b
    (w/* (song (s)
          bb   (buffer b)
          y    (cy bb w)
          x    (cx bb w)
          trg  (pat-trk-trg@time+tick (ticks->s y (tpm pat)) x pat trk)
          str  (mkstr "~a | ~a-~a:~a | ~5,3f:~5,3f | Step:~a | P~a:T~a "
                      (touched-str)
                      (meter-str pat y)
                      (note-str x)
                      x
                      (aif trg (start-tick it) 0)
                      (aif trg (dur-tick   it) 0)
                      *edit-step*
                      (idx pat (patterns song))
                      (idx trk (tracks   song))))
      (draw-modeline b (hl? w) str))))

; draw-black-keys

(def black-key? (key) (aand (mod key 12) (in it *black-keys*)))

(def draw-black-keys (b)
  (with-piano-roll b
    (_wattron b (color-pair *black-key-color*))
    (for (k 0 (keys trk))
      (if (black-key? k)
          (_vln b 0 k *black-key-char* (ticks/pat pat))))))

; draw-timesig-lines

(def draw-timesig-line (b beat)
  (with-piano-roll b
    (_hln b (* beat (resol pat)) 0 *acs-hline* (keys trk))))

(def measure-line-p (beat pat) (zerop (mod beat (beats pat))))

(defm draw-timesig-lines ((b piano-roll-buffer))
  (with-piano-roll b
    (for (beat 0 (beats/pat pat))
      (color-on b (if (measure-line-p beat pat)
                      *measure-line-color*
                      *beat-line-color*))
      (draw-timesig-line b beat))))

(def beat-in-loop-p (beat song)
  (and (>= beat (loop-start-beats song))
       (<  beat (loop-end-beats song))))

(defm draw-timesig-lines ((b root-piano-roll-buffer))
  (with-piano-roll b
    (for (beat 0 (beats/pat pat))
      (w/ (ml (measure-line-p beat pat)
           bl (beat-in-loop-p beat (s)))
        (_wattron b (color-pair (cif (and ml bl) *measure-in-loop-line-color*
                                     ml          *measure-line-color*
                                     bl          *beat-in-loop-line-color*
                                                 *beat-line-color*)))
        (draw-timesig-line b beat)))))

; draw-trgs

(def trg-color (trg b)
  (w/ (mute (muted trg)
       mark (markedp trg b)
       ptrg (pattern-trg-p trg))
    (color-pair
      (cif (and mute mark) *marked-muted-trg-color*
           mute            *muted-trg-color*
           (and mark ptrg) *marked-ptrg-color*
           ptrg            *ptrg-color*
           mark            *marked-trg-color*
                           *trg-color*))))

(def v-trg-line (trg b)
  (w/ (y (floor (start-tick trg))
       x (keynum trg)
       c (trg-color trg b))
    (_+ch b y x (+ *note-head-char* c))
    (_vln b (1+ y) x (+ *note-body-char* c)
          (1- (ceiling (dur-tick trg))))))

(def piano-roll-trgs (b)
  (with-piano-roll b (pat-trk-trgs pat trk)))

(def draw-trgs (b)
  (mapc [v-trg-line _ b]
        (sort (piano-roll-trgs b) #'< :key #'start-tick)))

; redraw

(def-redraw-widget piano-roll
  (wijcolor w buf (color w))
  (draw-black-keys buf)
  (draw-timesig-lines buf)
  (draw-trgs buf))

(defredraw root-piano-roll-buffer
  (draw-timesig-lines b)
  (draw-trgs b))

; redraw/recreate predicates

(defm touch-p ((trg trg) (b piano-roll-buffer))
  (and (eq (track   trg) (pget :trk (props b)))
       (eq (pattern trg) (pget :pat (props b)))))

(defm touch-p ((tr transport) (b piano-roll-buffer))
  (root-pattern-p (pget :pat (props b)) (s)))

(defms break-p ((p pattern) (b piano-roll-buffer)) (eq p (pget :pat (props b)))
       break-p ((trk track) (b piano-roll-buffer)) (eq trk (pget :trk (props b)))
       kill-p  ((p pattern) (b piano-roll-buffer)) (eq p (pget :pat (props b)))
       kill-p  ((trk track) (b piano-roll-buffer)) (eq trk (pget :trk (props b))))

; open-or-raise piano-roll-buffers

(def mk-proll-props (pat trk) (ls :pat pat :trk trk))

(def oor-proll (trk pat &o (w (c-win)) (s (s)))
  (open-or-raise (if (root-pattern-p pat s)
                     'root-piano-roll-buffer
                     'piano-roll-buffer)
                 (mk-proll-props pat trk)
                 w))

(defcmds oor-c-piano-roll        (&o (s (s))) (oor-proll (track s)  (pattern s))
         oor-root-piano-roll     (&o (s (s))) (oor-proll (track s)  (root-pattern s))
         oor-prev-pat-piano-roll (&o (s (s))) (oor-proll (track s)  (prev-pat s))
         oor-next-pat-piano-roll (&o (s (s))) (oor-proll (track s)  (next-pat s))
         oor-prev-trk-piano-roll (&o (s (s))) (oor-proll (prev-trk s) (pattern s))
         oor-next-trk-piano-roll (&o (s (s))) (oor-proll (next-trk s) (pattern s)))

; scaling commands

(defp *max-resol* 128)

(def set-resol (pat resol song)
  (if (within 1 *max-resol* resol)
      (pn (scro (round (* (cy) (/ resol (resol pat)))) (cx))
          (_slotf pat 'resol resol)
          (mb-msg (mkstr "Resol of pattern ~A set to ~A" (idx pat (patterns song)) resol)))
      (mb-msg (mkstr "New pattern resolution of ~A is out of range" resol))))

(def scale-resol (pat factor &o (s (s)))
  (w/ (new-resol (* (resol pat) factor))
    (if (whole-number-p new-resol)
        (set-resol pat (trunc new-resol) s)
        (mb-msg (mkstr "Resol of pattern ~A doesn't scale evenly by ~A"
                       (idx pat (patterns s)) factor)))))

(defcmd inc-zoom (&o (s (s)))
  (w/ (p (pattern s)) (set-resol p (1+ (resol p)) s)))

(defcmd dec-zoom (&o (s (s)))
  (w/ (p (pattern s)) (set-resol p (1- (resol p)) s)))

(defcmds double-zoom (&o (s (s)))
  (scale-resol (pattern s) 2))

(defcmds halve-zoom (&o (s (s)))
  (scale-resol (pattern s) .5))

(def trg-resol (trg song)
  (w/ (start (start trg)
       bpm   (bpm (pattern song)))
    (for (resol 1 128)
      (when (whole-number-p (s->ticks start (* resol bpm)))
        (exit trg-resol resol)))
    nil))

(defcmd set-zoom-to-trg-resol (&o (song (s)))
  (aand (trg-in-ctick song)
        (trg-resol it song)
        (set-resol (pattern song) it song)))

; navigation commands

(def goto-trg (trg) (scro (floor (start-tick trg)) (keynum trg)))

(defcmds goto-next-trg   (&o (s (s))) (awhen (next-trg s) (goto-trg it))
         goto-prev-trg   (&o (s (s))) (awhen (prev-trg s) (goto-trg it))
         center-next-trg (&o (s (s))) (pn (goto-next-trg s) (center-cursor-v))
         center-prev-trg (&o (s (s))) (pn (goto-prev-trg s) (center-cursor-v)))

; directional trg navigation

(defms avgy ((r trg)) (start-tick r)
       avgx ((r trg)) (keynum r)
       miny ((r trg)) (start-tick r)
       minx ((r trg)) (keynum r)
       maxy ((r trg)) (start-tick r)
       maxx ((r trg)) (keynum r))

(mac def-trg-dir-cmd (dir form)
  `(defcmd ,(mksym 'trg- dir) (&o (b (c-buf)) (w (c-win)) (s (s)))
     (awhen ,form
       (scro (floor (start-tick it)) (keynum it) b w)
       it)))

(def-trg-dir-cmd up    (get-upward-object    (cy b w) (cx b w) (trgs<time s)))
(def-trg-dir-cmd down  (get-downward-object  (cy b w) (cx b w) (trgs>tick s)))
(def-trg-dir-cmd left  (get-leftward-object  (cy b w) (cx b w) (cpat-ctrk-trgs s)))
(def-trg-dir-cmd right (get-rightward-object (cy b w) (cx b w) (cpat-ctrk-trgs s)))

; text-buffer subclasses

(classy help-buffer (standard-text-buffer)
  (always-redraw t)
  (color         w-gr7)
  (str           (keymaps-str (c-kms))))

(classy editor-buffer (standard-text-buffer))

(defcmd query-open-text-file ()
  (aand (mbq 'pathname "Filename: ")
        (get-text-file it)
        (raise (new 'editor-buffer :str it) (c-win))))

(defcmd toggle-read-only (&o (b (c-buf)))
  (if (text-buffer-p b)
      (pn (toggle (read-only b))
          (mb-msg (mkstr "Buffer is now ~:[read-only~;editable~]" (read-only b))))
      (mb-msg "Only text-buffers can be made read-only")))

;; (defcmd get-pt ()
;;   (w/ (b (open-and-raise 'editor-buffer))
;;     (set-text-buffer b 0 (mkstr "~%1234~%qwer"))
;;     (t! (read-only b))
;;     (bind "Return" #'text-return-line (keymap b)) ; <- keymap now in widget
;;     (w/ (res (cmd-loop))
;;       (kill-buffer b)
;;       (mb-msg "~A" res))))

; repl buffer

(classy repl-buffer (editor-buffer))

(def repl-prompt-str ()
  (mkstr "~a> " (package-name *package*)))

(classy replbox (textbox)
  (str     (repl-prompt-str))
  (point   (ln (repl-prompt-str)))
  (input   (ln (repl-prompt-str))))

(defm create-widgets ((b repl-buffer))
  (create-std-wij 'replbox))

; read
(def repl-read (str)
  (handler-case (stread str)
    (end-of-file (e) e nil)
    (error (e) e)))

; eval
(def repl-eval (obj)
  (handler-case (eval obj)
    (error (e) e)))

; print
(def repl-print (str val wij)
  (w/ (new-str (mkstr "~a~%~a~%~a" str val (repl-prompt-str)))
    (ls (=! (input wij) (ln new-str)) new-str)))

(textbox-cmd eval-input
  (acif (not (next-non-wspc (min (input wij) (ln str)) str))
          (repl-print str (mkstr "; No value") wij)
        (repl-read (subseq str (min (input wij) (ln str))))
          (repl-print str (repl-eval it) wij)
        (ls (1+ pt) (mkstr "~a~%~a" (subseq str 0 pt) (subseq str pt)))))

; other evals

(mac w/handle-msg body
  `(handler-case (pn ,@body)
     (error (e) (mb-msg (mkstr "~a" e)))))

(defcmd execute-extended-command ()
  (w/when (cmd (mbq 'command "M-x "))
    (aif (cmd->fn cmd)
         (w/handle-msg (=> it))
         (mb-msg (mkstr "No match for ~A" cmd)))))

(defcmd evaluate-expression ()
  (awhen (mbq 'list "Sexp: ")
    (w/handle-msg (mb-msg (mkstr "~A => ~A" it (eval it))))))

; open-or-raise cmds

(defcmds oor-title-buffer       () (open-or-raise 'title-buffer)
         oor-base-buffer        () (open-or-raise 'base-buffer)
         oor-help-buffer        () (open-or-raise 'help-buffer)
         oor-editor-buffer      () (open-or-raise 'editor-buffer)
         oor-repl-buffer        () (open-or-raise 'repl-buffer)
         oor-mixer-buffer       () (open-or-raise 'mixer-buffer)
         oor-inst-buffer        () (open-or-raise 'inst-buffer)
         oor-trg-buffer         () (open-or-raise 'trg-buffer))
