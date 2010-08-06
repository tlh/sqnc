(in-package :sqnc)

; envpt and ctl classes

(classy envpt ()
  (x              nil)
  (y              nil)
  (i              nil))

(classy ctl ()
  (name           nil)
  (val            nil)
  (lo             nil)
  (hi             nil))

(classy nctl (ctl))  ; numeric ctl

(classy dctl (nctl)) ; dynamic ctl

(classy cctl (nctl)) ; constant ctl

(classy ictl (nctl)) ; integer ctl

(classy fctl (nctl)) ; float ctl

(classy dfctl (dctl fctl)) ; dynamic float ctl

(classy dictl (dctl ictl)) ; dynamic int ctl

(classy cfctl (cctl fctl)) ; constant float ctl

(classy cictl (cctl ictl)) ; constant int ctl

(classy mixerctl (cictl))

(classy chnctl (cfctl))

(classy kchnctl (chnctl))

(classy ichnctl (chnctl))

(def mk-env pts
  (mc [dbind (x y i) _ (new 'envpt :x x :y y :i i)]
      (group pts 3)))

(def mk-ctl (type name lo hi val)
  (new type :name name :val val :lo lo :hi hi))

(def dfctl (name lo hi . env)
  (mk-ctl 'dfctl name lo hi (-> mk-env env)))

(def cfctl (name lo hi val)
  (mk-ctl 'cfctl name lo hi val))

(classy channel ()
  (name           nil)
  (amp            (mk-ctl 'kchnctl "Amp" 0 1 .7))
  (mute           (mk-ctl 'kchnctl "Mute" 0 1 1))
  (sends          nil)
  (inserts        nil))

(classy send (channel))

(classy mixer ()
  (inum            1000.1)
  (options         "-dWodac")
  (offline-options "-dW")
  (score           "f 0 1000000")
  (channels        nil)
  (sends           nil)
  (amp             (mk-ctl 'kchnctl  "amp" 0 1 .7))
  (pan             (mk-ctl 'kchnctl  "pan" 0 1 .5))
  (sr              (mk-ctl 'mixerctl "sr"        1000 100000 44100))
  (kr              (mk-ctl 'mixerctl "kr"        10 10000 441))
  (ksmps           (mk-ctl 'mixerctl "ksmps"     1 100000 100))
  (nchnls          (mk-ctl 'mixerctl "nchnls"    1 32 2))
  (0dbfs           (mk-ctl 'mixerctl "0dbfs"     1 1 1)))

(classy pattern ()
  (name           "Default Pattern")
  (bpm            170)
  (resol          8)
  (beats          7)
  (measures       8)
  (time@pt        0)
  (track          nil)
  (trg-hash       (table)))

(defm dur ((p pattern)) (beats->s (beats/pat p) (bpm p)))

(defs beats/pat  (pat) (* (beats pat) (measures pat))
      ticks/pat  (pat) (* (beats/pat pat) (resol pat))
      ticks/meas (pat) (* (beats pat) (resol pat)))

(classy track ()
  (name           "Default Track")
  (keys           128)
  (keynum         64)
  (amp            0.8)
  (solo           nil)
  (muted          nil))

(defps *trg-dur*     1/4
       *min-trg-dur* 1/1000
       *maxres*      200)

(classy trg ()
  (pattern        nil :nocp t)
  (track          nil :nocp t)
  (inst           nil :nocp t)
  (name           nil)
  (start          0)
  (dur            *trg-dur*)
  (keynum         nil)
  (res            (mk-ctl 'dfctl "Resolution"  1 *maxres* (mk-env 0 20 :l)))
  (muted          nil)
  (params         nil))

(classy pattern-trg (trg)
  (pat nil :nocp))

(defg generator (trg-type))

(defms start-tick ((trg trg)) (s->ticks (start trg) (tpm trg))
         dur-tick ((trg trg)) (s->ticks (dur   trg) (tpm trg))
         end-tick ((trg trg)) (+ (start-tick trg) (dur-tick trg))
         end      ((trg trg)) (+ (start      trg) (dur      trg)))

(classy inst ()
  (name           nil)
  (csmac-name     nil)
  (presets        nil)
  (preset         nil)
  (stmts          nil))

(classy sample-inst (inst)
  (sample-map     nil))

(classy effect ()
  (name           nil)
  (csmac-name     nil)
  (params         nil)
  (stmts          nil)
  (presets        nil)
  (preset         nil))

(classy transport ()
  (current-time   0)
  (loop-start     0)
  (loop-end       1))

(classy sample ()
  (table-num     nil)
  (path          nil)
  (id            nil)
  (slices        nil)
  (samples       nil)
  (bpm           nil)
  (sr            nil)
  (base-freq     nil))

(defm secs ((sample sample))
  (* (sr sample) (samples sample)))

(classy sample-table ()
  (hash          (table :test #'equal))
  (table-num     1000))

(classy song ()
  (name           nil)
  (path           nil)
  (inst           nil)
  (insts          nil)
  (pattern        nil)
  (patterns       nil)
  (tracks         nil)
  (trg            nil)
  (sample-maps    nil)
  (samples        (new 'sample-table))
  (mixer          (new 'mixer))
  (transport      (new 'transport)))

(defms track   ((s song)) (track (pattern s))
       presets ((s song)) (awhen (inst s) (presets it))
       preset  ((s song)) (awhen (inst s) (preset it))
       time@pt ((s song)) (time@pt (pattern s))
       keynum  ((s song)) (keynum (track s)))

(def time@pt+1 (song)
  (w/ (tpm (tpm song))
    (ticks->s (1+ (s->ticks (time@pt song) tpm)) tpm)))

(defms tpm ((p pattern)) (* (resol p) (bpm p))
       tpm ((s song))    (tpm (pattern s))
       tpm ((r trg))     (tpm (pattern r)))

; utils

(defp *song* nil)

(def s () *song*)

(def set-song (s) (=! *song* s))

(def meter-time (pat tick)
  (w/ (bs (beats pat) rs (resol pat))
    (mvbind (meas rem) (trunc tick (* bs rs))
      (mvbind (beat res) (trunc rem rs)
        (values meas beat res)))))

(def root-pattern (song) (car (patterns song)))

(def root-pattern-p (pat song) (eq pat (root-pattern song)))

(defs prev-pat     (song) (cprev (pattern song) (patterns song))
      next-pat     (song) (cnext (pattern song) (patterns song))
      prev-trk     (song) (cprev (track   song) (tracks   song))
      next-trk     (song) (cnext (track   song) (tracks   song))
      prev-inst    (song) (cprev (inst    song) (insts    song))
      next-inst    (song) (cnext (inst    song) (insts    song))
      prev-preset  (song) (cprev (preset  song) (presets  song))
      next-preset  (song) (cnext (preset  song) (presets  song))
      prev-pat!    (song) (=! (pattern song) (prev-pat song))
      next-pat!    (song) (=! (pattern song) (next-pat song))
      prev-trk!    (song) (=! (track (pattern song)) (prev-trk song))
      next-trk!    (song) (=! (track (pattern song)) (next-trk song))
      prev-inst!   (song) (=! (inst song) (prev-inst song))
      next-inst!   (song) (=! (inst song) (next-inst song))
      prev-preset! (song) (=! (preset (inst song)) (prev-preset song))
      next-preset! (song) (=! (preset (inst song)) (next-preset song)))

; ctl utils

(defm interp ((n number) &o secs)
  "Returns the number."
  (declare (ignore secs))
  n)

(defm interp ((c cctl) &o secs)
  "Returns the current value of c, since it's not an envelope."
  (declare (ignore secs))
  (val c))

(def normalize-env (env)
  "Adds an envpt at x=0 unless one already exists."
  (if (> (x (car env)) 0)
      (cons (new 'envpt :x 0 :y (y (car env)) :i :l) env)
      env))

(defm interp ((ctl dctl) &o (secs 0))
  "Interpolates the value of ctl's envelope at secs seconds."
  (w/* (env (val ctl)
        res (maplp [aand (cadr _) (< (-! secs (x it)) 0) _]
                   (normalize-env env))
        p1  (car  res)
        p2  (cadr res))
    (if (no (and p1 p2))
        (y (last1 env))
        (w/ (x1 (x p1) x2 (x p2))
          (if (= x1 x2)
              (y p1)
              (rescale (+ secs x2) 0 x2 (y p1) (y p2)))))))

(defm interp ((c dictl) &o (secs 0))
  "Truncates the value of interpolating c."
  (declare (ignore secs))
  (trunc (cnm)))

(defm interp ((c cictl) &o (secs 0))
  "Truncates the value of c."
  (declare (ignore secs))
  (trunc (cnm)))

(mac map-ctl (ctl . body)
  (w/uniq (new-ctl)
    `(w/n (,new-ctl (copy ,ctl))
       (w/slots (lo hi val typ) ,new-ctl
         (mapc [w/slots (y x i) _ ,@body] val)))))

(def rescale-ctl (ctl nlo nhi)
  (w/n (c (map-ctl ctl (=! y (rescale y lo hi nlo nhi))))
    (=! (lo c) nlo (hi c) nhi)))

(def repitch-ctl (ctl keynum base-key)
  (map-ctl ctl (=! y (repitch y keynum base-key))))

(def pitchbend-ctl (keynum pitchbend)
  (map-ctl pitchbend (=! y (midi-cps (+ y keynum)))))

(defm expt-ctl ((c dfctl) e) (map-ctl c (=! y (expt y e))))

(defm expt-ctl ((c cfctl) e) (expt (val c) e))

;;; trg copying

(def tag-trg (trg song . defs)
  (-> slotf trg
      'pattern   (pattern song)
      'track     (track   song)
      'start     (time@pt song)
      'keynum    (keynum  song)
      defs))

(def copy-and-tag-trg (trg song . defs)
  (-> tag-trg (copy trg) song defs))

(def c-new-trg (song . defs)
  (awhen (trg song) (-> copy-and-tag-trg it song defs)))

(def convert-dur (from)
  (ticks->s (ticks/pat from) (tpm from)))

(def c-new-ptrg (pat song . defs)
  (-> tag-trg (new 'pattern-trg :pat pat :dur (convert-dur pat))
      song defs))

;;; trg utils

(mac trg-hash-get (trg)
  `(gethash (start ,trg) (trg-hash (pattern ,trg))))

(def trg-hash-add (trg) (push trg (trg-hash-get trg)))

(def trg-hash-del (trg)
  (a=! (trg-hash-get trg) (remove trg it))
  (when (null (trg-hash-get trg))
    (remhash (start trg) (trg-hash (pattern trg)))))

(def trg-within-bounds (trg)
  (and (within 0 (dur (pattern trg)) (start trg))
       (within 0 (keys (track trg)) (keynum trg))))

(def min-keynum (objs) (-> keyed-min #'keynum objs))

(def max-keynum (objs) (-> keyed-max #'keynum objs))

(def sort-trgs-inc (trgs)
  (sort (sort trgs #'< :key #'keynum) #'< :key #'start))

(def sort-trgs-dec (trgs)
  (sort (sort trgs #'> :key #'keynum) #'> :key #'start))

;;; query-trgs

(def query-trgs (pat &k (beg 0) (end (dur pat)) all-active preds inclusive-end)
  (w/n (end-test (if inclusive-end #'<= #'<) res)
    (dohash (start trgs (trg-hash pat))
      (dolist (trg trgs)
        (and (if all-active
                 (and (=> end-test start end) (> (+ start (dur trg)) beg))
                 (and (<= beg start) (=> end-test start end)))
             (-> passes trg preds)
             (push trg res))))))

(def inst-trgs (i &o (s (s)))
  (ma [query-trgs _ :preds (ls [eq i (inst _)])] (patterns s)))

(def trk-trgs (trk . keys)
  (ma [-> pat-trk-trgs _ trk keys] (patterns (s))))

(def pat-trk-trgs (pat trk &r keys &k preds &a)
  (-> query-trgs pat :preds (cons [eq trk (track _)] preds) keys))

(def pat-range-trgs (pat beg end . keys)
  (-> query-trgs pat :beg beg :end end keys))

(def pat-trk-time-trgs (pat trk time . keys)
  (-> pat-trk-trgs pat trk :beg time :end time :inclusive-end t keys))

(def pat-trk-time-kn-trgs (pat trk time kn &r keys &k preds &a)
  (-> pat-trk-time-trgs pat trk time :preds (cons [= kn (keynum _)] preds) keys))

(def cpat-ctrk-trgs (song . keys)
  (-> pat-trk-trgs (pattern song) (track song) keys))

(def cpat-ctrk-time-trgs (time song . keys)
  (-> pat-trk-time-trgs (pattern song) (track song) time keys))

(def cpat-ctrk-ctime-trgs (song . keys)
  (-> cpat-ctrk-time-trgs (time@pt song) song keys))

(def cpat-ctrk-ctime-kn-trgs (kn song &r keys &k preds &a)
  (-> cpat-ctrk-ctime-trgs song :preds (cons [= kn (keynum _)] preds) keys))

(def pat-trk-trgs@time+tick (time keynum pat trk)
  (sort (pat-trk-trgs pat trk
                      :beg time :end (+ time (ticks->s 1 (tpm pat)))
                      :preds (ls [= keynum (keynum _)])
                      :all-active t)
        #'< :key #'start))

(def pat-trk-trg@time+tick (time keynum pat trk)
  (car (pat-trk-trgs@time+tick time keynum pat trk)))

(def trgs@time+tick (time song)
  (pat-trk-trgs@time+tick time (keynum song) (pattern song) (track song)))

(def trg@time+tick (time song) (car (trgs@time+tick time song)))

(def trg-in-ctick (song) (trg@time+tick (time@pt song) song))

(def trgs@p (song . keys)
  (-> cpat-ctrk-ctime-kn-trgs (keynum song) song keys))

(def trg@p (song) (car (trgs@p song)))

(def c-active-trgs (song) (trgs@p song :all-active t))

(def c-active-trg (song) (car (c-active-trgs song)))

(def trgs@trg (trg)
  (pat-trk-time-kn-trgs (pattern trg) (track trg) (start trg) (keynum trg)))

(def all-ptrgs-in-range (pat beg end)
  (pat-range-trgs pat beg end :all-active t :preds (ls #'pattern-trg-p)))

(def trgs-in-range (pat beg end)
  (pat-range-trgs pat beg end :preds (ls [no (pattern-trg-p _)])))

(def trgs<time (song)
  (cpat-ctrk-trgs song :preds (ls [< (start _) (time@pt song)])))

(def trgs>tick (song)
  (cpat-ctrk-trgs song :preds (ls [>= (start _) (time@pt+1 song)])))

(def next-trg (song)
  (car (sort-trgs-inc
        (or (cpat-ctrk-trgs song :beg   (time@pt song)
                                 :end   (time@pt+1 song)
                                 :preds (ls [> (keynum _) (keynum song)]))
            (trgs>tick song)
            (cpat-ctrk-trgs song)))))

(def prev-trg (song)
  (car (sort-trgs-dec
        (or (cpat-ctrk-ctime-trgs song :preds (ls [< (keynum _) (keynum  song)]))
            (cpat-ctrk-trgs       song :preds (ls [< (start  _) (time@pt song)]))
            (cpat-ctrk-trgs       song)))))

(def get-trgs-in-range (time-lo time-hi kn-lo kn-hi song)
  (cpat-ctrk-trgs song :beg time-lo :end time-hi
                  :preds (ls [<= kn-lo (keynum _)] [>= kn-hi (keynum _)])
                  :inclusive-end t))

;;; csd mixer gen

(defps *send-offset* 10 *chan-mult* 1000)

(def set-csoptions (str &o (s (s))) (=! (options (mixer s)) str))

; chnctl registry

(defps *chnctls* (table) *chnctl-idx* -1)

(def reset-chnctl-registry ()
  (=! *chnctls* (table) *chnctl-idx* -1))

(def chnctl-name (chnctl)
  (or (gethash chnctl *chnctls*)
      (putab chnctl (mkstr "chn~a" (+! *chnctl-idx*)) *chnctls*)))

(defm chnctl-var ((c kchnctl)) (mksym 'k (chnctl-name c)))

(defm chnctl-var ((c ichnctl)) (mksym 'i (chnctl-name c)))

; channel registry

(defps *channels* (table) *channel-idx* -1)

(def reset-channel-registry ()
  (=! *channels* (table) *channel-idx* -1))

(def record-channel (channel)
  (w/ (idx (+! *channel-idx*))
    (putab channel (ls (mksym 'at idx 0) (mksym 'at idx 1))
           *channels*)))

(def lch (channel)
  (or (car (gethash channel *channels*))
      (car (record-channel channel))))

(def rch (channel)
  (or (cadr (gethash channel *channels*))
      (cadr (record-channel channel))))

; inst registry

(defps *inst-registry* (table) *inst-idx* 0)

(def reset-inst-registry ()
  (=! *inst-registry* (table) *inst-idx* 0))

(def inst-inum (inst)
  (or (gethash inst *inst-registry*)
      (putab inst (+! *inst-idx*) *inst-registry*)))

; mixer-gen csmacs

(defcsmac ahdsr (a0 a1 iatt ihol idec isus irel)
  `(orc (maxes .000001 iatt ihol idec irel)
        (tigoto _ skipinit2)
        (linseg aenv 0 ,iatt 1 ,ihol 1 ,idec ,isus .01 ,isus)
        (label skipinit2)
        (tiestatus itie)
        (if (== itie 2) (linsegr aenv ,isus ,irel 0))
        (= ,a0 (* ,a0 aenv))
        (= ,a1 (* ,a1 aenv))))

(defcsmac balancer (a0 a1 bal)
  `(orc (if (> ,bal .5) (= klpan (* 2 (- ,bal .5))) (= klpan 0))
        (if (< ,bal .5) (= krpan (* 2 ,bal)) (= krpan 1))
        (pan2 (anl0 anr0) ,a0 klpan)
        (pan2 (anl1 anr1) ,a1 krpan)
        (= ,a0 (+ anl0 anl1))
        (= ,a1 (+ anr0 anr1))))

(defcsmac trackout (a0 a1 itrk)
  (w/ (from `(+ 1000 ,itrk) to `(+ 2000 ,itrk))
    `(orc (-mixer-send _ ,a0 ,from ,to 0)
          (-mixer-send _ ,a1 ,from ,to 1))))

(defcsmac trackin (a0 a1 itrk)
  (w/ (from `(+ 1000 ,itrk) to `(+ 2000 ,itrk))
    `(orc (-mixer-set-level _ ,from ,to 1)
          (-mixer-receive ,a0 ,to 0)
          (-mixer-receive ,a1 ,to 1))))

(def def-chnctl (ctl)
  (w/ (name (chnctl-name ctl) var (chnctl-var ctl))
    `(pn (chn_k _ ,name 1 2 ,(interp ctl) ,(lo ctl) ,(hi ctl))
         (chnset _ ,(interp ctl) ,name)
         (chnget ,var ,name))))

(def channel-knobs (ch)
  `(pn ,(def-chnctl (amp ch))
       ,(def-chnctl (mute ch))
       ,@(mc #'def-chnctl (sends ch))))

(def channel-in (ch chs)
  `(trackin ,(lch ch) ,(rch ch) ,(idx ch chs)))

(def channel-send (op send n channels)
  `(= ,(=> op send)
      (+ ,@(mc [pn `(* ,(=> op _) ,(chnctl-var (nth n (sends _))))]
               channels))))

(def channel-sends (send sends channels)
  (w/ (n (idx send sends))
    `(pn ,(channel-send #'lch send n channels)
         ,(channel-send #'rch send n channels))))

(def channel-insert (ch ins)
  `(pn ,@(mc [def-chnctl _] (params ins))
       (,(csmac-name ins) ,(lch ch) ,(rch ch)
         ,@(mc [chnctl-var _] (params ins)))))

(def channel-inserts (ch)
  `(pn ,@(mc [channel-insert ch _] (inserts ch))))

(def channel-level (ch)
  (w/ (a0   (lch ch)
       a1   (rch ch)
       amp  (chnctl-var (amp ch))
       mute (chnctl-var (mute ch)))
    `(pn (= ,a0 (* ,a0 ,amp ,mute))
         (= ,a1 (* ,a1 ,amp ,mute)))))

(def compile-master-channel (chs-sends m)
  `(pn (= am0 (+ ,@(mc [lch _] chs-sends)))
       (= am1 (+ ,@(mc [rch _] chs-sends)))
       ,(def-chnctl (pan m))
       ,(def-chnctl (amp m))
       (balancer am0 am1 ,(chnctl-var (pan m)))
       (= am0 (* am0 ,(chnctl-var (amp m))))
       (= am1 (* am1 ,(chnctl-var (amp m))))
       (outs _ am0 am1)
       (-mixer-clear)))

(def compile-mixer (song)
  (w/* (m (mixer song) chs (channels m) sends (sends m) chs-sends (ap chs sends))
    `(instr (master ,(inum m))
            ,@(mc [channel-knobs           _] chs-sends)
            ,@(mc [channel-in          _ chs] chs)
            ,@(mc [channel-sends _ sends chs] sends)
            ,@(mc [channel-inserts         _] chs-sends)
            ,@(mc [channel-level           _] chs-sends)
            ,(compile-master-channel chs-sends m))))

(def channel-setter (inum)
  `(instr (,inum)
          (strget -sname p5)
          (chnset _ p4 -sname)))

(def compile-header (song)
  (w/ (m (mixer song))
    `(pn (= sr     ,(interp (sr     m)))
         (= kr     ,(interp (kr     m)))
         (= ksmps  ,(interp (ksmps  m)))
         (= nchnls ,(interp (nchnls m)))
         (= 0dbfs  ,(interp (0dbfs  m)))
         ,@(ma [copy (stmts _)] (insts song)))))

(def compile-insts (song)
  `(pn ,@(mc [ls (csmac-name _) (inst-inum _)] (insts song))))

(def orchestra-gen (song)
  (reset-chnctl-registry)
  (reset-channel-registry)
  (reset-inst-registry)
  `(orc ,(compile-header song)
        ,(compile-insts  song)
        ,(compile-mixer  song)
        ,(channel-setter 1001)))

(def csd-gen (song &k options score)
  (with-output-to-string (str)
    (w/ (*standard-output* str m (mixer song))
      (w/error-screen (mkstr "There was an error generating the csd file:~%~%~A" e)
        (eval `(with "CsoundSynthesizer"
                     (as "CsOptions" ,(or options (options m)))
                     (with "CsInstruments" ,(orchestra-gen song))
                     (as "CsScore" ,(or score (score m)))))))))

;;; sample lib

(def add-sample (sample song)
  (w/ (st (samples song))
    (=! (table-num sample) (aif (getab (id sample) (hash st))
                                (table-num it)
                                (incf (table-num st))))
    (putab (id sample) sample (hash st))))

(def get-sample (id &o (s (s))) (getab id (hash (samples s))))

(mac def-sample def
  `(add-sample (new 'sample ,@def) (s)))

(def set-slices (id . slices)
  (slotf (get-sample id) 'slices slices))

; sample fns

(classy wav ()
  (compression    nil)
  (channels       nil)
  (sample-rate    nil)
  (bytes/sample   nil)
  (block-align    nil)
  (bits/sample    nil)
  (data-size      nil))

(def mk-wav (path)
  (with-open-file (stream path :direction :input :element-type '(unsigned-byte 8))
    (w/ (wav (new 'wav))
      (and (equal "RIFF" (byte-str stream 4))
           (read-32 stream)
           (equal "WAVE" (byte-str stream 4))
           (equal "fmt " (byte-str stream 4))
           (= 16 (read-32 stream))
           (=! (compression  wav) (read-16 stream)
               (channels     wav) (read-16 stream)
               (sample-rate  wav) (read-32 stream)
               (bytes/sample wav) (read-32 stream)
               (block-align  wav) (read-16 stream)
               (bits/sample  wav) (read-16 stream))
           (equal "data" (byte-str stream 4))
           (=! (data-size    wav) (read-32 stream))
           wav))))

(defm samples ((w wav))
  (/ (data-size w) (/ (bits/sample w) 8)))

(w/ (powers2 (mapa<b [expt 2 _] 0 50))
  (def max-power-of-2 (num)
    (w/ last
      (mapp [if (> _ num) last (pn (=! last _) nil)]
            powers2))))

(def import-sample-dir (pathspec &k power2)
  (mc [awhen (and (string= "WAV" (string-upcase (pathname-type _)))
                  (mk-wav _))
        (def-sample :id      (cat (pathname-name _) (when power2 "^2"))
                    :path    (namestring _)
                    :samples (if power2 (max-power-of-2 (samples it)) 0)
                    :sr      (sample-rate it))]
      (ls-dir pathspec)))

; sample range

(classy sample-range ()
  (id            nil)
  (start         nil)
  (end           nil)
  (base-key      nil)
  (start-slice   nil)
  (end-slice     nil))

(def start-samples (range)
  (w/when (sample (get-sample (id range))
           slice  (start-slice range))
    (if (negp slice) 0 (nth slice (slices sample)))))

(def start-secs (range)
  (awhen (start-samples range)
    (/ it (sr (get-sample (id range))))))

(def end-samples (range)
  (w/when (sample (get-sample (id range))
           slice  (end-slice range))
    (if (negp slice) (samples sample) (nth slice (slices sample)))))

(def end-secs (range)
  (awhen (end-samples range)
    (/ it (sr (get-sample (id range))))))

(def mk-srange (id start end base-key &o (start-slice -1) (end-slice -1))
  (new 'sample-range :id          id
                     :start       start
                     :end         end
                     :base-key    base-key
                     :start-slice start-slice
                     :end-slice   end-slice))

; sample map

(classy sample-map ()
  (id      nil)
  (ranges  nil))

(def mk-smap (id . ranges)
  (new 'sample-map :id id :ranges ranges))

(def get-sample-map (id song)
  (mapp [and (equal (id _) id) _] (sample-maps song)))

(def store-sample-map (smap &o (s (s)))
  (awhen (get-sample-map (id smap) s)
    (remove! it (sample-maps s)))
  (rpush smap (sample-maps s)))

(mac def-sample-map (id . defs)
  `(store-sample-map
    (mk-smap ,id ,@(mc [pn `(mk-srange ,@_)]
                       (group defs 6)))))

(def keynum->map-range (keynum sample-map)
  (mapp [when (within (start _) (end _) keynum) _]
        (ranges sample-map)))

(mac def-sample-map-from-dir (id pathspec &k (start 0) (range-length 1) (offset 0) power2)
  (w/ (idx start)
    `(def-sample-map ,id
       ,@(ma [when (string= "WAV" (string-upcase (pathname-type _)))
               (prog1 (ls (cat (pathname-name _) (if power2 "^2" ""))
                          idx (+ -1 idx range-length) (+ idx offset) -1 -1)
                      (+! idx range-length))]
             (ls-dir pathspec)))))

; sample-map pprinter

(set-pprint-dispatch 'sample-map
  (fn (stream obj)
    (format stream "(def-sample-map '~s" (id obj))
    (dolist (r (ranges obj))
      (format stream "~&~10@s ~3@a ~3@a ~3@a ~3@a ~3@a"
              (id r) (start r) (end r) (base-key r)
              (start-slice r) (end-slice r)))
    (format stream ")")))

;;; csound

; csound predicates

(defm online ((n null)))

(defm offline (obj) (no (online obj)))

(def csi-p (csi) (no (fnullp csi)))

(def cs-state (cs)
  (if cs
      (if (csi-p (csi cs))
          (if (csd cs)
              (aif (compile-code cs)
                   (if (= it 0)
                       (if (online cs) ; - - - - - - - Pew! Pew!
                               0
                           1)
                       2)
                   3)
              4)
          5)
      6))

(defp *cs-state-strings*
  '("Online"
    "Offline"
    "Compile error"
    "Uncompiled"
    "No csd"
    "csi-create error"
    "Uninitialized"))

(def cs-state-str () (nth (cs-state (c-cs)) *cs-state-strings*))

(defs online-p  (cs) (= (cs-state cs) 0)
      offline-p (cs) (= (cs-state cs) 1)
      ready-p   (cs) (< (cs-state cs) 2)
      csd-p     (cs) (< (cs-state cs) 4)
      init-p    (cs) (< (cs-state cs) 5))

; csound class

(defps *csound* nil *csd-name* "sqnc.csd")

(def c-cs () *csound*)

(def get-csi ()
  (when (>= (csoundinitialize (fnull) (fnull) 1) 0)
    (csoundcreate (fnull))))

(classy csound ()
  (csi           (get-csi))
  (csd           (namestring (merge-pathnames *sqnc-dir* *csd-name*)))
  (compile-code  nil)
  (online        nil)
  (processing    nil)
  (msgs          (make-array 0 :fill-pointer 0 :adjustable t))
  (num-msgs      30))

; cs messages

(defp *dump-cs-msgs* nil)

(def pop-cs-msg (cs)
  (prog1 (csoundgetfirstmessage (csi cs))
    (csoundpopfirstmessage (csi cs))))

(def cs-msgs () (aif (c-cs) (msgs it) ""))

(def dump-cs-msgs (cs)
  (w/ (msgs (msgs cs))
    (awhile (pop-cs-msg cs)
      (map nil [vpushx _ msgs] it))))

; csound thread

(def csound-perform (cs)
  (w/error-screen (mkstr "There was an error in the csound performance:~%~%~a" e)
    (t-within ((processing cs) (online cs))
      (w/ (csi (csi cs))
        (while (and (online cs) (zerop (csoundperformksmps csi)))
          (if *dump-cs-msgs* (dump-cs-msgs cs)))))))

(def csound-start-performance (cs)
  (when (offline-p cs)
    (mk-thread (fn () (csound-perform cs)) "csound")))

(def csound-stop-performance (cs)
  (when (online-p cs)
    (nil! (online cs))
    (sleep-while (processing cs))))

(def csound-kill (cs)
  (csound-stop-performance cs)
  (csoundreset   (csi cs))
  (csoundcleanup (csi cs)))

; stmt

(classy event ())

(classy stmt (event)
  (type-code     nil)
  (pfields       nil)
  (len           nil))

(classy csstr (event)
  (type-str      nil)
  (pf-str        nil))

(def mk-stmt (cc lst)
  (w/ (l (ln lst))
    (new 'stmt :type-code (char-code cc)
               :len       l
               :pfields   (list->farr lst (get-farr l)))))

(def mk-csstr (cc lst)
  (new 'csstr :type-str (mkstr "~a " cc)
              :pf-str   (mkstr "~{~S~^ ~}" lst)))

(defs -i  pf (mk-stmt  #\i pf)
      -f  pf (mk-stmt  #\f pf)
      -is pf (mk-csstr #\i pf)
      -fs pf (mk-csstr #\f pf))

; output-buffer

(classy output-buffer ()
  (buf            (make-array 100 :fill-pointer 0 :adjustable t))
  (buflen         1/4)
  (latency        1/4)
  (scheduling     nil)
  (processing     nil))

(defp *opb* (new 'output-buffer))

(def set-latency (latency &o (opb *opb*))
  (w/ (l (/ latency 2))
    (=! (latency opb) l (buflen opb) l)))

; output

(defm output (obj (n null))
  (mb-msg "Output's destination is nil"))

(defm output ((o output-buffer) dest)
  (output (buf o) dest))

(defm output ((v vector) dest)
  (map nil [output _ dest] v)
  (=! (fill-pointer v) 0))

(defm output ((h hash-table) dest)
  (dohash (k v h) (output v dest)))

(defm output ((l list) dest)
  (mapc [output _ dest] l))

;; (defm output ((r trg) dest)
;;   (output (generator r (start r)) dest))

(defm output ((r trg) dest)
  (output (offset (generator r) (start r)) dest))

(defp *output-mutex* (make-mutex "Csound output mutex"))

(defm output ((s stmt) (cs csound))
  (when (online-p cs)
    (w/mutex (*output-mutex*)
      (csoundscoreevent (csi cs) (type-code s) (pfields s) (len s))))
  (free-farr (pfields s) (len s)))

(defm output ((stmt stmt) (stream stream))
  (format stream "~a ~{~,5f~^ ~}~%" (code-char (type-code stmt))
          (nrev (for (n 0 (len stmt) res)
                  (push (faref n (pfields stmt)) res)))))

(def csstr (css) (cat (type-str css) " " (pf-str css)))

(defm output ((css csstr) (cs csound))
  (when (online-p cs)
    (w/mutex (*output-mutex*)
      (w/fstr (s (csstr css))
        (csoundinputmessage (csi cs) s)))))

(defm output ((css csstr) (stream stream))
  (format stream "~a~%" (csstr css)))

(def chnctl-css (ctl)
  (new 'csstr
       :type-str "i "
       :pf-str (mkstr "1001 0 0.0001 ~,5f ~s" (interp ctl) (chnctl-name ctl))))

(defm output ((c chnctl) (cs csound))
  (output (chnctl-css c) cs))

(defm output ((c chnctl) (stream stream))
  (output (chnctl-css c) stream))

(defm output ((e effect) dest)
  (mapc [output _ dest] (params e)))

(defm output ((ch channel) dest)
  (output (amp  ch) dest)
  (output (mute ch) dest)
  (mapc [output _ dest] (inserts ch))
  (mapc [output _ dest] (sends ch)))

(defm output ((s sample) dest)
  (output (-fs (table-num s) 0 (samples s) 1 (namestring (path s)) 0 0 0)
          dest))

(defm output ((s sample-table) dest)
  (output (hash s) dest))

(defm output ((m mixer) dest)
  (output (-i (inum m) 0 -1) dest)
  (output (amp m) dest)
  (output (pan m) dest)
  (mapc [output _ dest] (channels m)))

(defm output ((s song) dest)
  (output (mixer   s) dest)
  (output (samples s) dest))

; offset

(defm offset ((opb output-buffer) secs)
  (offset (buf opb) secs)
  opb)

(defm offset ((vec vector) secs)
  (map nil [offset _ secs] vec)
  vec)

(defm offset ((lst list) secs)
  (mapc [offset _ secs] lst))

(defm offset ((stmt stmt) secs)
  (+! (faref 1 (pfields stmt)) (coerce secs 'single-float))
  stmt)

; schedule

(defm schedule ((v vector) place)
  (map nil [schedule _ place] v)
  (free-arr v))

(defm schedule ((lst list) place)
  (map nil [schedule _ place] lst))

(defm schedule ((r trg) place)
  (schedule (offset (generator r) (start r)) place))

(defm schedule ((e event) (opb output-buffer))
  (vpushx e (buf opb)))

;;; scheduler

(def in-range (trg beg end)
  (w/ (secs (start trg))
    (when (and (<= beg secs) (< secs end))
      (- secs beg))))

(def mute-state (trg)
  (or (muted (track trg)) (muted trg)))

(def ptrg->event (ptrg beg end)
  (w/* (pbeg   (start ptrg)
        pdur   (dur ptrg)
        pend   (+ pbeg pdur)
        newbeg (max 0 (- beg pbeg))
        newend (+ newbeg (- (min pend end) (max pbeg beg)))
        event  (pattern->event (pat ptrg) newbeg newend)
        offset (- pbeg beg))
    (if (posp offset)
        (offset event offset)
        event)))

(def pattern->event (pat beg end)
  (w/n (event (get-arr))
    (dolist (trg (all-ptrgs-in-range pat beg end))
      (or (mute-state trg)
          (vpushx (ptrg->event trg beg end) event)))
    (dolist (trg (trgs-in-range pat beg end))
      (or (mute-state trg)
          (vpushx (offset (generator trg) (in-range trg beg end)) event)))))

(def loop-ranges (len song)
  (w/access ((ls loop-start) (le loop-end) (cur current-time)) (transport song)
    (loop with ref = 0
          for  rem = (- len ref)
          for  dif = (- le  cur)
          while (< ref len)
          collect (if (< rem dif)
                      (prog1 (ls cur (+ cur rem))
                             (+! ref rem)
                             (+! cur rem))
                      (prog1 (ls cur le)
                             (+! ref dif)
                             (=! cur ls))))))

(defm now (cs)
  (if (online-p cs)
      (csoundgetscoretime (csi cs))
      0))

(def scheduler (song cs &o (opb *opb*))
  (t-within ((scheduling opb) (processing opb))
    (loop while (scheduling opb)
          with pat     = (root-pattern song)
          with buflen  = (buflen opb)
          with latency = (latency opb)
          with error   = 0
          for  then    = (- (now cs) error)
          do (pn (sleep (- buflen error))
                 (loop for cur = 0 then (+ cur (- end beg))
                       for (beg end) in (loop-ranges buflen song)
                       do  (schedule (offset (pattern->event pat beg end) cur) opb))
                 (=! error (confine-to (- latency) latency (- (now cs) (+ then buflen))))
                 (output (offset opb (- latency error)) cs)))))

(def playing? () (scheduling *opb*))

(def play (song)
  (mk-thread (fn () (scheduler song (c-cs))) "sqnc-scheduler"))

(def stop (&o (opb *opb*))
  (nil! (scheduling opb))
  (sleep-while (processing opb)))

(def rewind (song)
  (zero! (current-time (transport song))))

;;; render song

(def generate-score-from-song (song start end)
  (with-output-to-string (str)
    (output (-fs 0 (1+ (float (- end start)))) str) ; Why is adding 1 necessary?
    (output song str)
    (output (offset (pattern->event (root-pattern song) start end) .01) str)))

(def generate-csd-from-song (song start end)
  (csd-gen song
           :score   (generate-score-from-song song start end)
           :options (offline-options (mixer song))))

;;; start-csound

(def csound-compile (cs)
  (when (csd-p cs)
    (=! (compile-code cs)
          (w/fstr (csd (sqnc-file-namestring (csd cs)))
            (csoundcompilecsd (csi cs) csd)))))

(def start-csound (song cs)
  (awhen cs (csound-kill it))
  (w/n (p (playing?) cs (new 'csound))
    (stop)
    (=! *csound* cs)
    (csoundenablemessagebuffer (csi cs) 0)
    (write-string-to-file (csd-gen song) (csd cs))
    (csound-compile cs)
    (csound-start-performance cs)
    (output song cs)
    (when p (play song))))

;;; primitive destructives

(def trg-details-str (trg song)
  (mkstr "~a - Track: ~a  Pattern: ~a  Start-time: ~a  Keynum: ~a"
         (type-of trg)
         (idx (track trg) (tracks song))
         (idx (pattern trg) (patterns song))
         (start trg)
         (keynum trg)))

(mac w/audio-restart body
  (w/uniq (cs playing online)
    `(w/* (,cs (c-cs)
           ,playing (playing?)
           ,online (online-p ,cs))
       (when (playing?) (stop))
       ,@body
       (when ,online (start-csound (s) (c-cs)))
       (when ,playing (play (s))))))

; _add

(defm _add ((p pattern) (s song) &o idx)
  (buf-ops :touch p)
  (w/ (idx (or idx (ln (patterns s))))
    (ins@n! p idx (patterns s))
    (a=! (pattern s) (or it p))
    (mb-msg (mkstr "Added pattern at index ~a" idx)))
  p)

(defm _add ((trk track) (s song) &o idx)
  (buf-ops :break trk)
  (w/ (idx (or idx (ln (tracks s))))
    (ins@n! trk idx (tracks s))
    (a=! (track (pattern s)) (or it trk))
    (mb-msg (mkstr "Added track at index ~a" idx)))
  trk)

(defm _add ((i inst) (s song) &o idx)
  (buf-ops :touch i :break i)
  (w/audio-restart
    (w/ (idx (or idx (ln (insts s))))
      (ins@n! i idx (insts s))
      (a=! (inst   s) (or it i))
      (a=! (preset i) (or it (car (presets i))))
      (a=! (trg    s) (or it (copy (preset i))))
      (mb-msg (mkstr "Added inst at index ~a" idx))))
  i)

(defm _add ((trg trg) (s song) &o idx)
  (declare (ignore idx))
  (when (trg-within-bounds trg)
    (buf-ops :touch trg)
    (trg-hash-add trg)
    (mb-msg (mkstr "Added ~a" (trg-details-str trg s)))
    trg))

(defm _add ((ch channel) (s song) &o idx)
  (buf-ops :break ch)
  (w/ (m (mixer s))
    (w/audio-restart
      (=! (sends ch) (mc [mk-ctl 'kchnctl (name _) 0 1 0] (sends m)))
      (w/ (idx (or idx (ln (channels m))))
        (ins@n! ch idx (channels m))
        (mb-msg (mkstr "Added Channel at index ~a" idx)))))
  ch)

(defm _add ((send send) (song song) &o idx)
  (buf-ops :break send)
  (w/ (mixer (mixer song))
    (w/audio-restart
      (w/ (idx (or idx (ln (sends mixer))))
        (ins@n! send idx (sends mixer))
        (mapc [ins@n! (mk-ctl 'kchnctl (name send) 0 1 0) idx (sends _)]
              (channels mixer))
        (mb-msg (mkstr "Added send channel at index ~a" idx)))))
  send)

(defm _add ((e effect) (ch channel) &o idx)
  (buf-ops :touch e)
  (w/audio-restart
    (w/ (idx (or idx (ln (inserts ch))))
      (ins@n! e idx (inserts ch))
      (mb-msg (mkstr "Added ~a at index ~a" (name e) idx))))
  e)

; _del

(defm _del ((p pattern) (s song))
  (buf-ops :kill p :touch p)
  (remove! p (patterns s))
  (when (eq p (pattern s)) (next-pat! s))
  (mb-msg (mkstr "Deleted pattern at index ~a" (idx p (patterns s))))
  p)

(defm _del ((trk track) (s song))
  (buf-ops :break trk :kill trk)
  (mb-msg (mkstr "Deleted track at index ~a" (idx trk (tracks s))))
  (remove! trk (tracks s))
  (when (eq trk (track s)) (next-trk! s))
  trk)

(defm _del ((i inst) (s song))
  (buf-ops :touch i :break i)
  (when (aand (trg s) (eq i (inst it))) (nil! (trg s)))
  (reset-kill-ring)
  (w/audio-restart
    (w/ (n (idx i (insts s)))
      (remove! i (insts s))
      (=! (inst s) (confined-nth n (insts s)))
      (mb-msg (mkstr "Deleted inst at index ~a" n))))
  i)

(defm _del ((trg trg) (s song))
  (when (trg-within-bounds trg)
    (buf-ops :touch trg)
    (trg-hash-del trg)
    (when (eq trg (trg s)) (=! (trg s) (preset s)))
    (mb-msg (mkstr "Deleted ~a" (trg-details-str trg s)))
    trg))

(defm _del ((ch channel) (song song))
  (w/ (mixer (mixer song))
    (w/audio-restart
      (buf-ops :break ch)
      (mb-msg (mkstr "Deleted channel at index ~a" (idx ch (channels mixer))))
      (remove! ch (channels mixer))))
  ch)

(defm _del ((send send) (song song))
  (w/ (mixer (mixer song))
    (w/audio-restart
      (buf-ops :break send)
      (w/ (i (idx send (sends mixer)))
        (remove! send (sends mixer))
        (mapc [a=! (sends _) (remove-nth i it)] (channels mixer))
        (mb-msg (mkstr "Deleted send channel at index ~a" i)))))
  send)

(defm _del ((e effect) (ch channel))
  (w/audio-restart
    (buf-ops :touch e)
    (remove! e (inserts ch))
    (mb-msg (mkstr "Deleted ~a" (name e))))
  e)

; _slotf

(defm _slotf (obj slot-name val)
  (mb-msg (mkstr "Slot ~a of ~a set to new value" slot-name obj))
  (=! (sval obj slot-name) val))

(defm _slotf ((p pattern) slot-name val)
  (buf-ops :break p)
  (cnm))

(defm _slotf ((trk track) slot-name val)
  (buf-ops :break trk)
  (cnm))

(defm _slotf ((trg trg) slot-name val)
  (buf-ops :touch trg)
  (trg-hash-del trg)
  (cnm)
  (trg-hash-add trg))

(defm _slotf ((c ctl) slot-name val)
  (buf-ops :break c :touch c)
  (cnm))

(defm _slotf ((c chnctl) slot-name val)
  (cnm)
  (awhen (c-cs) (output c it)))

(defm _slotf ((c mixerctl) slot-name val)
  (buf-ops :break c)
  (w/audio-restart (cnm)))

(defm _slotf ((tr transport) slot-name val)
  (buf-ops :touch tr)
  (cnm))

;;;   undo
;;;
;;;   xdo => (redo-thunk undo-thunk)
;;;   xdo => (xdo*)

(defps *xdo-lst* nil *xdo-idx* 0)

(def reset-undo () (=! *xdo-lst* nil *xdo-idx* 0))

(def xdo (xdo &o redo)
  (if (functionp (car xdo))
      (=> (if redo (car xdo) (cadr xdo)))
      (mapc [xdo _ redo] (if redo (rev xdo) xdo))))

(def undo-action () (nth *xdo-idx* *xdo-lst*))

(def redo-action ()
  (w/ (index (1- *xdo-idx*))
    (when (>= index 0)
      (nth index *xdo-lst*))))

(def undo ()
  (awhen (undo-action)
    (xdo it)
    (+! *xdo-idx*)))

(def redo ()
  (awhen (redo-action)
    (xdo it t)
    (-! *xdo-idx*)))

(mac undoable b
  (w/uniq (xdos)
    `(w/ ,xdos
       (w/fn (_u (xdo) (aand xdo (push it ,xdos)))
         ,@b
         (awhen ,xdos
           (nthcdr! *xdo-idx* *xdo-lst*)
           (push it *xdo-lst*)
           (=! *xdo-idx* 1)
           (redo))))))

(read-eval ;; {} - xdo read-macro:
  (set-macro-character
    #\{ (fn (s c) c `(undoable ,(read-delimited-list #\} s t))))
  (set-macro-character #\} (get-macro-character #\))))

(mac mk-xdo (redo undo) `(ls (fn () ,redo) (fn () ,undo)))

;; undoable actions

; uadd

(w/mac (h (obj it) `(mk-xdo (_add ,obj ,it idx) (_del ,obj ,it)))
  (defms uadd ((p pattern) (s song)    &o idx) (h p s)
         uadd ((k track)   (s song)    &o idx) (h k s)
         uadd ((n inst)    (s song)    &o idx) (h n s)
         uadd ((r trg)     (s song)    &o idx) (h r s)
         uadd ((c channel) (m mixer)   &o idx) (h c m)
         uadd ((e effect)  (c channel) &o idx) (h e c)))

; udel

(defm udel ((p pattern) (s song))
  (nrev (w/n (i (idx p (patterns s)) xdo)
          (push (mc [udel _ s] (query-trgs p)) xdo)
          (push (mk-xdo (_del p s) (_add p s i)) xdo))))

(defm udel ((trk track) (s song))
  (nrev (w/n (i (idx trk (tracks s)) xdo)
          (push (mc [udel _ s] (trk-trgs trk)) xdo)
          (push (mk-xdo (_del trk s) (_add trk s i)) xdo))))

(defm udel ((inst inst) (s song))
  (nrev (w/n (i (idx inst (insts s)) xdo)
          (push (mc [udel _ s] (inst-trgs inst)) xdo)
          (push (mk-xdo (_del inst s) (_add inst s i)) xdo))))

(defm udel ((trg trg) (s song))
  (mk-xdo (_del trg s) (_add trg s)))

(defm udel ((ch channel) (m mixer))
  (w/ (i (idx ch (channels m)))
    (mk-xdo (_del ch m) (_add ch m i))))

(defm udel ((e effect) (c channel))
  (w/ (i (idx e (inserts c)))
    (mk-xdo (_del e c) (_add e c i))))

; uslotf

(defm uslotf (obj slot-name val)
  (w/ (saved (sval obj slot-name))
    (mk-xdo (_slotf obj slot-name val)
            (_slotf obj slot-name saved))))

(def del-trgs-by-preds (lst . preds)
  (filter [unless (-> passes _ preds) (udel _ (s))]
          lst))

(defm uslotf ((trk track) slot-name val)
  (case slot-name
    (keys (cons (cnm) (del-trgs-by-preds (trk-trgs trk) [< (keynum _) val])))
    (otherwise (cnm))))

; misc undoables

(def uoffset-slot (obj sn off &k lo hi)
  (w/* (old (sval obj sn)
        new (+ old off)
        new (if lo (max lo new) new)
        new (if hi (min hi new) new))
    (unless (= old new) (uslotf obj sn new))))

(def uoffset-slots (objs sn off &k lo hi)
  (filter [uoffset-slot _ sn off :lo lo :hi hi] objs))

(defm usetnth (n lst val)
  (w/ (saved (nth n lst))
    (mk-xdo (=! (nth n lst) val)
            (=! (nth n lst) saved))))

;;; song

(def create-default-song ()
  (w/ (song (new 'song))
    (_add (new 'pattern :name "Root Pattern" :resol 1 :measures 200) song)
    (_add (new 'track) song)
    (_add (new 'pattern) song)
    (_add (c-new-ptrg (cadr (patterns song)) song 'keynum 0) song)
    (for (i 0 3) (_add (new 'send    :name (mkstr "Send ~a"    i)) song)) ;; REMOVEME
    (for (i 0 4) (_add (new 'channel :name (mkstr "Channel ~a" i)) song)) ;; REMOVEME
    song))

; song touch/untouch

(defp *savept* nil)

(defm touched ((s song)) (no (eq *savept* (undo-action))))

(defm untouch ((s song)) (=! *savept* (undo-action)))

; mute/unmute

(defm umute (obj) (uslotf obj 'muted t))

(defm uunmute (obj) (uslotf obj 'muted nil))

(defm mute (obj) {_u (umute obj)})

(defm unmute (obj) {_u (uunmute obj)})

(defm mute ((lst list)) {mapc [_u (umute _)] lst})

(defm unmute ((lst list)) {mapc [_u (uunmute _)] lst})

; channel muting

(def toggle-channel-mute (channel)
  (w/ (m (mute channel))
    {_u (uslotf m 'val (if (zerop (val m)) 1 0))}))

; ctl ops

(def ctl-name-set (name c) {_u (uslotf c 'name name)})

(mac w/dctl (ctl n . body)
  `(w/* (env (val ,ctl)
         pt  (nth ,n env))
     env pt ,@body))

(defm add-envpt (ctl &o n) (declare (ignore ctl n)))

(defm add-envpt ((ctl dfctl) &o (n 0))
  (w/dctl ctl n 
    (w/ (pt (copy (aif pt it (car env))))
      {_u (uslotf ctl 'val (ins@n pt n env))})))

(defm del-envpt (ctl &o n) (declare (ignore ctl n)))

(defm del-envpt ((ctl dfctl) &o (n 0))
  (w/dctl ctl n
    (when (and pt (> (ln env) 1))
      {_u (uslotf ctl 'val (remove pt env))})))

(def envpt (ctl n) (nth n (val ctl)))

(def dfctl-set-y (val ctl n)
  (w/* (pt  (envpt ctl n)
        npt (copy pt)
        v   (confine-to (lo ctl) (hi ctl) val))
    (and (!= (y npt) v)
         (=! (y npt) v)
         {_u (uslotf ctl 'val (swap npt pt (val ctl)))})))

(defps *min-env-secs* 0 *max-env-secs* 5)

(def dfctl-set-x (val ctl n)
  (w/* (pt  (envpt ctl n)
        npt (copy pt)
        v   (confine-to *min-env-secs* *max-env-secs* val))
    (and (!= (x npt) v)
         (=! (x npt) v)
         {_u (uslotf ctl 'val (swap npt pt (val ctl)))})))

(def cfctl-set (val ctl)
  (w/ (val (confine-to (lo ctl) (hi ctl) val))
    (unless (= val (val ctl))
      {_u (uslotf ctl 'val val)})))

(def cictl-set (val ctl)
  (w/ (val (trunc (confine-to (lo ctl) (hi ctl) val)))
    (unless (= val (val ctl))
      {_u (uslotf ctl 'val val)})))

;   looping

(def loop-start-ticks (&o (s (s)))
  (s->ticks (loop-start (transport s)) (tpm (root-pattern s))))

(def loop-start-beats (&o (s (s)))
  (s->beats (loop-start (transport s)) (bpm (root-pattern s))))

(def loop-end-ticks (&o (s (s)))
  (s->ticks (loop-end (transport s)) (tpm (root-pattern s))))

(def loop-end-beats (&o (s (s)))
  (s->beats (loop-end (transport s)) (bpm (root-pattern s))))

(def loop-length (&o (s (s)))
  (w/ (tr (transport s))
    (- (loop-end tr) (loop-start tr))))

; Set loop start
(def set-loop-start (start-secs &o (s (s)))
  (w/ (tr (transport s))
    (if (< start-secs (loop-end tr))
        (if (>= start-secs 0)
            (_slotf tr 'loop-start start-secs)
            (mb-msg "Loop start must be >= 0"))
        (mb-msg "Loop start must be less than loop end"))))

; Set loop end
(def set-loop-end (end-secs &o (s (s)))
  (w/ (tr (transport s))
    (if (> end-secs (loop-start tr))
        (_slotf tr 'loop-end end-secs)
        (mb-msg "Loop end must be greater than loop start"))))

; Set loop points
(def set-loop-points (start end &o (s (s)))
  (if (< start (loop-end (transport s)))
      (pn (set-loop-start start s) (set-loop-end end s))
      (pn (set-loop-end end s) (set-loop-start start s))))

(def set-loop-points-by-tick (start-tick end-tick song)
  (w/ (tpm (tpm song))
    (set-loop-points (ticks->s start-tick tpm)
                     (ticks->s end-tick tpm)
                     song)))

; offset loop start
(defm ols (offset &o (s (s)))
  (set-loop-start (+ (loop-start (transport s)) offset) s))

; offset loop end
(def ole (offset &o (s (s)))
  (set-loop-end (+ (loop-end (transport s)) offset) s))

; offset loop points
(def olp (offset &o (s (s)))
  (w/ (tr (transport s))
    (set-loop-points (+ offset (loop-start tr))
                     (+ offset (loop-end tr))
                     s)))

(def loop-trg (trg &o (s (s)))
  (if (root-pattern-p (pattern trg) s)
      (set-loop-points (start trg) (+ (start trg) (dur trg)))
      (mb-msg "Only trgs in the root pattern can be looped.")))

(def set-loop-point-to-point (&o end (s (s)))
  (if (root-pattern-p (pattern s) s)
      (if end
          (set-loop-end (time@pt s) s)
          (set-loop-start (time@pt s) s))
      (mb-msg "Must be in root pattern to set loop points")))

;;; event-gen

(defps *noteoff*    0.00001     ; min csound dur:
       *voice-inc*  0.0001)   ; min delta between csound voices

  ;; *voice-inc*  0.000001)   ; min delta between csound voices

(w/ (voice 0)
  (def voice-alloc ()
    (if (>= (+! voice *voice-inc*) 1)
        (=! voice 0)
        voice)))

(def trg-voice (trg song)
  (aif (idx (inst trg) (insts song))
       (+ 1 it (voice-alloc))
       (mb-msg "Trg's inst not a member of the song's instruments")))

(def trg-gen (trg . ctls)
  (w/n (song  (s)
        dur   (dur trg)
        num   (ceiling (* (interp (res trg)) dur))
        voice (trg-voice trg song)
        trk   (idx (track trg) (tracks song))
        arr   (get-arr))
    (loop for n to num
          for time = (* (/ n num) dur)
          for on/off = (if (= n num) *noteoff* -1)
          for pf = (mc [interp _ time] ctls)
          do (vpushx (-> -i voice time on/off trk pf) arr))))

;;; instrument macros

(defp *available-insts* nil)

(def available-insts () *available-insts*)

(mac definst (class-name supers note . slot-defs)
  `(pn (classy ,class-name ,supers ,@slot-defs)
       (classy ,@note)
       (definit-after (i ,class-name)
         (slotf i 'presets (ls (new ',(car note) :inst i))))
       (rpush-uniq ',class-name *available-insts*)
       (buf-ops :touch (new ',class-name)))) ;; Kludge

(defp *available-effects* nil)

(def available-effects () *available-effects*)

(mac defeffect (class-name . params)
  `(pn (classy ,class-name (effect) ,@params)
       (rpush-uniq ',class-name *available-effects*)))

; sample inst

(mac w/samp ((note keynum range start end) . body)
  (w/uniq (inst map)
    `(w/when (,inst   (inst ,note)
              ,map    (sample-map ,inst)
              ,keynum (keynum ,note)
              ,range  (keynum->map-range ,keynum ,map)
              ,start  (start-secs ,range)
              ,end    (end-secs   ,range))
      ,@body)))

; command loop

(defps *done*      nil
       *recursive* nil)

(def cmd-loop (&o *recursive*)
  (w/ *done*
    (until *done*
      (refresh-sqnc)
      (dispatch))))

;; (def cmd-loop (&o *recursive*)
;;   (w/ *done*
;;     (until *done*
;;       (w/error-screen (mkstr "An error occurred:~%~%~a" e)
;;         (refresh-sqnc)
;;      (dispatch)))))

(def full-screen-buffer-excursion (buffer-type &o (w (c-win)))
  (w/ (b (find-or-open buffer-type))
    (window-excursion
      (delete-other-windows w)
      (raise b w)
      (cmd-loop t))))

; minibuffer query

(defps *mbq-pathname-km* nil
       *mbq-command-km*  nil)

(def mbq (type &o (prompt "") (contents ""))
  (minibuffer-excursion
   (mb-set (ln contents) prompt contents)
   (w/ (mbkms (keymaps (miniwij)))
     (=! (keymaps (miniwij))
         (case type
           (pathname (cons *mbq-pathname-km* mbkms))
           (command  (cons *mbq-command-km*  mbkms))
           (otherwise mbkms)))
     (prog1 (awhen (cmd-loop t)
              (case type
                (string    it)
                (command   it)
                (pathname  (pathname it))
                (yn        (y? it))
                (otherwise (w/ (v (ignore-errors (stread it)))
                             (if (typep v type)
                                 v
                                 (mb-msg (mkstr "That command needs a value of type ~a" type)))))))
       (=! (keymaps (miniwij)) mbkms)))))

; toplevel

(def load-.sqnc ()
  (w/error-screen (mkstr "There was an error loading your .sqnc file:~%~%~a" e)
    (load (merge-pathnames (user-homedir-pathname) ".sqnc"))))

(def sqnc ()
  (rebind-stdio
   (enable-sigwinch #'sigwinch-handler)
   (init-display)
   (set-song (create-default-song))
   (set-loop-points-by-tick 0 4 (s))
   (load-.sqnc)
   (mb-msg "hack QWANward, sqncr")
   (cmd-loop)
   (quit-sqnc)))
