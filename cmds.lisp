(in-package :sqnc)

; defcmd

(defps *fns->cmds* (table)
       *cmds->fns* (table :test #'equal))

(macs cmd->fn (c) `(getab ,c *cmds->fns*)
      fn->cmd (f) `(getab ,f *fns->cmds*))

(def command-bind (c f)
  (aand (string-downcase c)
        (=! (cmd->fn it) f (fn->cmd f) it)))

(defm update-current ((b buffer) (w window) (s song)))

(mac defcmd (name args . body)
  (w/uniq (res)
    `(pn (def ,name ,args
           (mb-clear)
           (w/n (,res (pn ,@body))
             (update-current (c-buf) (c-win) (s))
             (run-hooks)))
         (command-bind (symbol-name ',name) #',name))))

(plural defcmd defcmds 3)

; undo/redo

(defcmd undo-command ()
  (unless (undo)
    (mb-msg "No more to undo")))

(defcmd redo-command ()
  (unless (redo)
    (mb-msg "No more to redo")))

; open/save

(def save-song (song pathname &o force)
  (if (or (touched song) force)
      (when (w/error-screen (mkstr "There was an error saving the song:~%~%~A" e)
              (cl-store:store song pathname))
        (pn (untouch song)
            (mb-msg (mkstr "Wrote ~A" pathname))))
      (mb-msg "(No changes need to be saved)")))

(def reset-environment ()
  (kill-all-buffers)
  (reset-undo)
  (reset-kill-ring)
  (unmark-mtrgs))

(def open-song (pathname)
  (if (no (probe-file pathname))
      (mb-msg "File doesn't exist")
      (awhen (w/error-screen (mkstr "There was an error opening the song:~%~%~A" e)
                             (cl-store:restore pathname))
        (w/audio-restart
          (reset-environment)
          (set-song it)
          (mb-msg (mkstr "Opened ~A" pathname))))))

(def new-song ()
  (reset-environment)
  (w/audio-restart
    (set-song (create-default-song))
    (mb-msg (mkstr "Opened new song"))))

(mac kill-song-check (form)
  `(when (or (not (touched (s)))
             (mbq 'yn "Current song hasn't been saved. Kill anyway? y/[n]:"))
     ,form))

(def check-open-song (pathname)
  (kill-song-check (open-song pathname)))

(defcmd new-song-cmd ()
  (kill-song-check (new-song)))

(def song-name (i)
  (sqnc-file-namestring (mkstr "song~4,'0d.sqnc" i)))

(def current-songname-index ()
  (w/ (i -1)
    (while (probe-file (song-name (+! i))))
    (when (posp i) (1- i))))

(def free-songname-index ()
  (aif (current-songname-index)
       (1+ it)
       0))

(defcmd query-save-song-as (&o (s (s)))
  (awhen (mbq 'pathname "Write file: " (song-name (free-songname-index)))
    (=! (path s) it)
    (save-song s it t)))

(defcmd query-save-song (&o (s (s)))
  (aif (path s)
       (save-song s it)
       (query-save-song-as s)))

(defcmd query-open-song ()
  (awhen (mbq 'pathname "Filename: " (aif (current-songname-index) (song-name it)))
    (check-open-song it)))

; exit

(def done! () (t! *done*))

(defcmd exit-sqnc (&o (s (s)))
  (when (or (no (touched s))
            (mbq 'yn "Current song hasn't been saved. Exit anyway? y/[n]:"))
    (done!)))

; track muting

(defcmd toggle-mute-c-trk (&o (s (s)))
  (awhen (track s) (if (muted it) (unmute it) (mute it))))

; prev and next

(defcmd prev-inst-cmd (&o (s (s)))
  (prev-inst! s)
  (mb-msg (mkstr "Current instrument set to ~a" (name (inst s)))))

(defcmd next-inst-cmd (&o (s (s)))
  (next-inst! s)
  (mb-msg (mkstr "Current instrument set to ~a" (name (inst s)))))

(defcmd prev-preset-cmd (&o (s (s)))
  (prev-preset! s)
  (mb-msg (mkstr "Current preset set to ~a" (preset s))))

(defcmd next-preset-cmd (&o (s (s)))
  (next-preset! s)
  (mb-msg (mkstr "Current preset set to ~a" (preset s))))

; insert and delete tracks

(defcmd insert-track (&o before (s (s)))
  {_u (uadd (new 'track) s
            (when before (max 1 (idx (track s) (tracks s)))))})

(defcmd insert-track-before () (insert-track t))

(defcmd delete-track (&o (s (s)) (trk (track s)))
  (if (= 1 (ln (tracks s)))
      (mb-msg "Song must have at least one track")
      (pn (when (eq trk (track s)) (next-trk! s))
            ;; (=! (track (pattern s)) (next-trk s)))
          {_u (udel trk s)})))

; insert and delete patterns

(defcmd insert-pattern ()
  {_u (uadd (new 'pattern) (s))})

(defcmd insert-pattern-before (&o (s (s)))
  (if (root-pattern-p (pattern s) s)
      (mb-msg "Can't insert pattern before the root pattern")
      {_u (uadd (new 'pattern) s (idx (pattern s) (patterns s)))}))

(defcmd delete-pattern (&o (s (s)) (p (pattern s)))
  (if (root-pattern-p p s)
      (mb-msg "The root pattern can not be deleted")
      (pn (when (eq p (pattern s)) (=! (pattern s) (next-pat s)))
          {_u (udel p s)})))

; insert and delete effects

;; (def c-channel () "This is a stub")

;; (defcmd insert-effect (&o (w (c-wij)) (ch (c-channel)))
;;   (awhen (nth (cur w) *available-effects*)
;;     {_u (uadd (new it) ch)}))

;; (defcmd delete-effect (effect channel)
;;   {_u (udel effect channel)}
;;   (mb-msg (mkstr "Deleted effect ~a" effect)))

; renaming

(defcmd query-rename-c-pat (&o (s (s)))
  (awhen (mbq 'string "Pattern name: " (name (pattern s)))
    {_u (uslotf (pattern s) 'name it)}))

(defcmd query-rename-c-trk (&o (s (s)))
  (awhen (mbq 'string "Track name: " (name (track s)))
    {_u (uslotf (track s) 'name it)}))

; misc

(defcmd quit-command ()
  (reset-num-arg)
  (reset-kmacro)
  (mb-msg "Quit"))

(push #'quit-command *meta-cmds*)

(defcmd reset-colors ()
  (init-colors)
  (mb-msg "Reset curses color palette"))

(defcmd init-swank-command ()
  (aif (init-swank)
       (mb-msg (mkstr "Swank initialized on port ~A" it))
       (mb-msg (mkstr "Swank already open on port ~A" (c-swank)))))

(defcmd reload-.sqnc () (load-.sqnc))

(defcmd toggle-dump-cs-msgs () (toggle *dump-cs-msgs*))

(defcmd export-csd (&o (s (s)))
  (awhen (mbq 'pathname "Filename: " *sqnc-dir*)
    (w/ (tr (transport s))
      (write-string-to-file
       (generate-csd-from-song s (loop-start tr) (loop-end tr))
       it))))

; edit step

(defp *edit-step* 1)

(def set-edit-step (n) (=! *edit-step* (confine-to 1 128 n)))

(defcmds incf-edit-step () (set-edit-step (1+ *edit-step*))
         decf-edit-step () (set-edit-step (1- *edit-step*)))

; c-oct and self-output-last-key

(defp *current-octave* 4)

(def c-oct () *current-octave*)

(def offset-c-oct (off)
  (a=! *current-octave* (confine-to 0 12 (+ it off))))

(defcmds incf-c-oct () (offset-c-oct 1)
         decf-c-oct () (offset-c-oct -1))

(defcmd self-output-last-key (&o (s (s)) (cs (c-cs)))
  (output (c-new-trg s 'keynum (+ (last-pitchclass) (* 12 (c-oct))) 'start 0)
          cs))

; keymap

(defcmd reset-keymaps (&o (w (c-wij)))
  (=! (keymaps w) (get-keymaps w)))

;;; trg

(defcmd cp-trg@p-c-trg (&o (s (s)))
  (awhen (trg-in-ctick s) (=! (trg s) (copy it))))

(defcmd cp-preset-c-trg (&o (s (s)))
  (awhen (preset s) (=! (trg s) (copy it))))

; insert and delete trgs

(defcmd insert-trg (&o (s (s)) (trg (c-new-trg s)) dups)
  (if trg
      (if (or dups (no (trg@p s)))
          (pn {_u (uadd trg s)}
              (mb-msg (mkstr "Added ~a" (trg-details-str trg s))))
          (mb-msg "There's already a trg at point"))
      (mb-msg "There are no instruments loaded")))

(def cyclic-ptrg? (ptrg)
  (w/fn (rec (pat)
          (or (eq pat (pattern ptrg))
              (mapp [rec (pat _)]
                    (query-trgs pat :preds (ls #'pattern-trg-p)))))
    (rec (pat ptrg))))

(defcmd query-insert-pattern-trg (&o (s (s)))
  (aand (mbq 'integer "Pattern index: ")
        (nth it (patterns s))
        (c-new-ptrg it s)
        (if (cyclic-ptrg? it)
            (mb-msg "That would cause a cycle in the pattern graph. Naughty, naughty.")
            (insert-trg it))))

(def delete-trg (trg)
  (unmark-trg trg)
  {_u (udel trg (s))}
  (mb-msg (mkstr "Deleted ~A at ~A and time ~A" (type-of trg)
                 (note-str (keynum trg)) (start trg))))

(def delete-trgs (trgs)
  (unmark-trgs trgs)
  {mapc [_u (udel _ (s))] trgs})

; trg marking

(mac c-mtrgs () `(marked (c-buf)))

(def markedp (trg &o (b (c-buf))) (in trg (marked b)))

(def unmark-trg (trg &o (b (c-buf)))
  (when (markedp trg b)
    (buf-ops :touch trg)
    (remove! trg (c-mtrgs))))

(def unmark-trgs (trgs) (mapc #'unmark-trg trgs))

(def unmark-mtrgs () (unmark-trgs (c-mtrgs)))

(defcmd quit-and-unmark () (unmark-mtrgs) (quit-command))
(push #'quit-and-unmark *meta-cmds*)

(def mark-trg (trg &o (b (c-buf)))
  (unless (markedp trg b)
    (buf-ops :touch trg)
    (push trg (c-mtrgs))))

(def reset-mark-trg (trg) (unmark-mtrgs) (mark-trg trg))

(def mark-trgs (trgs) (unmark-mtrgs) (mapc #'mark-trg trgs))

(defcmd mark-active-trg (&o (s (s)))
  (awhen (trg-in-ctick s) (reset-mark-trg it)))

(defcmd mark-add-active-trg (&o (s (s)))
  (awhen (trg-in-ctick s) (mark-trg it)))

(defcmd toggle-mark-add-active-trg (&o (s (s)))
  (awhen (trg-in-ctick s)
    (if (markedp it) (unmark-trg it) (mark-trg it))))

; mark rectangular region

(defps *mark-pattern* nil
       *mark-track*   nil
       *mark-time*    nil
       *mark-keynum*  nil)

(defcmd set-mark-command (&o (s (s)))
  (=! *mark-pattern* (pattern s)
      *mark-track*   (track   s)
      *mark-time*    (time@pt s)
      *mark-keynum*  (keynum  s)))

(defcmd mark-rectangular-region (&o (s (s)))
  (when (and *mark-pattern* *mark-track* *mark-time* *mark-keynum*)
    (mark-trgs (get-trgs-in-range (min (time@pt s) *mark-time*)
                                  (max (time@pt s) *mark-time*)
                                  (min (keynum  s) *mark-keynum*)
                                  (max (keynum  s) *mark-keynum*)
                                  s))))

; trg deletion

(defcmd delete-mtrgs () (delete-trgs (c-mtrgs)))

(defcmd delete-trg-p+d (&o (s (s)))
  (aif (trg-in-ctick s)
       (delete-trg it)
       (mb-msg "No trg at point")))

; trg kill

(defp *kill-ring* nil) ; not a "ring" yet

(def set-kill-ring (killed) (=! *kill-ring* killed))

(def reset-kill-ring () (nil! *kill-ring*))

(defcmd kill (&o just-save)
  (set-kill-ring (c-mtrgs))
  (unless just-save (delete-mtrgs)))

(defcmd kill-ring-save () (kill t))

(defcmd yank (&o mark (kr *kill-ring*) (s (s)))
  (when kr
    (unmark-mtrgs)
    {mapc [w/* (nst  (+ (- (start  _) (start  (-> keyed-min #'start  kr))) (time@pt s))
                nkn  (+ (- (keynum _) (keynum (-> keyed-min #'keynum kr))) (keynum  s))
                ntrg (copy-and-tag-trg _ s 'start nst 'keynum nkn))
            (when (and (no (trgs@trg ntrg))
                       (< nst (dur  (pattern ntrg)))
                       (< nkn (keys (track   ntrg))))
              (_u (uadd ntrg s))
              (if mark (mark-trg ntrg)))]
          kr}))

(defcmd yank-and-mark () (yank t))

(defcmd mark-whole-buffer (&o (s (s))) (mark-trgs (cpat-ctrk-trgs s)))

; trg sequence ops

(def tickify (seq &k (start 0) (inc 4))
  (loop for tick from start by inc
        for pc in seq
        append (ls tick pc)))

(def trgify (seq &o (s (s)))
  (mc [c-new-trg s 'start  (ticks->s (car _) (tpm s))
                   'keynum (cadr _)]
      (pair seq)))

(def output-pc-seq (seq &o (cs (c-cs)))
  (output (trgify (tickify seq)) cs))

(defcmd zelda-secret () (output-pc-seq *zelda-secret*))

;; (def insert-trg-seq (seq &o (s (s)))
;;   (mapc [insert-trg (c-new-trg s 'tick (car _) 'keynum (cadr _))]
;;      (pair seq)))

(def insert-trg-seq (seq &o (s (s)))
  (mapc [insert-trg (c-new-trg s 'start (ticks->s (car _) (tpm s))
                                 'keynum (cadr _))]
        (pair seq)))

(defcmd query-insert-trg-seq ()
  (awhen (mbq 'cons "Enter trg seq ([tick pitchclass]*): ")
    (insert-trg-seq it)))

; trg-off

(def round-or-offset-trg-dur (trg offset)
  (w/* (dur   (dur trg)
        tpm   (tpm trg)
        op    (if (negp offset) #'floor #'ceiling)
        rdur  (ticks->s (=> op (s->ticks dur tpm)) tpm)
        ndur  (if (= dur rdur) (+ dur (ticks->s offset tpm)) rdur)
        ndur  (max *min-trg-dur* ndur))
    (unless (= dur ndur)
      (uslotf trg 'dur ndur))))

(defcmd incf-trg-dur (&o (s (s)))
  (awhen (trg-in-ctick s)
    {_u (round-or-offset-trg-dur it *edit-step*)}))

(defcmd decf-trg-dur (&o (s (s)))
  (awhen (trg-in-ctick s)
    {_u (round-or-offset-trg-dur it (- *edit-step*))}))

(defcmd incf-mtrgs-dur ()
  (awhen (c-mtrgs)
    {pn (dolist (trg it)
          (_u (round-or-offset-trg-dur trg *edit-step*)))}))

(defcmd decf-mtrgs-dur ()
  (awhen (c-mtrgs)
    {pn (dolist (trg it)
          (_u (round-or-offset-trg-dur trg (- *edit-step*))))}))

(defcmd align-trg-start-to-time@pt (&o (song (s)))
  (awhen (trg-in-ctick song)
    {_u (uslotf it 'start (time@pt song))}))

; trg muting

(defcmd toggle-mute-c-active-trg (&o (s (s)))
  (awhen (trg-in-ctick s)
    (if (muted  it)
        (unmute it)
        (mute   it))))

(defcmds mute-mtrgs   () (mute   (c-mtrgs))
         unmute-mtrgs () (unmute (c-mtrgs)))

(defcmd toggle-mute-mtrgs ()
  (awhen (c-mtrgs)
    (if (muted (car it))
        (unmute-mtrgs)
        (mute-mtrgs))))

; invert selection

;; (mac mtrgs-reverse (op)
;;   (w/uniq (hi)
;;     `(awhen (c-mtrgs)
;;        (w/ (,hi (,op (-> keyed-max #',op it)))
;;       {mapc [_u (uslotf _ ',op (- ,hi (,op _)))] it}))))

;; (defcmds reverse-mtrgs-by-tick      () (mtrgs-reverse tick)
;;       reverse-mtrgs-by-keynum    () (mtrgs-reverse keynum)
;;       reverse-mtrgs-amps-by-tick () (mtrgs-reverse amp))

; goto mtrgs boundaries

(def goto-start-time (lst)
  (scro (floor (start-tick (-> keyed-min #'start lst)))
        (cx)))

(def goto-end-time (lst)
  (scro (floor (end-tick (-> keyed-max #'end lst)))
        (cx)))

(def goto-start-keynum (lst)
  (scro (cy) (keynum (-> keyed-min #'keynum lst))))

(def goto-end-keynum (lst)
  (scro (cy) (1+ (keynum (-> keyed-max #'keynum lst)))))

(defcmds goto-mtrgs-start-tick   () (awhen (c-mtrgs) (goto-start-time it))
         goto-mtrgs-end-tick     () (awhen (c-mtrgs) (goto-end-time it))
         goto-mtrgs-start-keynum () (awhen (c-mtrgs) (goto-start-keynum it))
         goto-mtrgs-end-keynum   () (awhen (c-mtrgs) (goto-end-keynum it))
         goto-mtrgs-top-left     () (pn (goto-mtrgs-start-tick) (goto-mtrgs-start-keynum))
         goto-mtrgs-bottom-left  () (pn (goto-mtrgs-end-tick)   (goto-mtrgs-start-keynum))
         goto-mtrgs-top-right    () (pn (goto-mtrgs-start-tick) (goto-mtrgs-end-keynum))
         goto-mtrgs-bottom-right () (pn (goto-mtrgs-end-tick)   (goto-mtrgs-end-keynum)))

(defcmd yank-and-mark-repeatable ()
  (yank-and-mark)
  (goto-mtrgs-bottom-left)
  (center-cursor-v))

; cursor movement

(defcmds point-line-up            () (mcro  1  0)
         point-line-down          () (mcro -1  0)
         point-1-left             () (mcro  0 -1)
         point-1-right            () (mcro  0  1)
         point-line-up-1-left     () (pn (point-line-up)   (point-1-left))
         point-line-down-1-left   () (pn (point-line-down) (point-1-left))
         point-line-up-1-right    () (pn (point-line-up)   (point-1-right))
         point-line-down-1-right  () (pn (point-line-down) (point-1-right))
         point-edit-step-up       () (mcro (- *edit-step*) 0)
         point-edit-step-down     () (mcro    *edit-step*  0)
         point-edit-step-left     () (mcro 0 (- *edit-step*))
         point-edit-step-right    () (mcro 0    *edit-step*)
         point-jump-up            () (mcro -16   0)
         point-jump-down          () (mcro  16   0)
         point-jump-left          () (mcro   0 -24)
         point-jump-right         () (mcro   0  24)
         point-beat-up            () (mcro (- (resol (pattern (s)))) 0)
         point-beat-down          () (mcro    (resol (pattern (s)))  0)
         point-octave-left        () (mcro 0 -12)
         point-octave-right       () (mcro 0  12)
         point-chunk-ul           () (pn (point-beat-up)   (point-octave-left))
         point-chunk-dl           () (pn (point-beat-down) (point-octave-left))
         point-chunk-ur           () (pn (point-beat-up)   (point-octave-right))
         point-chunk-dr           () (pn (point-beat-down) (point-octave-right))
         point-half-beat-up       () (mcro (- (trunc (resol (pattern (s))) 2)) 0)
         point-half-beat-down     () (mcro    (trunc (resol (pattern (s))) 2)  0)
         point-half-octave-left   () (mcro 0 -6)
         point-half-octave-right  () (mcro 0  6)
         point-half-chunk-ul      () (pn (point-half-beat-up)   (point-half-octave-left))
         point-half-chunk-dl      () (pn (point-half-beat-down) (point-half-octave-left))
         point-half-chunk-ur      () (pn (point-half-beat-up)   (point-half-octave-right))
         point-half-chunk-dr      () (pn (point-half-beat-down) (point-half-octave-right))
         point-page-up            () (mcro (- (height (c-win))) 0)
         point-page-down          () (mcro    (height (c-win))  0)
         point-measure-up         () (w/ (p (pattern (s))) (mcro (- (ticks/meas p)) 0))
         point-measure-down       () (w/ (p (pattern (s))) (mcro    (ticks/meas p)  0))
         goto-left                () (scro (cy) 0)
         goto-right               () (scro (cy) (maxvx (c-buf)))
         goto-top                 () (scro 0 (cx))
         goto-bottom              () (scro (maxvy (c-buf)) (cx))
         goto-top-left            () (scro 0 0)
         goto-bottom-left         () (scro (maxvy (c-buf)) 0))

; window scrolling

(def offcenter-y (b w)
  (- (cy b w) (trunc (avg (oy b w) (+ (oy b w) (- (height w) 2))))))

(def offcenter-x (b w)
  (- (cx b w) (trunc (avg (ox b w) (+ (ox b w) (- (width w) 2))))))

(defcmds window-vscroll-up          () (morc -4 0)
         window-vscroll-down        () (morc  4 0)
         window-hscroll-left        () (morc 0 -4)
         window-hscroll-right       () (morc 0  4)
         center-cursor-v            () (morc (offcenter-y (c-buf) (c-win)) 0)
         center-cursor-h            () (morc 0 (offcenter-x (c-buf) (c-win)))
         center-cursor-v-and-h      () (pn (center-cursor-v) (center-cursor-h))
         scroll-cursor-top          () (sorc (cy) (ox))
         scroll-cursor-bottom       () (sorc (maxvy (c-win)) (cx))
         scroll-cursor-left         () (sorc (oy) (cx))
         scroll-cursor-right        () (sorc (oy) (- (cx) (- (width (c-win)) 3)))
         scroll-cursor-top-left     () (pn (scroll-cursor-top)    (scroll-cursor-left))
         scroll-cursor-bottom-left  () (pn (scroll-cursor-bottom) (scroll-cursor-left))
         scroll-cursor-top-right    () (pn (scroll-cursor-top)    (scroll-cursor-right))
         scroll-cursor-bottom-right () (pn (scroll-cursor-bottom) (scroll-cursor-right)))

; query

(defcmd query-goto-measure (&o (s (s)))
  (awhen (mbq 'integer "Measure: " )
    (scro (ticks/pat (pattern s)) (cx))
    (scroll-cursor-top-left)))

; textbox cmds

(defcmd find-textfile (&o (w (c-wij)))
  (when (textbox-p w)
    (aand (mbq 'pathname "Find textfile: ")
          (handler-case
              (get-text-file it)
            (error (e) (mb-msg (mkstr "~a" e))))
          (=! (str w) it)
          (touch (c-buf)))))

(mac textbox-cmd (name . body)
  `(defcmd ,name (&o (wij (c-wij)) (buf (c-buf))) wij buf
     (w/* (str (str wij)
           pt  (confine-to 0 (ln str) (point wij)))
       (declare (ignorable pt str))
       (dbind (pt str) (pn ,@body)
         (set-textbox wij buf pt str)))))

(plural textbox-cmd textbox-cmds 2)

(textbox-cmds move-buf-beg   (ls 0 str)
              move-buf-end   (ls (ln str) str)
              backward-char  (ls (prev-ch   pt str) str)
              forward-char   (ls (next-ch   pt str) str)
              backward-word  (ls (prev-word pt str) str)
              forward-word   (ls (next-word pt str) str)
              move-line-beg  (ls (line-beg  pt str) str)
              move-line-end  (ls (line-end  pt str) str)
              prev-line      (ls (line-up   pt str) str)
              next-line      (ls (line-down pt str) str)
              page-up        (ls (line-up   pt str (height wij)) str)
              page-down      (ls (line-down pt str (height wij)) str))

; origin-row cmds

(defcmd center-textbox-point (&o (w (c-wij)) (b (c-buf)))
  (when (textbox-p w)
    (touch b)
    (a=! (origin-row w)
         (max 0 (+ it (- (pt->y (point w) (str w))
                         (+ it (trunc (height w) 2))))))))

; destructive textbox cmds

(def bsplice (from to str &o (ins "") (off 0))
  (ls (+ from off) (splice from to str ins)))

(mac textbox-cmd! (name . body)
  `(textbox-cmd ,name
     (if (read-only wij)
         (pn (mb-msg "Textbox is read-only")
             (ls pt str))
         (pn ,@body))))

(textbox-cmd! backward-delete-char
  (if (> pt 0)
      (bsplice (1- pt) pt str)
      (ls pt str)))

(textbox-cmd! delete-char
  (if (and (no (empty str)) (< pt (ln str)))
      (bsplice pt (1+ pt) str)
      (ls pt str)))

(defp *text-kill-ring* nil)

(def kill-text (kill)
  (or (empty kill) (push kill *text-kill-ring*)))

(textbox-cmd! backward-kill-word
  (w/* (index (prev-word pt str)
        kill  (subseq str index pt))
    (kill-text kill)
    (bsplice index pt str)))

(textbox-cmd! forward-kill-word
  (w/* (index (next-word pt str)
        kill  (subseq str pt index))
    (kill-text kill)
    (bsplice pt index str)))

(def line-kill-pos (pt str)
  (aif (next-nl pt str)
       (if (> it pt) it (1+ it))
       (ln str)))

(textbox-cmd! kill-line
  (w/* (pos  (line-kill-pos pt str)
        kill (subseq str pt pos))
    (if (empty kill)
        (ls pt str)
        (pn (push kill *text-kill-ring*)
            (bsplice pt pos str)))))

(textbox-cmd! text-yank
  (w/ (yank (car *text-kill-ring*))
    (bsplice pt pt str yank (ln yank))))

(textbox-cmd! self-insert-char
  (aif (last-str)
       (bsplice pt pt str it 1)
       (ls pt str)))

; text return cmds

(defcmd text-return-quit ()
  (quit-command)
  (done!)
  nil)

(push #'text-return-quit *meta-cmds*)

(defcmd text-return-str (&o (w (c-wij)))
  (done!)
  (str w))

(defcmd text-return-word (&o (w (c-wij)))
  (w/* (pt   (point w)
        str  (str w)
        pt1  (aif (prev-wspc pt str) (1+ it) 0)
        pt2  (or (next-wspc pt str) (ln str))
        sub  (subseq str pt1 pt2))
    (unless (empty sub)
      (done!)
      sub)))

(defcmd text-return-line (&o (b (c-buf)))
  (done!)
  (pt->y (point b) (str b)))

; tab

(defp *spaces-per-tab* 5)

(textbox-cmd! insert-tab-spaces
  (w/ (s *spaces-per-tab*)
    (bsplice pt pt str (make-string s :initial-element #\ ) s)))

; tab completion

(def msg-win (str)
  (delete-other-windows (next-window))
  (w/* (buf (open-or-raise 'editor-buffer 'completions (next-window))
        wij (car (widgets buf)))
    (set-textbox wij buf 0 str)
    (t! (read-only wij))))

(def homedir-complete (pt str)
  (if (and (>= pt 2) (string= "~/" (subseq str 0 2)))
      (w/ (home (namestring (user-homedir-pathname)))
        (ls (+ pt (- (ln home) 2)) (cat home (subseq str 2))))
      (ls pt str)))

(def complete (pt str wij names)
  (w/* (str    (or str "")
        pcomps (seqbeg= names str)
        idx    (lcs-idx pcomps))
    (if (> idx pt)
        (ls idx (subseq (car pcomps) 0 idx))
        (pn (if (no pcomps)
                (mb-msg (cat (prompt-str wij) str " [no match]"))
                (msg-win (mkstr "~{~a~%~}" pcomps)))
            (ls pt str)))))

(textbox-cmd! path-complete
  (dbind (pt str) (homedir-complete pt (or str ""))
    (complete pt str wij (mc #'namestring (ls-dir (str wij))))))

(textbox-cmd! cmd-complete
  (complete pt str wij (w/n tx (mh (push k tx) *cmds->fns*))))

;;; display cmds

(defcmds goto-prev-buffer-cmd          () (goto-prev-buffer)
         goto-next-buffer-cmd          () (goto-next-buffer)
         goto-prev-window              () (wgoto (prev-window))
         goto-next-window              () (wgoto (next-window))
         kill-buffer-cmd               () (kill-buffer (c-buf))
         kill-all-buffers-cmd          () (kill-all-buffers)
         split-window-horizontally-cmd () (split-window-horizontally)
         split-window-vertically-cmd   () (split-window-vertically)
         delete-window-cmd             () (delete-window)
         delete-other-windows-cmd      () (delete-other-windows)
         border-up-cmd                 () (border-up)
         border-down-cmd               () (border-down)
         border-left-cmd               () (border-left)
         border-right-cmd              () (border-right)
         border-ul-cmd                 () (border-ul)
         border-dl-cmd                 () (border-dl)
         border-ur-cmd                 () (border-ur)
         border-dr-cmd                 () (border-dr))

(mac def-wij-dir-cmd (dir form)
  `(defcmd ,(mksym 'widget- dir) (&o (b (c-buf)) (w (c-win)))
     (w/ (wij (c-wij))
       (awhen ,form
         (scro (avgy it) (avgx it) b w)
         (touch b)
         it))))

(plural def-wij-dir-cmd def-wij-dir-cmds 2)

(def-wij-dir-cmds up    (get-upward-object    (avgy wij) (avgx wij) (leaf-wijs b))
                  down  (get-downward-object  (avgy wij) (avgx wij) (leaf-wijs b))
                  left  (get-leftward-object  (avgy wij) (avgx wij) (leaf-wijs b))
                  right (get-rightward-object (avgy wij) (avgx wij) (leaf-wijs b)))

;;; key cmds

(defcmds kmacro-start-macro-cmd        () (kmacro-start-macro)
         kmacro-end-macro-cmd          () (kmacro-end-macro)
         kmacro-end-and-call-macro-cmd () (kmacro-end-and-call-macro)
         show-last-kmacro-cmd          () (show-last-kmacro)
         num-arg-cmd                   () (num-arg))

(push #'num-arg-cmd *meta-cmds*)

;;; transport cmds

(defcmds play-cmd  (&o (s (s))) (pn (play s)            (mb-msg "Play!"))
         pause-cmd ()           (pn (stop)              (mb-msg "Pause!"))
         stop-cmd  (&o (s (s))) (pn (stop) (rewind s)   (mb-msg "Stop!"))
         rew-cmd   (&o (s (s))) (pn (rewind s)          (mb-msg "Rewind!")))

(defcmd play-pause (&o (s (s)))
  (if (playing?) (pause-cmd) (play-cmd s)))

(defcmd csound-toggle-online (&o (cs (c-cs)))
  (cif (online-p  cs) (pn (csound-stop-performance cs)
                          (mb-msg "Csound offline"))
       (offline-p cs) (pn (csound-start-performance cs)
                          (mb-msg "Csound online"))
                      (mb-msg "Csound must be initialized first")))

(defcmd start-csound-cmd (&o (s (s)) (cs (c-cs)))
  (if (online-p (start-csound s cs))
      (mb-msg "Csound started")
      (mb-msg "There was an error restarting csound")))

(def loop-ratio-offset (ratio &o (s (s)))
  (olp (* ratio (loop-length s)) s))

(defcmds loop-offset-whole-range       () (loop-ratio-offset    1)
         loop-offset-neg-whole-range   () (loop-ratio-offset   -1)
         loop-offset-half-range        () (loop-ratio-offset   .5)
         loop-offset-neg-half-range    () (loop-ratio-offset  -.5)
         loop-offset-quarter-range     () (loop-ratio-offset  .25)
         loop-offset-neg-quarter-range () (loop-ratio-offset -.25))

(defcmd loop-active-trg (&o (s (s)))
  (aif (trg-in-ctick s)
       (loop-trg it)
       (mb-msg "No trg at point")))

(defcmd set-loop-start-to-point ()
  (set-loop-point-to-point))

(defcmd set-loop-end-to-point ()
  (set-loop-point-to-point t))

(defcmd loop-length-command (&o (s (s)))
  (w/* (secs (loop-length)
        mins (/ secs 60))
    (mb-msg (mkstr "~A:~2$ minutes at ~A bpm"
                   mins secs (bpm (root-pattern s))))))
