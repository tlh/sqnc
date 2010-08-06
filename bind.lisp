(in-package :sqnc)

(defp *sparse-km*
    (fill-keymap (fill-keymap-w/fn (table) #'num-arg *keynames->num-args*)
                 "S-M-F3"                  'zelda-secret
                 "C-x 0"                   'delete-window-cmd
                 "C-x 1"                   'delete-other-windows-cmd
                 "C-x 2"                   'split-window-horizontally-cmd
                 "C-x 3"                   'split-window-vertically-cmd
                 "C-x C-c"                 'exit-sqnc
                 "C-x C-f"                 'query-open-song
                 "C-x F"                   'query-open-text-file
                 "C-x C-s"                 'query-save-song
                 "C-x C-w"                 'query-save-song-as
                 "C-x C-n"                 'new-song-cmd
                 "C-x k"                   'kill-buffer-cmd
                 "C-x C-k"                 'kill-all-buffers-cmd
                 "C-x j"                   'split-window-horizontally-cmd
                 "C-x v"                   'split-window-vertically-cmd
                 "C-x ("                   'kmacro-start-macro-cmd
                 "C-x )"                   'kmacro-end-macro-cmd
                 "C-x e"                   'kmacro-end-and-call-macro-cmd
                 "C-x x"                   'export-csd
                 "Insert"                  'show-last-kmacro-cmd
                 "C-x a r"                 'reset-keymaps
                 ;; "C-x a R"                 'reset-all-keymaps
                 "C-x z"                   'reload-.sqnc
                 "C-x l 1"                 'loop-offset-whole-range
                 "C-x l 2"                 'loop-offset-neg-whole-range
                 "C-x l 3"                 'loop-offset-half-range
                 "C-x l 4"                 'loop-offset-neg-half-range
                 "C-x l 5"                 'loop-offset-quarter-range
                 "C-x l 6"                 'loop-offset-neg-quarter-range
                 "C-x l d"                 'loop-length-command
                 "C-x l l"                 'loop-active-trg
                 "C-x l s"                 'set-loop-start-to-point
                 "C-x l e"                 'set-loop-end-to-point
                 "C-x t m"                 'toggle-mute-c-trk
                 "C-g"                     'quit-command
                 "C--"                     'undo-command
                 "C-x u"                   'undo-command
                 "C-M--"                   'redo-command
                 "M-F1"                    'insert-pattern
                 "M-F2"                    'insert-pattern-before
                 "M-F3"                    'delete-pattern
                 "M-F4"                    'insert-track
                 "M-F5"                    'insert-track-before
                 "M-F6"                    'delete-track
                 "M-F10"                   'csound-toggle-online
                 "M-F11"                   'start-csound-cmd
                 "M-F12"                   'init-swank-command
                 "F1"                      'toggle-mixer
                 "F2"                      'toggle-inst-buffer
                 "M-Tab"                   'next-wij!
                 "S-M-Tab"                 'prev-wij!
                 "C-x m"                   'toggle-dump-cs-msgs
                 ))

(defp *extended-km*
    (fill-keymap (table)
                 "M-x"                     'execute-extended-command
                 "M-X"                     'evaluate-expression
                 ))

(defp *widget-nav-km*
    (fill-keymap (table)
                 "Left"                    'widget-left
                 "Right"                   'widget-right
                 "Down"                    'widget-down
                 "Up"                      'widget-up
                 "b"                       'widget-left
                 "f"                       'widget-right
                 "n"                       'widget-down
                 "p"                       'widget-up
                 ))

(defp *oor-km*
    (fill-keymap (table)
                 "C-x b z"                 'oor-title-buffer
                 "C-x b g"                 'oor-base-buffer
                 "C-h k"                   'oor-help-buffer
                 "C-x b e"                 'oor-editor-buffer
                 "C-x b r"                 'oor-repl-buffer
                 "C-x b m"                 'oor-cs-message-buffer
                 "C-x b t"                 'oor-trg-buffer
                 "C-x b o"                 'oor-c-piano-roll
                 "C-x b O"                 'oor-root-piano-roll
                 "M-["                     'oor-prev-pat-piano-roll
                 "M-]"                     'oor-next-pat-piano-roll
                 "C-["                     'oor-prev-trk-piano-roll
                 "C-]"                     'oor-next-trk-piano-roll
                 ))

(defp *offset-buffer-km*
    (fill-keymap (table)
                 "C-M-n"                   'goto-next-buffer
                 "C-M-p"                   'goto-prev-buffer
                 ))

(defp *offset-window-km*
    (fill-keymap (table)
                 "C-M-b"                   'goto-prev-window
                 "C-M-f"                   'goto-next-window
                 ))

(defp *cursor-movement-km*
    (fill-keymap (table)
                 "C-a"                     'goto-left
                 "C-e"                     'goto-right
                 "C-b"                     'point-1-left
                 "C-f"                     'point-1-right
                 "C-n"                     'point-line-up
                 "C-p"                     'point-line-down
                 "P"                       'point-half-beat-up
                 "N"                       'point-half-beat-down
                 "M-v"                     'point-page-up
                 "C-v"                     'point-page-down
                 "M-<"                     'goto-top
                 "M->"                     'goto-bottom
                 ))

(defp *border-km*
    (fill-keymap (table)
                 "M-P"                     'border-up-cmd
                 "M-N"                     'border-down-cmd
                 "M-B"                     'border-left-cmd
                 "M-F"                     'border-right-cmd
                 "M-Y"                     'border-ul-cmd
                 "M-U"                     'border-dl-cmd
                 "M-I"                     'border-ur-cmd
                 "M-O"                     'border-dr-cmd
                 ))

(defp *window-scroll-km*
    (fill-keymap (table)
                 "C-l"                     'center-cursor-v
                 "M-l"                     'center-cursor-h
                 "M-K"                     'window-vscroll-up
                 "M-J"                     'window-vscroll-down
                 "M-H"                     'window-hscroll-left
                 "M-L"                     'window-hscroll-right
                 ))

(defp *inst-km*
    (fill-keymap (table)
                 "C"                       'cp-preset-c-trg
                 "c"                       'cp-trg@p-c-trg
                 "<"                       'prev-inst-cmd
                 ">"                       'next-inst-cmd
                 "M-,"                     'prev-preset-cmd
                 "M-."                     'next-preset-cmd
                 ))

(defp *transport-km*
    (fill-keymap (table)
                 "Space"                   'play-pause
                 "C-M-s"                   'stop-cmd
                 "M-Rubout"                'rew-cmd
                 ))

(defp *piano-roll-km*
    (fill-keymap (table)
                 "C-c s"                   'query-insert-trg-seq
                 "C-x p"                   'query-insert-pattern-trg
                 "C-x r"                   'query-rename-c-pat
                 "C-x C-r"                 'query-rename-c-trk
                 "C-x h"                   'mark-whole-buffer
                 "C-w"                     'kill
                 "C-y"                     'yank
                 "C-M-y"                   'yank-and-mark-repeatable
                 "C-Space"                 'set-mark-command
                 "M-w"                     'kill-ring-save
                 "e"                       'insert-trg
                 "Return"                  'insert-trg
                 "d"                       'delete-trg-p+d
                 "u"                       'incf-trg-dur
                 "i"                       'decf-trg-dur
                 "a"                       'align-trg-start-to-time@pt
                 "y"                       'incf-mtrgs-dur
                 "o"                       'decf-mtrgs-dur
                 "9"                       'decf-edit-step
                 "0"                       'incf-edit-step
                 ;; "b"                    'point-edit-step-left
                 ;; "n"                    'point-edit-step-down
                 ;; "p"                    'point-edit-step-up
                 ;; "f"                    'point-edit-step-right
                 ;; "p"                    'point-jump-up
                 ;; "n"                    'point-jump-down
                 ;; "b"                    'point-jump-left
                 ;; "f"                    'point-jump-right
                 "p"                       'trg-up
                 "n"                       'trg-down
                 "b"                       'trg-left
                 "f"                       'trg-right
                 "j"                       'center-next-trg
                 "k"                       'center-prev-trg
                 "`"                       'toggle-mute-c-active-trg
                 "C-u"                     'toggle-mute-mtrgs
                 "M-b"                     'point-octave-left
                 "M-f"                     'point-octave-right
                 "M-p"                     'point-beat-up
                 "M-n"                     'point-beat-down
                 "B"                       'point-half-octave-left
                 "F"                       'point-half-octave-right
                 "M-y"                     'point-chunk-ul
                 "M-u"                     'point-chunk-dl
                 "M-i"                     'point-chunk-ur
                 "M-o"                     'point-chunk-dr
                 "Y"                       'point-half-chunk-ul
                 "U"                       'point-half-chunk-dl
                 "I"                       'point-half-chunk-ur
                 "O"                       'point-half-chunk-dr
                 "C-v"                     'point-measure-down
                 "M-v"                     'point-measure-up
                 "-"                       'dec-zoom
                 "="                       'inc-zoom
                 "_"                       'halve-zoom
                 "+"                       'double-zoom
                 "C-r"                     'set-zoom-to-trg-resol
                 "m"                       'toggle-mark-add-active-trg
                 "C-g"                     'quit-and-unmark
                 "M-Space"                 'mark-rectangular-region
                 "D"                       'delete-mtrgs
                 "M-A"                     'goto-mtrgs-top-left
                 "C-M-q"                   'query-goto-measure
                 "C-s"                     'toggle-grab-trgs
                 ))

(defp *numeric-widget-km*
    (fill-keymap (table)
                 "l"                       'wij-1+
                 "h"                       'wij-1-
                 "k"                       'wij-.1+
                 "j"                       'wij-.1-
                 "J"                       'wij-.01+
                 "K"                       'wij-.01-
                 "Return"                  'query-set-numeric-wij
                 "Rubout"                  'zap-kill-ring-ctl
                 ))

(defp *text-km*
    (fill-keymap (fill-keymap-w/fn (table) #'self-insert-char *keynames->char-codes*)
                 "Tab"                     'insert-tab-spaces
                 "C-x C-q"                 'toggle-read-only
                 "C-b"                     'backward-char
                 "C-f"                     'forward-char
                 "C-l"                     'center-textbox-point
                 "C-n"                     'next-line
                 "C-p"                     'prev-line
                 "C-v"                     'page-down
                 "M-v"                     'page-up
                 "C-a"                     'move-line-beg
                 "C-e"                     'move-line-end
                 "M-<"                     'move-buf-beg
                 "M->"                     'move-buf-end
                 "M-b"                     'backward-word
                 "M-f"                     'forward-word
                 "C-k"                     'kill-line
                 "M-Rubout"                'backward-kill-word
                 "M-d"                     'forward-kill-word
                 "C-y"                     'text-yank
                 "C-d"                     'delete-char
                 "Rubout"                  'backward-delete-char
                 ))

(defp *minibuffer-km*
    (fill-keymap (table)
                 "Return"                  'text-return-str
                 "C-g"                     'text-return-quit
                 ))

(defp *mbq-pathname-km*
    (fill-keymap (table)
                 "Tab"                     'path-complete))

(defp *mbq-command-km*
    (fill-keymap (table)
                 "Tab"                     'cmd-complete))

(defp *button-km*
    (fill-keymap (table)
                 "Return"                  'execute-button))

(defp *henvelope-km*
    (fill-keymap (table)
                 "i"                       'add-envpt-cmd
                 "d"                       'del-envpt-cmd))

(defp *text-widget-km*
    (fill-keymap (table)
                 "Return"                  'query-set-text-wij))

(defp *listbox-km*
    (fill-keymap (table)
                 "j"                       'listbox-next
                 "k"                       'listbox-prev))
(defp *live-player-km*
    (fill-keymap (fill-keymap-w/fn (table) #'self-output-last-key *keynames->pitchclasses*)
                 "F8"                      'decf-c-oct
                 "F9"                      'incf-c-oct
                 ))

(defp *sample-map-editor-km*
    (fill-keymap (table)
                 "M-Return"                'import-smap
                 "C-x C-f"                 'find-textfile))

(defp *available-insts-listbox-km*
    (fill-keymap (table)
                 "Return"                  'listbox-insert-inst))

(defp *c-insts-listbox-km*
    (fill-keymap (table)
                 "d"                       'listbox-delete-inst))

(defp *sample-map-listbox-km*
    (fill-keymap (table)
                 "Return"                  'set-insts-smap))

(defp *repl-km*
    (fill-keymap (table)
                 "Return"                  'eval-input))

(defp *available-effects-listbox-km*
    (fill-keymap (table)
                 "Return"                  'listbox-insert-effect))

(defp *inserted-effects-listbox-km*
    (fill-keymap (table)
                 "d"                       'listbox-delete-effect))

; get-keymaps methods

(defm get-keymaps ((w widget))
  (ls *transport-km* *inst-km* *window-scroll-km* *border-km* *cursor-movement-km*
      *widget-nav-km* *oor-km* *offset-buffer-km* *offset-window-km* *extended-km*
      *sparse-km*))

(defm get-keymaps ((w piano-roll))
  (cons *piano-roll-km* (cnm)))

(defm get-keymaps ((w numeric-widget))
  (cons *numeric-widget-km* (cnm)))

(defm get-keymaps ((w textbox))
  (cons *text-km* (cnm)))

(defm get-keymaps ((w miniwij))
  (ls *minibuffer-km* *extended-km* *offset-window-km* *text-km*))

(defm get-keymaps ((w button))
  (cons *button-km* (cnm)))

(defm get-keymaps ((w henv-slider))
  (cons *henvelope-km* (cnm)))

(defm get-keymaps ((w henv-floatbox))
  (cons *henvelope-km* (cnm)))

(defm get-keymaps ((w text-widget))
  (cons *text-widget-km* (cnm)))

(defm get-keymaps ((w listbox))
  (cons *listbox-km* (cnm)))

(defm get-keymaps ((w base-widget))
  (cons *live-player-km* (cnm)))

(defm get-keymaps ((w sample-map-editor))
  (cons *sample-map-editor-km* (cnm)))

(defm get-keymaps ((w available-insts-listbox))
  (cons *available-insts-listbox-km* (cnm)))

(defm get-keymaps ((w c-insts-listbox))
  (cons *c-insts-listbox-km* (cnm)))

(defm get-keymaps ((w sample-map-listbox))
  (cons *sample-map-listbox-km* (cnm)))

(defm get-keymaps ((w replbox))
  (cons *repl-km* (cnm)))

(defm get-keymaps ((w available-effects-listbox))
  (cons *available-effects-listbox-km* (cnm)))

(defm get-keymaps ((w inserted-effects-listbox))
  (cons *inserted-effects-listbox-km* (cnm)))
