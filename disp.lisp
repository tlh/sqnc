(in-package :sqnc)

(defps *stdscr*                nil
       *frame*                 nil
       *in-minibuf*            nil
       *recursive*             nil
       *current-window*        nil
       *highlighted-window*    nil
       *buffer-list*           nil
       *parent-cur*            nil
       *min-height*            3
       *min-width*             3
       *rootbuf*               nil
       *rootbuf-color*         w-gr5)

; stdscr

(def std-height () (getmaxy *stdscr*))

(def std-width ()  (getmaxx *stdscr*))

(defs height>=min? (height) (>= height *min-height*)
      width>=min?  (width)  (>= width  *min-width*))

(def test-size (h w)
  (unless (and (height>=min? h) (width>=min? w))
    (error "window too small")))

; pad pool

(classy pad () (pad nil))

(w/ pool
  (def get-pad (h w)
    (aif (w/n p
           (mapc [w/ (h2 (height _) w2 (width _))
                   (and (<= h h2) (<= w w2)
                        (or (no p) (and (<= h2 (height p))
                                        (<= w2 (width  p))))
                        (=! p _))]
                 pool))
         (pn (remove! it pool) it)
         (new 'pad :pad (newpad h w))))
  (def free-pad  (p) (or (in p pool) (push p pool)))
  (def get-pad-pool () pool))

; window

(classy window ()
  (bufrecs      nil :nocp)
  (buffer-list  nil :nocp)
  (mini         nil)
  (miny         nil)
  (minx         nil)
  (maxy         nil)
  (maxx         nil))

(defm buffer ((w window)) (car (buffer-list w)))

(def mk-win (bl ny nx xy xx &o br)
  (new 'window :buffer-list bl :bufrecs br
       :miny ny :minx nx :maxy xy :maxx xx))

; buffer-record

(classy buffer-record ()
  (br-oy     0)
  (br-ox     0)
  (br-cy     0)
  (br-cx     0)
  (br-my     0)
  (br-mx     0)
  (br-point  0))

;   widgets

(classy widget ()
  (name            nil)
  (keymaps         nil)
  (getter          nil)
  (setter          nil)
  (ctl             nil)
  (unit            nil)
  (widgets         nil)
  (minx            0)
  (miny            0)
  (height          nil)
  (width           nil)
  (hilite          nil)
  (intrac          nil)
  (color           w-gr8)
  (hilited-color   w-gr8)
  (cursor-visible  nil))

(definit-after (w widget)
  (=! (widgets w) (create-widgets w)
      (keymaps w) (get-keymaps w)))

(defm create-widgets (obj))

(defms maxy ((w widget)) (+ (miny w) (height w))
       maxx ((w widget)) (+ (minx w) (width  w)))

(w/ (pool (table)) ; widget pool
  (def new-wij (type . keys)
    (aif (pop (gethash type pool))
         (-> initialize-instance it keys)
         (-> new type keys)))
  (def free-wij (w)
    (mapc #'free-wij (widgets w))
    (mapc [slot-makunbound w _] (slot-names w))
    (push w (getab (type-of w) pool)))
  (def get-pool () pool))

;; (def wij-pool-count ()
;;   (w/n (i 0)
;;     (dohash (k v (get-pool))
;;       (mapc [incf i] v))))

(def free-wijs (b) (mapc #'free-wij (widgets b)))

(def calc-leaf-wijs (obj)
  (aif (and (not (buffer-p obj)) (intrac obj))
       (ls obj)
       (ma #'calc-leaf-wijs (widgets obj))))

; buffer classes

(classy buffer ()
  (name            "Buffer Name")
  (props           nil :nocp)
  (pad             nil)
  (color           w-gr8)
  (height          (std-height))
  (width           (std-width))
  (always-redraw   nil)
  (marked          nil)
  (hook            nil)
  (touched         t)
  (broken          nil)
  (widgets         nil)
  (leaf-wijs       nil))

(defm get-keymaps (obj))

(defm untouch (obj) (nil! (touched obj)))

(defm create-hook (obj))

(def create-std-wij (type)
  (ls (new-wij type :height (std-height) :width (std-width)
                    :miny 0 :minx 0 :intrac t)))

(defm create-widgets ((b buffer))
  (create-std-wij 'widget))

(defs wij-max-y (wl) (-> max (mc [+ (miny _) (height _)] wl))
      wij-max-x (wl) (-> max (mc [+ (minx _) (width  _)] wl)))

;; (definit-before (b buffer)
;;   (=! (props     b) (pget :props initargs)
;;       (widgets   b) (or (create-widgets b) (create-std-wij 'widget))
;;       (leaf-wijs b) (calc-leaf-wijs b)))

(definit-before (b buffer)
  (=! (props     b) (pget :props initargs)
      (widgets   b) (or (create-widgets b) (create-std-wij 'widget))
      (leaf-wijs b) (calc-leaf-wijs b)))

(definit (b buffer)
  (=! (height b) (aif (widgets b) (wij-max-y it) (std-height))
      (width  b) (aif (widgets b) (wij-max-x it) (std-width))))

(definit-after (b buffer)
  (slotf-if-nil b 'pad  (get-pad (height b) (width b))
                  'hook (create-hook b)))

; currents

(def set-frame (f) (=! *frame* f))

(def miniwindow () (last1 *frame*))

(def minibuffer () (buffer (miniwindow)))

(def minibuf-p (b) (eq b (minibuffer)))

(def miniwij () (car (widgets (minibuffer))))

(def rootbuf () *rootbuf*)

(def c-wt () (cddr (butlast *frame*)))

(def c-wl ()
  (w/ (wl (flatten-tree *frame* #'cddr))
    (if *in-minibuf* wl (butlast wl))))

(def w-set (w) (=! *current-window* w))

(def c-win () (or *current-window* (w-set (car (c-wl)))))

(def w-idx (&o (w (c-win))) (idx w (c-wl)))

(def hw-set (w) (=! *highlighted-window* w))

(def h-win () (or *highlighted-window* (hw-set (car (c-wl)))))

(def hl? (w) (eq w (h-win)))

(def c-buf () (buffer (c-win)))

(def m-bl () *buffer-list*)

(def c-bl () (buffer-list (c-win)))

(def c-kms () (keymaps (c-wij)))

; widget utils

(def sync-wij-curs (wij &o (b (c-buf)) (w (c-win)))
  (when wij
    (scro (miny wij) (minx wij) b w)
    (touch b)
    wij))

(def cursor-within-wij (y x wij)
  (and (within (miny wij) (maxvy wij) y)
       (within (minx wij) (maxvx wij) x)))

(def wij@point (y x wijlist)
  (w/fn (rec (w) (and w (cursor-within-wij y x w)
                        (acif (mapp #'rec (widgets w)) it
                              (intrac w)               w)))
    (mapp #'rec wijlist)))

(defms avgy ((w widget)) (+ (miny w) (trunc (1- (height w)) 2))
       avgx ((w widget)) (+ (minx w) (trunc (1- (width  w)) 2)))

(def closest-wij (y x wijs)
  (-> keyed-min [hypot (avgy _) y (avgx _) x] wijs))

(def c-wij (&o (b (c-buf)) (w (c-win)))
  (w/ (y (cy b w) x (cx b w) wijs (leaf-wijs b))
    (or (wij@point y x wijs)
        (sync-wij-curs (closest-wij y x wijs) b w))))

; border-buffers

(classy border-buffer (buffer)
  (buffer      nil)
  (color       w-gr8))

(classy hline-buffer    (border-buffer) (height 1))
(classy vline-buffer    (border-buffer) (width  1))
(classy top-buffer      (hline-buffer))
(classy bottom-buffer   (hline-buffer))
(classy left-buffer     (vline-buffer))
(classy right-buffer    (vline-buffer))
(classy ulcorner-buffer (border-buffer))
(classy llcorner-buffer (border-buffer))
(classy urcorner-buffer (border-buffer))
(classy lrcorner-buffer (border-buffer))

(classy modeline (bottom-buffer)
  (modeline-color gr3-gr9)
  (always-redraw  t))

(classy standard-buffer (buffer)
  (border-color   w-gr8)
  (top            nil)
  (bottom         nil)
  (left           nil)
  (right          nil)
  (ul             nil)
  (ll             nil)
  (ur             nil)
  (lr             nil))

(definit-after (b standard-buffer)
  (slotf-if-nil b
                'bottom (new        'modeline)
                'top    (new      'top-buffer)
                'left   (new     'left-buffer)
                'right  (new    'right-buffer)
                'ul     (new 'ulcorner-buffer)
                'll     (new 'llcorner-buffer)
                'ur     (new 'urcorner-buffer)
                'lr     (new 'lrcorner-buffer))
  (mapc [=! (buffer _) b (color _) (border-color b)] (borders b))
  b)

; border stuff

(def borders (b)
  (svals b 'top 'bottom 'left 'right 'ul 'll 'ur 'lr))

(defm untouch ((b standard-buffer))
  (map nil #'untouch (borders b))
  (cnm))

(defm pads-of ((b buffer)) (ls (pad b)))

(defm pads-of ((b standard-buffer))
  (ap (ma #'pads-of (borders b)) (cnm)))

(def free-pads (b) (mapc #'free-pad (pads-of b)))

(def raise (b w)
  (a=! (buffer-list w) (cons b (remove b it)))
  b)

; textbox

(classy textbox (widget)
  (color           w-gr9)
  (origin-row      0)
  (origin-col      0)
  (point           0)
  (mark            0)
  (str             "")
  (read-only       nil)
  (cursor-visible  t))

(classy miniwij (textbox)
  (msg-str         nil)
  (prompt-str      ""))

(classy text-buffer (buffer))

(classy minibuffer (text-buffer))

(classy standard-text-buffer (text-buffer standard-buffer))

(defm create-widgets ((b text-buffer))
  (create-std-wij 'textbox))

(defm create-widgets ((b minibuffer))
  (create-std-wij 'miniwij))

; ncurses ffi wrappers

(mac defcursesfn (name args cursefn)
  `(pn (defm ,name ,(cons '(p pad) args)
         (,cursefn (pad p) ,@args))
       (defm ,name ,(cons '(b buffer) args)
         (,name (pad b) ,@args))
       (defm ,name ,(cons '(w window) args)
         (,name (buffer w) ,@args))))

(plural defcursesfn defcursesfns 3)

(defcursesfns
    _intrflush    (bf)                    intrflush
    _keypad       (bf)                    keypad
    _wgetch       ()                      wgetch
    _werase       ()                      werase
    _wcolor-set   (c opts)                wcolor-set
    _wmove        (y x)                   wmove
    _+str         (y x str)               mvwaddstr
    _+ch          (y x ch)                mvwaddch
    _waddch       (ch)                    waddch
    _vln          (y x ch n)              mvwvline
    _hln          (y x ch n)              mvwhline
    _wbkgd        (cpair)                 wbkgd
    _box          (verch horch)           box
    _winch        ()                      winch
    _wattron      (attrs)                 wattron
    _wattroff     (attrs)                 wattroff
    _pnoutrefresh (pny pnx ny nx xy xx)   pnoutrefresh)

; char attrs

(defm cursch ((ch number)) ch)

(defm cursch ((ch character)) (char-code ch))

(def attr (ch attr) (+ attr (cursch ch)))

(def colorch (ch col) (attr ch (color-pair col)))

; alt char set

(mac defacs (name ch) `(defp ,name (attr ,ch a_altcharset)))

(defacs *acs-ulcorner* #\l)
(defacs *acs-llcorner* #\m)
(defacs *acs-urcorner* #\k)
(defacs *acs-lrcorner* #\j)
(defacs *acs-ltee*     #\t)
(defacs *acs-rtee*     #\u)
(defacs *acs-btee*     #\v)
(defacs *acs-ttee*     #\w)
(defacs *acs-hline*    #\q)
(defacs *acs-vline*    #\x)
(defacs *acs-plus*     #\n)
(defacs *acs-diamond*  #\`)
(defacs *acs-ckboard*  #\a)
(defacs *acs-degree*   #\f)
(defacs *acs-plminus*  #\g)
(defacs *acs-bullet*   #\~)
(defacs *acs-larrow*   #\,)
(defacs *acs-rarrow*   #\+)
(defacs *acs-darrow*   #\.)
(defacs *acs-uarrow*   #\-)
(defacs *acs-board*    #\h)
(defacs *acs-lantern*  #\i)
(defacs *acs-block*    #\o)
(defacs *acs-s3*       #\p)
(defacs *acs-s7*       #\r)
(defacs *acs-lequal*   #\y)
(defacs *acs-gequal*   #\z)
(defacs *acs-pi*       #\{)
(defacs *acs-not-eq*   #\|)
(defacs *acs-sterling* #\})

; drawing utils

(def cursor-off () (curs-set 0))

(def cursor-on () (curs-set 1))

(mac w/attr (obj attr . body)
  `(pn (_wattron  ,obj ,attr)
       ,@body
       (_wattroff ,obj ,attr)))

(mac cattr (buf exprs . body)
  (w/uniq (attr)
    `(w/ (,attr (+ ,@(mc [pn `(if ,@_ 0)] (pair exprs))))
       (w/attr ,buf ,attr ,@body))))

(def color-on (b c) (_wattron b (color-pair c)))

(def cbox (b ny nx xy xx &k (top t) (bottom t) (left t) (right t) (ul t) (ll t) (ur t) (lr t))
  (w/ (h (- xy ny 1) w (- xx nx 1))
    (when ul     (_+ch b ny nx *acs-ulcorner*))
    (when ll     (_+ch b xy nx *acs-llcorner*))
    (when ur     (_+ch b ny xx *acs-urcorner*))
    (when lr     (_+ch b xy xx *acs-lrcorner*))
    (when top    (_hln b ny (1+ nx) *acs-hline* w))
    (when bottom (_hln b xy (1+ nx) *acs-hline* w))
    (when left   (_vln b (1+ ny) nx *acs-vline* h))
    (when right  (_vln b (1+ ny) xx *acs-vline* h))))

(def capped-vline (b y x len &k (ttee *acs-ttee*) (btee *acs-btee*) (line *acs-vline*))
  (_+ch b y x ttee)
  (_+ch b (+ -1 y len) x btee)
  (_vln b (1+ y) x line (- len 2)))

(def capped-hline (b y x len &k (ltee *acs-ltee*) (rtee *acs-rtee*) (line *acs-hline*))
  (_+ch b y x ltee)
  (_+ch b y (+ -1 x len) rtee)
  (_hln b y (1+ x) line (- len 2)))

(def wijcolor (w buf color)
  (_wattron buf (color-pair color))
  (dotimes (i (height w))
    (_hln buf (+ (miny w) i) (minx w) (char-code #\ ) (width w))))

(def wijbox (w buf &k (ul t) (top t) (ur t) (left t) (right t) (ll t) (bottom t) (lr t))
  (cbox buf (miny w) (minx w) (maxvy w) (maxvx w)
        :ul ul :top top :ur ur :left left :right right :ll ll :bottom bottom :lr lr))

; redraw

(defm redraw-widget ((w widget) buf win))

(mac def-redraw-widget (type . body)
  `(defm redraw-widget ((w ,type) buf win)
     ,@body
     (w/ (*parent-cur* (cursor-within-wij (cy buf win) (cx buf win) w))
       (redraw-widgets w buf win))))

(def redraw-widgets (obj buf win)
  (map nil [redraw-widget _ buf win] (widgets obj)))

(defm redraw (buffer window) buffer)

(mac defredraw (type . body)
  `(defm redraw ((b ,type) (w window))
     (cnm) ,@body))

(plural defredraw defredraws 2)

(def draw-modeline (b hl str)
  (_hln b 0 0 *acs-hline* (width b))
  (_+ch b 0 1 *acs-rtee*)
  (_+ch b 0 (+ 2 (ln str)) *acs-ltee*)
  (cattr b (hl a_reverse t (color-pair (modeline-color b)))
    (_+str b 0 2 str)))

(defredraw buffer
  (_werase b)
  (_wbkgd b (color-pair (color b)))
  (c-wij b w) ;; <- ensures cursor is within an interactive widget
  (redraw-widgets b b w))

(defredraws modeline        (draw-modeline b (hl? w) *sqnc-str*)
            hline-buffer    (_hln b 0 0 *acs-hline* (width b))
            vline-buffer    (_vln b 0 0 *acs-vline* (height b))
            ulcorner-buffer (_+ch b 0 0 *acs-ulcorner*)
            llcorner-buffer (_+ch b 0 0 *acs-llcorner*)
            urcorner-buffer (_+ch b 0 0 *acs-urcorner*)
            lrcorner-buffer (_+ch b 0 0 *acs-lrcorner*))

; minibuffer messaging

(def mb-str (&o (w (miniwij)))
  (loop with acc   = ""
        with str   = (or (msg-str w) (cat (prompt-str w) (str w)))
        with l     = (ln str)
        with width = (1- (width (miniwindow)))
        for  i     from 0 by width
        for  j     = (min l (+ i width))
        while (< i l)
        do (cat! acc (subseq str i j) (mkstr "~:[~;~%~]" (< j l)))
        finally (return acc)))

(def resize-miniwindow (&o (w (miniwij)) (f *frame*) (mw (miniwindow)))
  (w/* (max   (trunc (std-height) 2)
        lines (num-lines (mb-str w))
        dif   (- (height mw) (confine-to 1 max lines)))
    (unless (zerop dif)
      (set-frame (wfill (wresize dif t mw f))))))

(def set-textbox (wij buf &o (point 0) (str ""))
  (=! (point wij) point (str wij) str)
  (touch buf))

(def mb-set (pt prompt cont &o (w (miniwij)) (b (minibuffer)))
  (=! (prompt-str w) prompt)
  (set-textbox w b pt cont))

(def mb-clear (&o (w (miniwij)) (b (minibuffer)))
  (unless *in-minibuf* (mb-set 0 "" "" w b))
  (nil! (msg-str w))
  (t! (cursor-visible w)))

(def mb-msg (str)
  (w/ (w (miniwij) b (minibuffer))
    (=! (msg-str w) str)
    (nil! (cursor-visible w))
    (touch b)
    nil))

; hooks

(defp *hooks* nil)

(def run-hooks () (mapc [=> (cadr _)] *hooks*))

(def remove-hooks (b) (remove! b *hooks* :key #'car))

(def add-hook (obj hook) (push (ls obj hook) *hooks*))

(def add-buffer (b)
  (awhen (hook b) (add-hook b it))
  (rpush b *buffer-list*)
  (map nil [rpush b (buffer-list _)] (c-wl))
  b)

(def remove-buffer (b)
  (remove-hooks b)
  (remove! b *buffer-list*)
  (map nil [a=! (buffer-list _) (remove b it)] (c-wl)))

; height and width

(defms height ((p pad))    (getmaxy (pad p))
       width  ((p pad))    (getmaxx (pad p))
       height ((w window)) (- (maxy w) (miny w))
       width  ((w window)) (- (maxx w) (minx w)))

(defms maxy ((b buffer)) (height b)
       maxx ((b buffer)) (width b))

; max viewables

(defs maxvy (obj) (1- (maxy obj))
      maxvx (obj) (1- (maxx obj)))

; border coords

(defs ny1  (w) (1+ (miny  w))
      nx1  (w) (1+ (minx  w))
      xvy1 (w) (1- (maxvy w))
      xvx1 (w) (1- (maxvx w)))

; max origin and port

(def border-size (b) (if (minibuffer-p b) 0 2))

(defs max-oy (w b) (+ (- (height b) (height w)) (border-size b))
      max-ox (w b) (+ (- (width  b) (width  w)) (border-size b))
      max-port-y (oy w b) (- (+ oy (1- (height w))) (border-size b))
      max-port-x (ox w b) (- (+ ox (1- (width  w))) (border-size b)))

; confine cursor/origin y/x to buffer/origin/cursor

(defs ccyb (y b)       (confine-to 0 (1- (height b)) y)
      ccxb (x b)       (confine-to 0 (1- (width  b)) x)
      coyb (y w b)     (confine-to 0 (max-oy w b) y)
      coxb (x w b)     (confine-to 0 (max-ox w b) x)
      ccyo (y w b br)  (confine-to (br-oy br) (max-port-y (br-oy br) w b) y)
      ccxo (x w b br)  (confine-to (br-ox br) (max-port-x (br-ox br) w b) x)
      coyc (oy w b br) (+ oy (astray (br-cy br) oy (max-port-y (br-oy br) w b)))
      coxc (ox w b br) (+ ox (astray (br-cx br) ox (max-port-x (br-ox br) w b))))

; buffer records

(def copy-brs (w)
  (mc [if (no (buffer-p _)) (copy _) _]
      (bufrecs w)))

(def find-br (b)
  (aif (mapp [pget b (bufrecs _)] (c-wl))
       (copy it)
       (new 'buffer-record)))

(defp *br-max-cache* 200)

(def resize-br-cache (w)
  (when (>= (ln (bufrecs w)) *br-max-cache*)
    (a=! (bufrecs w) (butlast it (trunc *br-max-cache* 2)))))

(def get-br (b w)
  (resize-br-cache w)
  (aif (pget b (bufrecs w))
       it
       (pset b (find-br b) (bufrecs w))))

(def zapc (b w) ; zap to cursor
  (w/n (br (get-br b w))
    (a=! (br-cy br) (ccyb it b)
         (br-cx br) (ccxb it b)
         (br-oy br) (coyc it w b br)
         (br-ox br) (coxc it w b br))))

(def zapo (b w) ; zap to origin
  (w/n (br (get-br b w))
    (a=! (br-oy br) (coyb it w b)
         (br-ox br) (coxb it w b)
         (br-cy br) (ccyo it w b br)
         (br-cx br) (ccxo it w b br))))

; current cursor and origin

(defs cy (&o (b (c-buf)) (w (c-win))) (br-cy (zapc b w))
      cx (&o (b (c-buf)) (w (c-win))) (br-cx (zapc b w))
      oy (&o (b (c-buf)) (w (c-win))) (br-oy (zapo b w))
      ox (&o (b (c-buf)) (w (c-win))) (br-ox (zapo b w)))

(def sync-cursor (b w)
  (_wmove w (cy b w) (cx b w)))

; move/scroll/recenter combos

(def scro (y x &o (b (c-buf)) (w (c-win)))    ; set cursor / recenter port
  (w/ (br (zapc b w))
    (=! (br-cy br) y (br-cx br) x)
    (zapc b w)))

(def sorc (y x &o (b (c-buf)) (w (c-win)))    ; set port / recenter cursor
  (w/ (br (zapo b w))
    (=! (br-oy br) y (br-ox br) x)
    (zapo b w)))

(def mcro (yo xo &o (b (c-buf)) (w (c-win)))  ; move cursor / recenter port
  (w/ (br (zapc b w))
    (scro (+ (br-cy br) yo) (+ (br-cx br) xo) b w)))

(def morc (yo xo &o (b (c-buf)) (w (c-win)))  ; move port / recenter cursor
  (w/ (br (zapo b w))
    (sorc (+ (br-oy br) yo) (+ (br-ox br) xo) b w)))

; prev/next window

(def wgoto (nw)
  (w-set nw)
  (when (neq nw (miniwindow)) (hw-set nw)))

(def wgoto-idx (idx &o (wl (c-wl)))
  (wgoto (confined-nth idx wl)))

(defs prev-window (&o (n 1)) (cprev (c-win) (c-wl) n)
      next-window (&o (n 1)) (cnext (c-win) (c-wl) n))

(def goto-prev-buffer (&o (w (c-win)))
  (a=! (buffer-list w) (cons (last1 it) (butlast it))))

(def goto-next-buffer (&o (w (c-win)))
  (a=! (buffer-list w) (ap (cdr it) (ls (car it)))))

; excursions

(mac window-excursion body
  (w/uniq (idx f res)
    `(w/n (,idx  (w-idx)
           ,f    (copy *frame*)
           ,res  (pn ,@body))
       (set-frame ,f)
       (wgoto-idx ,idx))))

(mac minibuffer-excursion body
  `(if *in-minibuf*
       (mb-msg "Command attempted to use minibuffer while in minibuffer")
       (window-excursion
        (wgoto (miniwindow))
        (w/ (*in-minibuf* t)
          ,@body))))


; title-buffer

(classy title-buffer (standard-buffer)
  (color           w-b9))

(defredraw title-buffer
  (w/ (str *sqnc-str*)
    (_+str b (- (trunc (std-height) 2) 2)
           (- (trunc (std-width) 2) (+ (trunc (ln str) 2) 2))
           str)))

; kill buffer

(def redirect-window-buffers (from &o to)
  (mapc [when (eq from (buffer _))
          (aif to (raise it _) (goto-next-buffer _))]
        (c-wl)))

(def kill-buffer (buf)
  (cif *recursive*   (mb-msg "Can't kill buffers while in a recursive command loop")
       (oney (m-bl)) (mb-msg "At least one buffer must remain open")
                     (pn (remove-buffer buf)
                         (free-pads buf)
                         (free-wijs buf)
                         (redirect-window-buffers buf))))

(def kill-all-buffers ()
  (add-buffer (new 'title-buffer))
  (map nil [kill-buffer _] (butlast (c-bl))))

; refresh-buffer

(def fix-buf? (b)
  (when (broken b)
    (fix b)
    (free-pads b)
    (free-wijs b)
    (w/ (props (props b))
      (unbind-all-slots b)
      (initialize-instance b :props props))))

(defm draw-buf? ((b buffer) w)
  (when (or (always-redraw b) (touched b))
    (redraw b w)))

(defm draw-buf? ((b standard-buffer) w)
  (mapc [draw-buf? _ w] (borders b))
  (cnm))

(defm refresh-buffer ((b buffer) w)
  (fix-buf? b)
  (draw-buf? b w)
  (untouch b))

; refresh window

(mac def-win-bounds (args py px ny nx xy xx)
  `(pn (defm ppy ,args ,py)
       (defm ppx ,args ,px)
       (defm pny ,args ,ny)
       (defm pnx ,args ,nx)
       (defm pxy ,args ,xy)
       (defm pxx ,args ,xx)))

(def-win-bounds ((b buffer) w)
  (oy b w) (ox b w) (miny w) (minx w) (maxvy w) (maxvx w))

(def-win-bounds ((b standard-buffer) w)
  (oy b w) (ox b w) (ny1 w) (nx1 w)
  (min (xvy1 w) (+ (height b) (miny w)))
  (min (xvx1 w) (+ (width  b) (minx w))))

(def-win-bounds ((b top-buffer) w)
  0 (ox (buffer w) w) (miny w) (1+ (minx w)) (miny w) (xvx1 w))

(def-win-bounds ((b bottom-buffer) w)
  0 0 (maxvy w) (1+ (minx w)) (maxvy w) (xvx1 w))

(def-win-bounds ((b left-buffer) w)
  (oy (buffer w) w) 0 (1+ (miny w)) (minx w) (1- (maxvy w)) (minx w))

(def-win-bounds ((b right-buffer) w)
  (oy (buffer w) w) 0 (1+ (miny w)) (maxvx w) (1- (maxvy w)) (maxvx w))

(def-win-bounds ((b ulcorner-buffer) w)
  0 0 (miny w) (minx w) (miny w) (minx w))

(def-win-bounds ((b llcorner-buffer) w)
  0 0 (maxvy w) (minx w) (maxvy w) (minx w))

(def-win-bounds ((b urcorner-buffer) w)
  0 0 (miny w) (maxvx w) (miny w) (maxvx w))

(def-win-bounds ((b lrcorner-buffer) w)
  0 0 (maxvy w) (maxvx w) (maxvy w) (maxvx w))

(defm refresh-window ((b buffer) w)
  (sync-cursor b w)
  (_pnoutrefresh b (ppy b w) (ppx b w) (pny b w) (pnx b w) (pxy b w) (pxx b w)))

(defm refresh-window ((b standard-buffer) w)
  (mapc [refresh-window _ w] (borders b))
  (cnm))

; refresh-sqnc

(def freshen (w &o (b (buffer w)))
  (w/fn (r (b w) (refresh-buffer b w) (refresh-window b w))
    (when (or (< (height b) (height w))
              (< (width  b) (width  w)))
      (r (rootbuf) w))
    (r b w)))

; asdf

(def refresh-sqnc (&o (cw (c-win)) (mw (miniwindow)) (wl (c-wl)))
  (resize-miniwindow)
  (mapc #'freshen `(,cw ,@(remove cw wl) ,mw ,@(unless (eq cw mw) (ls cw))))
  (cursor-off)
  (doupdate)
  (when (cursor-visible (c-wij)) (cursor-on)))

; window fill

(mac w-head (wt . body) `(ap (firstn 2 ,wt) ,@body))

(def map-wl (wt fn) (w-head wt (filter fn (cddr wt))))

(def winof (wt) (if (consp wt) (cadr wt) wt))

(def coord-dir (d) (if d '(minx maxx) '(miny maxy)))

(def set-top-bottom (wt)
  (w/ (f (cadr wt))
    (dbind (min max) (coord-dir (no (car wt)))
      (map-wl wt [pn (cp-slots (winof _) f min max) _]))))

(def set-endcaps (wt)
  (dbind (d w . wl) wt
    (dbind (min max) (coord-dir d)
      (cp-slots (winof (car   wl)) w min)
      (cp-slots (winof (last1 wl)) w max)))
  wt)

(def set-inners (wt)
  (dbind (min max) (coord-dir (car wt))
    (mapc (fn (p c) (=! (sval (winof p) max)
                        (sval (winof c) min)))
          (cddr wt) (cdddr wt)))
  wt)

(def cull-too-small (wt)
  (dbind (test size) (if (car wt) (ls #'width>=min? #'width)
                         (ls #'height>=min? #'height))
    (map-wl wt
      [w/ (w (winof _))
        (when (or (mini w) (=> test (=> size w)))
          _)])))

(def wfill-rec (wt)
  (map-wl wt [if (consp _) (wfill _) _]))

(def consolidate (wt)
  (w-head wt
    (ma [if (and (consp _) (eq (car _) (car wt)))
            (cddr _)
            (ls _)]
        (cddr wt))))

(def prune (wt) (if (eq 3 (ln wt)) (nth 2 wt) wt))

(def wfill (wt)
  (if (atom wt)
      wt
      (prune
       (consolidate
        (wfill-rec
         (set-inners
          (set-endcaps
           (cull-too-small
            (set-inners
             (set-endcaps
              (set-top-bottom wt)))))))))))

(mac wt-op (wt)
  (w/uniq (idx)
    `(w/ (,idx (w-idx))
       (set-frame (wfill ,wt))
       (wgoto-idx ,idx))))

; window split

(def split-ver (w)
  (w/* (nw (trunc (width w) 2)  split (+ (minx w) nw))
    (when (width>=min? nw)
      (values (maxy w) split (miny w) split))))

(def split-hor (w)
  (w/* (nh (trunc (height w) 2) split (+ (miny w) nh))
    (when (height>=min? nh)
      (values split (maxx w) split (minx w)))))

(def split-win (w &o d)
  (mvbind (xy xx ny nx) (if d (split-ver w) (split-hor w))
    (if (no xy)
        w
        (w/ (bl  (buffer-list w)
             br1 (copy-brs w)
             br2 (copy-brs w))
          (ls d w (mk-win (copy-list bl) (miny w) (minx w) xy xx br1)
                  (mk-win (copy-list bl) ny nx (maxy w) (maxx w) br2))))))

(def wsplit (&o d (w (c-win)) (wt *frame*))
  (cif (and (consp wt) (in w wt)) (swap (split-win w d) w wt)
       (consp wt)                 (map-wl wt [wsplit d w _])
       (eq w wt)                  (split-win w d)
                                  wt))

(defs split-window-horizontally () (wt-op (wsplit))
      split-window-vertically   () (wt-op (wsplit t)))

; window delete

(def wdelete (win frame)
  (cif (atom frame)    frame
       (in win frame)  (remove win frame)
                       (map-wl frame [wdelete win _])))

(def delete-window (&o (w (c-win)) (f *frame*))
  (unless (or (mini w) (= (ln (c-wl)) (if *in-minibuf* 2 1)))
    (wt-op (wdelete w f))))

(def delete-other-windows (&o (w (c-win)) (wl (c-wl)))
  (mapc #'delete-window (remove w wl)))

; window resize

(def find-wresize (dir win wt &o up)
  (when (consp wt)
    (w/ (i (in win wt) d (eq dir (car wt)))
      (cif (and i d up) (values wt up)
           (and i d)    (values nil nil)
           i            (values win wt)
                        (mapp2 [find-wresize dir win _ wt]
                               (cddr wt))))))

(def wresize (n &o dir (cw (c-win)) (f *frame*))
  (mvbind (win frame) (find-wresize dir cw f)
    (when (and win frame)
      (w/* (w (winof (next win frame))
            w (if (and w (no (mini w))) w (winof win)))
        (if dir (+! (miny w) n) (+! (minx w) n)))))
  f)

(defs border-up    () (wt-op (wresize -1 t))
      border-down  () (wt-op (wresize  1 t))
      border-left  () (wt-op (wresize -3))
      border-right () (wt-op (wresize  3))
      border-ul    () (pn (border-up)   (border-left))
      border-dl    () (pn (border-down) (border-left))
      border-ur    () (pn (border-up)   (border-right))
      border-dr    () (pn (border-down) (border-right)))

; error-window

(def error-window (message)
  (split-window-horizontally)
  (w/* (b (open-and-raise 'editor-buffer)
        wij (c-wij b))
    (=! (read-only wij) t
        (cursor-visible wij) nil
        (str wij) message)
    nil))

(mac w/error-screen (msg . body)
  `(handler-case (pn ,@body)
     (error (e)
       (error-window ,msg))))

; textbox widget

(def tyx-help (pt str) (ls (pt->y pt str) (pt->x pt str)))

(defm text-y-x ((w miniwij))
  (w/* (str (mb-str w)
        pt  (+ (point w) (ln (prompt-str w)))
        pt  (+ pt (trunc pt (width w)))
        pt  (confine-to 0 (ln str) pt))
    (tyx-help pt str)))

(defm text-y-x ((w textbox))
  (w/* (str (str w)
        pt  (confine-to 0 (ln str) (point w)))
    (tyx-help pt str)))

(def recenter-textbox-origin (w)
  (dbind (y x) (text-y-x w)
    (w/* (ydiff (- y (origin-row w))  yoff (- ydiff (1- (height w)))
          xdiff (- x (origin-col w))  xoff (- xdiff (1- (width  w))))
      (cif (posp yoff)  (+! (origin-row w) yoff)
           (negp ydiff) (=! (origin-row w) y))
      (cif (posp xoff)  (+! (origin-col w) xoff)
           (negp xdiff) (=! (origin-col w) x)))))

(def sync-textbox-point (wij buf win)
  (dbind (y x) (text-y-x wij)
    (scro (+ (miny wij) (- y (origin-row wij)))
          (+ (minx wij) (- x (origin-col wij)))
          buf win)))

(def draw-textbox (str wij buf win) ; FIXME - ugly
  (recenter-textbox-origin wij)
  (when (eq wij (c-wij buf win))
    (sync-textbox-point wij buf win))
  (w/* (color (color wij)
        ox    (origin-col wij)
        wx    (+ ox (width wij))
        str   (subseq str (coords->pt (origin-row wij) 0 str)))
    (wijcolor wij buf color)
    (_wattron buf (color-pair color))
    (with-input-from-string (stream (coerce str 'string))
      (loop for line = (read-line stream nil)
         for len = (ln line)
         for s = (if (< ox len) (subseq line ox (min len wx)) "")
         for y from (miny wij) below (+ (miny wij) (height wij))
         while s
         do (_+str buf y (minx wij) s)))))

(defm redraw-widget ((wij textbox) buf win)
  (draw-textbox (str wij) wij buf win))

(defm redraw-widget ((wij miniwij) buf win)
  (draw-textbox (mb-str wij) wij buf win))

(defm refresh-buffer ((b text-buffer) w)
  (w/ (wij (c-wij b w))
    (sync-textbox-point wij b w)
    (recenter-textbox-origin wij)
    (cnm)))

(defm refresh-window ((b text-buffer) w)
  (sync-textbox-point (c-wij b w) b w)
  (cnm))

; buf-ops

(defms touch-p (obj buf) nil
       break-p (obj buf) nil
       kill-p  (obj buf) nil)

(defm touch (obj) (t! (touched obj)))

(def break-buffer (b) (t! (broken b)))

(def fix (b) (nil! (broken b)))

(def map-bufs (obj test op)
  (map nil [and (=> test obj _) (=> op _)] (m-bl)))

(def buf-ops ops
  (mapc [dbind (op obj) _
               (case op (:touch (map-bufs obj #'touch-p #'touch))
                        (:break (map-bufs obj #'break-p #'break-buffer))
                        (:kill  (map-bufs obj #'kill-p #'kill-buffer)))]
        (pair ops)))

; buffer open and/or raise

(def open-buffer (type &o props)
  (add-buffer (new type :props props)))

(def open-and-raise (type &o props (w (c-win)))
  (raise (open-buffer type props) w))

(def get-buffer (lst &k type props)
  (w/ (y [typep _ type]
       p [equal props (props _)])
    (cif (and type props) (test-list lst y p)
         type             (test-list lst y)
         props            (test-list lst p)
                          (car lst))))

(def find-or-open (type &o props)
  (or (get-buffer (m-bl) :type type :props props)
      (open-buffer type props)))

(def open-or-raise (type &o props (w (c-win)))
  (raise (find-or-open type props) w))

; directional widget fns

(defps *y-slope* 1 *x-slope* 2)

(def y-diagonal (obj y &o (ys *y-slope*))
  (/ (- (avgy obj) y) ys))

(def x-diagonal (obj x &o (xs *x-slope*))
  (/ (- (avgx obj) x) xs))

(def obj-in-upward-view? (y x obj)
  (and (< (avgy obj) y)
       (< (x-diagonal obj x) (- (y-diagonal obj y)))
       (> (x-diagonal obj x)    (y-diagonal obj y))))

(def get-upward-object (y x objs)
  (w/n res
    (mapc [when (and (obj-in-upward-view? y x _)
                     (or (no res) (< (hypot (maxy _) y (avgx _) x)
                                     (hypot (maxy res) y (avgx res) x))))
            (=! res _)]
          objs)))

(def obj-in-downward-view? (y x obj)
  (and (> (avgy obj) y)
       (< (x-diagonal obj x)    (y-diagonal obj y))
       (> (x-diagonal obj x) (- (y-diagonal obj y)))))

(def get-downward-object (y x objs)
  (w/n res
    (mapc [when (and (obj-in-downward-view? y x _)
                     (or (no res) (< (hypot (miny _) y (avgx _) x)
                                     (hypot (miny res) y (avgx res) x))))
            (=! res _)]
          objs)))

(def obj-in-leftward-view? (y x obj)
  (and (< (avgx obj) x)
       (< (y-diagonal obj y) (- (x-diagonal obj x)))
       (> (y-diagonal obj y)    (x-diagonal obj x))))

(def get-leftward-object (y x objs)
  (w/n res
    (mapc [when (and (obj-in-leftward-view? y x _)
                     (or (no res) (< (hypot (avgy _) y (maxx _) x)
                                     (hypot (avgy res) y (maxx res) x))))
            (=! res _)]
          objs)))

(def obj-in-rightward-view? (y x obj)
  (and (> (avgx obj) x)
       (< (y-diagonal obj y)    (x-diagonal obj x))
       (> (y-diagonal obj y) (- (x-diagonal obj x)))))

(def get-rightward-object (y x objs)
  (w/n res
    (mapc [when (and (obj-in-rightward-view? y x _)
                     (or (no res) (< (hypot (avgy _) y (minx _) x)
                                     (hypot (avgy res) y (minx res) x))))
            (=! res _)]
          objs)))

; display initialization and sigwinch handling

(def init-windows (&o wt)
  (=! *stdscr* (initscr))
  (set-frame (split-win (mk-win nil 0 0 (std-height) (std-width))))
  (=! (mini (miniwindow)) t)
  (=! *rootbuf* (new 'standard-buffer :color *rootbuf-color*))
  (raise (new 'minibuffer) (miniwindow))
  (test-size (std-height) (std-width))
  (keypad *stdscr* 1) ; enables keypad
  (nonl)              ; converts newline to return
  (noecho)            ; disables stdscr getch echo
  (if wt
      (set-frame (w-head *frame* wt (ls (miniwindow))))
      (open-and-raise 'title-buffer)))

(def init-display ()
  (init-windows)
  (init-colors))

(def sigwinch-handler (a b c)
  (declare (ignore a b c))
  (disable-sigwinch)
  (endwin)
  (refresh)
  (init-windows (c-wt))
  (resize-miniwindow)
  (refresh-sqnc)
  (enable-sigwinch #'sigwinch-handler))
