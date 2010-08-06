(defpackage :util (:use :cl))

(in-package :util)

; macex fns

(export '(read-eval &o &r &b &k &a group pair last1 cat))

(eval-when (:execute :load-toplevel :compile-toplevel)
  (defmacro read-eval (&body body)
    `(eval-when (:execute :load-toplevel :compile-toplevel)
       ,@body)))

(read-eval

 ; argument expander
 (defun argex (args)
   (cond ((null args) nil)
         ((atom args) `(&rest ,args))
         (t (let ((c (car args)))
              (cons (if (consp c)
                        (argex c)
                        (case c
                          (&o '&optional)
                          (&r '&rest)
                          (&k '&key)
                          (&a '&allow-other-keys)
                          (&b '&body)
                          (otherwise c)))
                    (argex (cdr args)))))))

 (defun group (lst n)
   (labels ((inner (lst acc)
              (let ((rest (nthcdr n lst)))
                (if (consp rest)
                    (inner rest (cons (subseq lst 0 n) acc))
                    (nreverse (cons lst acc))))))
     (if lst (inner lst nil) nil)))

 (defun pair (lst) (group lst 2))

 (defun last1 (lst) (car (last lst)))

 (defun cat (&rest strs) (apply #'concatenate 'string strs))

)

; abbrevs built on argex

(export '(mac def fn dbind defg defm))

(defmacro mac (name args . body)
  `(defmacro ,name ,(argex args) ,@body))

(mac defargex (new orig)
  `(mac ,new (name args . body)
     `(,',orig ,name ,(argex args) ,@body)))

(defargex def defun)

(defargex defg defgeneric)

(defargex defm defmethod)

(mac fn (args . body)
  `(lambda ,(argex args) ,@body))

(mac dbind (lst expr . body)
  `(destructuring-bind ,(argex lst) ,expr ,@body))

; square bracket lambdas:

(export '(_))

(read-eval
  (set-macro-character
   #\[ (lambda (s c) c
         `(lambda (_)
            ,(read-delimited-list #\] s t))))
  (set-macro-character #\] (get-macro-character #\))))

; plural and abbrev

(export '(plural plurals abbrev abbrevs))

(mac plural (sng plr n)
  `(mac ,plr args
     `(progn ,@(mapcar [cons ',sng _]
                       (group args ,n)))))

(plural plural plurals 3)

(mac abbrev (short long)
  `(mac ,short args `(,',long ,@args)))

(plural abbrev abbrevs 2)

; abbrevs and plurals

(export '(=! +! -! ls no ln ap pn mc ma in rev nrev exit nodups mvbind
          => idx mx1 swap trunc vpushx table sval cnm w/slots w/access
          defv defp defconst deft macs defs defms defvs defps
          defconsts defts))

(abbrevs =!        setf
         +!        incf
         -!        decf
         ls        list
         no        not
         ln        length
         ap        append
         pn        progn
         mc        mapcar
         ma        mapcan
         in        member
         rev       reverse
         nrev      nreverse
         exit      return-from
         nodups    remove-duplicates
         mvbind    multiple-value-bind
         =>        funcall
         idx       position
         mx1       macroexpand-1
         swap      substitute
         trunc     truncate
         vpushx    vector-push-extend
         table     make-hash-table
         sval      slot-value
         cnm       call-next-method
         w/slots   with-slots
         w/access  with-accessors
         defv      defvar
         defp      defparameter
         defconst  defconstant
         deft      deftype)

(plurals mac       macs      3
         def       defs      3
         defm      defms     3
         defv      defvs     2
         defp      defps     2
         defconst  defconsts 2
         deft      defts     3)

; macros

(export '(cif w/ w/uniq w/* w/n w/macs w/mac w/fns w/fn w/when while
          until -> dmac for sequential-constants collect))

; "cond-ish" if:
(mac cif args
  (cond ((null args) nil)
        ((null (cdr args)) `,(car args))
        (t `(if ,(car  args)
                ,(cadr args)
                 (cif ,@(cddr args))))))

(mac w/ (binds . body)
  `(let ,(if (atom binds) `(,binds) (pair binds))
     ,@body))

(mac w/uniq (syms . body)
  `(w/ ,(ma [ls _ '(gensym)] syms) ,@body))

(mac w/* (binds . body)
  `(let* ,(if (atom binds) `(,binds) (pair binds))
     ,@body))

(mac w/n (binds . body)
  `(w/* ,binds ,@body
     ,(cif (atom binds)      binds
           (oddp (ln binds)) (last1 binds)
                             (nth (- (ln binds) 2) binds))))

(read-eval
  (def expfn ()
    [dbind (n a . b) _
      `(,n ,(argex a) ,@b)]))

(mac w/macs (macs . body)
  `(macrolet ,(mc (expfn) macs) ,@body))

(mac w/mac (mac . body) `(w/macs (,mac) ,@body))

(mac w/fns (fns . body)
  `(labels ,(mc (expfn) fns) ,@body))

(mac w/fn (fn . body) `(w/fns (,fn) ,@body))

(mac w/when (binds . body)
  (if (no binds)
      `(pn ,@body)
      (dbind (var expr . rest) binds
        `(w/ (,var ,expr)
           (when ,var
             (w/when ,rest ,@body))))))

(mac while (test . body)
  (w/uniq (res)
    `(do (,res) ((no ,test) ,res)
       (=! ,res (pn ,@body)))))

(mac until (test . body)
  `(while (no ,test) ,@body))

(mac -> (fn-name . args)
  `(apply #',fn-name ,@args))

(mac dmac (sub-char . body)
  `(read-eval
     (set-dispatch-macro-character
      #\# ,sub-char (fn (stream c1 c2) c1 c2 ,@body))))

(mac for ((i a b &o r) . body)
  (w/uniq (to res)
    `(w/ ,(or r res)
       (do ((,i ,a (1+ ,i))
            (,to ,b))
           ((>= ,i ,to) ,(or r res))
         ,@body))))

(mac sequential-constants (start form)
  (w/ (i (1- start))
    `(defconsts ,@(ma [ls _ (+! i)] (eval form)))))

(mac collect (op . defs)
  `(ls ,@(mc [cons op _] defs)))

; anaphoric macros

(export '(it aif acif aand awhen awhile a=!))

(mac aif (test then &o else)
  `(w/ (it ,test)
     (if it ,then ,else)))

(mac acif forms
  (cif (null forms)   nil
       (oney forms) `,(car forms)
                     `(aif ,(car forms)
                           ,(cadr forms)
                           (acif ,@(cddr forms)))))

(mac aand args
  (cif (null args)        t
       (null (cdr args))  (car args)
                         `(aif ,(car args) (aand ,@(cdr args)))))

(mac awhen (test . body)
  `(w/ (it ,test)
     (when it ,@body)))

(mac awhile (test . body)
  (w/uniq (res)
    `(do ((it ,test ,test) ,res)
         ((no it) ,res)
       (=! ,res (pn ,@body)))))

(mac a=! exprs
  `(pn ,@(mc [pn `(w/ (it ,(car _)) (=! ,@_))]
             (pair exprs))))

; mapping

(export '(mapa<b map0-n map1-n mapn mapp mapp2 maplp acc))

(def mapa<b (fn a b &o (step 1))
  (do ((i a (+ i step))
       (res nil))
      ((>= i b) (nrev res))
    (push (=> fn i) res)))

(def map0-n (fn n) (mapa<b fn 0 (1+ n)))

(def map1-n (fn n) (mapa<b fn 1 (1+ n)))

(def mapn (fn . lsts)
  (w/n res
    (-> mapc [=! res (=> fn _)] lsts)))

(def mapp (fn . lsts)
  (-> map nil [awhen (=> fn _) (exit mapp it)] lsts))

(def mapp2 (fn lst)
  (map nil [mvbind (a b) (=> fn _)
             (when b (exit mapp2 (values a b)))]
       lst))

(def maplp (fn . lsts)
  (-> mapl [awhen (=> fn _) (exit maplp it)] lsts)
  nil)

(mac defacc (name fn)
  `(def ,name (fn . lsts)
     (nrev (w/n acc (-> map nil ,fn lsts)))))

(defacc acc [when (=> fn _) (push _ acc)])

(defacc filter [aand (=> fn _) (push it acc)])

; numbers

(export '(!= posp negp inverse whole-number-p avg rescale within
          confine-to astray keyed-min keyed-max sin-list numlst
          hypot))

(def != (num1 num2) (not (= num1 num2)))

(def posp (num) (> num 0))

(def negp (num) (< num 0))

(def inverse (num) (/ 1 num))

(def whole-number-p (num)
  (mvbind (res rem) (trunc num 1) res (zerop rem)))

(def avg nums (/ (-> + nums) (ln nums)))

(def rescale (val oldmin oldmax newmin newmax)
  (w/ (dif (- oldmax oldmin))
    (when (zerop dif) (error "rescale: equal min and max -- attempted 0 div"))
    (+ (* (/ (- newmax newmin) dif) (- val oldmin)) newmin)))

(def within (lo hi num)
  (and (<= lo num) (>= hi num)))

(def confine-to (lo hi n) (max lo (min hi n)))

(def astray (val min max)
  (cif (< val min) (- val min)
       (> val max) (- val max)
                   0))

(def keyed-comp (key test objs)
  (w/n res
    (mapc [when (or (no res) (=> test (=> key _) (=> key res)))
            (=! res _)]
          objs)))

(def keyed-min (key . nums) (keyed-comp key #'< nums))

(def keyed-max (key . nums) (keyed-comp key #'> nums))

(def sin-list (n rads &o y1 y2)
  (loop for i below n
     collect (rescale (sin (rescale i 0 n 0 rads))
                      -1 1 y1 y2)))

(def numlst (a b) (loop for i from a below b collect i))

(def hypot (y1 y2 x1 x2)
  (sqrt (+ (expt (- y1 y2) 2) (expt (- x1 x2) 2))))

; list

(export '(oney dot firstn access alst confined-nth prev next cprev
          cnext ins@n passes test-list pget pset pdel rotlst
          interleave remove-nth))

(def oney (lst) (and (consp lst) (no (cdr lst))))

(deft oney () `(satisfies oney)) ; type comes in handy

(def dot (lst) (cdr (last lst)))

(def firstn (n lst)
  (unless (or (zerop n) (no lst))
    (cons (car lst) (firstn (1- n) (cdr lst)))))

(mac access (key lst) `(cadr (assoc ,key ,lst)))

(mac alst args
  ``(,,@(mc [pn ``(,,@_)] (pair args))))

(def confined-nth (n lst)
  (nth (confine-to 0 (1- (ln lst)) n) lst))

; maybe needs 2nd return val. hasn't been a problem yet:
(def list-offset (elt lst &o (n 1))
  (awhen (idx elt lst)
    (w/ (i (+ n it))
      (when (>= i 0) (nth i lst)))))

(def prev (elt lst &o (n 1))
  (list-offset elt lst (- n)))

(def next (elt lst &o (n 1))
  (list-offset elt lst n))

(def clist-offset (elt lst &o (n 1))
  (when lst
    (nth (mod (+ n (or (idx elt lst) 0))
              (ln lst))
         lst)))

(def cprev (elt lst &o (n 1))
  (clist-offset elt lst (- n)))

(def cnext (elt lst &o (n 1))
  (clist-offset elt lst n))

(def ins@n (elt n lst)
  (append (subseq lst 0 n) (ls elt) (subseq lst n)))

(def passes (obj . tests)
  (mapc [unless (=> _ obj) (exit passes)] tests)
  obj)

(def test-list (lst . tests)
  (map nil [when (-> passes _ tests) (exit test-list _)] lst))

(def pget (prop plist)
  (cif (null plist)           nil
       (eql prop (car plist)) (cadr plist)
                              (pget prop (cddr plist))))

(mac pset (key val plc)
  `(aif (in ,key ,plc)
        (=! (cadr it) ,val)
        (pn (a=! ,plc (ap it (ls ,key ,val)))
            ,val)))

(def pdel (key plst)
  (when plst
    (ap (unless (eq key (car plst)) (firstn 2 plst))
        (pdel :key (cddr plst)))))

(def rotlst (lst &o back)
  (if back
      (cons (last1 lst) (butlast lst))
      (append (cdr lst) (ls (car lst)))))

(def interleave lsts
  (-> mapcan (fn elts elts) lsts))

(def remove-nth (n lst)
  (ap (firstn n lst) (nthcdr (1+ n) lst)))

; tree

(export '(flatten-tree))

(def flatten-tree (obj &o by)
  (cif (consp obj) (ma [flatten-tree _ by] (if by (=> by obj) obj))
       obj         (ls obj)))

; destructive list

(export '(rpush firstn! nthcdr! append! remove! prev! next!  cprev!
          cnext! ins@n! push-uniq rpush-uniq))

(mac rpush (elt plc)
  `(=! ,plc (ap ,plc (ls ,elt))))

(mac destructify (op n)
  `(mac ,(intern (cat (symbol-name op) "!")) args
     `(=! ,(nth ,n args) ,(cons ',op args))))

(plural destructify destructifies 2)

(destructifies firstn 1 nthcdr 1 append 0 remove 1
               prev 0 next 0 cprev 0 cnext 0 ins@n 2)

(mac push-uniq (obj plc)
  `(or (in ,obj ,plc) (push ,obj ,plc)))

(mac rpush-uniq (obj plc)
  `(or (in ,obj ,plc) (rpush ,obj ,plc)))

; ch/arr/str

(export '(mkstr empty newline-p nlstr cat! split/spc splice pt-off
          prev-ch next-ch str-of hexch cc->str num-lines skip *wspc*
          wspc upcase-p lowcase-p prev-wspc next-wspc next-non-wspc
          prev-nl next-nl line-beg line-end pt->y pt->x coords->pt
          line-up line-down prev-word next-word y?  test-ln seqbeg=
          lcs-idx str-lst lst-str escape-tildes chop-str))

(def mkstr (fmt-str . args) (-> format nil fmt-str args))

(def empty (str) (eq (ln str) 0))

(def newline-p (ch) (eq #\newline ch))

(def nlstr () (string #\newline))

(mac cat! (plc . strs) `(=! ,plc (cat ,plc ,@strs)))

(def split/spc (str) ; from cl-cookbook
  (loop for i = 0 then (1+ j)
        as  j = (idx #\Space str :start i)
        collect (subseq str i j)
        while j))

(def splice (from to str &o (ins ""))
  (cat (subseq str 0 from) ins (subseq str to)))

(def pt-off (off pt str)
  (confine-to 0 (ln str) (+ pt off)))

(def prev-ch (pt str) (pt-off -1 pt str))

(def next-ch (pt str) (pt-off  1 pt str))

(def str-of obj (mkstr "~{~a~^ ~}" obj))

(def hexch (n) (aref (mkstr "~x" n) 0))

(def cc->str (cc) (string (code-char cc)))

(def num-lines (str) (1+ (count #\newline str)))

(def skip (pt str test &o back)
  (idx t str :key test :start (if back 0 pt) :end (if back pt) :from-end back))

(defp *wspc* '(#\space #\newline #\return #\tab))

(def wspc (ch) (when (in ch *wspc*) t))

(def upcase-p (ch) (within 65 90 (char-code ch)))

(def lowcase-p (ch) (within 97 122 (char-code ch)))

(def prev-wspc (pt str) (skip pt str #'wspc t))

(def next-wspc (pt str) (skip pt str #'wspc))

(def next-non-wspc (pt str) (skip pt str [not (wspc _)]))

(def prev-nl (pt str) (idx #\newline str :from-end t :end pt))

(def next-nl (pt str) (idx #\newline str :start pt))

(def line-beg (pt str) (aif (prev-nl pt str) (1+ it) 0))

(def line-end (pt str) (aif (next-nl pt str) it (ln str)))

(def pt->y (pt str)
  (w/n (pt (min pt (ln str)) i 0)
    (awhile (prev-nl pt str)
      (=! pt it)
      (+! i))))

(def pt->x (pt str) (- pt (aif (prev-nl pt str) (1+ it) 0)))

(def coords->pt (y x str)
  (w/ (p (loop for pt = 0 then (aif (next-nl pt str) (1+ it) pt)
               for i below y
               finally (return pt)))
    (+ p (min x (- (aif (next-nl p str) it (ln str)) p)))))

(def line-up (pt str &o (n 1))
  (if (zerop n) pt
      (line-up (aif (prev-nl pt str)
                    (w/ (b (line-beg it str))
                        (+ b (min (pt->x pt str) (- it b))))
                    0)
               str (1- n))))

(def line-down (pt str &o (n 1))
  (if (zerop n) pt
      (line-down (aif (next-nl pt str)
                      (min (+ 1 it (pt->x pt str))
                           (line-end (1+ it) str))
                      (ln str))
                 str (1- n))))

(def prev-word (pt str)
  (aif (aand (skip pt str #'alphanumericp t)
             (skip it str [no (alphanumericp _)] t))
       (1+ it)
       0))

(def next-word (pt str)
  (aif (aand (skip pt str #'alphanumericp)
             (skip it str [no (alphanumericp _)]))
       it
       (ln str)))

(def y? (yn) (or (equal yn "y") (equal yn "Y")))

(def test-ln (ln . seqs)
  (no (mapp [>= ln (ln _)] seqs)))

(def seqbeg= (seqs beg &o (idx 0))
  (w/ (end (+ idx (ln beg)))
    (filter [and (<= end (ln _)) (equal beg (subseq _ idx end)) _]
            seqs)))

; returns end index of longest-common-subseq:
(def lcs-idx (seqs &o (start 0))
  (if (and seqs
           (-> test-ln start seqs)
           (-> eqs (mc [aref _ start] seqs)))
      (lcs-idx seqs (1+ start))
      start))

(def str-lst (arr)
  (nrev (for (i 0 (ln arr) ret)
          (push (char-code (aref arr i)) ret))))

(def lst-str (lst)
  (coerce (mc #'code-char lst) 'string))

(def escape-tildes (str)
  (aif (idx #\~ str)
       (cat (subseq str 0 it) "~~" (escape-tildes (subseq str (1+ it))))
       str))

(def chop-str (str len)
  (w/ (l (ln str))
    (if (< l len) str (subseq str 0 len))))

; symbol

(export '(t! nil! zero! toggle mksym syma<b mkkey cased-sym?))

(mac setall (val . plcs)
  `(=! ,@(ma [ls _ val] plcs)))

(macs t!    plcs `(setall   t ,@plcs)
      nil!  plcs `(setall nil ,@plcs)
      zero! plcs `(setall   0 ,@plcs))

(mac toggle (plc) `(=! ,plc (no ,plc)))

(def mksym lst
  (w/fn (inner (lst) (aif (car lst)
                          (cat (mkstr "~A" it) (inner (cdr lst)))
                          ""))
    (intern (inner lst))))

(def syma<b (sym a b) (mapa<b [mksym sym _] a b))

(def mkkey (sym) (intern (symbol-name sym) "KEYWORD"))

;; (def cased-sym? (sym)
;;   (and (position t (symbol-name sym) :key #'lowcase-p) t))

(def cased-sym? (sym)
  (position t (symbol-name sym) :key #'lowcase-p))

; hash

(export '(tablep table getab putab k v mh dohash aputab tab+
          fill-table tabind deftabdb hashkeys))

(def tablep (tab)
  (and (hash-table-p tab) tab))

(deft table () `(satisfies tablep))

(abbrev getab gethash)

(mac putab (keyform valform hashform)
  `(=! (getab ,keyform ,hashform) ,valform))

(mac mh (expr hash)
  `(maphash (fn (k v) k v ,expr) ,hash))

(mac dohash ((k v hash) . body)
  `(maphash (fn (,k ,v) ,k ,v ,@body) ,hash))

(mac aputab (keyform valform hashform)
  `(a=! (getab ,keyform ,hashform) ,valform))

(mac tab+ (ht key obj)
  `(pn (=! (getab ,key ,ht) ,obj)
       ,ht))

(mac fill-table (expr . pairs)
  (w/uniq (ht)
    `(w/n (,ht ,expr)
       ,@(mc [pn `(putab ,@_ ,ht)] (pair pairs)))))

;; (def tabind (lst val table)
;;   (tab+ table (car lst)
;;      (if (oney lst)
;;          val
;;          (tabind (cdr lst) val
;;                  (or (tablep (getab (car lst) table))
;;                      (table))))))

(def tabind (lst val table &o no-dups)
  (w/* (key  (car lst)
        cur  (getab key table)
        tbp  (tablep cur))
    (if (and no-dups cur (or (no tbp) (oney lst)))
        (error "duplicate binding for ~a" key)
        (tab+ table key
              (if (oney lst)
                  val
                  (tabind (cdr lst) val
                          (or tbp (table)) no-dups))))))

(mac deftabdb (db-name getter-name)
  `(pn (defp ,db-name (table))
       (mac ,getter-name (id) `(getab ,id ,',db-name))))

(def hashkeys (hash) (w/n keys (mh (push k keys) hash)))

; stream

(export '(slurp-stream get-text-file stread write-string-to-file
          w/stdout-str byte-str read-16 read-32))

(def slurp-stream (stream)
  (w/ (seq (make-string (file-length stream)))
    (read-sequence seq stream)
    seq))

(def get-text-file (path)
  (with-open-file (file path)
    (slurp-stream file)))

(def stread (str)
  (read-from-string str nil nil :preserve-whitespace t))

(def write-string-to-file (string path)
  (with-open-file (stream path :direction :output :if-exists :supersede)
    (format stream string)))

(mac w/stdout-str body
  (w/uniq (str)
    `(with-output-to-string (,str)
       (w/ (*standard-output* ,str)
         ,@body))))

(def byte-str (stream num-bytes)
  (map 'string #'code-char
       (nrev (for (i 0 num-bytes r)
               (push (read-byte stream) r)))))

(def read-16 (stream)
  (w/when (b1 (read-byte stream)
           b2 (read-byte stream))
    (=! (ldb (byte 8 8) b1) b2)
    b1))

(def read-32 (stream)
  (awhen (read-byte stream)
    (=! (ldb (byte 8 8)  it) (read-byte stream)
        (ldb (byte 8 16) it) (read-byte stream)
        (ldb (byte 8 24) it) (read-byte stream))
    it))

; file/directory

(export '(ls-dir))

(def ls-dir (path)
  (directory (make-pathname :name :wild :type :wild :defaults path)))

; time

(export '(ms->s s->ms s->beats beats->s ms->beats ticks->s ticks->ms
          s->ticks ms->ticks))

(def ms->s (ms) (/ ms 1000))

(def s->ms (s) (* s 1000))

(def s->beats (secs bpm) (* secs (/ bpm 60)))

(def beats->s (beats bpm) (* beats (/ 60 bpm)))

(def ms->beats (ms bpm) (s->beats (ms->s ms) bpm))

(def ticks->s (ticks tpm) (* ticks (/ 60 tpm)))

(def ticks->ms (ticks tpm) (s->ms (ticks->s ticks tpm)))

(def s->ticks (secs tpm) (* secs (/ tpm 60)))

(def ms->ticks (ms tpm) (s->ticks (ms->s ms) tpm))

; misc.

(export '(neq eqs t-within sleep-while rebind-stdio get-stdout-str
          get-stderr-str))

(def neq (obj1 obj2) (not (eq obj1 obj2)))

(def eqs objs
  (if (cdr objs)
      (and (eq (car objs) (cadr objs))
           (-> eqs (cdr objs)))
      t))

(mac t-within (plcs . body)
  `(pn (t! ,@plcs) ,@body (nil! ,@plcs)))

(mac sleep-while (test &o (dur .01))
  `(while ,test (sleep ,dur)))

(mac rebind-stdio body
  `(w/ (*standard-output* (make-string-output-stream)
        *error-output*    (make-string-output-stream))
     ,@body))

(def get-stdout-str ()
  (get-output-stream-string *standard-output*))

(def get-stderr-str ()
  (get-output-stream-string *error-output*))

; sbcl specific

(export '(slot-names-of))

(def slot-names-of (obj)
  (mapcar #'sb-mop:slot-definition-name
          (sb-mop:compute-slots (class-of obj))))

; clos

(export '(new slot-names svals slot-vals slotf slotf* slotf-if-nil
          cp-slots definit definit-before definit-after initargs
          unbind-all-slots))

(def new (class . keys) (apply #'make-instance class keys))

(w/ (cache (table))
  (def slot-names (obj)
    (w/ (cname (class-name (class-of obj)))
      (aif (getab cname cache)
           it
           (putab cname (slot-names-of obj) cache)))))

(def svals (obj . snames) (mc [sval obj _] snames))

(def slot-vals (obj) (-> svals obj (slot-names obj)))

(def slotf (obj . assigns)
  (mapc [=! (sval obj (car _)) (cadr _)] (pair assigns))
  obj)

(mac slotf* (obj . assigns)
  `(=! ,@(ma [ls `(sval ,obj ,(car _)) (cadr _)]
             (group assigns 2))))

(def slotf-if-nil (obj . assigns)
  (mapc [dbind (sn v) _
          (unless (sval obj sn)
            (slotf obj sn v))]
        (pair assigns))
  obj)

(def cp-slots (to from . snames)
  (mapn [slotf to _ (sval from _)] snames))

(mac definit (args . body)
  `(defm initialize-instance (,args . initargs)
     initargs ,@body (cnm)))

(mac defdefinit (name &o op)
  `(mac ,name (args . body)
     `(defmethod initialize-instance ,',op (,args &rest initargs)
        initargs ,@body)))

(plural defdefinit defdefinits 2)

(defdefinits definit-before :before
             definit-after  :after)

(def unbind-all-slots (obj)
  (mapc [slot-makunbound obj _] (slot-names obj)))

; classy: value-added defclass

(export '(classy copy))

(defp *nocp* nil)

(def nocp (defs)
  (mapc [when (in :nocp _) (push (car _) *nocp*)] defs))

(def nocp? (sn) (in sn *nocp*))

(mac classy (name supers . defs)
  `(pn (defclass ,name ,supers
         ,(mc [dbind (n i . o) _ o
                `(,n :accessor ,n
                     :initarg  ,(mkkey n)
                     :initform ,i)]
              defs))
       (nocp ',defs)
       (def ,(mksym name '-p) (obj)
         (typep obj ',name))))

(defm copy (obj) obj)

(defm copy ((str string)) (copy-seq str))

(defm copy ((l list)) (mc #'copy l))

(defm copy ((h hash-table))
  (w/n (c (table :test (hash-table-test h)))
    (mh (putab k (copy v) c) h)))

(defm copy ((obj standard-object))
  (w/n (type (type-of obj) c (new type))
    (mapc [=! (sval c _) (w/ (v (sval obj _))
                           (if (nocp? _) v (copy v)))]
          (slot-names obj))))

; markup

(export '(as with))

(mac as (tag content)
  `(format t "~&<~A>~%~A~%</~A>~%" ,tag ,content ,tag))

(mac with (tag . body)
  `(pn (format t "~&<~A>~%" ,tag)
       ,@body
       (format t "~&</~A>~%" ,tag)))

; frequency

(export '(*freq-scale* repitch midi-cps))

(defp *freq-scale* 1.059463)

(def repitch (base-cps keynum base-key &k (scale *freq-scale*))
     (* base-cps (expt scale (- keynum base-key))))

(def midi-cps (notenum)
  (* 27.5 (expt *freq-scale* (- notenum 21))))
