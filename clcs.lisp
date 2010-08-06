;;; clcs - a common lisp dsl for generating csound code

;; (defpackage :clcs (:use :cl :util))
;; (in-package :clcs)
;; (export '(orc defcsmac def-opcode))
;; ;; (export '(defcsmac def-opcode *csmacs* *opcodes* *csstmts* *csexprs* orc))

(in-package :sqnc)

; cs macros

(deftabdb *csmacs* csmac?)

(mac defcsmac (name args . body)
  (w/ (macname (mksym name (gensym)))
    `(pn (=! (csmac? ',name) ',macname)
         (mac ,macname ,args ,@body))))

; cs stmts

(deftabdb *csstmts* csstmt?)

(mac defcsstmt (name args . body)
  `(=! (csstmt? ',name) (fn ,args ,@body)))

; cs expressions

(deftabdb *csexprs* csexpr?)

(mac defcsexpr (name args . body)
  `(=! (csexpr? ',name) (fn ,args ,@body)))

(mac defcsparen (namestr)
  `(defcsexpr ,(intern (string-upcase namestr)) (s form)
    `(format ,s "~a(~a)" ,',namestr ,(expr s (car form)))))

(plural defcsparen defcsparens 1)

(defcsparens "cpsmidinn" "abs" "exp" "log" "log10" "i" "int" "frac" "sqrt" "tanh")

; csd compiler

(def cs-sym (sym)
  (w/ (name (string-downcase (symbol-name sym)))
    (w/fn (rec (sym start)
            (aif (idx #\- name :start start)
                 (if (>= (1+ it) (ln name))
                     (subseq name start it)
                     (cat (subseq name start it)
                          (string (char-upcase (elt name (1+ it))))
                          (rec sym (+ 2 it))))
                 (subseq name start)))
      (rec sym 0))))

(def print-atom (s elt)
  (typecase elt
    (string  `(format ,s "~s" ',elt))
    (integer `(format ,s "~a" ',elt))
    (number  `(format ,s "~f" ',elt))
    (symbol  `(format ,s "~a" ,(cs-sym elt)))))

(def expr (s form)
  (acif (atom form)
          (print-atom s form)
        (csexpr? (car form))
          (=> it s (cdr form))
       `(format ,s "(~@{~a~^ ~a ~})"
                ,@(butlast
                   (ma [pn `(,(expr s _) ',(car form))]
                       (cdr form))))))

(def print-csv (s form)
  (if (consp form)
      `(format ,s " ~@{~a~^, ~}" ,@(mc [expr nil _] form))
      (print-atom s form)))

(def print-elt (s form)
  (acif (or (null form) (eq form '_))
          nil
        (atom form)
          (print-atom s form)
        (csstmt? (car form))
          (=> it s (cdr form))
        (consp form)
          (print-csv s form)))

(def print-stmt (s form)
  (dbind (op &o out . in) form
    `(pn (format ,s "~&  ")
         ,(print-elt s out)
         (format ,s " ")
         ,(print-elt s op)
         (format ,s " ")
         ,(print-elt s in))))

(def print-orc (s form)
  (if (atom form)
      (print-atom s form)
      (acif (csmac?  (car form)) (cons it (cdr form))
            (csstmt? (car form)) (=> it s (cdr form))
                                 (print-stmt s form))))

(mac orc forms
  `(pn ,@(ma [ls (print-orc t _) `(format t "~&")]
             forms)))

(mac defcsblock (type end)
  `(defcsstmt ,type (s form)
    `(pn (format ,s "~&~%~(~A~) " ',',type)
         ,(print-csv s (car form))
         (format ,s "~&")
         (orc ,@(cdr form))
         (format ,s "~&~(~A~)" ',',end))))

(defcsblock opcode endop)

(defcsblock instr  endin)

(defcsstmt if (s form &o rec)
  (w/ (ifn (csstmt? 'if))
    `(pn ,(case (ln form)
            (0 `(format ,s "~&  endif"))
            (1 `(pn (format ,s " ~&  else")
                    ,(print-orc s (car form))
                    ,(=> ifn s nil t)))
            (otherwise
             (if rec `(pn (format ,s "~&  elseif ~a then" ,(expr nil (car form)))
                          ,(print-orc s (cadr form))
                          ,(=> ifn s (cddr form) t))
                     `(pn (format ,s "~&  if ~A then" ,(expr nil (car form)))
                          ,(print-orc s (cadr form))
                          ,(=> ifn s (cddr form) t))))))))

; csmacs

(defcsmac pn body `(orc ,@body))

(defcsmac max (n n1 n2)
  `(orc (if (< ,n1 ,n2) (= ,n ,n2) (= ,n ,n1))))

(defcsmac min (n n1 n2)
  `(orc (if (< ,n1 ,n2) (= ,n ,n1) (= ,n ,n2))))

(defcsmac confine-to (new old lo hi)
  `(orc (max ,new ,old ,lo) (min ,new ,old ,hi)))

(defcsmac maxes (min . names)
  `(orc ,@(mc [pn `(max ,_ ,_ ,min)] names)))

(defcsmac label (sym)
  `(orc (,(mksym (string-downcase (symbol-name sym)) ":"))))

(defcsmac tiestatus (var)
  `(orc (tival ,var)
        (if (&& (== itie 0) (< p3 0)) (= ,var  0)
            (&& (< p3 0) (== itie 1)) (= ,var  1)
            (&& (> p3 0) (== itie 1)) (= ,var  2)
            (&& (> p3 0) (== itie 0)) (= ,var -1))))

(defcsmac rescale (newval val oldmin oldmax newmin newmax)
  `(orc (= ,newval (+ (* (/ (- ,newmax ,newmin)
                            (- ,oldmax ,oldmin))
                         (- ,val ,oldmin))
                      ,newmin))))

(defcsmac pfbind (n . names)
  `(orc ,@(mc [prog1 `(= ,_ ,(mksym 'p n)) (incf n)]
              names)))

(defcsmac tiskip body
  `(orc (tigoto _ skipinit)
        ,@body
        (label skipinit)))
