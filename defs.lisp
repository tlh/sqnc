; also contains code specific to sbcl and other external packages

(defpackage :sqnc
  (:use :cl :cl-user :cl-ncurses :cl-store :csound :util)
  (:shadow :wresize)
  (:shadowing-import-from :util :filter))

(in-package :sqnc)

(export '(sqnc init-swank save-sqnc-and-die))

(defp *zelda-secret* '(66 65 62 57 56 63 67 71))

(defp *sqnc-str* "  s  q  n  c  ")

;   sbcl specific

; sigwinch

(def enable-sigwinch (fn)
  (sb-sys:enable-interrupt sb-posix:sigwinch fn))

(def disable-sigwinch () (sb-sys:ignore-interrupt sb-posix:sigwinch))

; threading

(def mk-thread (fn &o name) (sb-thread:make-thread fn :name name))

(def kill-thread (thread) (sb-thread:destroy-thread thread))

(def ls-threads () (sb-thread:list-all-threads))

; mutexes

(def make-mutex (name) (sb-thread:make-mutex :name name))

(mac w/mutex (lst . body) `(sb-thread:with-mutex ,lst ,@body))

; profiling

(def profile () (sb-profile:profile "SQNC"))

(def unprofile () (sb-profile:unprofile "SQNC"))

(def profile-report () (sb-profile:report))

; misc

(def save-sqnc-and-die ()
  (sb-ext:save-lisp-and-die "sqnc" :executable t :toplevel #'sqnc))

(def quit-sqnc () (endwin) (sb-ext:quit))

; cffi specific

(abbrevs fnull       cffi:null-pointer
         fnullp      cffi:null-pointer-p
         w/fstr      cffi:with-foreign-string
         farr-free   cffi:foreign-free)

(def falloc (num &o (type :float))
  (cffi:foreign-alloc type :count num))

(mac faref (n farr &o (type :float))
  `(cffi:mem-aref ,farr ,type ,n))

(def list->farr (lst farr &o (cast #'float))
  (w/fn (inner (lst a n)
          (if lst
              (pn (=! (faref n farr) (=> cast (car lst)))
                  (inner (cdr lst) farr (1+ n)))
              a))
    (inner lst farr 0)))

; foreign array pool

(w/ (pool (table))
  (def get-farr (len)
    (if (getab len pool)
        (pop (getab len pool))
        (falloc len)))
  (def free-farr (arr len)
    (push arr (getab len pool)))
  (def farr-count ()
    (w/n (n 0)
      (mh (+! n (ln v)) pool))))

; native array pool

(w/ (array-pool (make-array 0 :fill-pointer 0 :adjustable t))
  (def get-arr ()
    (if (posp (fill-pointer array-pool))
        (vector-pop array-pool)
        (make-array 0 :fill-pointer 0 :adjustable t)))
  (def free-arr (arr)
    (zero! (fill-pointer arr))
    (vector-push-extend arr array-pool))
  (def get-array-pool () array-pool))

; swank specific

(defps *swank-port* 4005 *swank* nil)

(def c-swank () *swank*)

(def init-swank ()
  (unless *swank*
    (awhen (handler-case
               (swank:create-server :port *swank-port* :dont-close t)
             (error (e) e nil))
      (=! *swank* it))))

; sqnc path - should be set by the user in ~/.sqnc

(defp *sqnc-dir* (namestring (user-homedir-pathname)))

(def set-sqnc-dir (str) (=! *sqnc-dir* str))

(def sqnc-file-path (name) (merge-pathnames *sqnc-dir* name))

(def sqnc-file-namestring (name) (namestring (sqnc-file-path name)))

(def sqnc-load (name) (load (sqnc-file-path name)))
