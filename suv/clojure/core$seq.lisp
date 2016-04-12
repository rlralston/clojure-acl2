#|$ACL2s-Preamble$;
(include-book "../../mc/mc")

(include-book "../../mc/utilities")
(include-book "../big-step")

(include-book "lang/AFn")
(include-book "lang/AFunction")
(include-book "lang/RT")

(acl2::begin-book t :ttags :all);$ACL2s-Preamble$|#


(in-package "MC")

(defconst *core$seq-invoke*
  '("invoke" ((CLASS "java.lang.Object"))
    (ALOAD_1)
    (ACONST_NULL)
    (ASTORE_1)
    (INVOKESTATIC "clojure.lang.RT" "seq" 1)
    (ARETURN)))

(defconst *clojure.core$seq*
  (make-class-decl
   "clojure.core$seq"
   '(
     "clojure.lang.AFunction"
     "clojure.lang.AFn"
     "java.lang.Object"
    )
   '(
     ; clojure.lang.AFunction interfaces
     "clojure.lang.IObj" 
     "java.util.Comparator" 
     "clojure.lang.Fn" 
     "java.io.Serializable" 
     ; clojure.lang.AFn interfaces
     "clojure.lang.IFn"
    )
   '()
   '()
   '()
   (list
    *core$seq-invoke*
   )
   '(REF -1)))

(defun |core$seq|-loaded? (class-table heap)
  (and (|RT|-loaded? class-table heap)
       (loaded? class-table
                "clojure.core$seq"
                *clojure.core$seq*)))

(defthm |core$seq|-dep 
  (implies
   (|core$seq|-loaded? (class-table s) (heap s))
   (|RT|-loaded? (class-table s) (heap s)))
  :rule-classes :forward-chaining)

(defthm |core$seq|-method
  (implies 
   (|core$seq|-loaded? (class-table s) (heap s))
   (equal (lookup-method "invoke"
                         "clojure.core$seq"
                         (class-table s))
          *core$seq-invoke*)))

(in-theory (disable |core$seq|-loaded?))

(defun |core$seq|-p (ref heap)
  (and (not (nullrefp ref))
       (j-type-p "clojure.core$seq" 
                 ref 
                 heap)))

#|
(defun |core$seq:invoke|-poised (s)
  (and 
   (equal (next-inst s) 
          '(INVOKEINTERFACE "clojure.lang.IFn" 
                            "invoke" 
                            1))
   (equal (class-name-of-ref (top (pop (stack (top-frame s))))
                             (heap s))
          "clojure.core$seq")))
|#

(defun |core$seq:invoke|-param1 (s)
  (top (stack (top-frame s))))

(defun |core$seq:invoke|-poised (s)
  (let* ((ref (top (pop (stack (top-frame s)))))
         (p1 (|core$seq:invoke|-param1 s)))
    (and 
     (equal (next-inst s) 
            '(INVOKEINTERFACE "clojure.lang.IFn" 
                              "invoke" 
                              1))
     (|core$seq|-p ref (heap s))
     (seq-p p1 (heap s)))))

(local 
 (in-theory (enable ->-execute-ACONST_NULL
                    ->-execute-ALOAD_1
                    ->-execute-ASTORE_1
                    ->-execute-ARETURN)))

(defthm core$seq-domain-is-identity
  (let* ((coll (top (stack (top-frame s)))))
    (implies 
     (and (|core$seq|-loaded? (class-table s) (heap s))
          (|core$seq:invoke|-poised s))
     (-> s
         (modify s
                 :pc (+ 5 (pc (top-frame s)))
                 :stack (push (if (|EmptyList|-p coll (heap s))
                                (nullref)
                                coll)
                              (popn 2 (stack (top-frame s)))))))))#|ACL2s-ToDo-Line|#
