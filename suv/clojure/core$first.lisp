#|$ACL2s-Preamble$;
(include-book "../../mc/mc")
(include-book "../../mc/utilities")

(include-book "lang/RT")

(acl2::begin-book t :ttags :all);$ACL2s-Preamble$|#


(in-package "MC")

(defconst *core$first-invoke*
  '("invoke" (java.lang.Object)
    (ALOAD_1)
    (ACONST_NULL)
    (ASTORE_1)
    (INVOKESTATIC "clojure.lang.RT" "first" 1)
    (ARETURN)))

(defconst *clojure.core$first*
  (make-class-decl
   "clojure.core$first"
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
    *core$first-invoke*
   )
   '(REF -1)))

(defun |core$first|-loaded? (class-table heap)
  (and (|RT|-loaded? class-table heap)
       (loaded? class-table 
                "clojure.core$first" 
                *clojure.core$first*)))

(defthm |core$first|-dep 
  (implies
   (|core$first|-loaded? (class-table s) (heap s))
   (|RT|-loaded? (class-table s) (heap s)))
  :rule-classes :forward-chaining)

(defthm |core$first|-method
  (implies 
   (|core$first|-loaded? (class-table s) (heap s))
   (equal (lookup-method "invoke"
                         "clojure.core$first"
                         (class-table s))
          *core$first-invoke*)))

(in-theory (disable |core$first|-loaded?))

(defun |core$first|-p (ref heap)
  (and (not (nullrefp ref))
       (j-type-p "clojure.core$first" 
                 ref 
                 heap)))

(defun |core$first:invoke|-poised (s)
  (let* ((ref (top (pop (stack (top-frame s)))))
         (p1 (top (stack (top-frame s)))))
    (and 
     (equal (next-inst s) 
            '(INVOKEINTERFACE "clojure.lang.IFn" 
                              "invoke" 
                              1))
     (|core$first|-p ref (heap s))
     (seq-p p1 (heap s)))))

(local 
 (in-theory (enable ->-execute-ACONST_NULL 
                    ->-execute-ALOAD_1                    
                    ->-execute-ASTORE_1
                    ->-execute-ARETURN)))

(defthm |core$first:invoke|=first
  (let* ((param1 (top (stack (top-frame s)))))
    (implies 
     (and (|core$first|-loaded? (class-table s)
                                (heap s))
          (|core$first:invoke|-poised s)
          (seq-p param1 (heap s)))
     (-> s
         (modify s
                 :pc (+ 5 (pc (top-frame s)))
                 :stack (push (seq-first param1 (heap s))
                              (popn 2 (stack (top-frame s)))))))))#|ACL2s-ToDo-Line|#
