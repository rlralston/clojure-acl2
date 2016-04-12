#|$ACL2s-Preamble$;
(include-book "../../mc/mc")
(include-book "../../mc/utilities")

(include-book "lang/RT")

(acl2::begin-book t :ttags :all);$ACL2s-Preamble$|#


(in-package "MC")

(defconst *core$rest-invoke*
  '("invoke" ((CLASS "java.lang.Object"))
    (ALOAD_1)
    (ACONST_NULL)
    (ASTORE_1)
    (INVOKESTATIC "clojure.lang.RT" "more" 1)
    (ARETURN)))

(defconst *clojure.core$rest*
  (make-class-decl
   "clojure.core$rest"
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
    *core$rest-invoke*
   )
   '(REF -1))) 

(defun |core$rest|-loaded? (class-table heap)
  (and (|RT|-loaded? class-table heap)
       (loaded? class-table
                "clojure.core$rest" 
                *clojure.core$rest*)))

(defthm |core$rest|-dep 
  (implies
   (|core$rest|-loaded? (class-table s) (heap s))
   (|RT|-loaded? (class-table s) (heap s)))
  :rule-classes :forward-chaining)
  
(defthm |core$rest|-method
  (implies 
   (|core$rest|-loaded? (class-table s) (heap s))
   (equal (lookup-method "invoke"
                         "clojure.core$rest"
                         (class-table s))
          *core$rest-invoke*)))

(in-theory (disable |core$rest|-loaded?))

(defun |core$rest|-p (ref heap)
  (and (not (nullrefp ref))
       (j-type-p "clojure.core$rest" 
                 ref 
                 heap)))

(defun |core$rest:invoke|-poised (s)
  (let* ((ref (top (pop (stack (top-frame s)))))
         (p1 (top (stack (top-frame s)))))
    (and 
     (equal (next-inst s) 
            '(INVOKEINTERFACE "clojure.lang.IFn" 
                              "invoke" 
                              1))
     (|core$rest|-p ref (heap s))
     (seq-p p1 (heap s)))))

(local 
 (in-theory (enable ->-execute-ACONST_NULL 
                    ->-execute-ALOAD_1                    
                    ->-execute-ASTORE_1
                    ->-execute-ARETURN)))

(defthm |core$rest:invoke|=seq-more
  (let* ((param1 (top (stack (top-frame s)))))
    (implies 
     (and (|core$rest|-loaded? (class-table s)
                               (heap s))
          (|core$rest:invoke|-poised s)
          (seq-p param1 (heap s)))
          ;(|Cons|-p param1 (heap s)))
     (-> s
         (modify s
                 :pc (+ 5 (pc (top-frame s)))
                 :stack (push (seq-more param1
                                        (heap s)
                                        (class-table s))
                            (popn 2 (stack (top-frame s)))))))))

(defthm |core$rest:invoke(null)|=seq-more
  (let* ((param1 (top (stack (top-frame s)))))
    (implies 
     (and (|core$rest|-loaded? (class-table s)
                               (heap s))
          (|core$rest:invoke|-poised s)
          (nullrefp param1))
   (-> s
       (modify s
               :pc (+ 5 (pc (top-frame s)))
               :stack (push (seq-more param1
                                      (heap s)
                                      (class-table s))
                            (popn 2 (stack (top-frame s)))))))))#|ACL2s-ToDo-Line|#
