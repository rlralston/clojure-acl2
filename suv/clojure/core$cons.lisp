#|$ACL2s-Preamble$;
(include-book "../../mc/mc")
(include-book "../../mc/utilities")

(include-book "lang/RT")

(acl2::begin-book t :ttags :all);$ACL2s-Preamble$|#


(in-package "MC")

(defconst *core$cons-invoke*
  '("invoke" ((CLASS "java.lang.Object") (CLASS "java.lang.Object"))
    (ALOAD_1)
    (ACONST_NULL)
    (ASTORE_1)
    (ALOAD_2)
    (ACONST_NULL)
    (ASTORE_2)
    (INVOKESTATIC "clojure.lang.RT" "cons" 2)
    (ARETURN)))

(defconst *clojure.core$cons*
  (make-class-decl
   "clojure.core$cons"
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
    *core$cons-invoke*
   )
   '(REF -1)))

(defun |core$cons|-loaded? (class-table heap)
  (and (|RT|-loaded? class-table heap)
       (loaded? class-table
                "clojure.core$cons" 
                *clojure.core$cons*)))

(defthm |core$cons|-dep 
  (implies
   (|core$cons|-loaded? class-table heap)
   (|RT|-loaded? class-table heap))
   ;(|core$cons|-loaded? (class-table s) (heap s))
   ;(|RT|-loaded? (class-table s) (heap s)))
  :rule-classes (:forward-chaining :rewrite))
  
(defthm |core$cons|-method
  (implies 
   (|core$cons|-loaded? (class-table s) (heap s))
   (equal (lookup-method "invoke"
                         "clojure.core$cons"
                         (class-table s))
          *core$cons-invoke*)))

(in-theory (disable |core$cons|-loaded?))

(defun |core$cons|-p (ref heap)
  (and (not (nullrefp ref))
       (j-type-p "clojure.core$cons" 
                 ref 
                 heap)))

(defun |core$cons:invoke|-poised (s)
  (let* ((ref (top (pop (pop (stack (top-frame s)))))))
    (and 
     (equal (next-inst s) 
            '(INVOKEINTERFACE "clojure.lang.IFn" 
                              "invoke" 
                              2))
     (|core$cons|-p ref (heap s)))))

(local 
 (in-theory (enable ->-execute-ACONST_NULL 
                    ->-execute-ALOAD_1
                    ->-execute-ALOAD_2
                    ->-execute-ASTORE_1
                    ->-execute-ASTORE_2
                    ->-execute-ARETURN)))

(defthm |core$cons:invoke|=cons
  (let* ((x (top (pop (stack (top-frame s)))))
         (coll (top (stack (top-frame s)))))
  (implies    
   (and (|core$cons|-loaded? (class-table s) (heap s))               
        (|core$cons:invoke|-poised s)
        (seq-p coll (heap s)))
   (-> s
       (modify s
               :pc (+ 5 (pc (top-frame s)))
               :stack (push (list 'REF (len (heap s)))
                            (popn 3 (stack (top-frame s))))
               :heap (bind (len (heap s))
                           (|Cons|-init (|Cons|-new) x coll)
                           (heap s)))))))

(defthmd |RT:cons|-adds-new-PersistentList-loaded?
  (let* ((x (top (pop (stack (top-frame s)))))
         (coll (top (stack (top-frame s)))))
  (implies 
   (and (|RT|-loaded? (class-table s) (heap s)) 
        (|RT:cons|-poised s)
        (|PersistentList|-loaded? (class-table s) (heap s))
        (|ASeq|-loaded? (class-table s))
        (|Obj|-loaded? (class-table s))
        (|Object|-loaded? (class-table s))
        (nullrefp coll))
   (-> s
       (modify s
               :pc (+ 3 (pc (top-frame s)))
               :stack (push (list 'REF (len (heap s)))
                            (popn 2 (stack (top-frame s))))
               :heap (bind (len (heap s))
                           (|PersistentList|-init (|PersistentList|-new) x)
                           (heap s))))))
  :hints (("Goal" :in-theory (enable ->-execute-ACONST_NULL
                                     ->-execute-ALOAD_0
                                     ->-execute-ALOAD_1
                                     ->-execute-ALOAD_2
                                     ->-execute-ARETURN
                                     ->-execute-DUP
                                     ->-execute-GETFIELD
                                     ->-execute-GETSTATIC
                                     ->-execute-IADD
                                     ->-execute-ICONST_1
                                     ->-execute-ICONST_M1
                                     ->-execute-IFNONNULL
                                     ->-execute-INSTANCEOF
                                     ->-execute-INVOKEINTERFACE
                                     ->-execute-INVOKESPECIAL
                                     ->-execute-INVOKESTATIC
                                     ->-execute-INVOKEVIRTUAL
                                     ->-execute-IRETURN
                                     ->-execute-NEW
                                     ->-execute-PUTFIELD    
                                     ->-execute-RETURN))))

(defthm |RT:cons|-adds-new-PersistentList
  (let* ((x (top (pop (stack (top-frame s)))))
         (coll (top (stack (top-frame s)))))
  (implies 
   (and (|RT|-loaded? (class-table s) (heap s)) 
        (|RT:cons|-poised s)
        (nullrefp coll))
   (-> s
       (modify s
               :pc (+ 3 (pc (top-frame s)))
               :stack (push (list 'REF (len (heap s)))
                            (popn 2 (stack (top-frame s))))
               :heap (bind (len (heap s))
                           (|PersistentList|-init (|PersistentList|-new) x)
                           (heap s))))))
  :hints (("Goal" :use |RT:cons|-adds-new-PersistentList-loaded?)))

(defthm |core$cons:invoke|=cons-nullref
  (let* ((x (top (pop (stack (top-frame s)))))
         (coll (top (stack (top-frame s)))))
  (implies    
   (and (|core$cons|-loaded? (class-table s) (heap s)) 
        (|core$cons:invoke|-poised s)
        (nullrefp coll))
   (-> s
       (modify s
               :pc (+ 5 (pc (top-frame s)))
               :stack (push (list 'REF (len (heap s)))
                            (popn 3 (stack (top-frame s))))
               :heap (bind (len (heap s))
                           (|PersistentList|-init (|PersistentList|-new) x)
                           (heap s)))))))#|ACL2s-ToDo-Line|#
