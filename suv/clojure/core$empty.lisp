#|$ACL2s-Preamble$;
(include-book "../../mc/mc")

(include-book "../../mc/utilities")
(include-book "../big-step")

(include-book "lang/Var")

;(include-book "lang/AFn")
;(include-book "lang/AFunction")

(include-book "core$seq")
(include-book "core$not")

(acl2::begin-book t :ttags :all);$ACL2s-Preamble$|#


(in-package "MC")

(defconst *core$empty-invoke*
  '("invoke" ((CLASS "java.lang.Object"))
    (GETSTATIC "clojure.core$empty_QMARK_" "const__0" NIL)
    (INVOKEVIRTUAL "clojure.lang.Var" "getRawRoot" 0)
    (CHECKCAST "clojure.lang.IFn")
    (GETSTATIC "clojure.core$empty_QMARK_" "const__1" NIL)
    (INVOKEVIRTUAL "clojure.lang.Var" "getRawRoot" 0)
    (CHECKCAST "clojure.lang.IFn")
    (ALOAD_1)
    (ACONST_NULL)
    (ASTORE_1)
    (INVOKEINTERFACE "clojure.lang.IFn" "invoke" 1)
    (INVOKEINTERFACE "clojure.lang.IFn" "invoke" 1)
    (ARETURN)))

(defconst *clojure.core$empty_QMARK_*
  (make-class-decl   
   "clojure.core$empty_QMARK_"
   '("clojure.lang.AFunction")
   '()
   '()
   '("const__0"
     "const__1")
   '((STRING "clojure.core")
     (STRING "not")
     (STRING "seq"))

   (list
    *core$empty-invoke*
   )
   '(REF -1)))

(defun |core$empty:core$not|-get (heap class-table)
  (static-field-value "clojure.core$empty_QMARK_" 
                      "const__0" 
                      heap
                      class-table))

(defun |core$empty:core$seq|-get (heap class-table)
  (static-field-value "clojure.core$empty_QMARK_" 
                      "const__1" 
                      heap
                      class-table))#|ACL2s-ToDo-Line|#


(defun |core$empty|-loaded? (class-table heap)
  (and (|Var|-loaded? class-table)
       (|core$not|-loaded? class-table heap) 
       (|core$seq|-loaded? class-table heap)
       (loaded? class-table
                "clojure.core$empty_QMARK_"
                *clojure.core$empty_QMARK_*)
       (|Var|-p (|core$empty:core$not|-get heap class-table)
                heap)
       (|Var|-p (|core$empty:core$seq|-get heap class-table)
                heap)
       (|core$not|-p (|Var:root|-get (|core$empty:core$not|-get heap
                                                                class-table)
                                     heap)
                     heap)
       (|core$seq|-p (|Var:root|-get (|core$empty:core$seq|-get heap
                                                                class-table)
                                     heap)
                     heap)))

(defthm |core$empty|-dep 
  (implies
   (|core$empty|-loaded? (class-table s) (heap s))
   (and (|Var|-loaded? (class-table s))
        (|core$not|-loaded? (class-table s) (heap s))
        (|core$seq|-loaded? (class-table s) (heap s))))
  :rule-classes :forward-chaining)

(defthm |core$empty:Vars|-dep 
  (implies
   (|core$empty|-loaded? (class-table s) (heap s))
   (and (|Var|-p (|core$empty:core$not|-get (heap s) (class-table s))
                 (heap s))
        (|Var|-p (|core$empty:core$seq|-get (heap s) (class-table s))
                 (heap s))))
  :rule-classes :forward-chaining)

(defthm |core$empty:not|-dep 
  (implies
   (|core$empty|-loaded? (class-table s) (heap s))
   (|core$not|-p (|Var:root|-get (|core$empty:core$not|-get (heap s)
                                                            (class-table s))
                                 (heap s))
                 (heap s)))
  :rule-classes :forward-chaining)

(defthm |core$empty:seq|-dep 
  (implies
   (|core$empty|-loaded? (class-table s) (heap s))
   (|core$seq|-p (|Var:root|-get (|core$empty:core$seq|-get (heap s)
                                                            (class-table s))
                                 (heap s))
                 (heap s)))
  :rule-classes :forward-chaining)

(defthm |core$empty|-method
  (implies 
   (|core$empty|-loaded? (class-table s) (heap s))
   (equal (lookup-method "invoke"
                         "clojure.core$empty_QMARK_"
                         (class-table s))
          *core$empty-invoke*)))

(in-theory (disable |core$empty|-loaded?))

(defun |core$empty|-p (ref heap)
  (and (not (nullrefp ref))
       (j-type-p "clojure.core$empty_QMARK_"
                 ref 
                 heap)))

(defun |core$empty:invoke|-param1 (s)
  (top (stack (top-frame s))))

(defun |core$empty:invoke|-poised (s)
  (and 
   (equal (next-inst s) 
          '(INVOKEINTERFACE "clojure.lang.IFn" 
                            "invoke" 
                            1))
   (|core$empty|-p (top (pop (stack (top-frame s)))) 
                   (heap s))))

(local (in-theory (enable  
    ->-execute-ACONST_NULL
    ->-execute-ALOAD_1
    ->-execute-ARETURN
    ->-execute-ASTORE_1
    ->-execute-CHECKCAST
    ->-execute-GETSTATIC
    ->-execute-INVOKEINTERFACE
    ;->-execute-INVOKEVIRTUAL
    )))

(defmacro |core$empty_get-core$not|-poised (s)
  `(equal (next-inst ,s) 
         '(GETSTATIC "clojure.core$empty_QMARK_" 
                     "const__0" 
                     NIL)))

(defthm core$empty-core$not=core$not
  (implies 
   (and (|core$empty|-loaded? (class-table s) (heap s))
        (|core$empty_get-core$not|-poised s))
   (-> s
       (modify s
               :pc (+ 3 (pc (top-frame s)))
               :stack (push (|core$empty:core$not|-get (heap s)
                                                       (class-table s)) 
                            (stack (top-frame s)))))))

(defun |core$empty_get-core$seq|-poised (s)
  (equal (next-inst s) 
         '(GETSTATIC "clojure.core$empty_QMARK_" 
                     "const__1" 
                     NIL)))

(defthm core$empty-core$seq=core$seq
  (implies 
   (and (|core$empty|-loaded? (class-table s) (heap s))
        (|core$empty_get-core$seq|-poised s))
   (-> s
       (modify s
               :pc (+ 3 (pc (top-frame s)))
               :stack (push (|core$empty:core$seq|-get (heap s)
                                                       (class-table s)) 
                            (stack (top-frame s)))))))

(defthm |core$seq|-classname
  (implies 
   (and (|core$seq|-loaded? (class-table s) (heap s))
        (|core$seq|-p v
                      (heap s)))
   (equal (class-name-of-ref v
                             (heap s))
          "clojure.core$seq")))

(defthm |core$empty:core$seq|-classname-2
  (implies 
   (|core$empty|-loaded? (class-table s) (heap s))
   (equal (class-name-of-ref (|Var:root|-get (|core$empty:core$seq|-get (heap s)
                                                                        (class-table s))
                                             (heap s))
                             (heap s))
          "clojure.core$seq"))  
  :hints (("Goal" :in-theory (e/d (|core$empty|-loaded?)
                                  (|Var:root|-get 
                                   class-name-of-ref
                                   |core$empty:core$seq|-get)))))

(defthm |core$not|-classname
  (implies 
   (and (|core$not|-loaded? (class-table s) (heap s))
        (|core$not|-p v
                      (heap s)))
   (equal (class-name-of-ref v
                             (heap s))
          "clojure.core$not")))

(defthm |core$empty:core$not|-classname-2
  (implies 
   (|core$empty|-loaded? (class-table s) (heap s))
   (equal (class-name-of-ref (|Var:root|-get (|core$empty:core$not|-get (heap s)
                                                                        (class-table s))
                                             (heap s))
                             (heap s))
          "clojure.core$not"))  
  :hints (("Goal" :in-theory (e/d (|core$empty|-loaded?)
                                  (|Var:root|-get 
                                   class-name-of-ref
                                   |core$empty:core$not|-get)))))
#|
(defthm TRUE-and-FALSE-Boolean
  (implies 
   (|Boolean|-loaded? class-table heap)
   (and (|Boolean|-p (|Boolean:TRUE|-get heap class-table) heap)
        (|Boolean|-p (|Boolean:FALSE|-get heap class-table) heap)))
  :rule-classes :forward-chaining
  :hints (("Goal" :in-theory (enable |Boolean|-loaded?))))
|#
(defthm sequence-is-not-Boolean
  (implies 
   (and (|Boolean|-loaded? class-table heap)
        (or (|Cons|-p coll heap)
            (|PersistentList|-p coll heap)
            (|EmptyList|-p coll heap)))
   (not (|Boolean|-p coll heap)))
  :rule-classes :type-prescription)

(defthm |core$empty|-sequence-is-false
  (let* ((coll (top (stack (top-frame s)))))
    (implies 
     (and (|core$empty|-loaded? (class-table s) (heap s))       
          (|core$empty:invoke|-poised s)
          (|Boolean|-loaded? (class-table s) (heap s))
          (seq-p coll (heap s))
          (not (|EmptyList|-p coll (heap s))))
     (-> s
         (modify s
                 :pc (+ 5 (pc (top-frame s)))
                 :stack 
                 (push (|Boolean:FALSE|-get (heap s)
                                            (class-table s))
                       (popn 2 (stack (top-frame s))))))))
    :hints (("Goal" :in-theory (e/d
                                (->-execute-DUP
                                 ->-execute-IFNULL
                                 ->-execute-IF_ACMPEQ
                                 ->-execute-GOTO
                                 ->-execute-POP)
                                (|Var|-p
                                 |Var:root|-get   
                                 |Boolean:FALSE|-get
                                 |Cons|-p
                                 |PersistentList|-p
                                 ;seq-p
                                 |core$empty:core$not|-get
                                 |core$empty:core$seq|-get)))))
  
(defthmd core$empty-null-is-true
  (let* ((coll (top (stack (top-frame s)))))
    (implies 
     (and (|core$empty|-loaded? (class-table s) (heap s))
          (|core$empty:invoke|-poised s)
          (nullrefp coll))          
     (-> s
         (modify s
                 :pc (+ 5 (pc (top-frame s)))
                 :stack (push (|Boolean:TRUE|-get (heap s)
                                                  (class-table s))
                              (popn 2 (stack (top-frame s))))))))
    :hints (("Goal" :in-theory (e/d
                                (->-execute-DUP
                                 ->-execute-IFNULL
                                 ->-execute-IF_ACMPEQ
                                 ->-execute-GOTO
                                 ->-execute-POP)
                                (|Var|-p
                                 |Var:root|-get                                      
                                 |core$empty:core$not|-get
                                 |core$empty:core$seq|-get)))))

(defthmd core$empty-EmptyList-is-true
  (let* ((coll (top (stack (top-frame s)))))
    (implies 
     (and (|core$empty|-loaded? (class-table s) (heap s))
          (|core$empty:invoke|-poised s)         
          (|EmptyList|-p coll (heap s)))
     (-> s
         (modify s
                 :pc (+ 5 (pc (top-frame s)))
                 :stack (push (|Boolean:TRUE|-get (heap s)
                                                  (class-table s))
                              (popn 2 (stack (top-frame s))))))))
    :hints (("Goal" :in-theory (e/d
                                (->-execute-DUP
                                 ->-execute-IFNULL
                                 ->-execute-IF_ACMPEQ
                                 ->-execute-GOTO
                                 ->-execute-POP)
                                (|Var|-p
                                 |Var:root|-get                                      
                                 |core$empty:core$not|-get
                                 |core$empty:core$seq|-get)))))
    
(defthm core$empty-null-or-EmptyList-is-true
  (let* ((coll (top (stack (top-frame s)))))
    (implies 
     (and (|core$empty|-loaded? (class-table s) (heap s))
          (|core$empty:invoke|-poised s)          
          (or (nullrefp coll)
              (|EmptyList|-p coll (heap s))))
     (-> s
         (modify s
                 :pc (+ 5 (pc (top-frame s)))
                 :stack (push (|Boolean:TRUE|-get (heap s)
                                                  (class-table s))
                              (popn 2 (stack (top-frame s))))))))
    :hints (("Goal" :use (core$empty-null-is-true
                          core$empty-EmptyList-is-true))))