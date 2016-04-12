#|$ACL2s-Preamble$;
(include-book "../../../mc/mc")
(include-book "../../../mc/utilities")
(include-book "../../big-step")

(include-book "Obj")
(include-book "ASeq")

(acl2::begin-book t :ttags :all);$ACL2s-Preamble$|#



(in-package "MC")

(defconst *clojure.lang.PersistentList$EmptyList-<init>*
  '("<init>" ((CLASS "clojure.lang.IPersistentMap"))
    (ALOAD_0)
    (ALOAD_1)
    (INVOKESPECIAL "clojure.lang.Obj" "<init>" 1)
    (RETURN)))

(defconst *clojure.lang.PersistentList$EmptyList-first*
  '("first" NIL
    (ACONST_NULL)
    (ARETURN)))

(defconst *clojure.lang.PersistentList$EmptyList-next*
  '("next" NIL
    (ACONST_NULL)
    (ARETURN)))  

(defconst *clojure.lang.PersistentList$EmptyList-more*
  '("more" NIL
    (ALOAD_0)
    (ARETURN)))    
  
(defconst *clojure.lang.PersistentList$EmptyList-empty*
  '("empty" NIL
    (ALOAD_0)
    (ARETURN)))

(defconst *clojure.lang.PersistentList$EmptyList-count*
  '("count" NIL
    (ICONST_0)
    (IRETURN)))

(defconst *clojure.lang.PersistentList$EmptyList-seq*
  '("seq" NIL
    (ACONST_NULL)
    (ARETURN)))

(defconst *clojure.lang.PersistentList$EmptyList-size*
  '("size" NIL
    (ICONST_0)
    (IRETURN)))

(defconst *clojure.lang.PersistentList$EmptyList-isEmpty*
  '("isEmpty" NIL
    (ICONST_1)
    (IRETURN)))

(defconst *clojure.lang.PersistentList$EmptyList*
  (make-class-decl
   ; class name
   "clojure.lang.PersistentList$EmptyList"
  '("clojure.lang.Obj"
    "java.lang.Object"
   )
  '(
    "clojure.lang.IPersistentList" 
    "clojure.lang.List" 
    "clojure.lang.ISeq" 
    "clojure.lang.Counted"
    ; Obj interfaces
    "clojure.lang.IObj" 
    "clojure.lang.Serializable"
    ; IPersistentList
    "clojure.lang.Sequential"
    "clojure.lang.IPersistentStack"
    ; IPersistentStack
    "clojure.lang.IPersistentCollection"
    ; IPersistentCollection
    "clojure.lang.Seqable")
  '()
  '()
  '()

  (list
   *clojure.lang.PersistentList$EmptyList-<init>*
   *clojure.lang.PersistentList$EmptyList-first*
   *clojure.lang.PersistentList$EmptyList-next*
   *clojure.lang.PersistentList$EmptyList-more*
   *clojure.lang.PersistentList$EmptyList-empty*
   *clojure.lang.PersistentList$EmptyList-count*
   *clojure.lang.PersistentList$EmptyList-seq*
   *clojure.lang.PersistentList$EmptyList-size*
   *clojure.lang.PersistentList$EmptyList-isEmpty*)
  '(REF -1)))

(defun |EmptyList|-loaded? (class-table)
  (and (|Obj|-loaded? class-table)
       (loaded? class-table
                "clojure.lang.PersistentList$EmptyList" 
                *clojure.lang.PersistentList$EmptyList*)))

(defthm |EmptyList|-dep
  (implies (|EmptyList|-loaded? (class-table s))
           (|Obj|-loaded? (class-table s)))
  :rule-classes :forward-chaining)

(defthm EmptyList-seq-method
  (implies 
   (and (|EmptyList|-loaded? (class-table s))
        (equal (car classes) "clojure.lang.PersistentList$EmptyList"))
   (equal (lookup-method-in-superclasses "seq"
                                         classes
                                         (class-table s))
          *clojure.lang.PersistentList$EmptyList-seq*)))

(defthm EmptyList-superclasses
  (implies 
   (|EmptyList|-loaded? (class-table s))          
   (equal (class-superclasses "clojure.lang.PersistentList$EmptyList"
                              (class-table s))
          (list "clojure.lang.Obj"
                "java.lang.Object"))))

(defthm EmptyList-interfaces
  (implies 
   (|EmptyList|-loaded? (class-table s))
   (equal (class-interfaces "clojure.lang.PersistentList$EmptyList"
                            (class-table s))
          (list "clojure.lang.IPersistentList" 
                "clojure.lang.List" 
                "clojure.lang.ISeq" 
                "clojure.lang.Counted"
                "clojure.lang.IObj" 
                "clojure.lang.Serializable"
                "clojure.lang.Sequential"
                "clojure.lang.IPersistentStack"
                "clojure.lang.IPersistentCollection"
                "clojure.lang.Seqable"))))

(defthm EmptyList-instance-data
  (implies 
   (|EmptyList|-loaded? (class-table s))
   (equal (build-immediate-instance-data "clojure.lang.PersistentList$EmptyList"
                                         (class-table s))
          (list "clojure.lang.PersistentList$EmptyList"))))

(in-theory (disable |EmptyList|-loaded?))

(defun |EmptyList|-p (ref heap)
  (and (not (nullrefp ref))
       (j-type-p "clojure.lang.PersistentList$EmptyList" 
                 ref 
                 heap)))

(defun new-EmptyList ()
  (build-an-instance
   (list "clojure.lang.PersistentList$EmptyList"
         "clojure.lang.Obj"
         "java.lang.Object")
   (make-class-def (list *clojure.lang.Obj*
                         *clojure.lang.PersistentList$EmptyList*))))

(local (in-theory (enable ->-execute-INVOKEINTERFACE
                          ->-execute-ACONST_NULL
                          ->-execute-ARETURN
                          ->-execute-NEW)))

(defthm EmptyList-exec-new
  (implies 
   (and
    (|EmptyList|-loaded? (class-table s))
    (poised-to-new s "clojure.lang.PersistentList$EmptyList"))
   (-> 
    s
    (modify s
            :pc (+ 3 (pc (top-frame s)))
            :stack (push (list 'REF (len (heap s)))
                         (stack (top-frame s)))
            :heap (bind (len (heap s))
                        (new-EmptyList)
                        (heap s))))))

(defun |EmptyList:seq|-ref (s)
  (top (stack (top-frame s))))

(defun |EmptyList:seq|-poised (s)
  (and 
   (equal (next-inst s) 
          '(INVOKEINTERFACE "clojure.lang.Seqable"
                            "seq" 
                            0))
   (equal (class-name-of-ref (|EmptyList:seq|-ref s)
                             (heap s))
          "clojure.lang.PersistentList$EmptyList")))          

(defthm |EmptyList:seq|-is-null
  (implies 
   (and (|EmptyList|-loaded? (class-table s))
        (|EmptyList:seq|-poised s))
   (-> s
       (modify s
               :pc (+ 5 (pc (top-frame s)))
               :stack (push (nullref)
                            (pop (stack (top-frame s)))))))
  :hints (("Goal" :in-theory (enable next-inst))))#|ACL2s-ToDo-Line|#
       
