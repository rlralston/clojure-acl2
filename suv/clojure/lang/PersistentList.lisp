#|$ACL2s-Preamble$;
(include-book "../../../mc/mc")
(include-book "../../../mc/utilities")

(include-book "Obj")
(include-book "ASeq")
(include-book "PersistentList$Empty")

(acl2::begin-book t :ttags :all);$ACL2s-Preamble$|#


(in-package "MC")

(defconst *clojure.lang.PersistentList-<init>*
  '("<init>" (java.lang.Object)
    (ALOAD_0)
    (INVOKESPECIAL "clojure.lang.ASeq" 
                   "<init>" 
                   0)
    (ALOAD_0)
    (ALOAD_1)
    (PUTFIELD "clojure.lang.PersistentList" 
              "_first" 
              NIL)
    (ALOAD_0)
    (ACONST_NULL)
    (PUTFIELD "clojure.lang.PersistentList" 
              "_rest" 
              NIL)
    (ALOAD_0)
    (ICONST_1)
    (PUTFIELD "clojure.lang.PersistentList" 
              "_count" 
              NIL)
    (RETURN)))

(defconst *clojure.lang.PersistentList-first*
  '("first" NIL
    (ALOAD_0)
    (GETFIELD "clojure.lang.PersistentList" 
              "_first" 
              NIL)
    (ARETURN)))

(defconst *clojure.lang.PersistentList-next*  
  '("next" NIL
    (ALOAD_0)
    (GETFIELD "clojure.lang.PersistentList" 
              "_count" 
              NIL)
    (ICONST_1)
    (IF_ICMPNE 5)
    (ACONST_NULL)
    (ARETURN)
    (ALOAD_0)
    (GETFIELD "clojure.lang.PersistentList" 
              "_rest" 
              NIL)
    (CHECKCAST "clojure.lang.ISeq")
    (ARETURN)))

(defconst *clojure.lang.PersistentList-count*  
  '("count" NIL
    (ALOAD_0)
    (GETFIELD "clojure.lang.PersistentList" 
              "_count" 
              NIL)
    (IRETURN)))

(defconst *clojure.lang.PersistentList*
  (make-class-decl
   ; class name
   "clojure.lang.PersistentList"
   ; Parent Classes
   '("clojure.lang.ASeq" 
     "clojure.lang.Obj"
     "java.lang.Object")
   ; Interfaces
   '(
     ; PersistentList Interfaces
     "clojure.lang.IPersistentList" 
     "clojure.lang.IReduce" 
     "clojure.lang.List" 
     "clojure.lang.Counted"
     ; ASeq Interfaces
     "clojure.lang.ISeq" 
     "clojure.lang.Sequential" 
     "java.util.List" 
     "java.io.Serializable" 
     "clojure.lang.IHashEq"
     )
   '("_first" 
     "_rest" 
     "_count")
   '("EMPTY")
   '()

   (list
    *clojure.lang.PersistentList-<init>*
    *clojure.lang.PersistentList-first*
    *clojure.lang.PersistentList-next*
    *clojure.lang.PersistentList-count*
    )
   '(REF -1)))

(defun |PersistentList:EMPTY|-get (heap class-table)
  (static-field-value 
   "clojure.lang.PersistentList"
   "EMPTY"
   heap
   class-table))

(defun |PersistentList|-loaded? (class-table heap)
  (and 
   (|ASeq|-loaded? class-table)
   (|EmptyList|-loaded? class-table)
   (loaded? class-table
            "clojure.lang.PersistentList" 
            *clojure.lang.PersistentList*)
   (|EmptyList|-p (|PersistentList:EMPTY|-get heap class-table)
                  heap)))

(defthm |PersistentList|-dep
  (implies (|PersistentList|-loaded? (class-table s)
                                     (heap s))
           (and (|ASeq|-loaded? (class-table s))
                (|EmptyList|-loaded? (class-table s))))
  :rule-classes :forward-chaining)

(defthm |PersistentList:<init>|-method
  (implies 
   (|PersistentList|-loaded? (class-table s)
                             (heap s))
   (equal (lookup-method "<init>"
                         "clojure.lang.PersistentList"
                         (class-table s))
          *clojure.lang.PersistentList-<init>*)))

(defthm |PersistentList:first|-method
  (implies 
   (and (|PersistentList|-loaded? (class-table s)
                                  (heap s))
        (equal (car classes) "clojure.lang.PersistentList"))
   (equal (lookup-method-in-superclasses "first"
                                         classes
                                         (class-table s))
          *clojure.lang.PersistentList-first*)))

(defthm |PersistentList:more|-method
  (implies 
   (and (|PersistentList|-loaded? (class-table s)
                                  (heap s))
        (equal (car classes) "clojure.lang.PersistentList"))
   (equal (lookup-method-in-superclasses "next"
                                         classes
                                         (class-table s))
          *clojure.lang.PersistentList-next*)))

(defthm |PersistentList:count|-method
  (implies 
   (and (|PersistentList|-loaded? (class-table s)
                                  (heap s))
        (equal (car classes) "clojure.lang.PersistentList"))
   (equal (lookup-method-in-superclasses "count"
                                         classes
                                         (class-table s))
          *clojure.lang.PersistentList-count*)))

(defthm build-PersistentList-instance-data
  (implies (|PersistentList|-loaded? (class-table s)
                                     (heap s))
           (equal (build-immediate-instance-data "clojure.lang.PersistentList"
                                                 (class-table s))
                  (list "clojure.lang.PersistentList"
                        (cons "_first" 0) 
                        (cons "_rest"  0)
                        (cons "_count" 0)))))

(defthm PersistentList-superclasses
  (implies (|PersistentList|-loaded? (class-table s)
                                     (heap s))
           (equal (class-superclasses "clojure.lang.PersistentList"
                                      (class-table s))
                  (list "clojure.lang.ASeq"
                        "clojure.lang.Obj"
                        "java.lang.Object"))))

(defthm PersistentList-interfaces
  (implies (|PersistentList|-loaded? (class-table s)
                                     (heap s))
           (equal (class-interfaces "clojure.lang.PersistentList"
                                    (class-table s))
                  (list "clojure.lang.IPersistentList" 
                        "clojure.lang.IReduce" 
                        "clojure.lang.List" 
                        "clojure.lang.Counted"
                        "clojure.lang.ISeq" 
                        "clojure.lang.Sequential" 
                        "java.util.List" 
                        "java.io.Serializable" 
                        "clojure.lang.IHashEq"))))

(in-theory (disable |PersistentList|-loaded?))

(local (in-theory (enable
    ->-execute-ACONST_NULL
    ->-execute-ALOAD_0
    ->-execute-ALOAD_1
    ->-execute-ARETURN
    ->-execute-CHECKCAST
    ->-execute-GETSTATIC
    ->-execute-GETFIELD
    ->-execute-ICONST_1
    ->-execute-ICONST_M1
    ->-execute-IF_ICMPNE
    ->-execute-INSTANCEOF
    ->-execute-INVOKESPECIAL
    ->-execute-INVOKEVIRTUAL
    ->-execute-NEW
    ->-execute-PUTFIELD    
    ->-execute-RETURN)))

(defun |PersistentList:EMPTY|-poised (s)
  (equal 
   (next-inst s) 
   '(GETSTATIC "clojure.lang.PersistentList" 
               "EMPTY" 
               NIL)))

(defthm PersistentList-EMPTY=EMPTY
  (implies 
   (|PersistentList:EMPTY|-poised s)
   (-> s
       (modify 
        s
        :pc (+ 3 (pc (top-frame s)))
        :stack 
        (push 
         (|PersistentList:EMPTY|-get (heap s)
                                     (class-table s)) 
         (stack (top-frame s)))))))

(defun |PersistentList|-p (ref heap)
  (and (not (nullrefp ref))
       (j-type-p "clojure.lang.PersistentList" 
                 ref 
                 heap)))

(defun poised-to-new (s classname)
  (equal (next-inst s) 
         (list 'new classname)))

(defun |PersistentList:_first|-get (ref heap)
  (let* ((instance (deref ref heap)))
    (field-value "clojure.lang.PersistentList" 
                 "_first" 
                 instance)))

(defun |PersistentList:_first|-set (ref heap value)
  (set-instance-field "clojure.lang.PersistentList" 
                      "_first"
                      value
                      (deref ref heap)))

(defun |PersistentList:_rest|-get (ref heap)
  (let* ((instance (deref ref heap)))
    (field-value "clojure.lang.PersistentList" 
                 "_rest" 
                 instance)))

(defun |PersistentList:_rest|-set (ref heap value)
  (set-instance-field "clojure.lang.PersistentList" 
                      "_rest"
                      value
                      (deref ref heap)))

(defun |PersistentList:_count|-get (ref heap)
  (let* ((instance (deref ref heap)))
    (field-value "clojure.lang.PersistentList" 
                 "_count" 
                 instance)))

(defun |PersistentList:_count|-set (ref heap value)
  (set-instance-field "clojure.lang.PersistentList" 
                      "_count"
                      value
                      (deref ref heap)))

(defthm PersistentList-instanceof-ISeq
  (implies (and (|PersistentList|-loaded? (class-table s)
                                          (heap s))
                (equal ref (top (stack (top-frame s))))                
                (|PersistentList|-p ref (heap s))
                (equal (next-inst s)
                       '(INSTANCEOF "clojure.lang.ISeq")))
           (-> s
               (modify s
                       :pc (+ 3 (pc (top-frame s)))
                       :stack (push 1
                                    (pop (stack (top-frame s))))))))

(defthm PersistentList-instanceof-ASeq
  (implies (and (|PersistentList|-loaded? (class-table s)
                                          (heap s))
                (equal ref (top (stack (top-frame s))))
                (|PersistentList|-p ref (heap s))
                (equal (next-inst s)
                       '(INSTANCEOF "clojure.lang.ASeq")))
           (-> s
               (modify s
                       :pc (+ 3 (pc (top-frame s)))
                       :stack (push 1
                                    (pop (stack (top-frame s))))))))

; -----------------------------------------------------------------------------
; <init>

(defun init-Obj-parent (object)
  (set-instance-field "clojure.lang.Obj"
                       "_meta"
                       (nullref)
                       object))

(defun init-ASeq-parent (object)
  (set-instance-fields "clojure.lang.ASeq"
                       '("_hash" "_hasheq")
                       (list -1 -1)
                       (init-Obj-parent object)))

; Imitates constructing a new PersistentList object with ref
; as the element
(defun PersistentList-init (ref class-table)
  (let* ((new-object (build-an-instance
                      (cons "clojure.lang.PersistentList"
                            (class-decl-superclasses
                             (bound? "clojure.lang.PersistentList" class-table)))
                      class-table)))
    (set-instance-fields "clojure.lang.PersistentList"
                         '("_first" "_rest" "_count")
                         (list ref (nullref) 1)
                         (init-ASeq-parent new-object))))

(defun |PersistentList|-new ()
  (build-an-instance
   (list "clojure.lang.PersistentList"
         "clojure.lang.ASeq"
         "clojure.lang.Obj"
         "java.lang.Object")
   (make-class-def (list *clojure.lang.Obj* 
                         *clojure.lang.ASeq* 
                         *clojure.lang.PersistentList*))))

(defthm new-PersistentList-creates-new-PersistentList
  (implies 
   (and (poised-to-new s "clojure.lang.PersistentList")
        (|PersistentList|-loaded? (class-table s)
                                  (heap s)))
   (-> s
       (modify s
               :pc (+ 3 (pc (top-frame s)))
               :stack (push (list 'REF (len (heap s)))
                            (stack (top-frame s)))
               :heap (bind (len (heap s))
                           (|PersistentList|-new)
                           (heap s))))))

(defun |PersistentList:<init>|-poised (s)
  (equal (next-inst s) 
         '(invokespecial "clojure.lang.PersistentList" 
                         "<init>" 
                         1)))

(defun j-PersistentList-initValues (instance)
  (init-ASeq-parent (init-Obj-parent instance)))

(defun |PersistentList|-init (instance first)
    (set-instance-fields "clojure.lang.PersistentList"
                       '("_first" "_rest" "_count")
                       (list first (nullref) 1)
                       (j-PersistentList-initValues instance)))

(defthm |PersistentList:init|-initializes
  (implies 
   (and (|PersistentList|-loaded? (class-table s)
                                  (heap s))
        (|PersistentList:<init>|-poised s)
        (equal (deref (top (pop (stack (top-frame s))))
                      (heap s))
               (|PersistentList|-new)))
   (-> s
       (modify s
               :pc (+ 3 (pc (top-frame s)))
               :stack (popn 2 (stack (top-frame s)))
               :heap (bind (cadr (top (pop (stack (top-frame s)))))
                           (|PersistentList|-init (deref (top (pop (stack (top-frame s))))
                                                         (heap s))
                                                  (top (stack (top-frame s))))
                          (heap s))))))

; -----------------------------------------------------------------------------
; first

(defun |PersistentList:first|-poised (s)  
  (let* ((ref (top (stack (top-frame s))))
         (heap (heap s)))
    (and 
     (|PersistentList|-p ref heap)
     (equal (next-inst s) 
            '(invokeinterface "clojure.lang.ISeq" 
                              "first" 
                              0)))))

(defthm |PersistentList:first|=_first
  (implies 
   (and (|PersistentList|-loaded? (class-table s)
                                  (heap s))
        (|PersistentList:first|-poised s))                  
   (-> s          
       (modify s              
               :pc (+ 5 (pc (top-frame s)))                       
               :stack 
               (push 
                (|PersistentList:_first|-get (top (stack (top-frame s)))                                               
                                             (heap s))                                                          
                (pop (stack (top-frame s))))))))

; -----------------------------------------------------------------------------
; next

(defun |PersistentList:next|-poised (s)  
  (let* ((ref (top (stack (top-frame s))))
         (heap (heap s)))
    (and 
     (|PersistentList|-p ref heap)
     (equal (next-inst s) 
            '(invokevirtual "clojure.lang.ASeq" 
                            "next"                                
                            0)))))

(defthm |PersistentList:next|=_next
  (implies 
   (and (|PersistentList|-loaded? (class-table s)
                                  (heap s))
        (|PersistentList:next|-poised s))                  
   (-> s          
       (modify s              
               :pc (+ 3 (pc (top-frame s)))                       
               :stack 
               (push 
                (if (= (|PersistentList:_count|-get (top (stack (top-frame s)))                                               
                                                    (heap s))
                       1)
                  (nullref)
                  (|PersistentList:_rest|-get (top (stack (top-frame s)))                                               
                                              (heap s)))                                                          
                (pop (stack (top-frame s))))))))#|ACL2s-ToDo-Line|#

