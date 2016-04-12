#|$ACL2s-Preamble$;
(include-book "../../../mc/mc")
(include-book "../../../mc/utilities")

(include-book "Obj")
(include-book "ASeq")
(include-book "PersistentList")

(include-book "../../big-step")

(acl2::begin-book t :ttags :all);$ACL2s-Preamble$|#


(in-package "MC")

(defconst *clojure.lang.Cons-<init>*
  '("<init>" ((CLASS "java.lang.Object") (CLASS "clojure.lang.ISeq"))
    (ALOAD_0)
    (INVOKESPECIAL "clojure.lang.ASeq" "<init>" 0)
    (ALOAD_0)
    (ALOAD_1)
    (PUTFIELD "clojure.lang.Cons" "_first" NIL)
    (ALOAD_0)
    (ALOAD_2)
    (PUTFIELD "clojure.lang.Cons" "_more" NIL)
    (RETURN)))

(defconst *clojure.lang.Cons-first*
  '("first" NIL
    (ALOAD_0)
    (GETFIELD "clojure.lang.Cons" "_first" NIL)
    (ARETURN)))

(defconst *clojure.lang.Cons-next*
  '("next" NIL
    (ALOAD_0)
    (INVOKEVIRTUAL "clojure.lang.Cons" "more" 0)
    (INVOKEINTERFACE "clojure.lang.ISeq" "seq" 0)
    (ARETURN)))

(defconst *clojure.lang.Cons-more*
  '("more" NIL
    (ALOAD_0)
    (GETFIELD "clojure.lang.Cons" "_more" NIL)
    (IFNONNULL 7)
    (GETSTATIC "clojure.lang.PersistentList" "EMPTY" NIL)
    (ARETURN)
    (ALOAD_0)
    (GETFIELD "clojure.lang.Cons" "_more" NIL)
    (ARETURN)))

(defconst *clojure.lang.Cons-count*
  '("count" NIL
    (ICONST_1)
    (ALOAD_0)
    (GETFIELD "clojure.lang.Cons" "_more" NIL)
    (INVOKESTATIC "clojure.lang.RT" "count" 1)
    (IADD)
    (IRETURN)))

(defconst *clojure.lang.Cons*
  (make-class-decl
   ; class name
   "clojure.lang.Cons"
   '(
     "clojure.lang.ASeq"
     "clojure.lang.Obj"
     "java.lang.Object")
   ; interfaces
   '(
     ; ASeq Interfaces
     "clojure.lang.ISeq" 
     "clojure.lang.Sequential" 
     "java.util.List" 
     "java.io.Serializable" 
     "clojure.lang.IHashEq"
     )   
   '("_first" "_more")
   '()
   '()
   (list
    *clojure.lang.Cons-<init>*
    *clojure.lang.Cons-first*
    *clojure.lang.Cons-next*
    *clojure.lang.Cons-more*
    *clojure.lang.Cons-count*
   )
   '(REF -1)))

(defun |Cons|-loaded? (class-table heap)
  (and (|ASeq|-loaded? class-table)
       (|PersistentList|-loaded? class-table heap)
       (loaded? class-table 
                "clojure.lang.Cons" 
                *clojure.lang.Cons*)))

(defthm |Cons|-dep
  (implies (|Cons|-loaded? (class-table s)
                           (heap s))
           (and (|ASeq|-loaded? (class-table s))
                (|PersistentList|-loaded? (class-table s)
                                          (heap s))))
  :rule-classes :forward-chaining)

(defthm |Cons:<init>|-method
  (implies 
   (|Cons|-loaded? (class-table s)
                   (heap s))
   (equal (lookup-method "<init>"
                         "clojure.lang.Cons"
                         (class-table s))
          *clojure.lang.Cons-<init>*)))

(defthm |Cons:first|-method
  (implies 
   (and (|Cons|-loaded? (class-table s)
                        (heap s))
        (equal (car classes) "clojure.lang.Cons"))
   (equal (lookup-method-in-superclasses "first"
                                         classes
                                         (class-table s))
          *clojure.lang.Cons-first*)))

(defthm |Cons:next|-method
  (implies 
   (and (|Cons|-loaded? (class-table s)
                        (heap s))
        (equal (car classes) "clojure.lang.Cons"))
   (equal (lookup-method-in-superclasses "next"
                                         classes
                                         (class-table s))
          *clojure.lang.Cons-next*)))

(defthm |Cons:more|-method
  (implies 
   (and (|Cons|-loaded? (class-table s)
                        (heap s))
        (equal (car classes) "clojure.lang.Cons"))
   (equal (lookup-method-in-superclasses "more"
                                         classes
                                         (class-table s))
          *clojure.lang.Cons-more*)))

(defthm |Cons:count|-method
  (implies 
   (and (|Cons|-loaded? (class-table s)
                        (heap s))
        (equal (car classes) "clojure.lang.Cons"))
   (equal (lookup-method-in-superclasses "count"
                                         classes
                                         (class-table s))
          *clojure.lang.Cons-count*)))

(defthm build-Cons-instance-data
  (implies (|Cons|-loaded? (class-table s)
                           (heap s))
           (equal (build-immediate-instance-data "clojure.lang.Cons"
                                                 (class-table s))
                  (list "clojure.lang.Cons"
                        (cons "_first" 0) 
                        (cons "_more"  0)))))

(defthm Cons-superclasses
  (implies 
   (|Cons|-loaded? (class-table s)
                   (heap s))
   (equal (class-superclasses "clojure.lang.Cons"
                              (class-table s))
          (list "clojure.lang.ASeq"
                "clojure.lang.Obj"
                "java.lang.Object"))))

(defthm Cons-interfaces
  (implies 
   (|Cons|-loaded? (class-table s)
                   (heap s))
   (equal (class-interfaces "clojure.lang.Cons"
                            (class-table s))
          (list "clojure.lang.ISeq" 
                "clojure.lang.Sequential" 
                "java.util.List" 
                "java.io.Serializable" 
                "clojure.lang.IHashEq"))))

(in-theory (disable |Cons|-loaded?))

(local (in-theory (enable
    ->-execute-ACONST_NULL
    ->-execute-ALOAD_0
    ->-execute-ALOAD_1
    ->-execute-ALOAD_2
    ->-execute-ARETURN
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
    ->-execute-RETURN)))

(defun |Cons|-p (ref heap)
  (and (not (nullrefp ref))
       (j-type-p "clojure.lang.Cons" 
                 ref 
                 heap)))
       
(defun |Cons:_first|-get (ref heap)
  (let* ((instance (deref ref heap)))
    (field-value "clojure.lang.Cons" 
                 "_first" 
                 instance)))

(defun |Cons:_first|-set (ref heap value)
  (set-instance-field "clojure.lang.Cons" 
                      "_first"
                      value
                      (deref ref heap)))

(defun |Cons:_more|-get (ref heap)
  (let* ((instance (deref ref heap)))
    (field-value "clojure.lang.Cons" 
                 "_more" 
                 instance)))

(defun |Cons:_more|-set (ref heap value)
  (set-instance-field "clojure.lang.Cons" 
                      "_more"
                      value
                      (deref ref heap)))

(defun init-Obj-parent (object)
  (set-instance-field "clojure.lang.Obj"
                       "_meta"
                       (nullref)
                       object))

(defthm Cons-instanceof-ISeq
  (implies 
   (and (|Cons|-loaded? (class-table s)
                        (heap s))
        (equal ref (top (stack (top-frame s))))
        (|Cons|-p ref (heap s))
        (equal (next-inst s)
               '(INSTANCEOF "clojure.lang.ISeq")))
   (-> s
       (modify s
               :pc (+ 3 (pc (top-frame s)))
               :stack (push 1
                            (pop (stack (top-frame s))))))))

(defthm Cons-is-ASeq
  (implies 
   (and (|Cons|-loaded? (class-table s)
                        (heap s))
        (equal ref (top (stack (top-frame s))))
        (|Cons|-p ref (heap s))
        (equal (next-inst s)
               '(INSTANCEOF "clojure.lang.ASeq")))
   (-> s
       (modify s
               :pc (+ 3 (pc (top-frame s)))
               :stack (push 1
                            (pop (stack (top-frame s))))))))

; -----------------------------------------------------------------------------
; <init>

(defun init-ASeq-parent (object)
  (set-instance-fields "clojure.lang.ASeq"
                       '("_hash" "_hasheq")
                       (list -1 -1)
                       (init-Obj-parent object)))

(defun Cons-init (first more class-table)
  (let* ((new-object (build-an-instance
                      (cons "clojure.lang.Cons"
                            (class-decl-superclasses
                             (bound? "clojure.lang.Cons" class-table)))
                      class-table)))
    (set-instance-fields "clojure.lang.Cons"
                         '("_first" "_more")
                         (list first more)
                         (init-ASeq-parent new-object))))

(defun |Cons|-new ()
  (build-an-instance
   (list "clojure.lang.Cons"
         "clojure.lang.ASeq"
         "clojure.lang.Obj"
         "java.lang.Object")
   (make-class-def (list *clojure.lang.Obj* 
                         *clojure.lang.ASeq* 
                         *clojure.lang.Cons*))))

(defthm new-Cons-creates-new-Cons
  (implies 
   (and (poised-to-new s "clojure.lang.Cons")
        (|Cons|-loaded? (class-table s)
                        (heap s)))
   (-> s
       (modify s
               :pc (+ 3 (pc (top-frame s)))
               :stack (push (list 'REF (len (heap s)))
                            (stack (top-frame s)))
               :heap (bind (len (heap s))
                           (|Cons|-new)
                           (heap s))))))

(defun |Cons:<init>|-poised (s)
  (equal (next-inst s) 
         '(invokespecial "clojure.lang.Cons" 
                         "<init>" 
                         2)))

(defun j-Cons-initValues (instance)
  (init-ASeq-parent (init-Obj-parent instance)))

(defun |Cons|-init (instance first more)
    (set-instance-fields "clojure.lang.Cons"
                       '("_first" "_more")
                       (list first more)
                       (j-Cons-initValues instance)))

(defthm |Cons:init|-initializes
  (implies 
   (and (|Cons|-loaded? (class-table s)
                        (heap s))
        (|Cons:<init>|-poised s)
        (equal (deref (top (pop (stack (top-frame s))))
                      (heap s))
               (|Cons|-new)))
   (-> s
       (modify s
               :pc (+ 3 (pc (top-frame s)))
               :stack (popn 3 (stack (top-frame s)))
               :heap (bind (cadr (top (pop (pop (stack (top-frame s))))))
                           (|Cons|-init (deref (top (pop (pop (stack (top-frame s)))))
                                               (heap s))
                                        (top (pop (stack (top-frame s))))
                                        (top (stack (top-frame s))))
                           (heap s))))))

; -----------------------------------------------------------------------------
; first

(defun |Cons:first|-poised (s)  
  (let* ((ref (top (stack (top-frame s))))
         (heap (heap s)))
    (and 
     (|Cons|-p ref heap)
     (equal (next-inst s) 
            '(invokeinterface "clojure.lang.ISeq" 
                              "first" 
                              0)))))

(defthm |Cons:first|=_first
  (implies 
   (and (|Cons|-loaded? (class-table s)
                        (heap s))
        (|Cons:first|-poised s))                  
   (-> s          
       (modify s              
               :pc (+ 5 (pc (top-frame s)))                       
               :stack 
               (push 
                (|Cons:_first|-get (top (stack (top-frame s)))                                               
                                   (heap s))                                                          
                (pop (stack (top-frame s))))))))

; -----------------------------------------------------------------------------
; more

(defun |Cons:more|-poised (s)  
  (let* ((ref (top (stack (top-frame s))))
         (heap (heap s)))
    (and 
     (|Cons|-p ref heap)
     (equal (next-inst s) 
            '(invokeinterface "clojure.lang.ISeq" 
                              "more"                                
                              0)))))

(defthm |Cons:next|=_next
  (implies 
   (and (|Cons|-loaded? (class-table s)
                        (heap s))
        (|Cons:more|-poised s))
   (let* ((more (|Cons:_more|-get (top (stack (top-frame s)))                                               
                                  (heap s))))
     (-> s          
         (modify s              
                 :pc (+ 5 (pc (top-frame s)))                       
                 :stack 
                 (push 
                  (if (nullrefp more)
                    (|PersistentList:EMPTY|-get (heap s)
                                                (class-table s))
                    more)                                                          
                  (pop (stack (top-frame s)))))))))#|ACL2s-ToDo-Line|#
