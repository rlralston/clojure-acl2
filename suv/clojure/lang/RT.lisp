#|$ACL2s-Preamble$;
(include-book "../../../mc/mc")
(include-book "../../../mc/utilities")
(include-book "../../big-step")

(include-book "PersistentList")
(include-book "Cons")

(acl2::begin-book t :ttags :all);$ACL2s-Preamble$|#


(in-package "MC")

(defconst *clojure.lang.RT-seq*
  '("seq" ((CLASS "java.lang.Object"))
    (ALOAD_0)
    (INSTANCEOF "clojure.lang.ASeq")
    (IFEQ 8)
    (ALOAD_0)
    (CHECKCAST "clojure.lang.ASeq")
    (ARETURN)
    (ALOAD_0)
    (INSTANCEOF "clojure.lang.LazySeq")
    (IFEQ 11)
    (ALOAD_0)
    (CHECKCAST "clojure.lang.LazySeq")
    (INVOKEVIRTUAL "clojure.lang.LazySeq" "seq" 0)
    (ARETURN)
    (ALOAD_0)
    (INVOKESTATIC "clojure.lang.RT" "seqFrom" 1)
    (ARETURN)))

(defconst *clojure.lang.RT-seqFrom*
  '("seqFrom" ((CLASS "java.lang.Object"))
    (ALOAD_0)
    (INSTANCEOF "clojure.lang.Seqable")
    (IFEQ 13)
    (ALOAD_0)
    (CHECKCAST "clojure.lang.Seqable")
    (INVOKEINTERFACE "clojure.lang.Seqable" "seq" 0)
    (ARETURN)
    (ALOAD_0)
    (IFNONNULL 5)
    (ACONST_NULL)
    (ARETURN)
    (ALOAD_0)
    (INSTANCEOF "java.lang.Iterable")
    (IFEQ 16)
    (ALOAD_0)
    (CHECKCAST "java.lang.Iterable")
    (INVOKEINTERFACE "java.lang.Iterable" "iterator" 0)
    (INVOKESTATIC "clojure.lang.IteratorSeq" "create" 1)
    (ARETURN)
    (ALOAD_0)
    (INVOKEVIRTUAL "java.lang.Object" "getClass" 0)
    (INVOKEVIRTUAL "java.lang.Class" "isArray" 0)
    (IFEQ 8)
    (ALOAD_0)
    (INVOKESTATIC "clojure.lang.ArraySeq" "createFromObject" 1)
    (ARETURN)
    (ALOAD_0)
    (INSTANCEOF "java.lang.CharSequence")
    (IFEQ 11)
    (ALOAD_0)
    (CHECKCAST "java.lang.CharSequence")
    (INVOKESTATIC "clojure.lang.StringSeq" "create" 1)
    (ARETURN)
    (ALOAD_0)
    (INSTANCEOF "java.util.Map")
    (IFEQ 16)
    (ALOAD_0)
    (CHECKCAST "java.util.Map")
    (INVOKEINTERFACE "java.util.Map" "entrySet" 0)
    (INVOKESTATIC "clojure.lang.RT" "seq" 1)
    (ARETURN)
    (ALOAD_0)
    (INVOKEVIRTUAL "java.lang.Object" "getClass" 0)
    (ASTORE_1)
    (ALOAD_1)
    (INVOKEVIRTUAL "java.lang.Class" "getSuperclass" 0)
    (ASTORE_2)
    (NEW "java.lang.IllegalArgumentException")
    (DUP)
    (NEW "java.lang.StringBuilder")
    (DUP)
    (INVOKESPECIAL "java.lang.StringBuilder" "<init>" 0)
    (LDC 19)
    (INVOKEVIRTUAL "java.lang.StringBuilder" "append" 1)
    (ALOAD_1)
    (INVOKEVIRTUAL "java.lang.Class" "getName" 0)
    (INVOKEVIRTUAL "java.lang.StringBuilder" "append" 1)
    (INVOKEVIRTUAL "java.lang.StringBuilder" "toString" 0)
    (INVOKESPECIAL "java.lang.IllegalArgumentException" "<init>" 1)
    (ATHROW)))

(defconst *clojure.lang.RT-cons*
  '("cons" (java.lang.Object java.lang.Object)
    (ALOAD_1)
    (IFNONNULL 12)
    (NEW "clojure.lang.PersistentList")
    (DUP)
    (ALOAD_0)
    (INVOKESPECIAL "clojure.lang.PersistentList" "<init>" 1)
    (ARETURN)
    (ALOAD_1)
    (INSTANCEOF "clojure.lang.ISeq")
    (IFEQ 16)
    (NEW "clojure.lang.Cons")
    (DUP)
    (ALOAD_0)
    (ALOAD_1)
    (CHECKCAST "clojure.lang.ISeq")
    (INVOKESPECIAL "clojure.lang.Cons" "<init>" 2)
    (ARETURN)
    (NEW "clojure.lang.Cons")
    (DUP)
    (ALOAD_0)
    (ALOAD_1)
    (INVOKESTATIC "clojure.lang.RT" "seq" 1)
    (INVOKESPECIAL "clojure.lang.Cons" "<init>" 2)
    (ARETURN)))
    
(defconst *clojure.lang.RT-first*
  '("first" ((CLASS "java.lang.Object"))
    (ALOAD_0)
    (INSTANCEOF "clojure.lang.ISeq")
    (IFEQ 13)
    (ALOAD_0)
    (CHECKCAST "clojure.lang.ISeq")
    (INVOKEINTERFACE "clojure.lang.ISeq" "first" 0)
    (ARETURN)
    (ALOAD_0)
    (INVOKESTATIC "clojure.lang.RT" "seq" 1)
    (ASTORE_1)
    (ALOAD_1)
    (IFNONNULL 5)
    (ACONST_NULL)
    (ARETURN)
    (ALOAD_1)
    (INVOKEINTERFACE "clojure.lang.ISeq" "first" 0)
    (ARETURN)))
    
(defconst *clojure.lang.RT-next*
  '("next" ((CLASS "java.lang.Object"))
    (ALOAD_0)
    (INSTANCEOF "clojure.lang.ISeq")
    (IFEQ 13)
    (ALOAD_0)
    (CHECKCAST "clojure.lang.ISeq")
    (INVOKEINTERFACE "clojure.lang.ISeq" "next" 0)
    (ARETURN)
    (ALOAD_0)
    (INVOKESTATIC "clojure.lang.RT" "seq" 1)
    (ASTORE_1)
    (ALOAD_1)
    (IFNONNULL 5)
    (ACONST_NULL)
    (ARETURN)
    (ALOAD_1)
    (INVOKEINTERFACE "clojure.lang.ISeq" "next" 0)
    (ARETURN)))
  
(defconst *clojure.lang.RT-more*    
  '("more" ((CLASS "java.lang.Object"))
    (ALOAD_0)
    (INSTANCEOF "clojure.lang.ISeq")
    (IFEQ 13)
    (ALOAD_0)
    (CHECKCAST "clojure.lang.ISeq")
    (INVOKEINTERFACE "clojure.lang.ISeq" "more" 0)
    (ARETURN)
    (ALOAD_0)
    (INVOKESTATIC "clojure.lang.RT" "seq" 1)
    (ASTORE_1)
    (ALOAD_1)
    (IFNONNULL 7)
    (GETSTATIC "clojure.lang.PersistentList" "EMPTY" NIL)
    (ARETURN)
    (ALOAD_1)
    (INVOKEINTERFACE "clojure.lang.ISeq" "more" 0)
    (ARETURN)))
  
(defconst *clojure.lang.RT*
  (make-class-decl
   ; class name
   "clojure.lang.RT"
   ; superclasses
   '("java.lang.Object")
   ; interfaces
   '()   
   ; fields
   '()
   ; static fields
   '()  
   ; constant pool
   '()
   ; methods
   (list
    *clojure.lang.RT-seq*
    *clojure.lang.RT-seqFrom*
    *clojure.lang.RT-cons*
    *clojure.lang.RT-first*
    *clojure.lang.RT-next*
    *clojure.lang.RT-more*
   )
  '(REF -1)))

(defun |RT|-loaded? (class-table heap)
  (and (|PersistentList|-loaded? class-table heap)
       (|Cons|-loaded? class-table heap)
       (loaded? class-table
                "clojure.lang.RT"
                *clojure.lang.RT*)))

(defthm |RT|-dependencies
  (implies 
   (|RT|-loaded? (class-table s)
                 (heap s))
   (and (|PersistentList|-loaded? (class-table s)
                                  (heap s))
        (|Cons|-loaded? (class-table s)
                        (heap s))))
  :rule-classes :forward-chaining)

(defthm |RT:seq|-method
  (implies 
   (|RT|-loaded? (class-table s)
                 (heap s))
   (equal (lookup-method "seq"
                         "clojure.lang.RT"
                         (class-table s))
          *clojure.lang.RT-seq*)))

(defthm |RT:seqFrom|-method
  (implies 
   (|RT|-loaded? (class-table s)
                 (heap s))
   (equal (lookup-method "seqFrom"
                         "clojure.lang.RT"
                         (class-table s))
          *clojure.lang.RT-seqFrom*)))

(defthm |RT:cons|-method
  (implies 
   (|RT|-loaded? (class-table s)
                 (heap s))
   (equal (lookup-method "cons"
                         "clojure.lang.RT"
                         (class-table s))
          *clojure.lang.RT-cons*)))

(defthm |RT:first|-method
  (implies 
   (|RT|-loaded? (class-table s)
                 (heap s))
   (equal (lookup-method "first"
                         "clojure.lang.RT"
                         (class-table s))
          *clojure.lang.RT-first*)))

(defthm |RT:next|-method
  (implies 
   (|RT|-loaded? (class-table s)
                 (heap s))
   (equal (lookup-method "next"
                         "clojure.lang.RT"
                         (class-table s))
          *clojure.lang.RT-next*)))

(defthm |RT:more|-method
  (implies 
   (|RT|-loaded? (class-table s)
                 (heap s))
   (equal (lookup-method "more"
                         "clojure.lang.RT"
                         (class-table s))
          *clojure.lang.RT-more*)))

(in-theory (disable |RT|-loaded?))

; -----------------------------------------------------------------------------
; sequences

(defun seq-p (ref heap)
  (and 
   (not (nullrefp ref))
   (or (|Cons|-p ref heap)
       (|PersistentList|-p ref heap)
       (|EmptyList|-p ref heap))))

(defun seq-first (ref heap)
  (if (|Cons|-p ref heap)    
    (|Cons:_first|-get ref heap)
    (if (|PersistentList|-p ref heap)
      (|PersistentList:_first|-get ref heap)
      (nullref))))

(defun seq-more (ref heap class-table)
  (if (|EmptyList|-p ref heap)
    ref
    (if (nullrefp ref)
      (|PersistentList:EMPTY|-get heap class-table)
      (if (|Cons|-p ref heap)    
        (if (nullrefp (|Cons:_more|-get ref heap))
          (|PersistentList:EMPTY|-get heap class-table)
          (|Cons:_more|-get ref heap))
        (if (or (= (|PersistentList:_count|-get ref heap) 1)
                (nullrefp (|PersistentList:_rest|-get ref heap)))
          (|PersistentList:EMPTY|-get heap class-table)
          (|PersistentList:_rest|-get ref heap))))))

(defun seq-equal (xs xs-seq heap class-table)
  (if (endp xs)
    ;(equal xs-seq
    ;       (|PersistentList:EMPTY|-get heap class-table))
    (|EmptyList|-p xs-seq heap)
    (let* ((more (seq-more xs-seq heap class-table)))
      (and (not (|EmptyList|-p xs-seq heap))
           (seq-p more heap)            
           (equal (car xs) (seq-first xs-seq heap))
           (seq-equal (cdr xs) more heap class-table)))))

(defthm Empty-is-seq-p
  (implies 
   (|PersistentList|-loaded? class-table heap)
   (seq-p (|PersistentList:EMPTY|-get heap class-table)
          heap))
  :rule-classes :type-prescription
  :hints (("Goal" :in-theory (enable |PersistentList|-loaded?))))

(defthm seq-more-EmptyList-is-EmptyList
  (implies 
   (|PersistentList|-loaded? class-table heap)
   (equal (seq-more (|PersistentList:EMPTY|-get heap class-table)
                    heap 
                    class-table) 
          (|PersistentList:EMPTY|-get heap class-table)))
  :hints (("Goal" :in-theory (enable                                                   
                              |PersistentList|-loaded?))))

(defthm seq-more-EmptyList-is-EmptyList-2
  (implies 
   (|RT|-loaded? class-table heap)
   (equal (seq-more (|PersistentList:EMPTY|-get heap class-table)
                    heap 
                    class-table) 
          (|PersistentList:EMPTY|-get heap class-table)))
  :hints (("Goal" :in-theory (e/d (|RT|-loaded?) 
                                  (seq-more
                                   |PersistentList:EMPTY|-get)))))

(defthm seq-more-is-seq
  (implies 
   (and (|RT|-loaded? class-table heap)
        (seq-p xs-seq heap)        
        (seq-equal xs xs-seq heap class-table))
   (seq-p (seq-more xs-seq heap class-table) heap)))
;  :hints (("Goal" :in-theory (disable
;                              |Cons|-p
;                              |PersistentList|-p
;                              |EmptyList|-p
;                              seq-more
;                             |PersistentList:EMPTY|-get))))                             

(local (in-theory (enable
    ->-execute-ACONST_NULL               
    ->-execute-ALOAD_0
    ->-execute-ALOAD_1
    ->-execute-ARETURN
    ->-execute-ASTORE_1
    ->-execute-CHECKCAST    
    ->-execute-DUP
    ->-execute-GETSTATIC
    ->-execute-GETFIELD
    ->-execute-ICONST_1
    ->-execute-IFEQ
    ->-execute-IFNONNULL
    ->-execute-INSTANCEOF
    ->-execute-INVOKEINTERFACE
    ->-execute-INVOKESTATIC
    ->-execute-INVOKEVIRTUAL
    ->-execute-LDC
    ->-execute-NEW)))

(defun |RT:seqFrom|-poised (s)
  (equal (next-inst s) 
         '(invokestatic "clojure.lang.RT" 
                        "seqFrom" 
                        1)))

(defun |RT:seqFrom|-param1 (s)
  (top (stack (top-frame s))))

(defthm seqFrom-nullref-is-nullref
  (implies 
   (and (|RT|-loaded? (class-table s)
                      (heap s))
        (|RT:seqFrom|-poised s)
        (nullrefp (|RT:seqFrom|-param1 s)))
   (-> s
       (modify s
               :pc (+ 3 (pc (top-frame s)))
               :stack (push (nullref)
                            (pop (stack (top-frame s))))))))

(defun |RT:seq|-poised (s)
  (equal (next-inst s) 
         '(invokestatic "clojure.lang.RT" 
                        "seq" 
                        1)))

(defun |RT:seq|-param1 (s)
  (top (stack (top-frame s))))

; Domain functions are predicates that check that the inputs 
; match the portion of the method domain that we are interested
; in reasoing about; not the full domain of the method. 
(defun |RT:seq|-domain (s)
  (let* ((ref (|RT:seq|-param1 s)))
    (or (nullrefp ref)
        (seq-p ref (heap s)))))
        
(defthm |RT:seq|-null-input
  (let* ((coll (top (stack (top-frame s)))))
    (implies 
     (and (|RT|-loaded? (class-table s)
                        (heap s))
          (|RT:seq|-poised s)
          (nullrefp coll))
     (-> s
         (modify s
                 :pc (+ 3 (pc (top-frame s)))
                 :stack (push (nullref) 
                              (pop (stack (top-frame s)))))))))    

(defthm |RT:seq|-sequence-input
  (let* ((coll (top (stack (top-frame s)))))
    (implies 
     (and (|RT|-loaded? (class-table s)
                        (heap s))
          (|RT:seq|-poised s)
          (seq-p coll (heap s)))
   (-> s
       (modify s
               :pc (+ 3 (pc (top-frame s)))
               :stack 
               (push (if (|EmptyList|-p coll (heap s))
                       (nullref)
                       coll)
                     (pop (stack (top-frame s)))))))))#|ACL2s-ToDo-Line|#


; -----------------------------------------------------------------------------
; first

(defun |RT:first|-poised (s)
  (equal (next-inst s) 
         '(invokestatic "clojure.lang.RT" 
                        "first" 
                        1)))

(defun |RT:first|-param1 (s)
  (top (stack (top-frame s))))

(defun |RT:first|-domain (s)
  (let* ((ref (|RT:first|-param1 s)))
    (seq-p ref (heap s))))

(defthm |EmptyList:first|-method
  (implies 
   (and (|EmptyList|-loaded? (class-table s))
        (equal (car classes) "clojure.lang.PersistentList$EmptyList"))
   (equal (lookup-method-in-superclasses "first"
                                         classes
                                         (class-table s))
          *clojure.lang.PersistentList$EmptyList-first*))
  :hints (("Goal" :in-theory (enable |EmptyList|-loaded?))))

(defthm |RT:first|=first
  (let* ((x (top (stack (top-frame s)))))
    (implies 
     (and (|RT|-loaded? (class-table s)
                        (heap s))
          (|RT:first|-poised s)
          (seq-p x (heap s)))
     (-> s
         (modify s
                 :pc (+ 3 (pc (top-frame s)))                 
                 :stack (push (seq-first x (heap s))
                              (pop (stack (top-frame s)))))))))

; -----------------------------------------------------------------------------
; more

(defun |RT:more|-poised (s)
  (equal (next-inst s) 
         '(invokestatic "clojure.lang.RT" 
                        "more" 
                        1)))

(defun |RT:more|-param1 (s)
  (top (stack (top-frame s))))

(defun |RT:more|-domain (s)
  (let* ((ref (|RT:more|-param1 s)))
    (seq-p ref (heap s))))

(defthm |PersistentList:more2|-method
  (implies 
   (|PersistentList|-loaded? (class-table s)
                             (heap s))
   (equal (lookup-method "more"
                         "clojure.lang.PersistentList"
                         (class-table s))
          *clojure.lang.ASeq-more*))
   :hints (("Goal" :in-theory (enable |PersistentList|-loaded?))))

(local (in-theory (enable
                   execute-ASTORE_X
                   ->-execute-ASTORE_1
                   ->-execute-IF_ICMPNE)))

(defthm |EmptyList:more|-method
  (implies 
   (and (|EmptyList|-loaded? (class-table s))
        (equal (car classes) "clojure.lang.PersistentList$EmptyList"))
   (equal (lookup-method-in-superclasses "more"
                                         classes
                                         (class-table s))
          *clojure.lang.PersistentList$EmptyList-more*))
  :hints (("Goal" :in-theory (enable |EmptyList|-loaded?))))

(defthm |RT:more|=seq-more
  (implies 
   (and (|RT|-loaded? (class-table s)
                      (heap s))
        (|RT:more|-poised s) 
        (seq-p (|RT:more|-param1 s) (heap s)))
   (-> s
       (modify s
               :pc (+ 3 (pc (top-frame s)))
               :stack (push (seq-more (|RT:more|-param1 s)
                                      (heap s)
                                      (class-table s))
                            (pop (stack (top-frame s))))))))

(defthm |RT:more(null)|=null
  (implies 
   (and (|RT|-loaded? (class-table s)
                      (heap s))
        (|RT:more|-poised s)
        (nullrefp (|RT:more|-param1 s)))
   (-> s
       (modify s
               :pc (+ 3 (pc (top-frame s)))
               :stack (push (seq-more (|RT:more|-param1 s)
                                      (heap s)
                                      (class-table s))
                            (pop (stack (top-frame s))))))))

; -----------------------------------------------------------------------------
; next

(defun |RT:next|-poised (s)
  (equal (next-inst s) 
         '(invokestatic "clojure.lang.RT" 
                        "next" 
                        1)))

(defun |RT:next|-param1 (s)
  (top (stack (top-frame s))))

(defun |RT:next|-domain (s)
  (let* ((ref (|RT:next|-param1 s)))
    (seq-p ref (heap s))))

(defun seq-next (ref heap)
  (if (|Cons|-p ref heap)        
      (|Cons:_more|-get ref heap)    
      (|PersistentList:_rest|-get ref heap)))

;(defthm |PersistentList:more2|-method
;  (implies 
;   (|PersistentList|-loaded? s)
;   (equal (lookup-method "more"
;                         "clojure.lang.PersistentList"
;                         (class-table s))
;          *clojure.lang.ASeq-more*))
;   :hints (("Goal" :in-theory (enable |PersistentList|-loaded?))))

;(local (in-theory (enable
;                   execute-ASTORE_X
;                   ->-execute-ASTORE_1
;                   ->-execute-IF_ICMPNE)))

#|
(defthm |RT:next|=seq-next
  (implies 
   (and (|RT|-loaded? s)
        (|RT:next|-poised s)
        (|RT:next|-domain s)
        (|Cons|-p (|RT:next|-param1 s)
                  (heap s)))
   (-> s
       (modify s
               :pc (+ 3 (pc (top-frame s)))
               :stack (push (seq-next (|RT:next|-param1 s)
                                      (heap s))
                            (pop (stack (top-frame s))))))))
|#

; -----------------------------------------------------------------------------
; cons

(defun |RT:cons|-poised (s)
  (equal (next-inst s) 
         '(invokestatic "clojure.lang.RT" "cons" 2)))

(local (in-theory (enable ->-execute-ALOAD_2
                          ->-execute-ICONST_M1
                          ->-execute-INVOKESPECIAL
                          ->-execute-PUTFIELD
                          ->-execute-RETURN)))

(defthm EmptyList-instanceof-ISeq
  (let* ((ref (top (stack (top-frame s)))))
  (implies 
   (and (|EmptyList|-loaded? (class-table s))
        (|EmptyList|-p ref (heap s))
        (equal (next-inst s)
               '(INSTANCEOF "clojure.lang.ISeq")))
   (-> s
       (modify s
               :pc (+ 3 (pc (top-frame s)))
               :stack (push 1
                            (pop (stack (top-frame s)))))))))

(defthm |Cons:init|-initializes-2
  ;(let* ((cons-new (top (pop (stack (top-frame s))))))    
    (implies 
     (and (|Cons|-loaded? (class-table s)
                          (heap s))
          (|Cons:<init>|-poised s))
        ;  (|Cons|-p cons-new (heap s)))
        ;(equal (deref (top (pop (stack (top-frame s))))
        ;              (heap s))
        ;       (|Cons|-new)))
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

;(in-theory (disable |Cons-NEW|))

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

(defthmd |RT:cons|-Cons-adds-new-Cons
  (let* ((x (top (pop (stack (top-frame s)))))
         (coll (top (stack (top-frame s)))))
  (implies 
   (and (|RT|-loaded? (class-table s) (heap s)) 
        (|Cons|-loaded? (class-table s) (heap s))
        (|ASeq|-loaded? (class-table s))
        (|Obj|-loaded? (class-table s))
        (|Object|-loaded? (class-table s))
        (|RT:cons|-poised s)
        (|Cons|-p coll (heap s)))
   (-> s
       (modify s
               :pc (+ 3 (pc (top-frame s)))
               :stack (push (list 'REF (len (heap s)))
                            (popn 2 (stack (top-frame s))))
               :heap (bind (len (heap s))
                           (|Cons|-init (|Cons|-new) x coll)
                           (heap s)))))))

(defthmd |RT:cons|-PersistentList-adds-new-Cons
  (let* ((x (top (pop (stack (top-frame s)))))
         (coll (top (stack (top-frame s)))))
  (implies 
   (and (|RT|-loaded? (class-table s) (heap s)) 
        (|Cons|-loaded? (class-table s) (heap s))
        (|ASeq|-loaded? (class-table s))
        (|Obj|-loaded? (class-table s))
        (|Object|-loaded? (class-table s))
        (|RT:cons|-poised s)
        (|PersistentList|-p coll (heap s)))
   (-> s
       (modify s
               :pc (+ 3 (pc (top-frame s)))
               :stack (push (list 'REF (len (heap s)))
                            (popn 2 (stack (top-frame s))))
               :heap (bind (len (heap s))
                           (|Cons|-init (|Cons|-new) x coll)
                           (heap s)))))))

(defthmd |RT:cons|-EmptyList-adds-new-Cons
  (let* ((x (top (pop (stack (top-frame s)))))
         (coll (top (stack (top-frame s)))))
  (implies 
   (and (|RT|-loaded? (class-table s) (heap s))
        (|EmptyList|-loaded? (class-table s))
        (|Cons|-loaded? (class-table s) (heap s))
        (|ASeq|-loaded? (class-table s))
        (|Obj|-loaded? (class-table s))
        (|Object|-loaded? (class-table s))
        (|RT:cons|-poised s)
        (|EmptyList|-p coll (heap s)))
   (-> s
       (modify s
               :pc (+ 3 (pc (top-frame s)))
               :stack (push (list 'REF (len (heap s)))
                            (popn 2 (stack (top-frame s))))
               :heap (bind (len (heap s))
                           (|Cons|-init (|Cons|-new) x coll)
                           (heap s)))))))

(local (in-theory (disable
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

(defthm |RT:cons|-adds-new-Cons
  (let* ((x (top (pop (stack (top-frame s)))))
         (coll (top (stack (top-frame s)))))
  (implies 
   (and (|RT|-loaded? (class-table s) (heap s)) 
        (|RT:cons|-poised s)
        (seq-p coll (heap s)))
   (-> s
       (modify s
               :pc (+ 3 (pc (top-frame s)))
               :stack (push (list 'REF (len (heap s)))
                            (popn 2 (stack (top-frame s))))
               :heap (bind (len (heap s))
                           (|Cons|-init (|Cons|-new) x coll)
                           (heap s))))))
  :hints (("Goal" :use (|RT:cons|-Cons-adds-new-Cons
                        |RT:cons|-PersistentList-adds-new-Cons
                        |RT:cons|-EmptyList-adds-new-Cons))))