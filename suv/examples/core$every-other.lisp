#|$ACL2s-Preamble$;
(include-book "../../mc/mc")
(include-book "../../mc/utilities")
(include-book "../big-step")

(include-book "../clojure/lang/Var")

(include-book "../clojure/core$first")
(include-book "../clojure/core$rest")
(include-book "../clojure/core$cons")
(include-book "../clojure/core$empty")

(acl2::begin-book t :ttags :all);$ACL2s-Preamble$|#


(in-package "MC")

(defconst *examples$every_other-invoke*
  '("invoke" ((CLASS "java.lang.Object"))
    (GETSTATIC "examples.core$every_other" "const__0" NIL)
    (INVOKEVIRTUAL "clojure.lang.Var" "getRawRoot" 0)
    (CHECKCAST "clojure.lang.IFn")
    (ALOAD_1)
    (INVOKEINTERFACE "clojure.lang.IFn" "invoke" 1)
    (DUP)
    (IFNULL 13)
    (GETSTATIC "java.lang.Boolean" "FALSE" NIL)
    (IF_ACMPEQ 8)
    (ACONST_NULL)
    (GOTO 78)
    (POP)
    (GETSTATIC "examples.core$every_other" "const__1" NIL)
    (INVOKEVIRTUAL "clojure.lang.Var" "getRawRoot" 0)
    (CHECKCAST "clojure.lang.IFn")
    (GETSTATIC "examples.core$every_other" "const__2" NIL)
    (INVOKEVIRTUAL "clojure.lang.Var" "getRawRoot" 0)
    (CHECKCAST "clojure.lang.IFn")
    (ALOAD_1)
    (INVOKEINTERFACE "clojure.lang.IFn" "invoke" 1)
    (GETSTATIC "examples.core$every_other" "const__3" NIL)
    (INVOKEVIRTUAL "clojure.lang.Var" "getRawRoot" 0)
    (CHECKCAST "clojure.lang.IFn")
    (GETSTATIC "examples.core$every_other" "const__4" NIL)
    (INVOKEVIRTUAL "clojure.lang.Var" "getRawRoot" 0)
    (CHECKCAST "clojure.lang.IFn")
    (GETSTATIC "examples.core$every_other" "const__4" NIL)
    (INVOKEVIRTUAL "clojure.lang.Var" "getRawRoot" 0)
    (CHECKCAST "clojure.lang.IFn")
    (ALOAD_1)
    (ACONST_NULL)
    (ASTORE_1)
    (INVOKEINTERFACE "clojure.lang.IFn" "invoke" 1)
    (INVOKEINTERFACE "clojure.lang.IFn" "invoke" 1)
    (INVOKEINTERFACE "clojure.lang.IFn" "invoke" 1)
    (INVOKEINTERFACE "clojure.lang.IFn" "invoke" 2)
    (ARETURN)))
  
(defconst *examples$every_other*
  (make-class-decl
   "examples.core$every_other"  
   '("clojure.lang.AFunction")
   '()
   '()
   '("const__0"
     "const__1"
     "const__2"
     "const__3"
     "const__4")
   '((STRING "clojure.core")
     (STRING "empty?")
     (STRING "cons")
     (STRING "first")
     (STRING "examples.core")
     (STRING "every-other")
     (STRING "rest"))

   (list
    *examples$every_other-invoke*
   )
   '(REF -1)))

(defun |every_other:empty?|-get (heap class-table)
  (static-field-value 
   "examples.core$every_other"
   "const__0"
   heap
   class-table))

(defun |every_other:cons|-get (heap class-table)
  (static-field-value 
   "examples.core$every_other"
   "const__1"
   heap
   class-table))

(defun |every_other:first|-get (heap class-table)
  (static-field-value 
   "examples.core$every_other"
   "const__2"
   heap
   class-table))

(defun |every_other:every_other|-get (heap class-table)
  (static-field-value 
   "examples.core$every_other"
   "const__3"
   heap
   class-table))

(defun |every_other:rest|-get (heap class-table)
  (static-field-value 
   "examples.core$every_other"
   "const__4"
   heap
   class-table))

(defun |every_other|-p (ref heap)
  (and (not (nullrefp ref))
       (j-type-p "examples.core$every_other"
                 ref 
                 heap)))

(defun |every_other|-loaded? (class-table heap)
  (let* ((first-var (|every_other:first|-get heap
                                             class-table))
         (rest-var (|every_other:rest|-get heap
                                           class-table))
         (cons-var (|every_other:cons|-get heap
                                           class-table))         
         (empty-var (|every_other:empty?|-get heap
                                              class-table))
         (every-var (|every_other:every_other|-get heap
                                                   class-table)))
  (and   
   (|Var|-loaded? class-table)
   (|Boolean|-loaded? class-table heap)
   (|core$first|-loaded? class-table heap)
   (|core$cons|-loaded? class-table heap)
   (|core$rest|-loaded? class-table heap)
   (|core$empty|-loaded? class-table heap)
   (loaded? class-table
            "examples.core$every_other" 
            *examples$every_other*)
   (|Var|-p first-var heap)
   (|Var|-p rest-var heap)
   (|Var|-p cons-var heap)
   (|Var|-p empty-var heap)
   (|Var|-p every-var heap)
   (|core$first|-p (|Var:root|-get first-var heap)
                   heap)
   (|core$rest|-p (|Var:root|-get rest-var heap)
                   heap)
   (|core$cons|-p (|Var:root|-get cons-var heap)
                  heap)
   (|core$empty|-p (|Var:root|-get empty-var heap)
                   heap)
   (|every_other|-p (|Var:root|-get every-var heap)
                    heap))))

(defthm |every_other|-dep 
  (implies
   (|every_other|-loaded? (class-table s) (heap s))
   (and (|Var|-loaded? (class-table s))
        (|Boolean|-loaded? (class-table s) (heap s))
        (|core$first|-loaded? (class-table s) (heap s))
        (|core$cons|-loaded? (class-table s) (heap s))        
        (|core$rest|-loaded? (class-table s) (heap s))
        (|core$empty|-loaded? (class-table s) (heap s))))
  :rule-classes :forward-chaining)

(defthm |every_other:Vars|-dep 
  (implies
   (|every_other|-loaded? (class-table s) (heap s))
   (and (|Var|-p (|every_other:first|-get (heap s) (class-table s))
                 (heap s))
        (|Var|-p (|every_other:cons|-get (heap s) (class-table s))
                 (heap s))
        (|Var|-p (|every_other:rest|-get (heap s) (class-table s))
                 (heap s))
        (|Var|-p (|every_other:empty?|-get (heap s) (class-table s))
                 (heap s))
        (|Var|-p (|every_other:every_other|-get (heap s) (class-table s))
                 (heap s))))
  :rule-classes :forward-chaining)

(defthm |every_other:first|-dep 
  (implies
   (|every_other|-loaded? (class-table s) (heap s))
   (|core$first|-p (|Var:root|-get (|every_other:first|-get (heap s)
                                                            (class-table s))
                                   (heap s))
                   (heap s)))
  :rule-classes :forward-chaining)

(defthm |every_other:rest|-dep 
  (implies
   (|every_other|-loaded? (class-table s) (heap s))
   (|core$rest|-p (|Var:root|-get (|every_other:rest|-get (heap s)
                                                          (class-table s))
                                  (heap s))
                  (heap s)))
  :rule-classes :forward-chaining)

(defthm |every_other:cons|-dep 
  (implies
   (|every_other|-loaded? (class-table s) (heap s))
   (|core$cons|-p (|Var:root|-get (|every_other:cons|-get (heap s)
                                                          (class-table s))
                                  (heap s))
                  (heap s)))
  :rule-classes :forward-chaining)

(defthm |every_other:empty?|-dep 
  (implies
   (|every_other|-loaded? (class-table s) (heap s))
   (|core$empty|-p (|Var:root|-get (|every_other:empty?|-get (heap s)
                                                             (class-table s))
                                   (heap s))
                   (heap s)))
  :rule-classes (:forward-chaining :rewrite))

(defthm |every_other:every_other|-dep 
  (implies
   (|every_other|-loaded? (class-table s) (heap s))
   (|every_other|-p (|Var:root|-get (|every_other:every_other|-get (heap s)
                                                                   (class-table s))
                                    (heap s))
                    (heap s)))
  :rule-classes :forward-chaining)

(defthm |every_other|-method
  (implies 
   (|every_other|-loaded? (class-table s) (heap s))
   (equal (lookup-method "invoke"
                         "examples.core$every_other"
                         (class-table s))
          *examples$every_other-invoke*)))

(in-theory (disable |core$empty|-loaded?))

(defun every-other (xs)
  (if (endp xs)
    nil
    (cons (car xs)
          (every-other (cdr (cdr xs))))))

(defun alloc-list (xs heap)
  (if (endp xs)
    heap
    ;(let* ((x (car xs)))
    (let* ((x (seq-first (car xs) heap)))
      (if (endp (cdr xs))
        (bind (len heap)
              (|PersistentList|-init (|PersistentList|-new) x)
              heap)
        (let* ((new-heap (alloc-list (cdr xs) heap))
               (coll (list 'REF (len heap))))
          (bind (len new-heap)
                (|Cons|-init (|Cons|-new) x coll)
                new-heap))))))

(defun |every_other:invoke|-poised (s)
  (and 
   (equal (next-inst s) 
          '(INVOKEINTERFACE "clojure.lang.IFn" 
                            "invoke" 
                            1))
   (|every_other|-p (top (pop (stack (top-frame s)))) 
                    (heap s))))

(local (in-theory (enable ->-execute-ACONST_NULL
                          ->-execute-ALOAD_1
                          ->-execute-ARETURN
                          ->-execute-ASTORE_1                          
                          ->-execute-CHECKCAST
                          ->-execute-DUP
                          ->-execute-GETSTATIC
                          ->-execute-GOTO
                          ->-execute-IF_ACMPEQ
                          ->-execute-IFNULL
                          ->-execute-POP)))
    
(defthm every_other-core$empty=core$empty
  (implies 
   (and (|every_other|-loaded? (class-table s) (heap s))
        (equal (next-inst s) 
               '(GETSTATIC "examples.core$every_other" 
                           "const__0" 
                           NIL)))
   (-> s
       (modify s
               :pc (+ 3 (pc (top-frame s)))
               :stack (push (|every_other:empty?|-get (heap s)
                                                      (class-table s)) 
                            (stack (top-frame s)))))))

(defthm every_other-core$cons=core$cons
  (implies 
   (and (|every_other|-loaded? (class-table s) (heap s))
        (equal (next-inst s) 
               '(GETSTATIC "examples.core$every_other" 
                           "const__1" 
                           NIL)))
   (-> s
       (modify s
               :pc (+ 3 (pc (top-frame s)))
               :stack (push (|every_other:cons|-get (heap s)
                                                    (class-table s)) 
                            (stack (top-frame s)))))))

(defthm every_other-core$first=core$first
  (implies 
   (and (|every_other|-loaded? (class-table s) (heap s))
        (equal (next-inst s) 
               '(GETSTATIC "examples.core$every_other" 
                           "const__2" 
                           NIL)))
   (-> s
       (modify s
               :pc (+ 3 (pc (top-frame s)))
               :stack (push (|every_other:first|-get (heap s)
                                                     (class-table s)) 
                            (stack (top-frame s)))))))

(defthm every_other-every_other=every_other
  (implies 
   (and (|every_other|-loaded? (class-table s) (heap s))
        (equal (next-inst s) 
               '(GETSTATIC "examples.core$every_other" 
                           "const__3" 
                           NIL)))
   (-> s
       (modify s
               :pc (+ 3 (pc (top-frame s)))
               :stack (push (|every_other:every_other|-get (heap s)
                                                           (class-table s)) 
                            (stack (top-frame s)))))))

(defthm every_other-core$rest=core$rest
  (implies 
   (and (|every_other|-loaded? (class-table s) (heap s))
        (equal (next-inst s) 
               '(GETSTATIC "examples.core$every_other" 
                           "const__4" 
                           NIL)))
   (-> s
       (modify s
               :pc (+ 3 (pc (top-frame s)))
               :stack (push (|every_other:rest|-get (heap s)
                                                    (class-table s)) 
                            (stack (top-frame s)))))))

(local 
 (in-theory (disable |every_other|-loaded?
                     |Var|-p
                     |Var:root|-get    
                     |EmptyList|-p
                     |Boolean|-loaded?
                     |core$empty|-p
                     
                     |Boolean:FALSE|-get
                     |Boolean:TRUE|-get
                     
                     |core$empty:core$not|-get
                     |core$empty:core$seq|-get                              
                     |every_other:first|-get
                     |every_other:rest|-get
                     |every_other:empty?|-get
                     |every_other:cons|-get
                     |every_other:every_other|-get)))

(defun seq-listp (xs heap class-table)
  (if (endp xs)
    (equal xs nil)
    ;(equal (car xs) 
    ;       (|PersistentList:EMPTY|-get heap class-table))
    (and (seq-p (car xs) heap)
         (not (|EmptyList|-p (car xs) heap))
         (if (endp (cdr xs))
           (equal (seq-more (car xs) heap class-table)
                  (|PersistentList:EMPTY|-get heap class-table))
           (equal (seq-more (car xs) heap class-table)
                  (cadr xs)))
         (seq-listp (cdr xs) heap class-table))))

(defun every_other-base-state (s)
  (and (|every_other|-loaded? (class-table s) (heap s))
       (equal (pc (top-frame s)) 0)
       (equal (stack (top-frame s)) nil)
       (equal (program (top-frame s)) (method-program *examples$every_other-invoke*))  
       (|every_other|-p (top (locals (top-frame s))) (heap s))))

(defconst *dummy* 
  (make-state 
   (list (make-frame 0 
                     (list (list 'REF 30)) 
                     #\S 
                     #\P 
                     #\L)) 
   #\H 
   #\T))  

(defun every_other-start-state (xs-ref s)
  (modify s
          :locals (update-nth 1 
                              xs-ref
                              (locals (top-frame s)))))

(defun every_other-states (xs s)
  (if (endp xs)
    (list (every_other-start-state (nullref) s))
    (cons (every_other-start-state (car xs) s)
          (every_other-states (cdr (cdr xs)) s))))
    ;(modify s
    ;        :locals (update-nth 1 
    ;                            (nullref)
    ;                            (locals (top-frame s))))
    ;(cons (modify s
    ;              :locals (update-nth 1 
    ;                                  (car xs)
    ;                                  (locals (top-frame s))))
    ;      (every_other-states (cdr (cdr xs)) s))))

(defun every_other-end-state (eo-ref heap s)
  (modify s
          :pc 104
          :stack (push eo-ref
                       (stack (top-frame s)))
          :heap heap))
  
(defun every_other-results (xs s)
  (if (endp xs)
    (list (every_other-end-state (nullref) (heap s) s))
    (let* ((eo (every-other xs)))
      (cons (every_other-end-state (push (list 'REF (+ (len (heap s))
                                                       (len eo)
                                                       -1))
                                         (stack (top-frame s))) 
                                   (alloc-list eo (heap s)) 
                                   s)
            (every_other-results (cdr xs) s)))))
            ;(every_other-results (cdr (cdr xs)) s)))))
    
    
    ;(modify s
    ;        :pc 104 ; only 1 return statement in bytecode (first return goto's second) 
    ;        :stack (push (nullref)
    ;                     (stack (top-frame s))))
    ;(let* ((eo (every-other xs)))
    ;  (cons (modify s
    ;                :pc 104 ; only 1 return statement in bytecode (first return goto's second) 
    ;                :stack (push (list 'REF (+ (len (heap s))
    ;                                           (len eo)
    ;                                           -1))
    ;                             (stack (top-frame s)))
    ;                :heap (alloc-list eo (heap s)))
    ;        (every_other-results (cdr xs) s)))))
          
(defun ->-equals (states1 states2)
  (if (endp states1)
    (endp states2)
    (and (-> (car states1)
             (car states2))
         (->-equals (cdr states1) (cdr states2)))))

(local (in-theory (disable ->-execute-INVOKEINTERFACE
                           execute-INVOKEINTERFACE)))

(local (include-book "std/lists/update-nth" :dir :system))

(defthm every_other=every-other
  (implies 
   (every_other-base-state s)
   ;(and (every_other-base-state s)
   ;     (|core$empty|-loaded? (class-table s) (heap s)))
   
   (-> (every_other-start-state '(REF -1) s)
       (every_other-end-state '(REF -1) (heap s) s)))
   
   ;(-> (every_other-start-state (nullref) s)
   ;    (every_other-end-state (nullref) (heap s) s)))
   
  ;(-> (modify s
   ;            :locals (update-nth 1 
   ;                                (nullref)
   ;                                (locals (top-frame s))))
   ;    (modify s
   ;            :pc 104 ; only 1 return statement in bytecode (first return goto's second) 
   ;            :stack (push (nullref)
   ;                         (stack (top-frame s))))))
  :hints (("Goal" :use (:instance TRUE-and-FALSE-Boolean
                        (class-table (class-table s))
                        (heap (heap s))))))

(defthm every_other=every-other-2
  (implies 
   (and (every_other-base-state s)
        (seq-listp xs (heap s) (class-table s))
        (endp xs))
   (->-equals (every_other-states xs s)
              (every_other-results xs s)))
  :hints (("Goal" :in-theory (disable every_other-start-state
                                      every_other-end-state))))

; Assuming state s starts at every_other-base-state, then this
; function updates the state to the instruction that will 
; recursively call the every_other method. 
(defun every_other-recursive-state (ref s)
  (modify 
   s
   :pc 94
   :locals (update-nth 1
                       (nullref)
                       (locals (top-frame s)))
   :stack 
   (push 
; 84-89 (rest (rest xs))
    (seq-more (seq-more ref (heap s) (class-table s))
              (heap s)
              (class-table s))
    (push (|Var:root|-get 
           (|every_other:every_other|-get (heap s)
                                          (class-table s))
           (heap s))
          (push (seq-first ref (heap s))
                (push (|Var:root|-get 
                       (|every_other:cons|-get (heap s)
                                               (class-table s))
                       (heap s))
                (stack (top-frame s))))))))

(local (in-theory (enable ;->-execute-INVOKEINTERFACE
                           execute-INVOKEINTERFACE)))

(local 
 (in-theory (disable |core$first|-p
                     |core$rest|-p
                     |core$cons|-p
                     seq-more
                     seq-first
                     ;|every_other|-p
                     )))

(defthm every_other-start->every-other-recursive
  (implies 
   (and (every_other-base-state s)
        (seq-p xs (heap s))
        (seq-p (seq-more xs (heap s) (class-table s)) 
               (heap s))
        (seq-p (seq-more (seq-more xs (heap s) (class-table s))
                         (heap s)
                         (class-table s))
               (heap s))        
        (not (|EmptyList|-p xs (heap s))))
        ;(seq-listp xs (heap s) (class-table s))
        ;(not (endp xs)))   
   (-> (every_other-start-state xs s)
       (every_other-recursive-state xs s))))

(defun every_other-results-2 (xs s)
  (if (or (endp xs)
          (|EmptyList|-p (car xs) (heap s)))
    (list (every_other-end-state (nullref) (heap s) s))
    (let* ((eo (every-other xs)))
      (cons (every_other-end-state (list 'REF (+ (len (heap s))
                                                 (len eo)
                                                 -1)) 
                                   (alloc-list eo (heap s)) 
                                   s)
            (every_other-results-2 (cdr xs) s)))))

(defthm every_other=every-other-4
  (implies 
   (and (every_other-base-state s)
        (seq-listp xs (heap s) (class-table s))
        (endp xs))
   (->-equals (every_other-states xs s)
              (every_other-results-2 xs s)))  
  :hints (("Goal" :use (:instance TRUE-and-FALSE-Boolean
                        (class-table (class-table s))
                        (heap (heap s))))))

(defthm |PersistentList:EMPTY|-is-EmptyList
  (implies 
   (|PersistentList|-loaded? class-table heap)
   (|EmptyList|-p (|PersistentList:EMPTY|-get heap class-table)
                  heap))
  :rule-classes :type-prescription
  :hints (("Goal" :in-theory (enable |PersistentList|-loaded?))))

(defthm |PersistentList:EMPTY|-is-EmptyList-2
  (implies 
   (|PersistentList|-loaded? class-table heap)
   (not (equal (|PersistentList:EMPTY|-get heap class-table)
               '(REF -1))))
  :hints (("Goal" :in-theory (enable |EmptyList|-p
                                     |PersistentList|-loaded?))))

(defthm seq-more-is-seq-3
  (implies (and (|Cons|-loaded? class-table heap )
                (|PersistentList|-loaded? class-table heap )
                (seq-listp xs heap class-table)
                (not (endp xs)))
                ;(not (endp (cdr xs))))
           (seq-p (seq-more (car xs) heap class-table) heap))
  :hints (("Goal" :in-theory                   
                  ;(e/d (nullrefp
                  ;      |EmptyList|-p)
                       (disable |PersistentList:EMPTY|-get))))

(defthm seq-more-is-seq-4
  (implies (and (|Cons|-loaded? class-table heap )
                (|PersistentList|-loaded? class-table heap )
                (seq-listp xs heap class-table)
                (not (endp xs)))
                ;(not (endp (cdr xs))))
           (seq-p (seq-more (seq-more (car xs) 
                                      heap 
                                      class-table)
                            heap
                            class-table)
                  heap))
  :hints (("Goal" :in-theory                   
                  ;(e/d (nullrefp
                  ;      |EmptyList|-p)
                       (disable |PersistentList:EMPTY|-get))))

(local 
 (in-theory 
  (disable ->-EXECUTE-ACONST_NULL
           ->-EXECUTE-ALOAD_1
           ->-EXECUTE-ARETURN
           ->-EXECUTE-ASTORE_1
           ->-EXECUTE-CHECKCAST
           ->-EXECUTE-DUP
           ->-EXECUTE-GETSTATIC
           ->-EXECUTE-GOTO
           ->-EXECUTE-IFNULL
           ->-EXECUTE-IF_ACMPEQ
           ->-EXECUTE-POP
           seq-p)))

(set-gag-mode nil)
(accumulated-persistence t)

(defthm seq-more-more
  (implies 
   (and (|PersistentList|-loaded? class-table heap)
        (equal x (|PersistentList:EMPTY|-get heap class-table)))
   (seq-p (seq-more x heap class-table) heap))
  :hints (("Goal" :in-theory (e/d (seq-p seq-more)
                                  (|EmptyList|-p
                                   |Cons|-p
                                   |PersistentList|-p
                                   |PersistentList:EMPTY|-get)))))

(defthm every_other=every-other-Empty
  (let* ((xs-seq (top (stack (top-frame s)))))
    (implies 
     (and (|every_other|-loaded? (class-table s) (heap s))
          (|every_other:invoke|-poised s)
          (|EmptyList|-p xs-seq (heap s)))
     (-> s
         (modify s
                 :pc (+ 5 (pc (top-frame s)))
                 :stack (push (nullref)                               
                              (popn 2 (stack (top-frame s))))))))
  :hints (("Goal" 
           :in-theory (enable ->-EXECUTE-ACONST_NULL
                              ->-EXECUTE-ALOAD_1
                              ->-EXECUTE-ARETURN
                              ->-EXECUTE-ASTORE_1
                              ->-EXECUTE-CHECKCAST
                              ->-EXECUTE-DUP
                              ->-EXECUTE-GETSTATIC
                              ->-EXECUTE-GOTO
                              ->-EXECUTE-IFNULL
                              ->-EXECUTE-IF_ACMPEQ
                              ->-EXECUTE-POP)
           :use (:instance TRUE-and-FALSE-Boolean
                        (class-table (class-table s))
                        (heap (heap s))))))

(defthm every_other->-base-case-2
  (implies 
   (and (every_other-base-state s)
        (seq-p xs (heap s))
        (equal (seq-more xs (heap s) (class-table s)) 
               (|PersistentList:EMPTY|-get (heap s) (class-table s)))
        (not (|EmptyList|-p xs (heap s))))
   (-> (every_other-start-state xs s)
       (every_other-end-state 
        (list 'REF (len (heap s)))                        
        ;(alloc-list eo (heap s))
        (bind (len (heap s))
              (|PersistentList|-init (|PersistentList|-new) 
                                     (seq-first xs
                                                (heap s)))
              (heap s))        
        s)))
  :hints (("Goal" :in-theory (e/d (->-EXECUTE-ACONST_NULL
                                   ->-EXECUTE-ALOAD_1
                                   ->-EXECUTE-ARETURN
                                   ->-EXECUTE-ASTORE_1
                                   ->-EXECUTE-CHECKCAST
                                   ->-EXECUTE-DUP
                                   ->-EXECUTE-GETSTATIC
                                   ->-EXECUTE-GOTO
                                   ->-EXECUTE-IFNULL
                                   ->-EXECUTE-IF_ACMPEQ
                                   ->-EXECUTE-POP)
                                  (|PersistentList:EMPTY|-get
                                   lookup-method
                                   class-name-of-ref
                                   |every_other|-p)))
          ("Subgoal 2" :use every_other=every-other-Empty)))

(defthm |every_other|-classname
  (implies 
   (and (|every_other|-loaded? (class-table s) (heap s))
        (|every_other|-p v (heap s)))
   (equal (class-name-of-ref v (heap s))
          "examples.core$every_other")))

 

(defthm every_other=every-other-5
  (implies 
   (and (every_other-base-state s)
        (seq-p xs (heap s)))
   (every_other-base-state (step (every_other-recursive-state xs s))))
;    :hints (("Goal" :in-theory (enable |every_other|-loaded?))))
   :hints (("Goal" :in-theory (e/d (;|every_other|-loaded? 
                                    top)
                                   (lookup-method
                                    class-name-of-ref
                                    |every_other|-p))
                   :use |every_other|-method)))

(defun every_other-start-state-2 (obj xs s)
  (modify 
   s
   :call-stack 
   (push (make-frame 0
                     (list obj xs)
                     nil
                     (method-program *examples$every_other-invoke*)
                     "clojure.lang.IFn")
         (call-stack s))))

(defun step-recursive (xs s)
  (let* 
    ((s1 (modify s
                 :pc 99
                 :stack (popn 2 (stack (top-frame s)))))
    (new-state (modify 
                s1
                :call-stack
                (push 
                 (make-frame 0
                             (reverse 
                              (bind-formals 2 
                                            (stack (top-frame s))))
                             nil
                             (method-program *examples$every_other-invoke*)
                             "clojure.lang.IFn")
                 (call-stack s1)))))
    (every_other-start-state xs new-state)))
    
(defthm every_other=every-other-6
  (implies 
   (and (every_other-base-state s)
        (seq-p xs (heap s)))
   (equal 
    (step (every_other-recursive-state xs s))
    (step-recursive (seq-more (seq-more xs
                                        (heap s)
                                        (class-table s))
                              (heap s)
                              (class-table s))
                    (every_other-recursive-state xs s)))))#|ACL2s-ToDo-Line|#

                                                      
(defthm every_other=every-other-7
  (implies 
   (and (every_other-base-state s)
        (seq-listp xs (heap s) (class-table s))
        (->-equals (every_other-states (cdr (cdr xs)) s)
                   (every_other-results-2 (cdr (cdr xs)) s)))
   (-> (every_other-start-state (seq-more (seq-more xs
                                                    (heap s)
                                                    (class-table s))
                                          (heap s)
                                          (class-table s)) 
                                s)
       (every_other-end-state (seq-more (seq-more xs
                                                  (heap s)
                                                  (class-table s))
                                        (heap s)
                                        (class-table s)) 
                              s)))
  :hints 
  (("Goal" 
    :in-theory (disable every_other-base-state
                        every_other-start-state
                        every_other-end-state
                        every_other-recursive-state
                        |PersistentList:EMPTY|-get))))

(defthm every_other=every-other-7
  (implies 
   (and (every_other-base-state s)
        (seq-listp xs (heap s) (class-table s))
        (->-equals (every_other-states (cdr (cdr xs)) s)
                   (every_other-results-2 (cdr (cdr xs)) s)))
   (->-equals (every_other-states xs s)
              (every_other-results-2 xs s)))
  :hints 
  (("Goal" 
    :in-theory (disable every_other-base-state
                        every_other-start-state
                        every_other-end-state
                        every_other-recursive-state
                        |PersistentList:EMPTY|-get))))

(defun every_other-base-state (s)
  (and (|every_other|-loaded? (class-table s) (heap s))
       (equal (pc (top-frame s)) 0)
       (equal (stack (top-frame s)) nil)
       (equal (program (top-frame s)) (method-program *examples$every_other-invoke*))  
       (|every_other|-p (top (locals (top-frame s))) (heap s))))

(defthm every_other=every-other-5
  (implies 
   (and (every_other-base-state s)
        (seq-listp xs (heap s) (class-table s)))
   (->-equals (every_other-states xs s)
              (every_other-results-2 xs s)))
  :hints 
  (("Goal" 
    :in-theory (disable every_other-base-state
                        every_other-start-state
                        every_other-end-state
                        every_other-recursive-state
                        |PersistentList:EMPTY|-get))))

(defthm ->-equals-every-other-recursive
  (implies 
   (and (every_other-base-state s)
        (seq-p xs (heap s))
        (seq-p (seq-more xs (heap s) (class-table s)) 
               (heap s))
        (seq-p (seq-more (seq-more xs (heap s) (class-table s))
                         (heap s)
                         (class-table s))
               (heap s))        
        (not (|EmptyList|-p xs (heap s))))
        ;(seq-listp xs (heap s) (class-table s))
        ;(not (endp xs)))   
   (-> (every_other-start-state xs s)
       (every_other-recursive-state xs s))))


 



(defun every_other-end-state (eo-ref heap s)
  (modify s
          :pc 104
          :stack (push eo-ref
                       (stack (top-frame s)))
          :heap heap))
  
(defun every_other-results (xs s)
  (if (endp xs)
    (list (every_other-end-state (nullref) (heap s) s))
    (let* ((eo (every-other xs)))
      (cons (every_other-end-state (push (list 'REF (+ (len (heap s))
                                                       (len eo)
                                                       -1))
                                         (stack (top-frame s))) 
                                   (alloc-list eo (heap s)) 
                                   s)
            (every_other-results (cdr xs) s)))))


(local (in-theory (disable ->-execute-ACONST_NULL
                           ->-execute-ALOAD_1
                           ->-execute-ARETURN
                           ->-execute-ASTORE_1                          
                           ->-execute-CHECKCAST
                           ->-execute-DUP
                           ->-execute-GETSTATIC
                           ->-execute-GOTO
                           ->-execute-IF_ACMPEQ
                           ->-execute-IFNULL
                           ->-execute-POP)))


(defun seq-listp (xs heap class-table)
  (if (endp xs)
    (equal xs nil)
    ;(equal (car xs) 
    ;       (|PersistentList:EMPTY|-get heap class-table))
    (and (seq-p (car xs) heap)
         (not (|EmptyList|-p (car xs) heap))
         (if (endp (cdr xs))
           (equal (seq-more (car xs) heap class-table)
                  (|PersistentList:EMPTY|-get heap class-table))
           (equal (seq-more (car xs) heap class-table)
                  (cdr xs)))
         (seq-listp (cdr xs) heap class-table))))

(defthm every_other=every-other-4
  (implies 
   (and (every_other-base-state s)
        (seq-listp xs (heap s) (class-table s)))
   (->-equals (every_other-states xs s)
              (every_other-results xs s)))
  :hints (("Goal" :in-theory (disable seq-more 
                                      every_other-start-state
                                      every_other-end-state))))

(defthm every_other=every-other-4
  (implies 
   (and (every_other-base-state s)
        (seq-p xs (heap s))
        (-> 
         (every_other-recursive-state (seq-more (seq-more xs
                                                          (heap s)
                                                          (class-table s))
                                                (heap s)
                                                (class-table s))
                                      s)
         (every_other-end-state (seq-more (seq-more xs
                                                    (heap s)
                                                    (class-table s))
                                          (heap s)
                                          (class-table s)) 
                                (heap s) 
                                s)))                       
   (-> (every_other-recursive-state xs s)
       (every_other-end-state xs (heap s) s))))

(defthm every_other=every-other-4
  (implies 
   (and (every_other-base-state s)
        (seq-listp xs (heap s) (class-table s)))
   (->-equals (every_other-recursive-state xs s)
              (every_other-end-state xs (heap s) s))))

(defthm every_other=every-other-4
  (implies 
   (and (every_other-base-state s)
        (seq-listp xs (heap s) (class-table s)))
   (->-equals (every_other-states xs s)
              (every_other-results xs s))))
  :hints (("Goal" :in-theory (disable every_other-start-state
                                      every_other-end-state))))

(defthm every_other=every-other-3
  (implies 
   (and (every_other-base-state s)
        (seq-listp xs (heap s) (class-table s)))
   (->-equals (every_other-states xs s)
              (every_other-results xs s))))

(defthm every_other=every-other
  (implies 
   (and (every_other-base-state s)
        (seq-listp xs))
   (-> s
       (modify s
               :pc (+ 5 (pc (top-frame s)))
               :stack (push (nullref)                               
                            (popn 2 (stack (top-frame s))))))))

(defthm seq-equal-xs-len-1
  (implies 
   (and (seq-equal xs xs-seq (heap s) (class-table s))
        (endp (cdr xs)))
   (|EmptyList|-p (seq-more xs-seq (heap s) (class-table s))
                  (heap s))))

