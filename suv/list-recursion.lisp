(include-book "../mc/mc")
(include-book "clojure/lang/Obj")
(include-book "clojure/lang/ASeq")
(include-book "clojure/lang/PersistentList")
(include-book "clojure/lang/Cons")
(include-book "clojure/lang/RT")
(include-book "clojure/core$first")
(include-book "../mc/utilities")
(include-book "big-step")

(in-package "MC")

(defun j-ASeq-more ()
  (bound? "more" (class-decl-methods *clojure.lang.ASeq*)))

(defun poised-PersistentList-more (s)  
  (let* ((ref (top (stack (top-frame s))))
         (heap (heap s)))
    (and (j-PersistentList-p ref heap)
         (equal (next-inst s) 
                '(invokeinterface "clojure.lang.ISeq" "more" 0))
         (equal (lookup-method "more" 
                               "clojure.lang.PersistentList" 
                               (class-table s))
                (j-ASeq-more))
         (equal (lookup-method "next" 
                               "clojure.lang.PersistentList" 
                               (class-table s))
                (j-PersistentList-next)))))

(defthm ASeq-more-PersistentList-is-_rest
  (implies (and (poised-PersistentList-more s)
                (equal rest 
                       (j-PersistentList-_rest (top (stack (top-frame s)))
                                               (heap s)))
                (equal count 
                       (j-PersistentList-_count (top (stack (top-frame s)))
                                                (heap s)))
                (not (nullrefp rest)))
           (-> s
               (modify s
                       :pc (+ 5 (pc (top-frame s)))                       
                       :stack (push (if (equal count 1) 
                                      (static-field-value "clojure.lang.PersistentList" 
                                                          "EMPTY" 
                                                          s)
                                      rest)
                                    (pop (stack (top-frame s))))))))


(defun poised-RT-more (s)  
    (and (equal (next-inst s) 
                '(invokestatic "clojure.lang.RT" "more" 1))
         (equal (lookup-method "more" 
                               "clojure.lang.RT" 
                               (class-table s))
                (j-RT-more))))

(defun poised-RT-PersistentList (s)  
  (let* ((ref (top (stack (top-frame s))))
         (heap (heap s)))
    (and (j-PersistentList-p ref heap)
         (equal (lookup-method "more" 
                               "clojure.lang.PersistentList" 
                               (class-table s))
                (j-ASeq-more))
         (equal (lookup-method "next" 
                               "clojure.lang.PersistentList" 
                               (class-table s))
                (j-PersistentList-next)))))

(defthm PersistentList-instanceof-ISeq
  (implies (and (equal (next-inst s) 
                '(instanceof "clojure.lang.ISeq"))
                (poised-RT-PersistentList s)
                (equal (bound? "clojure.lang.PersistentList" 
                               (class-table s))
                       *clojure.lang.PersistentList*))
           (-> s
               (modify s
                       :pc (+ 3 (pc (top-frame s)))
                       :stack (push 1
                                    (pop (stack (top-frame s))))))))

(defthm RT-more-PersistentList-is-_rest
  (implies (and (poised-RT-more s)
                (poised-RT-PersistentList s)
                (equal rest 
                       (j-PersistentList-_rest (top (stack (top-frame s)))
                                               (heap s)))
                (equal count 
                       (j-PersistentList-_count (top (stack (top-frame s)))
                                                (heap s)))
                (not (nullrefp rest))
                (equal (bound? "clojure.lang.PersistentList" 
                               (class-table s))
                       *clojure.lang.PersistentList*))
           (-> s
               (modify s
                       :pc (+ 3 (pc (top-frame s)))
                       :stack (push (if (equal count 1) 
                                      (static-field-value "clojure.lang.PersistentList" 
                                                          "EMPTY" 
                                                          s)
                                      rest)
                                    (pop (stack (top-frame s)))))))
  :hints (("Goal" :in-theory (disable ASEQ-MORE-PERSISTENTLIST-IS-_REST
                                      PERSISTENTLIST-INSTANCEOF-ISEQ))))

(defun j-EmptyList-p (ref heap)
  (and (j-refp ref)
       (let* ((obj (deref ref heap))
              (class-name (j-instance-classname obj)))
         (equal class-name "clojure.lang.PersistentList$EmptyList"))))

(defun j-class-has-field (class-name field-name s)  
  (not (equal (static-field-value class-name field-name s) nil)))


(defthm RT-more-PersistentList-is-_rest
  (implies (and (poised-RT-more s)
                (poised-RT-PersistentList s)
                (equal rest 
                       (j-PersistentList-_rest (top (stack (top-frame s)))
                                               (heap s)))
                (equal count 
                       (j-PersistentList-_count (top (stack (top-frame s)))
                                                (heap s)))
                (not (nullrefp rest))
                (equal (bound? "clojure.lang.PersistentList" 
                               (class-table s))
                       *clojure.lang.PersistentList*))
           (-> s
               (modify s
                       :pc (+ 3 (pc (top-frame s)))
                       :stack (push (if (equal count 1) 
                                      (static-field-value "clojure.lang.PersistentList" 
                                                          "EMPTY" 
                                                          s)
                                      rest)
                                    (pop (stack (top-frame s))))))))

(defun j-RT-first ()
  (bound? "first" (class-decl-methods *clojure.lang.RT*)))

; Recognizer for core$first Heap References 
(defun j-core$first-p (ref heap)
  (and (j-refp ref)
       (let* ((obj (deref ref heap))
              (class-name (j-instance-classname obj)))
         (equal class-name "clojure.core$first"))))

(defun j-core$first-invoke ()
  (bound? "invoke" (class-decl-methods *clojure.core$first*)))

(defun poised-core$first (s)  
  (let* ((ref (top (stack (top-frame s))))
         (ref2 (top (pop (stack (top-frame s)))))
         (heap (heap s)))
    (and (j-core$first-p ref heap)
         (j-Cons-p ref2 heap)
         (equal (next-inst s) 
                '(invokeinterface "clojure.lang.IFn" "invoke" 1))
         (equal (lookup-method "invoke" 
                               "clojure.core$first" 
                               (class-table s))
                (j-core$first-invoke))
         (equal (lookup-method "first" 
                               "clojure.lang.RT" 
                               (class-table s))
                (j-RT-first)))))

(defun j-Cons-_first (ref heap)
  (let* ((instance (deref ref heap)))
    (field-value "clojure.lang.Cons" "_first" instance)))

(defthm core$first-is-_first
  (implies (and (poised-core$first s)               
                (equal first 
                       (j-Cons-_first 
                        (top (pop (stack (top-frame s))))                                               
                        (heap s))))
           (-> s                           
               (modify s              
                       :pc (+ 5 (pc (top-frame s)))                       
                       :stack 
                       (push first                                                          
                             (popn 2 (stack (top-frame s))))))))



#|

; ---------------------------------------------------------
; Example PersistentList state

(defconst *new-PersistentList* 
  (make-state 
    (push 
     (make-frame 
      0 
      nil 
      ;(push '(REF 1) (push 0 nil))
      nil
      '((NEW "clojure.lang.PersistentList")
        (DUP)
        (ALOAD_0)
        (INVOKESPECIAL "clojure.lang.PersistentList" "<init>" 1))
      "clojure.lang.RT")
     nil)
    '()
    (make-class-def (list *clojure.lang.Obj* *clojure.lang.ASeq* *clojure.lang.PersistentList* *clojure.lang.RT*))))

|#

#|

; ---------------------------------------------------------
; Example RT.more(PersistentList) 

(defconst *rt-heap*
  '((0 . (("java.lang.Class" ("<name>" . "java.math.BigInteger"))
          ("java.lang.Object")))   
    (1 . (("clojure.lang.PersistentList" ("_first" . '(REF 0))
                                         ("_count" . 1)
                                         ("_more" . '(REF -1)))
          ("clojure.lang.ASeq")))
    (1 . (("clojure.lang.PersistentList" ("_first" . '(REF 0))
                                         ("_count" . 2)
                                         ("_more" . '(REF 1)))
          ("clojure.lang.ASeq")))))    

(defconst *rt-more-state*
  (make-state 
    (push 
     (make-frame 
      0       
      nil        
      (push '(REF 1) nil)
      '((INVOKESTATIC "clojure.lang.RT" "next" 1))       
      "clojure.lang.RT")
     nil)
    *rt-heap*
    (make-class-def (list *clojure.lang.Obj* *clojure.lang.ASeq* *clojure.lang.PersistentList* *clojure.lang.RT*))))

    
; (index-into-program 0 (program (top-frame (step *rt-more-state*))))
; (state-pp (step-n 4 *rt-more-state*))    
|#