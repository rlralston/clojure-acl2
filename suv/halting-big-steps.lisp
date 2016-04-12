(include-book "../mc/mc")
(include-book "../mc/utilities")

(in-package "MC")

(set-gag-mode nil)

(defun haltedp (s)
  (equal (step s) s))

(include-book "misc/defpun" :dir :system)

(acl2::defpun stepw (s)
  (if (haltedp s)
    s
    (stepw (step s))))

(defun == (s1 s2)
  (equal (stepw s1)
         (stepw s2)))

(defthm ==-execute-ALOAD
  (implies (equal (next-inst s) '(ALOAD_0))
           (== s
               (modify  s
                        :pc (+ 1 (pc (top-frame s)))
                        :stack (push (nth 0
                                          (locals (top-frame s)))
                                     (stack (top-frame s)))))))

(defthm ==-execute-ARETURN
  (implies (equal (next-inst s) '(ARETURN))             
           (let* ((val (top (stack (top-frame s))))
                  (s1 (modify s
                              :call-stack (pop (call-stack s)))))
             (== s
                    (modify  s1                        
                             :stack (push val
                                          (stack (top-frame s1))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(include-book "../mc/mc")
(include-book "clojure/lang/Obj")
(include-book "clojure/lang/ASeq")
(include-book "clojure/lang/PersistentList")
(include-book "clojure/lang/Cons")
(include-book "clojure/lang/RT")

(set-gag-mode nil)

(defun nullref () 
  '(REF -1))

(defun j-instance-classname (instance)
  (caar instance))

(defun j-instance-has-field (class-name field-name instance)
  (not (equal (bound? field-name (binding class-name instance)) nil)))

(defun j-refp (ref)
  (and (consp ref)
       (equal (car ref) 'REF)))

; Recognizer for PersistentList Heap References 
(defun j-PersistentList-p (ref heap)
  (and (j-refp ref)
       (let* ((obj (deref ref heap))
              (class-name (j-instance-classname obj)))
         (and (equal class-name "clojure.lang.PersistentList")
              (j-instance-has-field class-name "_first" obj)
              (j-instance-has-field class-name "_rest" obj)
              (j-instance-has-field class-name "_count" obj)))))

(defun j-PersistentList-_first (ref heap)
  (let* ((instance (deref ref heap)))
    (field-value "clojure.lang.PersistentList" "_first" instance)))

(defun j-PersistentList-first ()
  (bound? "first" (class-decl-methods *clojure.lang.PersistentList*)))

(defun poised-PersistentList-first (s)  
  (let* ((ref (top (stack (top-frame s))))
         (heap (heap s)))
    (and (j-PersistentList-p ref heap)
         (equal (next-inst s) 
                '(invokeinterface "clojure.lang.ISeq" "first" 0))
         (equal (lookup-method "first" 
                               "clojure.lang.PersistentList" 
                               (class-table s))
                (j-PersistentList-first)))))

(defthm execute-ALOAD-inst
  (implies (and (equal (next-inst s) inst)
                (equal (op-code inst) 'ALOAD_0))
           (equal (do-inst inst s) (execute-ALOAD_X inst s 0))))

;(defthm stepw-opener
;  (equal (stepw s)
;         (if (haltedp s)
;           s
;           (stepw (step s)))))

(defthm PersistentList-first-is-_first-step-1
  (implies (and (poised-PersistentList-first s)
                (equal first 
                       (j-PersistentList-_first (top (stack (top-frame s)))
                                                (heap s))))
           (equal (next-inst (step-n 1 s))
                  '(ALOAD_0))))
                  
(defthm PersistentList-first-is-_first-step-3
  (implies (and (poised-PersistentList-first s)
                (equal first 
                       (j-PersistentList-_first (top (stack (top-frame s)))
                                                (heap s))))
           (equal (next-inst (step-n 3 s))
                  '(ARETURN))))

(defthm stepw-is-halting
  (implies (haltedp (stepw s))
           (equal (stepw (stepw s))
                  (stepw s))))

(defun inst-known-p (s)
  (let* ((inst (next-inst s))
         (op (op-code inst)))
    (or (equal op 'ALOAD_0)
        (equal op 'INVOKEINTERFACE)
        (equal op 'GETFIELD)
        (equal op 'ARETURN)
        
        (equal op 'CHECKCAST)
        (equal op 'ACONST_NULL)
        (equal op 'IF_ICMPNE)
        (equal op 'ICONST_1)
        (equal op 'INVOKEVIRTUAL))))

(defthm stepw-opener
  (implies (inst-known-p s)
           (equal (stepw s)
                    (stepw (step s)))))


(defthm PersistentList-first-is-_first-stepw
  (implies (and 
                (poised-PersistentList-first s)
                (equal first 
                       (j-PersistentList-_first (top (stack (top-frame s)))
                                                (heap s))))
           (== (step-n 1 s)          
                  (modify s
                          :pc (+ 3 (pc (top-frame s)))
                          :stack (push first
                                       (pop (stack (top-frame s))))))))

;(defthm PersistentList-first-is-_first-stepw
;  (implies (and 
;                (poised-PersistentList-first s)
;                (equal first 
;                       (j-PersistentList-_first (top (stack (top-frame s)))
;                                                (heap s))))
;           (inst-known-p (step-n 3 s))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defthm PersistentList-first-is-_first-stepw-2
  (implies (and 
                (poised-PersistentList-first s)
                (equal first 
                       (j-PersistentList-_first (top (stack (top-frame s)))
                                                (heap s))))
           (== s          
                  (modify s
                          :pc (+ 3 (pc (top-frame s)))
                          :stack (push first
                                       (pop (stack (top-frame s))))))))

(defun j-PersistentList-_rest (ref heap)
  (let* ((instance (deref ref heap)))
    (field-value "clojure.lang.PersistentList" "_rest" instance)))

(defun j-PersistentList-next ()
  (bound? "next" (class-decl-methods *clojure.lang.PersistentList*)))

(defun poised-PersistentList-next (s)  
  (let* ((ref (top (stack (top-frame s))))
         (heap (heap s)))
    (and (j-PersistentList-p ref heap)
         (equal (next-inst s) 
                '(invokevirtual "clojure.lang.ASeq" "next" 0))
         (equal (lookup-method "next" 
                               "clojure.lang.PersistentList" 
                               (class-table s))
                (j-PersistentList-next)))))

(defun j-PersistentList-_count (ref heap)
  (let* ((instance (deref ref heap)))
    (field-value "clojure.lang.PersistentList" "_count" instance)))


(defthm PersistentList-next-is-_rest-stepr
  (implies (and (poised-PersistentList-next s)
                (equal rest 
                       (j-PersistentList-_rest (top (stack (top-frame s)))
                                               (heap s)))
                (equal count 
                       (j-PersistentList-_count (top (stack (top-frame s)))
                                                (heap s)))
                (integerp count)
                (equal 1 count)
                (equal ret (if (equal count 1) (nullref) rest)))
           (==  s          
                  (modify s
                          :pc (+ 3 (pc (top-frame s)))
                          :stack (push ret
                                       (pop (stack (top-frame s))))))))




;(defthm stepw-opener
;  (equal (stepw s)
;         (if (haltedp s)
;           s
;           (stepw (step s)))))

(defthm if-not-halting
  (implies (not (haltedp s))
           (equal (stepw s)
                  (stepw (step s)))))

(defthm step-is-stepw
  (implies (haltedp state)
           (== (step s) s)))

(defun program-length (program)
  (if (endp program)
      0
      (+ (inst-length (car program))
         (program-length (cdr program)))))

(defthm inst-length-is-positive
  (< 0 (inst-length p)))

;(defthm program-length-is-natural
;  (natp (program-length p)))

(defun program-last (program)
  (if (endp (cdr program))
      (car program)
      (program-last (cdr program))))

;(defun index-into-program (byte-offset program)
;  (declare (xargs :measure (len program)))
;  (if (endp program)
;      nil
;      (if (zp byte-offset)
;          (car program)
;          (index-into-program (- byte-offset
;                                 (inst-length (car program)))
;                              (cdr program)))))

(defthm index-exact-length
  (implies (= pc (program-length p))
           (equal (index-into-program pc p) nil)))

(defthm program-length-type
  (natp (program-length p))
  :rule-classes :type-prescription)

(defthm natp-incr-is-posp
  (implies (and (natp i) 
                (posp j))
           (posp (+ j i))))

(defthm pc-not-zp
  (implies (and
            (integerp pc)
            (< (+ 1 (program-length p)) pc))
           (not (zp pc))))


(defthm index-after-max-is-nil
  (implies (and (integerp pc)
                (< (program-length p) pc))
           (equal (index-into-program pc p) nil)))

(defthm step-closer
  (implies (not (consp (next-inst s)))
           (equal (step s) s))
  :hints (("Goal" :in-theory (enable step))))

;(defthm halted
;  (implies (and (equal (program-length p) pc)
;                (equal (make-frame pc l s p cur-class) frame)
;                (equal (make-state (push frame nil) heap class-table) s))
;           (haltedp (step pc))))

(defthm stepw-opener
  (equal (stepw s)
         (if (haltedp s)
           s
           (stepw (step s)))))