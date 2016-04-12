#|$ACL2s-Preamble$;
(include-book "mc-structures")

(acl2::begin-book t :ttags :all);$ACL2s-Preamble$|#


(in-package "MC")

; -----------------------------------------------------------------------------
; Native method calls

(defun Java_java_lang_Object_getClass (s)
  (modify s 
          :stack (push 
                  (class-decl-heapref
                   (bound? (class-name-of-ref 
                            (top (stack (top-frame s))) (heap s))
                           (class-table s)))                  
                  (pop (stack (top-frame s))))))

(defun System_arraycopy (s src dst-ref index)
  (if (endp src)
    s
    (System_arraycopy
     (modify s
             :heap (bind (cadr dst-ref)
                         (set-element-at (car src)
                                         index
                                         (deref dst-ref (heap s))
                                         (class-table s))
                            (heap s)))
     (cdr src)
     dst-ref
     (1+ index))))

(defun Java_java_lang_System_arraycopy (s)
  (let* ((frame-stack (stack (top-frame s)))
         (length (nth 0 frame-stack))
         (dst-pos (nth 1 frame-stack))
         (dst-ref (nth 2 frame-stack))
         (src-pos (nth 3 frame-stack))
         (src-ref (nth 4 frame-stack))
         (src-data (array-data (deref src-ref (heap s)))))
  (System_arraycopy
   (modify s 
          :stack (popn 5 frame-stack))
   (subseq src-data src-pos (+ length src-pos)) 
   dst-ref
   dst-pos)))

(defun Java_java_util_Arrays_copyOfRange_int (s)
  (let* ((frame-stack (stack (top-frame s)))
         (orig-ref  (nth 2 frame-stack))
         (orig-data (array-data (deref orig-ref (heap s))))
         (from      (nth 1 frame-stack))
         (to        (nth 0 frame-stack))
         (count (- to from))
         (addr (len (heap s)))
         (obj (makearray 'INT
                         count
                         (subseq orig-data from to)
                         (class-table s))))
   (modify s 
           :stack (push (list 'REF addr)
                       (popn 3 frame-stack))
           :heap (bind addr obj (heap s)))))

; (class-decl-name (deref (top (stack (top-frame (step-n 5 *addp-Integer.Integer*)))) (heap (step-n 5 *addp-Integer.Integer*))))

(defun Java_sun_misc_Unsafe_putObject_HashEntry (s)
  (let* ((frame-stack (stack (top-frame s)))
         (value (nth 0 frame-stack))
         (o-ref (nth 2 frame-stack))
         (instance (deref o-ref (heap s)))
         (address (cadr o-ref)))    
        (modify s
                :stack (popn 3 frame-stack)
                :heap  (bind address
                             (set-instance-field "java.util.concurrent.ConcurrentHashMap$HashEntry"
                                                 "next"
                                                 value
                                                 instance)
                             (heap s)))))

(defun Java_sun_misc_Unsafe_putObject_setEntryAt (s)
  (let* ((frame-stack (stack (top-frame s)))
         (x-ref (nth 0 frame-stack))
         (offset (nth 1 frame-stack))
         (o-ref (nth 2 frame-stack)))                    
    (modify  s
             :stack (popn 3 frame-stack)
             :heap (bind (cadr o-ref)
                         (set-element-at x-ref
                                         offset
                                         (deref o-ref (heap s))
                                         (class-table s))
                         (heap s)))))                    

(defun Java_sun_misc_Unsafe_putObject (s)
  (let* ((calling-class (cur-class (top-frame s))))
    (cond ((equal calling-class "java.util.concurrent.ConcurrentHashMap$HashEntry")
           (Java_sun_misc_Unsafe_putObject_HashEntry s))
          ((equal calling-class "java.util.concurrent.ConcurrentHashMap")
           (Java_sun_misc_Unsafe_putObject_setEntryAt s))
          (t s))))

(defun Java_sun_misc_Unsafe_getObject (s)
  (let* ((frame-stack (stack (top-frame s)))
         (offset (nth 0 frame-stack))
         (o-ref (nth 1 frame-stack))
         (entry (element-at offset (deref o-ref (heap s)))))                    
    (modify  s             
             :stack (push 
                     (if entry
                       entry
                       (nullref))
                       (popn 2 frame-stack)))))

(defun execute-native (method s)
  (cond ((equal method "getClass")
         (Java_java_lang_Object_getClass s))
        ((equal method "arraycopy")
         (Java_java_lang_System_arraycopy s))
        ((equal method "copyOfRange")
         (Java_java_util_Arrays_copyOfRange_int s))
        ((equal method "putOrderedObject")
         (Java_sun_misc_Unsafe_putObject s))
        ((equal method "getObjectVolatile")
         (Java_sun_misc_Unsafe_getObject s))        
        (t s)))#|ACL2s-ToDo-Line|#
