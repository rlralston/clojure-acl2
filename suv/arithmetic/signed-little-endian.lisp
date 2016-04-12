#|$ACL2s-Preamble$;
(acl2::begin-book t :ttags :all);$ACL2s-Preamble$|#


(in-package "ACL2")

(include-book "unsigned-little-endian")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Signed, little-endian implementation of add
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jint-p (x)
  (signed-byte-p 32 x))

(defun jint-listp (xs)
  (cond ((atom xs) (eq xs nil))
        (t (and (jint-p (car xs))
                (jint-listp (cdr xs))))))

(defun longmask (x)
  (logand (logmask 32) x))

(defun sle-to-nat (xs)
  (if (endp xs)
    0
    (+ (longmask (car xs))
       (* (expt 2 32)
          (sle-to-nat (cdr xs))))))

(defthm sle-to-nat-is-nat
  (implies (jint-listp xs)
           (natp (sle-to-nat xs)))
  :rule-classes :type-prescription)

(defun add-jint (x y carry)
  (let* ((i (if (jint-p x) x 0))
         (j (if (jint-p y) y 0)))
    (logext 32 (loghead 32 (+ (longmask i) (longmask j) carry)))))

(defthm add-jint-is-jint
  (implies (and (jint-p x)
                (jint-p y)
                (or (equal carry 0)
                    (equal carry 1)))
           (jint-p (add-jint x y carry)))
  :rule-classes :type-prescription)

(defun add-jint-carry (x y carry)
  (let* ((i (if (jint-p x) x 0))
         (j (if (jint-p y) y 0)))
    (logtail 32 (+ (longmask i) (longmask j) carry))))

(encapsulate
  () 
  
  (local (include-book "arithmetic-3/top" :dir :system)) 

  (defthm add-jint-carry-is-0-or-1
    (implies (and (jint-p x)
                  (jint-p y)
                  (add-carry-p carry))
                  ;(or (equal carry 0)
                  ;    (equal carry 1)))
             ;(<= (add-jint-carry x y carry) 1))
             (add-carry-p (add-jint-carry x y carry)))
    :rule-classes :type-prescription
    :hints (("Goal" :in-theory (enable logtail loghead)))))

(defthm add-jint-carry-commutative
  (implies (and (jint-p x)
                (jint-p y)
                ;(integerp carry))
                (add-carry-p carry))
           (equal (add-jint-carry x y carry)
                  (add-jint-carry y x carry))))

(defthm add-jint-commutative
  (implies (and (jint-p x)
                (jint-p y)
                ;(integerp carry))
                (add-carry-p carry))
           (equal (add-jint x y carry)
                  (add-jint y x carry))))

(defun sle-propogate (xs carry)
  (if (endp xs)
    (if (< 0 carry)
      (cons carry nil)
      nil)
    (cons (add-jint (car xs) nil carry)
          (sle-propogate (cdr xs) 
                         (add-jint-carry (car xs) nil carry)))))

(defun sle-add (xs ys carry)
  (if (or (endp xs) (endp ys)) 
    (sle-propogate (if (endp xs) ys xs) carry)
    (cons (add-jint (car xs) (car ys) carry)
          (sle-add (cdr xs)                     
                   (cdr ys)                                               
                   (add-jint-carry (car xs) (car ys) carry)))))

(defthm sle-add-commutative-empty-list
  (implies (and (jint-listp xs)
                (not (consp ys))
                (add-carry-p carry))
                ;(integerp carry))
           (equal (sle-add xs ys carry)
                  (sle-add ys xs carry))))

(defthm sle-add-commutative-empty-list-2
  (implies (and (jint-listp ys)
                (not (consp xs))
                ;(integerp carry))
                (add-carry-p carry))
           (equal (sle-add xs ys carry)
                  (sle-add ys xs carry))))

(defthm sle-add-commutative
  (implies (and (jint-listp xs)
                (jint-listp ys)
                ;(integerp carry))
                (add-carry-p carry))
           (equal (sle-add xs ys carry)
                  (sle-add ys xs carry)))
  :hints 
  (("Goal" :in-theory (disable add-jint-carry))     
   ("Subgoal *1/2.2'" :use (:instance add-jint-carry-is-0-or-1  
                            (x (car xs)) 
                            (y (car ys))
                            (carry 1)))
   ("Subgoal *1/2.1'" :use (:instance add-jint-carry-is-0-or-1  
                            (x (car xs)) 
                            (y (car ys))
                            (carry 0)))))

(defthm jint-longmask-is-uint
  (implies (jint-p x)
           (uint-p (longmask x)))
  :rule-classes :type-prescription)

(defun sle-to-ule (xs)
  (if (endp xs)
    nil
    (cons (longmask (car xs))       
          (sle-to-ule (cdr xs)))))

(defthm sle-to-nat-is-ule-to-nat
  (implies (jint-listp xs)
           (equal (sle-to-nat xs)
                  (ule-to-nat (sle-to-ule xs)))))

(defthm add-jint-is-add-uint
  (implies (and (jint-p x)
                (jint-p y)
                ;(natp carry))
                (add-carry-p carry))
           (equal (add-jint x y carry)
                  (logext 32 (add-uint (longmask x) (longmask y) carry)))))

(defthm add-jint-carry-is-add-uint-carry
  (implies (and (jint-p x)
                (jint-p y)
                ;(natp carry))
                (add-carry-p carry))
           (equal (add-jint-carry x y carry)
                  (add-uint-carry (longmask x) (longmask y) carry))))

(defun ule-to-sle (xs)
  (if (endp xs)
    nil
    (cons (logext 32 (car xs))       
          (ule-to-sle (cdr xs)))))

(defthm logext-is-jint
  (implies (uint-p x)
           (jint-p (logext 32 xs)))
  :rule-classes :type-prescription)

(defthm ule-to-sle-is-sle
  (implies (uint-listp xs)
           (jint-listp (ule-to-sle xs)))
  :rule-classes :type-prescription)

(encapsulate
  ()
  
  (local (include-book "arithmetic-3/top" :dir :system))

  (defthm sle-propogate-is-ule-propogate
    (implies (and (jint-listp xs)
                  ;(or (= carry 1)
                  ;    (= carry 0)))
                  (add-carry-p carry))
             (equal (sle-propogate xs carry)
                    (ule-to-sle (ule-propogate (sle-to-ule xs) carry))))
    :hints (("Goal" :in-theory (enable logext logapp logbitp loghead logtail))))
  
  (defthm add-uint-carry-is-0-or-1-2
    (implies (and (uint-p x)
                  (add-carry-p carry))
                  ;(or (equal carry 0)
                  ;    (equal carry 1)))
             ;(<= (add-uint-carry x y carry) 1))
             (add-carry-p (add-uint-carry x nil carry)))
    :rule-classes :type-prescription
    :hints (("Goal" :in-theory (enable logtail))))  
  
  (defthm ule-propogate-is-uint
    (implies (and (uint-listp xs)
                  (add-carry-p carry))
                  ;(natp carry)
                  ;(uint-p carry))
             (uint-listp (ule-propogate xs carry)))
    :rule-classes :type-prescription
    :hints (("Goal" :in-theory (disable add-uint-carry add-carry-p))
            ("Subgoal *1/4''" :use (:instance add-uint-carry-is-0-or-1-2
                                    (x (car xs)))))))
  
(encapsulate
 ()
 
 (local (include-book "arithmetic-5/lib/floor-mod/top" :dir :system))

 (defthm sle-to-nat-ule-to-sle-is-ule-to-nat
   (implies (uint-listp xs)
            (equal (sle-to-nat (ule-to-sle xs))
                   (ule-to-nat xs)))
   :hints (("Goal" :in-theory (enable logext logapp logbitp loghead logtail)))))

(defthm sle-to-ule-is-ule
  (implies (jint-listp xs)
           (uint-listp (sle-to-ule xs)))
  :rule-classes :type-prescription)

(defthm ule-propogate-of-sle-is-uint-list
  (implies (and (jint-listp xs)
                ;(uint-p carry)
                ;(natp carry))
                (add-carry-p carry))
           (uint-listp (ule-propogate (sle-to-ule xs) carry)))
  :rule-classes :type-prescription)

(defthm sle-propogate-carry-works-2
  (implies (and (jint-listp xs)
                (add-carry-p carry))
                ;(or (= carry 1)
                ;    (= carry 0)))
            (equal (sle-to-nat (sle-propogate xs carry))
                   (+ (ule-to-nat (sle-to-ule xs)) carry))))

(defthm sle-propogate-carry-works-3
  (implies (and (jint-listp xs)
                (add-carry-p carry))
                ;(or (= carry 1)
                ;    (= carry 0)))
            (equal (sle-to-nat (sle-propogate xs carry))
                   (+ (sle-to-nat xs) carry))))

(defthm sle-add-is-ule-add-not-consp
  (implies (and (jint-listp xs)                  
                (jint-listp ys)                
                (not (consp ys))
                (add-carry-p carry))
                ;(or (= carry 1)                
                ;    (= carry 0)))             
           (equal (sle-add xs ys carry)           
                  (ule-to-sle (ule-add (sle-to-ule xs) (sle-to-ule ys) carry)))))    
 
;(defun add-carry-p (x) 
;  (or (equal x 1)  
;      (equal x 0)))
  
(encapsulate 
  ()
  
  (local (include-book "arithmetic-3/top" :dir :system))
  
  (defthm add-carry-is-0-or-1
    (implies (and (uint-p x)
                  (uint-p y)
                  (add-carry-p carry))
                  ;(or (= carry 1)
                  ;    (= carry 0)))             
             (add-carry-p (add-uint-carry x y carry)))
    :rule-classes :type-prescription
    :hints (("Goal" :in-theory (enable logtail))))

    (defthm add-carry-is-0-or-1-p2
    (implies (and (jint-p x)
                  (jint-p y)
                  (add-carry-p carry))
                  ;(or (= carry 1)
                  ;    (= carry 0)))
             (add-carry-p (add-uint-carry (loghead 32 x) (loghead 32 y) carry)))
    :rule-classes :type-prescription
    :hints (("Goal" :in-theory (enable logtail loghead)))))
     
(defthm sle-add-is-ule-add
  (implies (and (jint-listp xs)  
                (jint-listp ys)
                (add-carry-p carry))
                ;(or (= carry 1)                
                ;    (= carry 0)))            
           (equal (sle-add xs ys carry)                   
                  (ule-to-sle (ule-add (sle-to-ule xs) (sle-to-ule ys) carry))))   
  :hints (("Goal" :in-theory (e/d (logext logapp logbitp logtail)                                
                                  (jint-p add-uint add-uint-carry add-jint add-jint-carry)))
          ("Subgoal *1/2.4" :use (:instance add-carry-is-0-or-1-p2 (x (car xs)) (y (car ys))))          
          ("Subgoal *1/2.3" :use (:instance add-carry-is-0-or-1-p2 (x (car xs)) (y (car ys))))))

(encapsulate 
  ()
  
  (local (include-book "arithmetic-3/top" :dir :system))
  
(defthm ule-add-is-uint-list
  (implies (and (uint-listp xs)
                (uint-listp ys)
                (add-carry-p carry))
                ;(or (= 0 carry)
                ;    (= 1 carry)))
           (uint-listp (ule-add xs ys carry)))
  :rule-classes :type-prescription
  :hints (("Goal" :in-theory (enable logtail))
          ("Subgoal *1/1'" :use ule-propogate-is-uint)))  
  
(defthm sle-add-lists-works
  (implies (and (jint-listp xs)
                (jint-listp ys)
                (add-carry-p carry))
                ;(or (= 0 carry)
                ;    (= 1 carry)))  
           (equal (sle-to-nat (sle-add xs ys carry))
                  (+ (sle-to-nat xs)
                     (sle-to-nat ys)
                     carry)))))#|ACL2s-ToDo-Line|#
