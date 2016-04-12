#|$ACL2s-Preamble$;
(acl2::begin-book t :ttags :all);$ACL2s-Preamble$|#


(in-package "ACL2")

(include-book "centaur/bitops/ihsext-basics" :dir :system)
(include-book "centaur/bitops/signed-byte-p" :dir :system)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Unsigned, little-endian implementation of add
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun uint-p (x)
  (unsigned-byte-p 32 x))

(defun uint-listp (xs)
  (cond ((atom xs) (eq xs nil))
        (t (and (uint-p (car xs))
                (uint-listp (cdr xs))))))

(defun ule-to-nat (xs)
  (if (endp xs)
    0
    (+ (car xs)
       (* (expt 2 32)
          (ule-to-nat (cdr xs))))))

(defthm ule-to-nat-is-nat
  (implies (uint-listp xs)
           (natp (ule-to-nat xs)))
  :rule-classes :type-prescription)

(defun add-uint (x y carry)
  (let* ((i (if (uint-p x) x 0))
         (j (if (uint-p y) y 0)))
    (loghead 32 (+ i j carry))))

;(defthm add-uint-is-unsigned-byte-32
;  (implies (and (unsigned-byte-p 32 x)
;                (unsigned-byte-p 32 y)
;                (or (equal carry 0)
;                    (equal carry 1)))
;           (unsigned-byte-p 32 (add-uint x y carry)))
;  :rule-classes :type-prescription)

(defthm add-uint-is-uint-p
  (implies (and (uint-p x)
                (uint-p y)
                (or (equal carry 0)
                    (equal carry 1)))
           (uint-p (add-uint x y carry)))
  :rule-classes :type-prescription)

(defun add-uint-carry (x y carry)
  (let* ((i (if (uint-p x) x 0))
         (j (if (uint-p y) y 0)))
    (logtail 32 (+ i j carry))))

(defun add-carry-p (x)
  (and (integerp x)
       (or (equal x 1)  
           (equal x 0))))

(encapsulate
  () 
  
  (local (include-book "arithmetic-3/top" :dir :system)) 

  (defthm add-uint-carry-is-0-or-1
    (implies (and (uint-p x)
                  (uint-p y)
                  (add-carry-p carry))
                  ;(or (equal carry 0)
                  ;    (equal carry 1)))
             ;(<= (add-uint-carry x y carry) 1))
             (add-carry-p (add-uint-carry x y carry)))
    :rule-classes :type-prescription
    :hints (("Goal" :in-theory (enable logtail)))))

(defun ule-propogate (xs carry)
  (if (endp xs)
    (if (< 0 carry)
      (cons carry nil)
      nil)
    (cons (add-uint (car xs) nil carry)
          (ule-propogate (cdr xs) 
                         (add-uint-carry (car xs) nil carry)))))

(defun ule-add (xs ys carry)
  (if (or (endp xs) (endp ys)) 
    (ule-propogate (if (endp xs) ys xs) carry)
    (cons (add-uint (car xs) (car ys) carry)
          (ule-add (cdr xs)                     
                   (cdr ys)                                               
                   (add-uint-carry (car xs) (car ys) carry)))))

(defthm add-uint-carry-commutative
  (implies (and (uint-p x)
                (uint-p y)
                (add-carry-p carry))
                ;(integerp carry))
           (equal (add-uint-carry x y carry)
                  (add-uint-carry y x carry))))

(defthm add-uint-commutative
  (implies (and (uint-p x)
                (uint-p y)
                (add-carry-p carry))
                ;(integerp carry))
           (equal (add-uint x y carry)
                  (add-uint y x carry))))

(defthm ule-add-commutative-empty-list
  (implies (and (uint-listp xs)
                (not (consp ys))
                (add-carry-p carry))
                ;(integerp carry))
           (equal (ule-add xs ys carry)
                  (ule-add ys xs carry))))

(defthm ule-add-commutative-empty-list-2
  (implies (and (uint-listp ys)
                (not (consp xs))
                (add-carry-p carry))
                ;(integerp carry))
           (equal (ule-add xs ys carry)
                  (ule-add ys xs carry))))

(defthm ule-add-commutative
  (implies (and (uint-listp xs)
                (uint-listp ys)
                (add-carry-p carry))
                ;(integerp carry))
           (equal (ule-add xs ys carry)
                  (ule-add ys xs carry)))
  :hints 
  (("Goal" :in-theory (disable add-uint-carry))
   ("Subgoal *1/2.2'" :use (:instance add-uint-carry-is-0-or-1  
                            (x (car xs)) 
                            (y (car ys))
                            (carry 1)))
   ("Subgoal *1/2.1'" :use (:instance add-uint-carry-is-0-or-1  
                            (x (car xs)) 
                            (y (car ys))
                            (carry 0)))))

(defthm ule-propogate-identity
  (implies (uint-listp xs)
           (equal (ule-propogate xs 0) xs)))

(defthm add-uint-0-is-identity
  (implies (uint-p x)
           (equal (add-uint x nil 0) x)))

(encapsulate
  ()
  
  (local (include-book "arithmetic-5/lib/floor-mod/top" :dir :system))

  (defthm propogate-carry-works
    (implies (and (uint-listp xs)
                  (= carry 1))
             (equal (ule-to-nat (ule-propogate xs carry))
                    (+ (ule-to-nat xs) carry)))
    :hints (("Goal" :in-theory (enable logtail loghead))))

  (defthm add-lists-works
    (implies (and (uint-listp xs)
                  (uint-listp ys)
                  (add-carry-p carry))
                  ;(or (= 0 carry)
                  ;    (= 1 carry)))  
             (equal (ule-to-nat (ule-add xs ys carry))
                    (+ (ule-to-nat xs)
                       (ule-to-nat ys)
                       carry)))
    :hints (("Goal" :in-theory (enable loghead logtail)))))#|ACL2s-ToDo-Line|#
