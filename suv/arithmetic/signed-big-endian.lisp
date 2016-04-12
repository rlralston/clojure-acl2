#|$ACL2s-Preamble$;
(acl2::begin-book t :ttags :all);$ACL2s-Preamble$|#


(in-package "ACL2")

(include-book "signed-little-endian")

(defun sbe-to-nat-i (xs idx) 
  (if (zp idx)
    0
    (+ (longmask (nth (1- idx) xs))
       (* (expt 2 32)
          (sbe-to-nat-i xs (1- idx))))))

(defun sle-to-nat-i (xs i)
  (if (zp i)
    0
    (+ (longmask (nth (- (len xs) i) xs))
       (* (expt 2 32)
          (sle-to-nat-i xs (1- i))))))

(include-book "std/basic/inductions" :dir :system)

(defthm sle-idx-inductive-equality
  (implies (and (jint-listp xs)
                (<= i (len (cdr xs))))
           (equal (sle-to-nat-i xs i)
                  (sle-to-nat-i (cdr xs) i)))
  :hints (("Goal" :induct (dec-induct i))))

(defthm sle-idx-to-sle
  (implies (jint-listp xs)
           (equal (sle-to-nat-i xs (len xs))
                  (sle-to-nat xs)))
  :hints (("Goal" :induct (cdr-dec-induct xs (1- (len xs))))))

(include-book "std/lists/top" :dir :system)

(defthm nth-end-rev-first
      (implies (consp xs)
               (equal (nth (1- (len xs)) xs)
                      (car (rev xs)))))

(defthm distance-moved
  (implies (and (consp xs) (atom y))
           (equal (nth (len xs) (append xs (list y))) y)))

(defthm neg-neg
  (implies (integerp i)
           (equal (- (- i)) i)))

(defthm sbe-to-nat-is-sle-to-nat
  (implies (and (jint-listp xs)
                (<= i (len xs)))
           (equal (sbe-to-nat-i xs i)
                  (sle-to-nat-i (rev xs) i)))
  :hints (("Goal" :induct (dec-induct i))))

(defun sle-propogate-i (xs carry i)
  (if (zp i)
    (if (< 0 carry)
      (cons carry nil)
      nil)
    (cons (add-jint (nth (- (len xs) i) xs) 
                    nil 
                    carry)
          (sle-propogate-i xs 
                           (add-jint-carry (nth (- (len xs) i) xs) 
                                           nil 
                                           carry)
                           (1- i)))))

(defun sle-add-i (xs ys carry xi yi)
  (if (or (zp xi) (zp yi)) 
    (sle-propogate-i (if (zp xi) ys xs) carry (if (zp xi) yi xi))
    (cons (add-jint (nth (- (len xs) xi) xs) 
                    (nth (- (len ys) yi) ys) 
                    carry)
          (sle-add-i xs                    
                     ys                                                                  
                     (add-jint-carry (nth (- (len xs) xi) xs) 
                                     (nth (- (len ys) yi) ys) 
                                     carry)
                     (1- xi)
                     (1- yi)))))

(encapsulate
  ()

  (local (include-book "arithmetic-3/top" :dir :system))
   
  (local 
   (defthmd sle-propogate-idx-inductive-equality  
     (implies (and (jint-listp xs)
                   (<= i (len (cdr xs)))
                   (integerp carry))
              (equal (sle-propogate-i xs carry i)
                     (sle-propogate-i (cdr xs) carry i)))))
   
   (local 
    (defthmd sle-propogate-idx-base-case
     (implies (and (jint-listp xs)
                   (integerp carry)
                   (<= (len xs) 1))
              (equal (sle-propogate-i xs carry (len xs))
                     (sle-propogate xs carry)))))
  
  (defthm sle-propogate-idx-to-sle-propogate
    (implies (and (jint-listp xs)
                  (integerp carry))
             (equal (sle-propogate-i xs carry (len xs))
                    (sle-propogate xs carry)))
    :hints (("Subgoal *1/2''" :in-theory (enable sle-propogate-idx-base-case))
            ("Subgoal *1/5" :in-theory (enable sle-propogate-idx-inductive-equality)))))

(defthmd sle-add-idx-inductive-equality
  (implies (and (jint-listp xs)
                (jint-listp ys)
                (integerp carry)
                (<= xi (len (cdr (double-rewrite xs))))
                (<= yi (len (cdr (double-rewrite ys)))))
           (equal (sle-add-i xs ys carry xi yi)
                  (sle-add-i (cdr xs) (cdr ys) carry xi yi)))
  :hints (("Goal" :in-theory (disable add-jint add-jint-carry))))

(defthm sle-add-idx-to-sle-add
  (implies (and (jint-listp xs)
                (jint-listp ys)
                (integerp carry))
           (equal (sle-add-i xs ys carry (len xs) (len ys))
                  (sle-add xs ys carry)))
  :hints (("Goal" :in-theory (disable add-jint add-jint-carry))
          ("Subgoal *1/4'" :in-theory (enable sle-add-idx-inductive-equality))))
           
(defun sbe-propogate-i (xs carry i)
  (if (zp i)
    (if (< 0 carry)
      (cons carry nil)
      nil)
    (append (sbe-propogate-i xs 
                             (add-jint-carry (nth (1- i) xs) 
                                             nil 
                                             carry)
                             (1- i))
            (list (add-jint (nth (1- i) xs) nil carry)))))

(defun sbe-add-i (xs ys carry xi yi)
  (if (or (zp xi) (zp yi)) 
    (sbe-propogate-i (if (zp xi) ys xs) carry (if (zp xi) yi xi))
    (append
          (sbe-add-i xs                    
                     ys                                                                  
                     (add-jint-carry (nth (1- xi) xs) 
                                     (nth (1- yi) ys) 
                                     carry)
                     (1- xi)
                     (1- yi))
     
          (list (add-jint (nth (1- xi) xs) 
                          (nth (1- yi) ys) 
                          carry)))))

(encapsulate
  ()
  
 (local (include-book "arithmetic-5/lib/floor-mod/top" :dir :system))

   (defthm add-carry-is-0-or-1-p3
    (implies (and (jint-p x)
                  (add-carry-p carry))
             (add-carry-p (add-jint-carry x nil carry)))
    :rule-classes :type-prescription
    :hints (("Goal" :in-theory (enable logtail loghead))))
  
(defthm nth-of-jint-listp-is-jintp
  (implies (and (jint-listp xs)
                (<= 0 i)
                (< i (len xs)))
           (jint-p (nth i xs)))
  :rule-classes :type-prescription)


(defthm logext-loghead-jint-is-identity
  (implies (jint-p x)
           (equal (logext 32 (loghead 32 x)) x))
  :hints (("Goal" :in-theory (enable logext loghead logapp logbitp)))))

(defthm sle-propogate-carry-0-is-xs
  (implies (jint-listp xs)
           (equal (sle-propogate-i xs 0 (len xs)) xs)))

(defthm add-jint-carry-xs-nil-0
  (implies (jint-p x)
           (equal (add-jint-carry x nil 0) 0)))

 ;(local (include-book "std/lists/append" :dir :system))

 (defthm nth-of-jint-listp-is-jintp-2
   (implies (and (jint-listp xs)
                 (<= i (len xs))
                 (not (zp i))
                 (add-carry-p carry))
            (jint-p (add-jint-carry (nth (1- i) xs) nil carry)))
   :rule-classes :type-prescription)

(encapsulate
  ()
  
  (local (include-book "arithmetic-5/lib/floor-mod/top" :dir :system))
 
  (defthm add-jint-carry-is-0-or-1-2
    (implies (and (jint-listp xs)
                  (<= i (len xs))
                  (not (zp i))
                  (add-carry-p carry))
             (add-carry-p (add-jint-carry (nth (1- i) xs) nil carry)))
    :rule-classes :type-prescription
    :hints (("Goal" :in-theory (enable logtail loghead)))) 
 
  (defthm sbe-propogate-is-sle-propogate
    (implies (and (jint-listp xs)
                  (integerp carry)
                  (<= xi (len xs))
                  (add-carry-p carry))
             (equal (sbe-propogate-i xs carry xi)
                    (rev (sle-propogate-i (rev xs) carry xi))))
    :hints (("Goal" :in-theory (e/d (logext logapp logbitp logtail)                                
                                    (jint-p add-jint add-jint-carry)))
            ("Subgoal *1/3.3'" :use (:instance add-jint-carry-is-0-or-1-2 (i xi) (carry 1)))))
    
  (defthm add-jint-carry-is-0-or-1-3  
    (implies (and (jint-listp xs)
                  (<= xi (len xs))
                  (not (zp xi))
                  (jint-listp ys)
                  (<= yi (len ys))
                  (not (zp yi))
                  (add-carry-p carry))
             (add-carry-p (add-jint-carry (nth (1- xi) xs) (nth (1- yi) ys) carry)))
    :rule-classes :type-prescription
    :hints (("Goal" :in-theory (enable logtail loghead)))) 
  )


;(local (include-book "arithmetic-5/lib/floor-mod/top" :dir :system))
;(local (include-book "arithmetic-3/top" :dir :system))

;(local (include-book "std/lists/append" :dir :system))

(defthm sbe-add-is-sle-add
  (implies (and (jint-listp xs)
                (jint-listp ys)
                (add-carry-p carry)
                (<= xi (len xs))
                (<= yi (len ys)))
           (equal (sbe-add-i xs ys carry xi yi)
                  (rev (sle-add-i (rev xs) (rev ys) carry xi yi))))
  :hints (("Goal" :in-theory (disable add-jint add-jint-carry))
          ("Subgoal *1/2.6'" :expand (sle-add-i (rev xs) (rev ys) 1 xi yi))
          ("Subgoal *1/2.5'" :expand (sle-add-i (rev xs) (rev ys) 0 xi yi))
          ("Subgoal *1/2.4'" :use (:instance add-jint-carry-is-0-or-1-3 (carry 1)))
          ("Subgoal *1/2.3'" :use (:instance add-jint-carry-is-0-or-1-3 (carry 0)))                   
          ("Subgoal *1/2.2'" :expand (sle-add-i (rev xs) (rev ys) 1 xi yi))
          ("Subgoal *1/2.1'" :expand (sle-add-i (rev xs) (rev ys) 0 xi yi))))

(defun sle-add-w (xs ys carry)
  (sle-add-i xs ys carry (len xs) (len ys)))

(defun sbe-add-w (xs ys carry)
  (sbe-add-i xs ys carry (len xs) (len ys)))

(defthm sbe-w-is-sle-add-w
  (implies (and (jint-listp xs)
                (jint-listp ys)
                (add-carry-p carry))
           (equal (sbe-add-w xs ys carry)
                  (rev (sle-add-w (rev xs) (rev ys) carry)))))

(defun sle-to-nat-w (xs)
  (sle-to-nat-i xs (len xs)))

(defun sbe-to-nat-w (xs)
  (sbe-to-nat-i xs (len xs)))

(defthm sbe-to-nat-w-is-sle-to-nat-w
  (implies (jint-listp xs)
           (equal (sbe-to-nat-w xs)
                  (sle-to-nat-w (rev xs)))))

(defthm sle-add-w-works
  (implies (and (jint-listp xs)
                (jint-listp ys)
                (add-carry-p carry))  
           (equal (sle-to-nat-w (sle-add-w xs ys carry))
                  (+ (sle-to-nat-w xs)
                     (sle-to-nat-w ys)
                     carry))))

(defthm sle-add-w-type
  (implies (and (jint-listp xs)
                (jint-listp ys)
                (add-carry-p carry))
           (jint-listp (sle-add-w xs ys carry)))
  :rule-classes :type-prescription)

(defthm rev-jint-listp-is-jint-listp
  (implies (jint-listp xs)
           (jint-listp (rev xs)))
  :rule-classes :type-prescription)

(defthm sbe-add-w-works
  (implies (and (jint-listp xs)
                (jint-listp ys)
                (add-carry-p carry))  
           (equal (sbe-to-nat-w (sbe-add-w xs ys carry))
                  (+ (sbe-to-nat-w xs)
                     (sbe-to-nat-w ys)
                     carry)))
  :hints 
  (("Goal"
    :in-theory (disable sle-add-w sbe-add-w sle-to-nat-w sbe-to-nat-w))))#|ACL2s-ToDo-Line|#


