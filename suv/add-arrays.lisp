(set-gag-mode nil)

(include-book "arithmetic/signed-big-endian")

(include-book "../mc/mc")

(in-package "MC")

(defun bi-longmask (x)
  (logand 4294967295 (long-fix x)))

(encapsulate 
  ()
  
  (local (include-book "arithmetic-5/lib/floor-mod/top" :dir :system))

  (defthm jint-long-fix-identity
    (implies 
     (acl2::jint-p x)
     (equal (long-fix x) x)))
  
  (defthm bi-longmask-is-longmask
    (implies (acl2::jint-p x)
             (equal (bi-longmask x)
                    (acl2::longmask x)))))

(defun new-sum (x y sum)
  (+ (+ (bi-longmask x) (bi-longmask y))
     (lushr sum 32)))

(local (include-book "arithmetic-5/lib/floor-mod/top" :dir :system))
;(include-book "std/basic/inductions" :dir :system)

;(local (include-book "coi/super-ihs/logext" :dir :system))
;(local (include-book "coi/super-ihs/loghead" :dir :system))

(defthm int-fix-is-logext-loghead
  (implies 
   (integerp x)
   (equal (int-fix x)
          (acl2::logext 32 (acl2::loghead 32 x))))
  :hints 
  (("Goal" :in-theory (enable acl2::loghead
                              acl2::logext
                              acl2::logapp
                              acl2::logbitp))))  

(defthm new-sum-is-add-jint
  (implies 
   (and (acl2::jint-p x)
        (acl2::jint-p y)
        (integerp sum)
        (>= sum 0)
        (<= sum 8589934591))
   (equal (int-fix (new-sum x y sum))
          (acl2::add-jint x y (lushr sum 32))))
  :hints 
  (("Goal" :in-theory (disable acl2::jint-p
                               acl2::longmask
                               int-fix
                               bi-longmask))))

(defun update-result-1 (xs ys result sum xi yi)
  (update-nth 
   xi
   (int-fix (new-sum (nth xi xs) 
                     (nth yi ys) 
                     sum))
   result))

(defthm new-sum-<=-sum-max
  (implies (and (acl2::jint-p x)
                (acl2::jint-p y)
                (integerp sum)
                (>= sum 0)
                (<= sum 8589934591))
           (<= (new-sum x y sum) 8589934591)))

(defthm new-sum-is-positive
  (implies (and (acl2::jint-p x)
                (acl2::jint-p y)
                (integerp sum)
                (>= sum 0)
                (<= sum 8589934591))
           (>= (new-sum x y sum) 0)))

(defthm new-sum-is-integer
  (implies (and (acl2::jint-p x)
                (acl2::jint-p y)
                (integerp sum)
                (>= sum 0)
                (<= sum 8589934591))
           (integerp (new-sum x y sum)))
  :rule-classes :type-prescription)

(defthm new-sum-<=-sum-max-endp
  (implies (and (or (acl2::jint-p x)
                    (endp x))
                (or (acl2::jint-p y)
                    (endp y))
                (integerp sum)
                (>= sum 0)
                (<= sum 8589934591))
           (<= (new-sum x y sum) 8589934591)))

(defthm new-sum-is-positive-endp
  (implies (and (or (acl2::jint-p x)
                    (endp x))
                (or (acl2::jint-p y)
                    (endp y))
                (integerp sum)
                (>= sum 0)
                (<= sum 8589934591))
           (>= (new-sum x y sum) 0)))

(defthm new-sum-is-integer-endp
  (implies (and (or (acl2::jint-p x)
                    (endp x))
                (or (acl2::jint-p y)
                    (endp y))
                (integerp sum)
                (>= sum 0)
                (<= sum 8589934591))
           (integerp (new-sum x y sum)))
  :rule-classes :type-prescription)

;(defthm bi-longmask-less-than-sum-max
;  (implies (and (acl2::jint-p x)
;                (acl2::jint-p y))
;           (<= (+ (bi-longmask x)
;                  (bi-longmask y))
;               8589934590)))

; Pad the rest of the result with 0's 
(defun pad-result (i)
  (if (zp i)
    nil
    (append (pad-result (1- i))
            (list 0))))
    
#|
(defun propogate (xs sum xi step)
  (if (zp step) 
    (pad-result xi)
    (if (zp xi)
      (if (< 0 (lushr sum 32))
        (cons (lushr sum 32) nil)
        nil)
      (append (propogate xs 
                         (new-sum (nth (1- xi) xs) nil sum)
                         (1- xi)
                         (1- step))
              (list (int-fix (new-sum (nth (1- xi) xs) nil sum)))))))

(defun add-mag (xs ys sum xi yi step) 
  (if (zp step) 
    (pad-result xi)
    (if (or (zp xi) (zp yi))
      (propogate (if (zp xi) ys xs)
                 sum
                 (if (zp xi) yi xi)
                 step)      
      (append
       (add-mag xs
                ys   
                (new-sum (nth (1- xi) xs) 
                         (nth (1- yi) ys) 
                         sum)
                (1- xi)
                (1- yi)
                (1- step))
       (list (int-fix (new-sum (nth (1- xi) xs) 
                               (nth (1- yi) ys) 
                               sum)))))))
|#

(defun propogate (xs sum xi)
  (if (zp xi)
    (if (< 0 (lushr sum 32))
      (cons (lushr sum 32) nil)
      nil)
    (append (propogate xs 
                       (new-sum (nth (1- xi) xs) nil sum)
                       (1- xi))
            (list (int-fix (new-sum (nth (1- xi) xs) nil sum))))))

(defun add-mag (xs ys sum xi yi) 
  (if (or (zp xi) (zp yi))
    (propogate (if (zp xi) ys xs)
               sum
               (if (zp xi) yi xi))      
    (append
     (add-mag xs
              ys   
              (new-sum (nth (1- xi) xs) 
                       (nth (1- yi) ys) 
                       sum)
              (1- xi)
              (1- yi))
     (list (int-fix (new-sum (nth (1- xi) xs) 
                             (nth (1- yi) ys) 
                             sum))))))

(defun sum-at (xs ys sum xi yi step) 
  (if (zp step) 
    (int-fix sum)    
    (sum-at xs
            ys
            (new-sum (if (zp xi) nil (nth (1- xi) xs)) 
                     (if (zp yi) nil (nth (1- yi) ys))  
                     sum)
            (1- xi)
            (1- yi)
            (1- step))))
    
(defun add-mag-sumat (xs ys xi) 
  (if (zp xi)
    nil
    (append
     (add-mag-sumat xs
                    ys
;(new-sum (nth (1- xi) xs) 
;         (nth (1- yi) ys) 
;         sum)
                    (1- xi))
     (list (sum-at xs ys 0 (len xs) (len ys) (- (len xs) xi))))))#|ACL2s-ToDo-Line|#

     
(defun R1 (xs ys result xi yi sum)
  (and (acl2::jint-listp xs)
       (acl2::jint-listp ys)
       (< xi (len xs))
       (< yi (len ys))
       (<= yi xi)
       (integerp sum)
       (>= sum 0)
       (<= sum 8589934591) ; 4294967295 + 4294967295
       (= sum (sum-at xs ys 0 (len xs) (len ys) (- (len xs) xi)))
       (equal result 
              (add-mag xs ys 0 (len xs) (len ys) (- (len xs) xi))))) 

(defthm jint-listp-len-gt-1-is-jint
  (implies (and (acl2::jint-listp xs)
                (< 1 (len xs)))
           (acl2::jint-p (cadr xs)))
  :rule-classes :type-prescription)

(defthm len-add-mag
  (implies 
   (and (acl2::jint-listp xs)
        (acl2::jint-listp ys)

(defthm equal-add-mag-case-split
  (implies 
   (and (acl2::jint-listp xs)
        (acl2::jint-listp ys)
        (<= (len ys) (len xs))
        (integerp n)
        (< 0 n)
        (< n (len xs)))
   (equal (nth (1- n) 
               (add-mag xs ys 0 (len xs) (len ys) n))
          (sum-at xs ys 0 (len xs) (len ys) n)))
  :hints 
  (("Goal" 
    :induct (acl2::dec-induct n)
    :in-theory (e/d ()
                    (int-fix-is-logext-loghead
                     acl2::jint-p
                     acl2::jint-listp
                     acl2::longmask
                     acl2::add-jint
                     int-fix
                     bi-longmask
                     new-sum)))))

(defthm equal-add-mag-case-split
  (implies 
   (and (acl2::jint-listp xs)
       (acl2::jint-listp ys)
       (< (len ys) (len xs))
       (< 0 (len xs))
       (integerp n)
       (< 0 n)
       (< n (len xs)))
   (equal (nthcdr (- (len xs) n) 
                  (add-mag xs ys 0 (len xs) (len ys) n))
          (nthcdr (- (len xs) n) 
                  (add-mag xs ys 0 (len xs) (len ys) (1- n)))))
  :hints 
  (("Goal" 
    :induct (acl2::dec-induct xi)
    :in-theory (e/d (nthcdr)
                    (acl2::jint-p
                     acl2::jint-listp
                     acl2::longmask
                     int-fix
                     bi-longmask
                     new-sum)))))
          

(defthm R1-implies-R1
  (implies
   (and (R1 xs ys result xi yi sum)
        (< 0 yi)
        (< 0 xi))
   (R1 xs
       ys
       (update-result-1 xs ys result sum (1- xi) (1- yi))
       (1- xi)
       (1- yi)
       (new-sum (nth xi xs)
                (nth yi ys)
                sum)))
  :hints 
  (("Goal" 
    :induct (acl2::dec-induct xi)
    :in-theory (e/d (update-nth)
                    (acl2::jint-p
                     acl2::jint-listp
                     acl2::longmask
                     int-fix
                     bi-longmask
                     new-sum)))))

(defun expand (carry i e)
  (let* ((nat-i (nfix i))
         (nat-e (nfix e)))
    (if (and (= nat-i nat-e)
             (zp i)
             (< 0 carry))
      (cons carry nil)
      nil)))

(defun propagate (xs carry i e)
  (if (<= (nfix i) (nfix e))
      (expand carry i e)
      (append (propagate xs
                         (add-jint-carry (nth (1- i) xs)                                        
                                         nil                                        
                                         carry)
                         (1- i)
                         e)
              (list (add-jint (nth (1- i) xs) nil carry)))))

(defthm prop-expand-is-sbe-propogate
  (implies (and (jint-listp xs)
                (integerp carry)
                (<= i (len xs)))
           (equal (propagate xs carry i 0)
                  (sbe-propogate-i xs carry i))))

(defun add-mag (xs ys carry xi yi xe ye)
  (let* ((nat-xi (nfix xi))
         (nat-yi (nfix yi))
         (nat-xe (nfix xe))
         (nat-ye (nfix ye))
         (endx (<= nat-xi nat-xe))
         (endy (<= nat-yi nat-ye)))
  (if (or endx endy) 
    (propagate (if endx xs ys) 
               carry 
               (if endx xi yi)
               (if endx xe ye))
    (append
          (add-mag xs
                   ys   
                   (add-jint-carry (nth (1- xi) xs) 
                                   (nth (1- yi) ys) 
                                   carry)
                   (1- xi)
                   (1- yi)
                   xe
                   ye)          
          (list (add-jint (nth (1- xi) xs) 
                          (nth (1- yi) ys) 
                          carry))))))

(defthm prop-increment
  (implies (and (jint-listp xs)                
                (or (= carry 0)
                    (= carry 1))                
                (<= xi (len xs))
                (< 0 xe))
           (equal (propagate xs carry xi (1- xe)) 
                  (append (propagate xs carry xi xe)
                          (list (add-jint (nth (1- xe) xs)                                  
                                          nil                                  
                                          carry)))))
    :hints 
    (("Goal" :in-theory (disable add-jint add-jint-carry 
                                 sbe-add-is-sle-add
                                 sbe-w-is-sle-add-w
                                 sbe-propogate-is-sle-propogate))))
    ("Subgoal *1/2.2'" :use (:instance add-jint-carry-is-0-or-1-3 (carry 0)))
    ("Subgoal *1/2.1'" :use (:instance add-jint-carry-is-0-or-1-3 (carry 1)))))

(defthm prop-increment
  (implies (and (jint-listp xs)
                (jint-listp ys)
                (or (= carry 0)
                    (= carry 1))                
                (<= xi (len xs))
                (<= yi (len ys))
                (<= (len ys) (len xs))
                (< 0 xe)
                (zp ye))
           (equal (add-mag xs ys carry xi yi (1- xe) ye) 
                  (append (list (add-jint (nth (1- xe) xs)                                  
                                          nil                                  
                                          carry))                       
                          (add-mag xs ys carry xi yi xe ye)))))
    :hints 
    (("Goal" :in-theory (disable add-jint add-jint-carry 
                                 sbe-add-is-sle-add
                                 sbe-w-is-sle-add-w
                                 sbe-propogate-is-sle-propogate))))
    ("Subgoal *1/2.2'" :use (:instance add-jint-carry-is-0-or-1-3 (carry 0)))
    ("Subgoal *1/2.1'" :use (:instance add-jint-carry-is-0-or-1-3 (carry 1)))))


(defthm add-mag-is-add
  (implies (and (jint-listp xs)
                (jint-listp ys)
                (or (= carry 0)
                    (= carry 1))                
                (<= xi (len xs))
                (<= yi (len ys)))
           (equal (add-mag xs ys carry xi yi 0 0) 
                  (sbe-add-i xs ys carry xi yi)))
    :hints 
    (("Goal" :in-theory (disable add-jint add-jint-carry 
                                 sbe-add-is-sle-add
                                 sbe-w-is-sle-add-w
                                 sbe-propogate-is-sle-propogate))
    ("Subgoal *1/2.2'" :use (:instance add-jint-carry-is-0-or-1-3 (carry 0)))
    ("Subgoal *1/2.1'" :use (:instance add-jint-carry-is-0-or-1-3 (carry 1)))))


(defun post_add (x0 y0 result xIdx yIdx carry)
  (and (zp xIdx)
       (zp yIdx)
       (zp carry)
       (equal (add-mag x0 y0 carry (len x0) (len y0) xIdx yIdx)       
              result)))

;; The third loop invariant
(defun R3 (x0 y0 result xStart yStart xIdx yIdx carry)
  (and (zp yIdx)
       (zp carry)
       (<= xStart (len x0))
       (<= yStart (len y0))
       (equal (add-mag x0 y0 carry xStart yStart xIdx yIdx)
              result)))

(defthm R3-implies-post_add
  (implies (and (equal xStart (len x0))
                (equal yStart (len y0))
                (R3 x0 y0 result xStart yStart xIdx yIdx carry)
                (zp xIdx))
           (post_add x0 y0 result xIdx yIdx carry)))

(defthm R3-implies-R3
  (implies (and (R3 x0 y0 result xStart yStart xIdx yIdx carry)
                (jint-listp x0)
                (jint-listp y0)
                (jint-listp result))
           (R3 x0 
               y0
               (append (add-jint (nth (1- xIdx) x0) 
                                 nil 
                                 carry)
                       result)
               ;(append (list (nth (1- xIdx) x0)) 
               ;        result)
               xStart
               yStart
               (1- xIdx)
               yIdx 
               carry))
  :hints 
  (("Goal" :in-theory (disable add-jint add-jint-carry 
                               sbe-add-is-sle-add
                               sbe-w-is-sle-add-w
                               sbe-propogate-is-sle-propogate))))

(defun swap-p (x0 y0 xs ys)
  (if (< (len x0) (len y0))
         (and (equal ys x0)
              (equal xs y0))
         (and (equal ys y0)
              (equal xs x0))))

(defun pre_add (x0 y0)
  (and (jint-listp x0)
       (jint-listp y0)))    

(defun post_swap (x0 y0 xs ys)
  (and (jint-listp xs)
       (jint-listp ys)
       (<= (len ys) (len xs))
       (swap-p x0 y0 xs ys)))

(defthm pre-add_to_post-swap
  (implies (pre_add x0 y0)
           (post_swap x0
                      y0
                      (if (< (len x0) (len y0)) y0 x0)
                      (if (< (len x0) (len y0)) x0 y0))))

(defun Q (x0 y0 xs ys result)
  (and (jint-listp x0)
       (jint-listp y0)
       (jint-listp result)
       (swap-p x0 y0 xs ys)
       (equal (sbe-to-nat-w result)
              (+ (sbe-to-nat-w x0)
                 (sbe-to-nat-w y0)))))
    
(defthm add-mags-adds
  (implies (#| Top of stack is 2 arrays|#)
           (#| Top of stack is array added |#)))

(defun R3 (x0 y0 xs ys result)
  (and (jint-listp x0)
       (jint-listp y0)
       (jint-listp result)
       (

(defun R1 (x0 y0 xs ys rs xIdx yIdx sum)
  (and (jint-listp xs)
       (jint-listp ys)
       (jint-listp rs)
       (<= (len ys) (len xs))
       (swap-p x0 y0 xs ys)
       (

(defun post_add (x0 y0 result xIdx yIdx)
  (and (zp xIdx)
       (zp yIdx)          
       (equal (+ (sbe-to-nat-w x0) (sbe-to-nat-w y0))       
              (sbe-to-nat-w result))))