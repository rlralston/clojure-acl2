;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A Big Endian version of the loop invariants for
;; Verifying BigInteger
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
(defun jlong-p (x)
  (and (integerp x)
       (>= x 0)
       (<= x 8589934591)))

(defun jlong-listp (xs)
  (cond ((atom xs) (eq xs nil))
        (t (and (jlong-p (car xs))
                (jlong-listp (cdr xs))))))

|#

(set-gag-mode nil)

(include-book "../../mc/mc")

(in-package "MC")

(defun intp (x)
  (and (integerp x)
       (<= (- (expt 2 31)) x)
       (< x (expt 2 31))))

(defun sump (x)
  (and (integerp x)
       (<= 0 x)
       (< x (expt 2 33))))

(defun int-listp (xs)
  (cond ((atom xs) (eq xs nil))
        (t (and (intp (car xs))
                (int-listp (cdr xs))))))

(defun bi-longmask (x)
  (logand 4294967295 (long-fix x)))

(defun new-sum (x y sum)
  (+ (bi-longmask x) 
     (bi-longmask y)
     (lushr sum 32)))

(local (include-book "std/lists/append" :dir :system))
(local (include-book "std/lists/take" :dir :system))
(local (include-book "std/lists/nth" :dir :system))
(local (include-book "std/lists/nthcdr" :dir :system))
(local (include-book "std/lists/update-nth" :dir :system))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (big-endian version)
;; propagate does not expand the array even if a carry 
;; still exists
(defun propagate-array (xs i carry)
  (if (zp i)
    nil
    (let* 
      ((x (nth (1- i) xs))
       (r (if carry (1+ x) x))
       (nc (if carry (zp r) nil)))
      (append (propagate-array xs (1- i) nc) (list r)))))

(defun add-arrays (xs ys xi yi sum)
  (if (zp yi)
    (propagate-array xs 
                     xi
                     (not (zp (lushr sum 32))))
    (let* ((x (nth (1- xi) xs))
           (y (nth (1- yi) ys))
           (ns (new-sum x y sum)))
      (append (add-arrays xs ys (1- xi) (1- yi) ns)
              (list (int-fix ns)))))) 

(defun add-mag (xs ys)
  (add-arrays xs ys (len xs) (len ys) 0))

(in-theory (disable lushr long-fix int-fix bi-longmask new-sum intp sump))

(encapsulate
  ()
  
  (local (include-book "arithmetic-5/lib/floor-mod/top" :dir :system))

  (defthm long-fix-int
    (implies (intp x)
             (equal (long-fix x) x))
    :hints (("Goal" :in-theory (enable long-fix intp))))

  (defthm new-sum-is-sump
    (implies 
     (and (or (intp x) (endp x))
          (or (intp y) (endp y))
          (sump sum))
     (sump (new-sum x y sum)))
    :rule-classes (:rewrite :type-prescription)
    :hints (("Goal" :in-theory (enable new-sum sump intp bi-longmask
                                       lushr long-fix)))))

(defthm nth-of-int-listp-is-intp
  (implies (and (int-listp xs)
                (<= 0 i)
                (< i (len xs)))
           (intp (nth i xs)))
  :rule-classes :type-prescription)

(defthm new-sum-is-integerp
  (integerp (new-sum x y s))
  :hints (("Goal" :in-theory (enable new-sum bi-longmask lushr))))

(defthm lushr-is-integerp  
  (integerp (lushr x 32))
  :hints (("Goal" :in-theory (enable lushr))))

(defthm len-of-propagate-is-xi
  (implies (natp xi)
           (equal (len (propagate-array xs xi c)) xi)))

(defthm len-of-add-arrays-is-xi
  (implies 
   (and 
    (natp xi)
    (<= yi xi))
   (equal (len (add-arrays xs ys xi yi sum)) xi)))

(defthm take-xi-is-add-arrays
  (implies 
   (and (natp xi)
        (<= yi xi))
   (equal (take xi (add-arrays xs ys xi yi sum0))
          (add-arrays xs ys xi yi sum0))))
 
(defthm take-xi-1-is-add-arrays
  (implies 
   (and (natp xi)
        (posp yi)
        (<= yi xi))
   (let* ((sumn (new-sum (nth (1- xi) xs)
                         (nth (1- yi) ys)
                         sum0)))
     (equal (take (1- xi) (add-arrays xs ys xi yi sum0))
            (add-arrays xs ys (1- xi) (1- yi) sumn)))))

(defthmd take-less-append
  (equal (take (1- n) xs) 
         (take (1- n) (take n xs))))
          
(defthmd take-less-helper
  (implies  
   (and (natp xi)
        (posp yi)
        (<= yi xi))
     (equal (take (1- xi) (add-mag xs ys))
            (take (1- xi) (take xi (add-mag xs ys))))))

(defthm take-less-add-arrays
  (implies 
   (and (natp xi)
        (posp yi)
        (<= yi xi)
        
        (equal (take xi (add-mag xs ys))
               (add-arrays xs ys xi yi sumi)))
   (let* ((sumn (new-sum (nth (1- xi) xs)
                         (nth (1- yi) ys)
                         sumi)))
     (equal (take (1- xi) (add-mag xs ys))
            (add-arrays xs ys (1- xi) (1- yi) sumn))))
  :hints 
  (("Goal"     
    :in-theory (disable add-mag add-arrays)
    :use ((:instance take-xi-1-is-add-arrays
           (sum0 sumi))))))#|ACL2s-ToDo-Line|#


(defthm nthcdr-more-add-arrays
  (implies 
   (and (natp xi)
        (posp yi)
        (<= yi xi)        
        (equal (nthcdr xi (add-mag xs ys))
               (nthcdr xi result)))
   (let* ((sumn (new-sum (nth (1- xi) xs)
                         (nth (1- yi) ys)
                         sumi)))

(defthmd cdr-add-arrays 
  (implies 
   (and (natp yi)
        (< yi (len ys)))
   (let* ((sumn (new-sum (nth xi xs)
                         (nth yi ys)
                         sumi)))
     (equal (cdr (add-arrays xs ys xi yi sumi))
            (add-arrays xs ys (1+ xi) (1+ yi) sumn)))))

(defthmd decompose-add-arrays 
  (implies 
   (and (natp yi)
        (< yi (len ys)))  
   (let* ((sumn (new-sum (nth xi xs)
                         (nth yi ys)
                         sumi)))
     (equal (add-arrays xs ys xi yi sumi)
            (cons (int-fix sumn)
                  (add-arrays xs ys (1+ xi) (1+ yi) sumn))))))


(defthmd nthcdr-cdr-add-arrays
  (implies
   (and (natp i)
        (equal (nthcdr i (add-arrays xs ys xi yi sum0))
               (add-arrays xs ys (+ xi i) (+ yi i) sumi)))
   (equal (nthcdr (1+ i) (add-arrays xs ys xi yi sum0))
          (cdr (add-arrays xs ys (+ xi i) (+ yi i) sumi)))))



(defthmd nthcdr-add-arrays-reduce
  (implies
   (and (natp i)
        (posp yi)
        (< (+ i yi) (len ys))
        (equal (nthcdr i (add-arrays xs ys xi yi sum0))
               (add-arrays xs ys (+ xi i) (+ yi i) sumi)))
   (let* ((sumn (new-sum (nth (+ xi i) xs)
                         (nth (+ yi i) ys)
                         sumi)))
   (equal (nthcdr (1+ i) (add-arrays xs ys xi yi sum0))
          (add-arrays xs ys (1+ (+ xi i)) (1+ (+ yi i)) sumn))))
  :hints
  (("Goal" 
    :do-not-induct t
    :in-theory (disable posp natp)
    :use ((:instance nthcdr-cdr-add-arrays)
          (:instance cdr-add-arrays 
           (xs xs)
           (ys ys)
           (xi (+ xi i))
           (yi (+ yi i)))))))

(defthmd nth-nthcdr
  (implies 
   (and (consp xs)
        (< i (len xs))
        (not (zp i)))
   (equal (append (list (nth i xs))          
                  (nthcdr (1+ i) xs))
          (nthcdr i xs))))

(defthmd take-nthcdr
  (implies
   (and (consp xs)
        (< i (len xs))
        (not (zp i)))
   (equal (append (take i xs)                  
                  (append (list (nth i xs))
                          (nthcdr (1+ i) xs)))          
          xs)))

(defthmd take-nth-nthcdr
  (implies
   (and (consp xs)
        (< i (len xs))
        (not (zp i)))
   (equal (append (append (take i xs)                  
                          (list (nth i xs)))
                  (nthcdr (1+ i) xs))          
          xs)))

(defthmd take-more-nth
  (implies 
   (not (zp i))
   (equal (take i xs)
          (append (take (1- i) xs)
                  (list (nth (1- i) xs))))))    

(defthmd take-more
  (implies 
   (and (consp xs)
        (consp ys)
        (<= i (len ys))
        (<= i (len xs))
        (not (zp i))
        (equal (take i xs) 
               (take i ys)))
  (equal (equal (nth i xs)
                (nth i ys))
         (equal (take (1+ i) xs)
                (take (1+ i) ys)))))

;; xs     := the x array
;; ys     := the y array
;; result := the result array
;; xi     := xIndex
;; yi     := yIndex
;; sumi   := the sum at the beginning of the iteration
;; sumn   := the sum now
(defun R1 (xs ys result xi yi sumi)
  (let* ((index (- (len xs) xi)))
    (and (int-listp xs)
         (int-listp ys)       
         (<= (len ys) (len xs))
         
         (natp xi)
         (natp yi)
         (<= xi (len xs))
         (<= yi (len ys))
         
         (sump sumi)
       
         (equal (take index (add-mag xs ys))
                (add-arrays xs ys xi yi sumi))      
       
         (equal (nthcdr index (add-mag xs ys))
                (nthcdr index result)))))

(local (include-book "std/lists/update-nth" :dir :system))

(defthm i->i+1
  (implies 
   (equal i (- (len xs) xi)) 
   (equal (- (len xs) (1- xi)) (1+ i))))

(defthm nthcdr-cdr-add-mag
  (implies
   (and (natp i)
        (equal (nthcdr i (add-mag xs ys))
               (add-arrays xs ys xi yi sumi)))
   (equal (nthcdr (1+ i) (add-mag xs ys))
          (cdr (add-arrays xs ys xi yi sumi)))))

(defthm R1->nthcdr
  (implies 
   (and (R1 xs ys result xi yi sumi)
        (< yi (len ys))
        (equal xi yi))        
   (let* ((next-sum (new-sum (nth xi xs) 
                             (nth yi ys)
                             sumi)))
   (equal (nthcdr (1+ xi) (add-mag xs ys))
          (add-arrays xs ys (1+ xi) (1+ xi) next-sum))
   
   ))
  :hints 
  (("Goal" 
    :do-not-induct t
    :in-theory (disable posp natp add-mag))))

(defthm take-nth+1
  (implies 
   (and (equal (take i r1)
               (take i r2))
        (equal (nth (1+ i) r1)
               (nth (1+ i) r2)))        
   (equal (take i r1)
          (take i r2))))

(defthm expand-update-nth
  (implies 
   (and (natp i)
        (<= i (len xs)))
   (equal (take (1+ i) (update-nth i v xs))
          (if (zp i)
            (list v)
            (append (take i xs)
                    (list v))))))

(defthm nthcdr->car
  (implies 
   (and (natp yi)
        (< yi (len ys)))
   (let* ((next-sum (new-sum (nth xi xs) 
                             (nth yi ys)
                             sumi)))   
     (equal (car (add-arrays xs ys xi yi sumi))    
            (int-fix next-sum))))
  :hints 
  (("Goal" 
    ;:do-not-induct t
    :in-theory (disable add-mag))))

(defthmd nth-is-car-nthcdr
  (equal (nth i xs)
         (car (nthcdr i xs))))

(defthmd nthcdr->car-nthcdr
  (implies 
   (and (natp xi)
        (natp yi)
        (< yi (len ys))
        (equal (nthcdr xi (add-mag xs ys))                                            
               (add-arrays xs ys xi yi sumi))) 
   (let* ((next-sum (new-sum (nth xi xs) 
                             (nth yi ys)
                             sumi)))   
     (equal (car (nthcdr xi (add-mag xs ys))) ;(nth xi (add-mag xs ys))          
            (int-fix next-sum))))
  :hints 
  (("Goal" 
    :do-not-induct t
    :in-theory (disable add-mag
                        ACL2::car-of-nthcdr)
    )))

(defthm nthcdr->nth
  (implies 
   (and (natp xi)
        (natp yi)
        (< yi (len ys))
        (equal (nthcdr xi (add-mag xs ys))                                            
               (add-arrays xs ys xi yi sumi))) 
   (let* ((next-sum (new-sum (nth xi xs) 
                             (nth yi ys)
                             sumi)))   
     (equal (nth xi (add-mag xs ys))          
            (int-fix next-sum))))
  :hints 
  (("Goal" 
    :do-not-induct t
    :in-theory (disable add-mag)
    :use (nthcdr->car-nthcdr))))

(defthm nthcdr->take
  (implies 
   (and (natp xi)
        (natp yi)
        (< yi (len ys))
        (equal (nthcdr xi (add-mag xs ys))                                            
               (add-arrays xs ys xi yi sumi)))        
   (let* ((next-sum (new-sum (nth xi xs) 
                             (nth yi ys)
                             sumi)))
     (equal (take (1+ xi) (add-mag xs ys))
            (append (take xi (add-mag xs ys))
                    (list (int-fix next-sum))))
   
   ))
  :hints 
  (("Goal" 
    :do-not-induct t
    :in-theory (e/d (take-more-nth) 
                    (posp natp add-mag)))))

(defthm R1->take
  (implies 
   (and (R1 xs ys result xi yi sumi)
        (equal xi yi)
        (< yi (len ys)))
   (let* ((next-sum (new-sum (nth xi xs) 
                             (nth yi ys) 
                             sumi)))
     (equal (take (1+ xi) (add-mag xs ys))
            (take (1+ xi) 
                  (update-nth xi (int-fix next-sum) result)))))         
  :hints 
  (("Goal" 
    :do-not-induct t
    :in-theory (e/d (take-more-nth) 
                    (posp natp add-mag)))))

(defthm R1->R1
  (implies 
   (and (R1 xs ys result xi yi sumi)
        (equal xi yi)
        (< yi (len ys)))
   (let* ((next-sum (new-sum (nth xi xs) 
                             (nth yi ys) 
                             sumi)))
   (R1 xs 
       ys 
       (update-nth xi
                   (int-fix next-sum) 
                   result)
       (1+ xi)
       (1+ yi)
       next-sum)))
  :hints 
  (("Goal" 
    :do-not-induct t
    :in-theory (e/d (take-more-nth) 
                    (posp natp add-mag)))))