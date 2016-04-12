;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A Little Endian version of the loop invariants for
;; Verifying BigInteger
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
;; (little-endian version)
;; propagate does not expand the array even if a carry 
;; still exists
(defun propagate-array (xs i carry)
  (declare (xargs :measure (nfix (- (len xs) i))))
  (if (zp (- (len xs) i))
    nil
    (let* ((x (nth i xs))
           (r (if carry (1+ x) x))
           (nc (if carry (zp r) nil)))
      (cons r (propagate-array xs (1+ i) nc)))))

(defun add-arrays (xs ys xi yi sum)
  (declare (xargs :measure (nfix (- (len ys) yi))))
  (if (zp (- (len ys) yi))
    (propagate-array xs 
                     xi
                     (not (zp (lushr sum 32))))
    (let* ((x (nth xi xs))
           (y (nth yi ys))
           (ns (new-sum x y sum)))
      (cons (int-fix ns)
            (add-arrays xs ys (1+ xi) (1+ yi) ns))))) 

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

(local (include-book "std/lists/nthcdr" :dir :system))

(defthmd nthcdr-cdr-add-arrays
  (implies
   (and (natp i)
        (equal (nthcdr i (add-arrays xs ys xi yi sum0))
               (add-arrays xs ys (+ xi i) (+ yi i) sumi)))
   (equal (nthcdr (1+ i) (add-arrays xs ys xi yi sum0))
          (cdr (add-arrays xs ys (+ xi i) (+ yi i) sumi)))))

(defthm new-sum-is-integerp
  (integerp (new-sum x y s))
  :hints (("Goal" :in-theory (enable new-sum bi-longmask lushr))))

(defthm lushr-is-integerp  
  (integerp (lushr x 32))
  :hints (("Goal" :in-theory (enable lushr))))

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

(defun add-mag (xs ys)
  (add-arrays xs ys 0 0 0))

;; xs     := the x array
;; ys     := the y array
;; result := the result array
;; xi     := xIndex
;; yi     := yIndex
;; sumi   := the sum at the beginning of the iteration
;; sumn   := the sum now
(defun R1 (xs ys result xi yi sumi)
  (and (int-listp xs)
       (int-listp ys)       
       (<= (len ys) (len xs))
       
       (natp xi)
       (natp yi)
       (<= xi (len xs))
       (<= yi (len ys))
       
       (sump sumi)
       
       (equal (take xi (add-mag xs ys))
              (take xi result))      
       
       (equal (nthcdr xi (add-mag xs ys))                                            
              (add-arrays xs ys xi yi sumi))))

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
  :hints (("Goal" :in-theory (disable add-mag))))

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
     (equal (car (nthcdr xi (add-mag xs ys)))           
            (int-fix next-sum))))
  :hints 
  (("Goal" 
    :do-not-induct t
    :in-theory (disable add-mag
                        ACL2::car-of-nthcdr))))

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
                    (list (int-fix next-sum))))))
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
                    (posp natp add-mag)))))#|ACL2s-ToDo-Line|#
