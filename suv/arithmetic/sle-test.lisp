(set-gag-mode nil)

(include-book "unsigned-little-endian")

(in-package "ACL2")

(defun carry-at (i xs ys carry)
  (if (zp i)
    carry
    (carry-at (1- i)
              xs
              ys
              (add-uint-carry (car xs) (car ys) carry))))

(defthm ule-add-car
  (implies (and (uint-listp xs)
                (uint-listp ys)
                (consp xs)
                (consp ys))
   (equal (car (ule-add xs ys carry))
          (add-uint (car xs) (car ys) carry))))

(defthm ule-add-carry
  (implies 
   (and (uint-listp xs)
        (consp xs)        
        (uint-listp ys)   
        (consp ys)
        (add-carry-p carry))
   (equal (cdr (ule-add xs 
                        ys 
                        carry))
          (ule-add (cdr xs)
                   (cdr ys)
                   (add-uint-carry (car xs) (car ys) carry))))
  :hints
  (("Goal"
    :in-theory (disable add-uint-carry
                        uint-p
                        add-carry-p))))

(defthm nthcdr-is-nth-nthcdr
  (implies 
   (and (integerp i)
        (< 0 i)
        (< i (len xs)))
   (equal (nthcdr i xs)
          (cons (nth i xs)
                (nthcdr (1+ i) xs)))))

(defthm not-cadr-not-consp
  (implies (not (consp ys))
           (not (cadr ys))))

(defthm nthcdr-grows
  (implies 
   (and (equal (nthcdr (1+ i) xs)
               (nthcdr (1+ i) ys))
        (equal (nth i xs)
               (nth i ys))
        (integerp i)
        (< 0 i)
        (not (equal i (len xs)))
        (equal (len xs) (len ys)))
   (equal (nthcdr i xs)
          (nthcdr i ys))))#|ACL2s-ToDo-Line|#


(defthm ule-add-carry-at
  (implies 
   (and (uint-listp xs)
        (consp xs)        
        (uint-listp ys)   
        (consp ys)
        (add-carry-p carry))
   (equal (carry-at i xs ys carry)
          (add-uint-carry (car xs) (car ys) carry))))
  :hints
  (("Goal"
    :in-theory (disable add-uint-carry
                        uint-p
                        add-carry-p))))