#|$ACL2s-Preamble$;
(include-book "mc")

#|

Utilities is a set of functions and theorems adopted from M5's 
utility book by J Moore. The work is modified to address the 
differences between M5 and MC, mostly by removing references to
threads. 

As I've worked with this book, I've added additional comments to  
remind myself of the purpose by some of the pieces.

|#

(acl2::begin-book t :ttags :all);$ACL2s-Preamble$|#


(in-package "MC")

(defthm states
  (and (equal (call-stack (make-state cs h c)) cs)
       (equal (heap (make-state tt h c)) h)
       (equal (class-table (make-state tt h c)) c)))

(in-theory (disable make-state call-stack heap class-table))

(defthm frames
  (and
   (equal (pc (make-frame pc l s prog cur-class))
          pc)
   (equal (locals (make-frame pc l s prog cur-class))
          l)
   (equal (stack (make-frame pc l s prog cur-class))
          s)
   (equal (program (make-frame pc l s prog cur-class))
          prog)
   (equal (cur-class (make-frame pc l s prog cur-class))
          cur-class)))

(in-theory
 (disable make-frame pc locals stack program cur-class))

(defthm stacks
  (and (equal (top (push x s)) x)
       (equal (pop (push x s)) s)))

(in-theory (disable push top pop))

(defthm assoc-equal-bind
  (equal (assoc-equal key1 (bind key2 val alist))
         (if (equal key1 key2)
             (cons key1 val)
           (assoc-equal key1 alist))))

(defthm bind-bind
  (equal (bind x v (bind x w a))
         (bind x v a)))

(defthm bind-formals-opener
  (implies (and (integerp n)
                (<= 0 n))
           (equal (bind-formals (+ 1 n) stack)
                  (cons (top stack)
                        (bind-formals n (pop stack))))))

(defthm nth-opener
  (and (equal (nth 0 lst) (car lst))
       (implies (and (integerp n)
                     (<= 0 n))
                (equal (nth (+ 1 n) lst)
                       (nth n (cdr lst))))))

(in-theory (disable nth))

(defthm step-opener
  (implies (consp (next-inst s))
           (equal (step s)
                  (do-inst (next-inst s) s)))
  :hints (("Goal" :in-theory (disable do-inst))))

(in-theory (disable step))

(defthm popn-opener
  (implies (and (integerp n)
                (<= 0 n))
           (equal (popn (+ 1 n) stack)
                  (popn n (pop stack)))))

(defthm run-opener
  (implies (and (integerp x)
                (not (zp x)))
           (equal (step-n x s)
                  (step-n (1- x) (step s))))
  :hints (("Goal" :in-theory (disable step))))

;;

(defun loaded? (class-table class-name class-decl)
  (equal (bound? class-name class-table) class-decl))

(defconst *java.lang.Object*
  (bound? "java.lang.Object" (base-class-def)))

(defun |Object|-loaded? (class-table)
  (loaded? class-table
           "java.lang.Object"
           *java.lang.Object*))

(defthm |Object:<init>|-method
  (implies 
   (|Object|-loaded? (class-table s))
   (equal (lookup-method "<init>"
                         "java.lang.Object"
                         (class-table s))
          (bound? "<init>" (class-decl-methods *java.lang.Object*)))))

(defthm build-Object-instance-data
  (implies (|Object|-loaded? (class-table s))
           (equal (build-immediate-instance-data "java.lang.Object"
                                                 (class-table s))
                  (list "java.lang.Object"))))

(in-theory (disable |Object|-loaded?))

(defun set-instance-fields (class fields values instance)
  (if (endp fields)
    instance
    (set-instance-fields class
                         (cdr fields)
                         (cdr values)
                         (set-instance-field class
                                             (car fields)
                                             (car values)
                                             instance))))

(defun j-instance-classname (instance)
  (caar instance))

(defun poised-to-new (s classname)
  (equal (next-inst s) 
         (list 'new classname)))

(defun j-type-p (class ref heap)
  (let* ((instance (deref ref heap))
         (class-name (j-instance-classname instance)))
    (equal class-name class)))

(defthmd j-type-equal-if-equal
  (implies (and (j-type-p class1 ref heap)
                (not (equal class1 class2)))
           (not (j-type-p class2 ref heap))))#|ACL2s-ToDo-Line|#
