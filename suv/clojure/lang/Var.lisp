#|$ACL2s-Preamble$;
(include-book "../../../mc/mc")
(include-book "../../../mc/utilities")
(include-book "../../big-step")

(acl2::begin-book t :ttags :all);$ACL2s-Preamble$|#


(in-package "MC")

(defconst *clojure.lang.Var-getRawRoot*
  '("getRawRoot" NIL
    (ALOAD_0)
    (GETFIELD "clojure.lang.Var" "root" NIL)
    (ARETURN)))

(defconst *clojure.lang.Var*
  (make-class-decl
   "clojure.lang.Var"
   '("clojure.lang.ARef"
     "clojure.lang.AReference")
   '("clojure.lang.IFn" 
     "clojure.lang.IRef"
     "clojure.lang.IDeref"
     "clojure.lang.Settable"
     "clojure.lang.IReference"
     "clojure.lang.IMeta")
   '("root")
   '()
   '()   
    (list
     *clojure.lang.Var-getRawRoot*
    )
    '(REF -1)))

(defun |Var|-loaded? (class-table)
  (loaded? class-table
           "clojure.lang.Var"
           *clojure.lang.Var*))

(defthm |Var:getRawRoot|-method
  (implies 
   (|Var|-loaded? (class-table s))
   (equal (lookup-method "getRawRoot"
                         "clojure.lang.Var"
                         (class-table s))
          *clojure.lang.Var-getRawRoot*)))

(defun |Var:root|-get (ref heap)
  (let* ((instance (deref ref heap)))
    (field-value "clojure.lang.Var" 
                 "root" 
                 instance)))

(in-theory (disable |Var|-loaded?))

(defun |Var|-p (ref heap)
  (and (not (nullrefp ref))
       (j-type-p "clojure.lang.Var" 
                 ref 
                 heap)))  

(defun |Var:getRawRoot|-poised (s)
  (let* ((ref (top (stack (top-frame s))))
         (heap (heap s)))
    (and 
     (|Var|-p ref heap)
     (equal (next-inst s) 
            '(INVOKEVIRTUAL "clojure.lang.Var" 
                            "getRawRoot" 
                            0)))))

(local (in-theory (enable
    ->-execute-ALOAD_0
    ->-execute-ARETURN
    ->-execute-GETFIELD
    ->-execute-INVOKEVIRTUAL)))

(defthm |Var:getRawRoot|-is-root
  (implies 
   (and (|Var|-loaded? (class-table s))
        (|Var:getRawRoot|-poised s)
        (|Var|-p (top (stack (top-frame s))) 
                 (heap s)))
   (-> s
       (modify s
               :pc (+ 3 (pc (top-frame s)))
               :stack (push (|Var:root|-get (top (stack (top-frame s)))
                                            (heap s))
                            (pop (stack (top-frame s))))))))#|ACL2s-ToDo-Line|#
