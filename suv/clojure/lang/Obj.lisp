#|$ACL2s-Preamble$;
(include-book "../../../mc/mc")
(include-book "../../../mc/utilities")

(acl2::begin-book t :ttags :all);$ACL2s-Preamble$|#


(in-package "MC")

(defconst *clojure.lang.Obj-<init>*
  '("<init>" NIL
    (ALOAD_0)
    (INVOKESPECIAL "java.lang.Object" "<init>" 0)
    (ALOAD_0)
    (ACONST_NULL)
    (PUTFIELD "clojure.lang.Obj" "_meta" NIL)
    (RETURN)))

(defconst *clojure.lang.Obj*
  (make-class-decl
   ; class name
  "clojure.lang.Obj"
  '("java.lang.Object")
  '()
  '("_meta")
  '()
  '()
  (list *clojure.lang.Obj-<init>*)
  '(REF -1)))

(defun |Obj|-loaded? (class-table)
  (and (|Object|-loaded? class-table)
       (loaded? class-table 
                "clojure.lang.Obj" 
                *clojure.lang.Obj*)))

(defthm |Obj:<init>|-method
  (implies 
   (|Obj|-loaded? (class-table s))
   (equal (lookup-method "<init>"
                         "clojure.lang.Obj"
                         (class-table s))
          *clojure.lang.Obj-<init>*)))

(defthm build-Obj-instance-data
  (implies (|Obj|-loaded? (class-table s))
           (equal (build-immediate-instance-data "clojure.lang.Obj"
                                                 (class-table s))
                  (list "clojure.lang.Obj"
                        (cons "_meta" 0)))))

(defthm |Obj|-dep
  (implies (|Obj|-loaded? (class-table s))
           (|Object|-loaded? (class-table s)))
  :rule-classes :forward-chaining)

(in-theory (disable |Obj|-loaded?))#|ACL2s-ToDo-Line|#
