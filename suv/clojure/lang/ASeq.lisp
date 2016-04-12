#|$ACL2s-Preamble$;
(include-book "../../../mc/mc")

(include-book "Obj")

(acl2::begin-book t :ttags :all);$ACL2s-Preamble$|#


(in-package "MC")

(defconst *clojure.lang.ASeq-<init>*
  '("<init>" NIL
    (ALOAD_0)
    (INVOKESPECIAL "clojure.lang.Obj" "<init>" 0)
    (ALOAD_0)
    (ICONST_M1)
    (PUTFIELD "clojure.lang.ASeq" "_hash" NIL)
    (ALOAD_0)
    (ICONST_M1)
    (PUTFIELD "clojure.lang.ASeq" "_hasheq" NIL)
    (RETURN)))

(defconst *clojure.lang.ASeq-more*
  '("more" NIL
    (ALOAD_0)
    (INVOKEVIRTUAL "clojure.lang.ASeq" "next" 0)
    (ASTORE_1)
    (ALOAD_1)
    (IFNONNULL 7)
    (GETSTATIC "clojure.lang.PersistentList" "EMPTY" NIL)
    (ARETURN)
    (ALOAD_1)
    (ARETURN)))

(defconst *clojure.lang.ASeq*
  (make-class-decl
   ; class name
   "clojure.lang.ASeq"
  '("clojure.lang.Obj")
  '("clojure.lang.ISeq" "clojure.lang.Sequential" "java.util.List" "java.io.Serializable" "clojure.lang.IHashEq")
  '("_hash" "_hasheq")
  '()
  '()

  (list
   *clojure.lang.ASeq-<init>*
   *clojure.lang.ASeq-more*
   )
  '(REF -1)))

(defun |ASeq|-loaded? (class-table)
  (and (|Obj|-loaded? class-table)
       (loaded? class-table 
                "clojure.lang.ASeq" 
                *clojure.lang.Aseq*)))

(defthm |ASeq:<init>|-method
  (implies 
   (|ASeq|-loaded? (class-table s))
   (equal (lookup-method "<init>"
                         "clojure.lang.ASeq"
                         (class-table s))
          *clojure.lang.ASeq-<init>*)))

(defthm |ASeq:more|-method
  (implies 
   (and (|ASeq|-loaded? (class-table s))
        (equal (car classes) "clojure.lang.ASeq"))
   (equal (lookup-method-in-superclasses "more"
                                         classes
                                         (class-table s))
          *clojure.lang.ASeq-more*)))

(defthm build-ASeq-instance-data
  (implies (|ASeq|-loaded? (class-table s))
           (equal (build-immediate-instance-data "clojure.lang.ASeq"
                                                 (class-table s))
                  (list "clojure.lang.ASeq"
                        (cons "_hash" 0) 
                        (cons "_hasheq"  0)))))

(defthm |ASeq|-dep
  (implies (|ASeq|-loaded? (class-table s))
           (|Obj|-loaded? (class-table s)))
  :rule-classes :forward-chaining)

(in-theory (disable |ASeq|-loaded?))#|ACL2s-ToDo-Line|#
