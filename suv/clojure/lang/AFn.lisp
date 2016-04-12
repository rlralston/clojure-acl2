#|$ACL2s-Preamble$;
(include-book "../../../mc/mc")

(acl2::begin-book t :ttags :all);$ACL2s-Preamble$|#


(in-package "MC")

(defconst *clojure.lang.AFn*
  (make-class-decl
   "clojure.lang.AFn"
   '("java.lang.Object")
   '("clojure.lang.IFn")
   '()
   '((STRING "__"))
   '()
   (list
     '("<init>" NIL
       (ALOAD_0)
       (INVOKESPECIAL "java.lang.Object" "<init>" 0)
       (RETURN)))
   '(REF -1)))#|ACL2s-ToDo-Line|#
