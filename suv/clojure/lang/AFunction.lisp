#|$ACL2s-Preamble$;
(include-book "../../../mc/mc")

(acl2::begin-book t :ttags :all);$ACL2s-Preamble$|#


(in-package "MC")

(defconst *clojure.lang.AFunction*
  (make-class-decl
   "clojure.lang.AFunction"
   '("clojure.lang.AFn")
   '(
     "clojure.lang.IObj" 
     "java.util.Comparator" 
     "clojure.lang.Fn" 
     "java.io.Serializable"
     ; AFn interfaces
     "clojure.lang.IFn"
    )
   '()
   '("__methodImplCache")
   '()

   (list
    '("<init>" NIL
      (ALOAD_0)
      (INVOKESPECIAL "clojure.lang.AFn" "<init>" 0)
      (RETURN)))
   '(REF -1)))#|ACL2s-ToDo-Line|#
