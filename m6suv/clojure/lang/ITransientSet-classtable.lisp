; ITransientSet-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:51 CDT 2014.
;

(defconst *clojure.lang.ITransientSet*
 (make-class-def
      '(class "clojure.lang.ITransientSet"
            "java.lang.Object"
            (constant_pool)
            (fields)
            (methods
                        (method "disjoin"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "clojure.lang.ITransientSet"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "contains"
                              (parameters (class "java.lang.Object"))
                              (returntype . boolean)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "get"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code)))
            (interfaces "clojure.lang.ITransientCollection" "clojure.lang.Counted")
            (accessflags  *abstract*  *interface*  *public* )
            (attributes
              (attribute "SourceFile")))))


(defconst *ITransientSet-class-table*
  (make-static-class-decls 
   *clojure.lang.ITransientSet*))

(defconst *package-name-map* 
  ("clojure.lang.ITransientSet" . "clojure.lang"))

