; ITransientVector-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:51 CDT 2014.
;

(defconst *clojure.lang.ITransientVector*
 (make-class-def
      '(class "clojure.lang.ITransientVector"
            "java.lang.Object"
            (constant_pool)
            (fields)
            (methods
                        (method "assocN"
                              (parameters int (class "java.lang.Object"))
                              (returntype . (class "clojure.lang.ITransientVector"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "pop"
                              (parameters )
                              (returntype . (class "clojure.lang.ITransientVector"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code)))
            (interfaces "clojure.lang.ITransientAssociative" "clojure.lang.Indexed")
            (accessflags  *abstract*  *interface*  *public* )
            (attributes
              (attribute "SourceFile")))))


(defconst *ITransientVector-class-table*
  (make-static-class-decls 
   *clojure.lang.ITransientVector*))

(defconst *package-name-map* 
  ("clojure.lang.ITransientVector" . "clojure.lang"))

