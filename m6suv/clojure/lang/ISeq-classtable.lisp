; ISeq-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:51 CDT 2014.
;

(defconst *clojure.lang.ISeq*
 (make-class-def
      '(class "clojure.lang.ISeq"
            "java.lang.Object"
            (constant_pool)
            (fields)
            (methods
                        (method "first"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "next"
                              (parameters )
                              (returntype . (class "clojure.lang.ISeq"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "more"
                              (parameters )
                              (returntype . (class "clojure.lang.ISeq"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "cons"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "clojure.lang.ISeq"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code)))
            (interfaces "clojure.lang.IPersistentCollection")
            (accessflags  *abstract*  *interface*  *public* )
            (attributes
              (attribute "SourceFile")))))


(defconst *ISeq-class-table*
  (make-static-class-decls 
   *clojure.lang.ISeq*))

(defconst *package-name-map* 
  ("clojure.lang.ISeq" . "clojure.lang"))
