; IPersistentMap-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:51 CDT 2014.
;

(defconst *clojure.lang.IPersistentMap*
 (make-class-def
      '(class "clojure.lang.IPersistentMap"
            "java.lang.Object"
            (constant_pool)
            (fields)
            (methods
                        (method "assoc"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "clojure.lang.IPersistentMap"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "assocEx"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "clojure.lang.IPersistentMap"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "without"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "clojure.lang.IPersistentMap"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code)))
            (interfaces "java.lang.Iterable" "clojure.lang.Associative" "clojure.lang.Counted")
            (accessflags  *abstract*  *interface*  *public* )
            (attributes
              (attribute "SourceFile")))))


(defconst *IPersistentMap-class-table*
  (make-static-class-decls 
   *clojure.lang.IPersistentMap*))

(defconst *package-name-map* 
  ("clojure.lang.IPersistentMap" . "clojure.lang"))
