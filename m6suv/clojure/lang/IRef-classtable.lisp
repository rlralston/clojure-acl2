; IRef-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:51 CDT 2014.
;

(defconst *clojure.lang.IRef*
 (make-class-def
      '(class "clojure.lang.IRef"
            "java.lang.Object"
            (constant_pool)
            (fields)
            (methods
                        (method "setValidator"
                              (parameters (class "clojure.lang.IFn"))
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getValidator"
                              (parameters )
                              (returntype . (class "clojure.lang.IFn"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getWatches"
                              (parameters )
                              (returntype . (class "clojure.lang.IPersistentMap"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "addWatch"
                              (parameters (class "java.lang.Object") (class "clojure.lang.IFn"))
                              (returntype . (class "clojure.lang.IRef"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "removeWatch"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "clojure.lang.IRef"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code)))
            (interfaces "clojure.lang.IDeref")
            (accessflags  *abstract*  *interface*  *public* )
            (attributes
              (attribute "SourceFile")))))


(defconst *IRef-class-table*
  (make-static-class-decls 
   *clojure.lang.IRef*))

(defconst *package-name-map* 
  ("clojure.lang.IRef" . "clojure.lang"))

