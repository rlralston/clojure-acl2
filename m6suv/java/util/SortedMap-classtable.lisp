; SortedMap-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:48 CDT 2014.
;

(defconst *java.util.SortedMap*
 (make-class-def
      '(class "java.util.SortedMap"
            "java.lang.Object"
            (constant_pool)
            (fields)
            (methods
                        (method "comparator"
                              (parameters )
                              (returntype . (class "java.util.Comparator"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "subMap"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.util.SortedMap"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "headMap"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.util.SortedMap"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "tailMap"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.util.SortedMap"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "firstKey"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "lastKey"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "keySet"
                              (parameters )
                              (returntype . (class "java.util.Set"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "values"
                              (parameters )
                              (returntype . (class "java.util.Collection"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "entrySet"
                              (parameters )
                              (returntype . (class "java.util.Set"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code)))
            (interfaces "java.util.Map")
            (accessflags  *abstract*  *interface*  *public* )
            (attributes
              (attribute "SourceFile")
              (attribute "Signature")
              (attribute "InnerClasses")))))


(defconst *SortedMap-class-table*
  (make-static-class-decls 
   *java.util.SortedMap*))

(defconst *package-name-map* 
  ("java.util.SortedMap" . "java.util"))

