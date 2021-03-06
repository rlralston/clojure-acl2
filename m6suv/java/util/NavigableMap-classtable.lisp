; NavigableMap-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:46 CDT 2014.
;

(defconst *java.util.NavigableMap*
 (make-class-def
      '(class "java.util.NavigableMap"
            "java.lang.Object"
            (constant_pool)
            (fields)
            (methods
                        (method "lowerEntry"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.util.Map$Entry"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "lowerKey"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "floorEntry"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.util.Map$Entry"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "floorKey"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "ceilingEntry"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.util.Map$Entry"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "ceilingKey"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "higherEntry"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.util.Map$Entry"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "higherKey"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "firstEntry"
                              (parameters )
                              (returntype . (class "java.util.Map$Entry"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "lastEntry"
                              (parameters )
                              (returntype . (class "java.util.Map$Entry"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "pollFirstEntry"
                              (parameters )
                              (returntype . (class "java.util.Map$Entry"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "pollLastEntry"
                              (parameters )
                              (returntype . (class "java.util.Map$Entry"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "descendingMap"
                              (parameters )
                              (returntype . (class "java.util.NavigableMap"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "navigableKeySet"
                              (parameters )
                              (returntype . (class "java.util.NavigableSet"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "descendingKeySet"
                              (parameters )
                              (returntype . (class "java.util.NavigableSet"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "subMap"
                              (parameters (class "java.lang.Object") boolean (class "java.lang.Object") boolean)
                              (returntype . (class "java.util.NavigableMap"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "headMap"
                              (parameters (class "java.lang.Object") boolean)
                              (returntype . (class "java.util.NavigableMap"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "tailMap"
                              (parameters (class "java.lang.Object") boolean)
                              (returntype . (class "java.util.NavigableMap"))
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
                              (code)))
            (interfaces "java.util.SortedMap")
            (accessflags  *abstract*  *interface*  *public* )
            (attributes
              (attribute "SourceFile")
              (attribute "Signature")
              (attribute "InnerClasses")))))


(defconst *NavigableMap-class-table*
  (make-static-class-decls 
   *java.util.NavigableMap*))

(defconst *package-name-map* 
  ("java.util.NavigableMap" . "java.util"))

