; NavigableSet-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:46 CDT 2014.
;

(defconst *java.util.NavigableSet*
 (make-class-def
      '(class "java.util.NavigableSet"
            "java.lang.Object"
            (constant_pool)
            (fields)
            (methods
                        (method "lower"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "floor"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "ceiling"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "higher"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "pollFirst"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "pollLast"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "iterator"
                              (parameters )
                              (returntype . (class "java.util.Iterator"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "descendingSet"
                              (parameters )
                              (returntype . (class "java.util.NavigableSet"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "descendingIterator"
                              (parameters )
                              (returntype . (class "java.util.Iterator"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "subSet"
                              (parameters (class "java.lang.Object") boolean (class "java.lang.Object") boolean)
                              (returntype . (class "java.util.NavigableSet"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "headSet"
                              (parameters (class "java.lang.Object") boolean)
                              (returntype . (class "java.util.NavigableSet"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "tailSet"
                              (parameters (class "java.lang.Object") boolean)
                              (returntype . (class "java.util.NavigableSet"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "subSet"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.util.SortedSet"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "headSet"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.util.SortedSet"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "tailSet"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.util.SortedSet"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code)))
            (interfaces "java.util.SortedSet")
            (accessflags  *abstract*  *interface*  *public* )
            (attributes
              (attribute "SourceFile")
              (attribute "Signature")))))


(defconst *NavigableSet-class-table*
  (make-static-class-decls 
   *java.util.NavigableSet*))

(defconst *package-name-map* 
  ("java.util.NavigableSet" . "java.util"))

