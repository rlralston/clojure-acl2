; java_util_Collections$EmptySet_PersistenceDelegate-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:31 CDT 2014.
;

(defconst *java.beans.java_util_Collections$EmptySet_PersistenceDelegate*
 (make-class-def
      '(class "java.beans.java_util_Collections$EmptySet_PersistenceDelegate"
            "java.beans.java_util_Collections"
            (constant_pool
                        (STRING  "emptySet"))
            (fields)
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.beans.java_util_Collections" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "instantiate"
                              (parameters (class "java.lang.Object") (class "java.beans.Encoder"))
                              (returntype . (class "java.beans.Expression"))
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 6) (max_locals . 3) (code_length . 15)
                                   (parsedcode
                                      (0 (new (class "java.beans.Expression")))
                                      (3 (dup))
                                      (4 (aload_1))
                                      (5 (ldc_w ))
                                      (8 (ldc 0))         ;;STRING:: "emptySet"
                                      (10 (aconst_null))
                                      (11 (invokespecial
					(methodCP "<init>" "java.beans.Expression" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.String") (array (class "java.lang.Object"))) void)))
                                      (14 (areturn))
                                      (endofcode 15))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *java_util_Collections$EmptySet_PersistenceDelegate-class-table*
  (make-static-class-decls 
   *java.beans.java_util_Collections$EmptySet_PersistenceDelegate*))

(defconst *package-name-map* 
  ("java.beans.java_util_Collections$EmptySet_PersistenceDelegate" . "java.beans"))
