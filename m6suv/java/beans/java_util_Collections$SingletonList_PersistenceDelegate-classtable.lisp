; java_util_Collections$SingletonList_PersistenceDelegate-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:31 CDT 2014.
;

(defconst *java.beans.java_util_Collections$SingletonList_PersistenceDelegate*
 (make-class-def
      '(class "java.beans.java_util_Collections$SingletonList_PersistenceDelegate"
            "java.beans.java_util_Collections"
            (constant_pool
                        (STRING  "singletonList"))
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
                                   (max_stack . 10) (max_locals . 4) (code_length . 33)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (checkcast (class "java.util.List")))
                                      (4 (astore_3))
                                      (5 (new (class "java.beans.Expression")))
                                      (8 (dup))
                                      (9 (aload_1))
                                      (10 (ldc_w ))
                                      (13 (ldc 0))        ;;STRING:: "singletonList"
                                      (15 (iconst_1))
                                      (16 (anewarray (class "java.lang.Object")))
                                      (19 (dup))
                                      (20 (iconst_0))
                                      (21 (aload_3))
                                      (22 (iconst_0))
                                      (23 (invokeinterface
					(methodCP "get" "java.util.List" (int) (class "java.lang.Object")) 2))
                                      (28 (aastore))
                                      (29 (invokespecial
					(methodCP "<init>" "java.beans.Expression" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.String") (array (class "java.lang.Object"))) void)))
                                      (32 (areturn))
                                      (endofcode 33))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *java_util_Collections$SingletonList_PersistenceDelegate-class-table*
  (make-static-class-decls 
   *java.beans.java_util_Collections$SingletonList_PersistenceDelegate*))

(defconst *package-name-map* 
  ("java.beans.java_util_Collections$SingletonList_PersistenceDelegate" . "java.beans"))

