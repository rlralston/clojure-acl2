; java_util_Collections$SynchronizedSet_PersistenceDelegate-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:31 CDT 2014.
;

(defconst *java.beans.java_util_Collections$SynchronizedSet_PersistenceDelegate*
 (make-class-def
      '(class "java.beans.java_util_Collections$SynchronizedSet_PersistenceDelegate"
            "java.beans.java_util_Collections"
            (constant_pool
                        (STRING  "synchronizedSet"))
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
                                   (max_stack . 9) (max_locals . 4) (code_length . 34)
                                   (parsedcode
                                      (0 (new (class "java.util.HashSet")))
                                      (3 (dup))
                                      (4 (aload_1))
                                      (5 (checkcast (class "java.util.Set")))
                                      (8 (invokespecial
					(methodCP "<init>" "java.util.HashSet" ((class "java.util.Collection")) void)))
                                      (11 (astore_3))
                                      (12 (new (class "java.beans.Expression")))
                                      (15 (dup))
                                      (16 (aload_1))
                                      (17 (ldc_w ))
                                      (20 (ldc 0))        ;;STRING:: "synchronizedSet"
                                      (22 (iconst_1))
                                      (23 (anewarray (class "java.lang.Object")))
                                      (26 (dup))
                                      (27 (iconst_0))
                                      (28 (aload_3))
                                      (29 (aastore))
                                      (30 (invokespecial
					(methodCP "<init>" "java.beans.Expression" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.String") (array (class "java.lang.Object"))) void)))
                                      (33 (areturn))
                                      (endofcode 34))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *java_util_Collections$SynchronizedSet_PersistenceDelegate-class-table*
  (make-static-class-decls 
   *java.beans.java_util_Collections$SynchronizedSet_PersistenceDelegate*))

(defconst *package-name-map* 
  ("java.beans.java_util_Collections$SynchronizedSet_PersistenceDelegate" . "java.beans"))

