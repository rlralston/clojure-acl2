; java_util_Collections$CheckedMap_PersistenceDelegate-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:31 CDT 2014.
;

(defconst *java.beans.java_util_Collections$CheckedMap_PersistenceDelegate*
 (make-class-def
      '(class "java.beans.java_util_Collections$CheckedMap_PersistenceDelegate"
            "java.beans.java_util_Collections"
            (constant_pool
                        (STRING  "java.util.Collections$CheckedMap.keyType")
                        (STRING  "java.util.Collections$CheckedMap.valueType")
                        (STRING  "checkedMap"))
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
                                   (max_stack . 9) (max_locals . 6) (code_length . 60)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (ldc 0))         ;;STRING:: "java.util.Collections$CheckedMap.keyType"
                                      (3 (invokestatic
					(methodCP "getPrivateFieldValue" "java.beans.MetaData" ((class "java.lang.Object") (class "java.lang.String")) (class "java.lang.Object"))))
                                      (6 (astore_3))
                                      (7 (aload_1))
                                      (8 (ldc 1))         ;;STRING:: "java.util.Collections$CheckedMap.valueType"
                                      (10 (invokestatic
					(methodCP "getPrivateFieldValue" "java.beans.MetaData" ((class "java.lang.Object") (class "java.lang.String")) (class "java.lang.Object"))))
                                      (13 (astore 4))
                                      (15 (new (class "java.util.HashMap")))
                                      (18 (dup))
                                      (19 (aload_1))
                                      (20 (checkcast (class "java.util.Map")))
                                      (23 (invokespecial
					(methodCP "<init>" "java.util.HashMap" ((class "java.util.Map")) void)))
                                      (26 (astore 5))
                                      (28 (new (class "java.beans.Expression")))
                                      (31 (dup))
                                      (32 (aload_1))
                                      (33 (ldc_w ))
                                      (36 (ldc 2))        ;;STRING:: "checkedMap"
                                      (38 (iconst_3))
                                      (39 (anewarray (class "java.lang.Object")))
                                      (42 (dup))
                                      (43 (iconst_0))
                                      (44 (aload 5))
                                      (46 (aastore))
                                      (47 (dup))
                                      (48 (iconst_1))
                                      (49 (aload_3))
                                      (50 (aastore))
                                      (51 (dup))
                                      (52 (iconst_2))
                                      (53 (aload 4))
                                      (55 (aastore))
                                      (56 (invokespecial
					(methodCP "<init>" "java.beans.Expression" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.String") (array (class "java.lang.Object"))) void)))
                                      (59 (areturn))
                                      (endofcode 60))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *java_util_Collections$CheckedMap_PersistenceDelegate-class-table*
  (make-static-class-decls 
   *java.beans.java_util_Collections$CheckedMap_PersistenceDelegate*))

(defconst *package-name-map* 
  ("java.beans.java_util_Collections$CheckedMap_PersistenceDelegate" . "java.beans"))
