; java_util_Collections$CheckedRandomAccessList_PersistenceDelegate-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:31 CDT 2014.
;

(defconst *java.beans.java_util_Collections$CheckedRandomAccessList_PersistenceDelegate*
 (make-class-def
      '(class "java.beans.java_util_Collections$CheckedRandomAccessList_PersistenceDelegate"
            "java.beans.java_util_Collections"
            (constant_pool
                        (STRING  "java.util.Collections$CheckedCollection.type")
                        (STRING  "checkedList"))
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
                                   (max_stack . 9) (max_locals . 5) (code_length . 47)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (ldc 0))         ;;STRING:: "java.util.Collections$CheckedCollection.type"
                                      (3 (invokestatic
					(methodCP "getPrivateFieldValue" "java.beans.MetaData" ((class "java.lang.Object") (class "java.lang.String")) (class "java.lang.Object"))))
                                      (6 (astore_3))
                                      (7 (new (class "java.util.ArrayList")))
                                      (10 (dup))
                                      (11 (aload_1))
                                      (12 (checkcast (class "java.util.Collection")))
                                      (15 (invokespecial
					(methodCP "<init>" "java.util.ArrayList" ((class "java.util.Collection")) void)))
                                      (18 (astore 4))
                                      (20 (new (class "java.beans.Expression")))
                                      (23 (dup))
                                      (24 (aload_1))
                                      (25 (ldc_w ))
                                      (28 (ldc 1))        ;;STRING:: "checkedList"
                                      (30 (iconst_2))
                                      (31 (anewarray (class "java.lang.Object")))
                                      (34 (dup))
                                      (35 (iconst_0))
                                      (36 (aload 4))
                                      (38 (aastore))
                                      (39 (dup))
                                      (40 (iconst_1))
                                      (41 (aload_3))
                                      (42 (aastore))
                                      (43 (invokespecial
					(methodCP "<init>" "java.beans.Expression" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.String") (array (class "java.lang.Object"))) void)))
                                      (46 (areturn))
                                      (endofcode 47))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *java_util_Collections$CheckedRandomAccessList_PersistenceDelegate-class-table*
  (make-static-class-decls 
   *java.beans.java_util_Collections$CheckedRandomAccessList_PersistenceDelegate*))

(defconst *package-name-map* 
  ("java.beans.java_util_Collections$CheckedRandomAccessList_PersistenceDelegate" . "java.beans"))

