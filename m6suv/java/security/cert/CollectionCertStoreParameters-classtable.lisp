; CollectionCertStoreParameters-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:40 CDT 2014.
;

(defconst *java.security.cert.CollectionCertStoreParameters*
 (make-class-def
      '(class "java.security.cert.CollectionCertStoreParameters"
            "java.lang.Object"
            (constant_pool
                        (STRING  "CollectionCertStoreParameters: [\n")
                        (STRING  "  collection: ")
                        (STRING  "\n")
                        (STRING  "]"))
            (fields
                        (field "coll" (class "java.util.Collection") (accessflags  *class*  *private* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.util.Collection"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 22)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokespecial (methodCP "<init>" "java.lang.Object" () void))) 
                                      (4 (aload_1)) 
                                      (5 (ifnonnull 16))  ;;to TAG_0
                                      (8 (new (class "java.lang.NullPointerException"))) 
                                      (11 (dup)) 
                                      (12 (invokespecial (methodCP "<init>" "java.lang.NullPointerException" () void))) 
                                      (15 (athrow)) 
                                      (16 (aload_0)) ;;at TAG_0
                                      (17 (aload_1)) 
                                      (18 (putfield (fieldCP "coll" "java.security.cert.CollectionCertStoreParameters" (class "java.util.Collection")))) 
                                      (21 (return)) 
                                      (endofcode 22))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 12)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (aload_0))
                                      (5 (getstatic (fieldCP "EMPTY_SET" "java.util.Collections" (class "java.util.Set"))))
                                      (8 (putfield (fieldCP "coll" "java.security.cert.CollectionCertStoreParameters" (class "java.util.Collection"))))
                                      (11 (return))
                                      (endofcode 12))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getCollection"
                              (parameters )
                              (returntype . (class "java.util.Collection"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "coll" "java.security.cert.CollectionCertStoreParameters" (class "java.util.Collection"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "clone"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 18)
                                   (parsedcode
                                      (0 (aload_0)) ;;at TAG_0
                                      (1 (invokespecial (methodCP "clone" "java.lang.Object" () (class "java.lang.Object")))) 
                                      (4 (areturn)) ;;at TAG_1
                                      (5 (astore_1)) ;;at TAG_2
                                      (6 (new (class "java.lang.InternalError"))) 
                                      (9 (dup)) 
                                      (10 (aload_1)) 
                                      (11 (invokevirtual (methodCP "toString" "java.lang.CloneNotSupportedException" () (class "java.lang.String")))) 
                                      (14 (invokespecial (methodCP "<init>" "java.lang.InternalError" ((class "java.lang.String")) void))) 
                                      (17 (athrow)) 
                                      (endofcode 18))
                                   (Exceptions 
                                     (handler 0 4  5 (class "java.lang.CloneNotSupportedException")))
                                   (StackMap )))
                        (method "toString"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 59)
                                   (parsedcode
                                      (0 (new (class "java.lang.StringBuffer")))
                                      (3 (dup))
                                      (4 (invokespecial
					(methodCP "<init>" "java.lang.StringBuffer" () void)))
                                      (7 (astore_1))
                                      (8 (aload_1))
                                      (9 (ldc 0))         ;;STRING:: "CollectionCertStoreParameters: [\n"
                                      (11 (invokevirtual
					(methodCP "append" "java.lang.StringBuffer" ((class "java.lang.String")) (class "java.lang.StringBuffer"))))
                                      (14 (pop))
                                      (15 (aload_1))
                                      (16 (new (class "java.lang.StringBuilder")))
                                      (19 (dup))
                                      (20 (invokespecial
					(methodCP "<init>" "java.lang.StringBuilder" () void)))
                                      (23 (ldc 1))        ;;STRING:: "  collection: "
                                      (25 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder"))))
                                      (28 (aload_0))
                                      (29 (getfield (fieldCP "coll" "java.security.cert.CollectionCertStoreParameters" (class "java.util.Collection"))))
                                      (32 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" ((class "java.lang.Object")) (class "java.lang.StringBuilder"))))
                                      (35 (ldc 2))        ;;STRING:: "\n"
                                      (37 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder"))))
                                      (40 (invokevirtual
					(methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String"))))
                                      (43 (invokevirtual
					(methodCP "append" "java.lang.StringBuffer" ((class "java.lang.String")) (class "java.lang.StringBuffer"))))
                                      (46 (pop))
                                      (47 (aload_1))
                                      (48 (ldc 3))        ;;STRING:: "]"
                                      (50 (invokevirtual
					(methodCP "append" "java.lang.StringBuffer" ((class "java.lang.String")) (class "java.lang.StringBuffer"))))
                                      (53 (pop))
                                      (54 (aload_1))
                                      (55 (invokevirtual
					(methodCP "toString" "java.lang.StringBuffer" () (class "java.lang.String"))))
                                      (58 (areturn))
                                      (endofcode 59))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.security.cert.CertStoreParameters")
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *CollectionCertStoreParameters-class-table*
  (make-static-class-decls 
   *java.security.cert.CollectionCertStoreParameters*))

(defconst *package-name-map* 
  ("java.security.cert.CollectionCertStoreParameters" . "java.security.cert"))
