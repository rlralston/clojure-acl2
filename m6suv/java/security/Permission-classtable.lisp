; Permission-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:40 CDT 2014.
;

(defconst *java.security.Permission*
 (make-class-def
      '(class "java.security.Permission"
            "java.lang.Object"
            (constant_pool
                        (LONG -5636570222231596674)
                        (STRING  "(\"")
                        (STRING  "\" \"")
                        (STRING  "\")"))
            (fields
                        (field "serialVersionUID" long (accessflags  *class*  *final*  *private*  *static* ) 0)
                        (field "name" (class "java.lang.String") (accessflags  *class*  *private* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.lang.String"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (putfield (fieldCP "name" "java.security.Permission" (class "java.lang.String"))))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "checkGuard"
                              (parameters (class "java.lang.Object"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 14)
                                   (parsedcode
                                      (0 (invokestatic (methodCP "getSecurityManager" "java.lang.System" () (class "java.lang.SecurityManager")))) 
                                      (3 (astore_2)) 
                                      (4 (aload_2)) 
                                      (5 (ifnull 13))  ;;to TAG_0
                                      (8 (aload_2)) 
                                      (9 (aload_0)) 
                                      (10 (invokevirtual (methodCP "checkPermission" "java.lang.SecurityManager" ((class "java.security.Permission")) void))) 
                                      (13 (return)) ;;at TAG_0
                                      (endofcode 14))
                                   (Exceptions )
                                   (StackMap )))
                        (method "implies"
                              (parameters (class "java.security.Permission"))
                              (returntype . boolean)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "equals"
                              (parameters (class "java.lang.Object"))
                              (returntype . boolean)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "hashCode"
                              (parameters )
                              (returntype . int)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getName"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *final*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "name" "java.security.Permission" (class "java.lang.String"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getActions"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "newPermissionCollection"
                              (parameters )
                              (returntype . (class "java.security.PermissionCollection"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 2)
                                   (parsedcode
                                      (0 (aconst_null))
                                      (1 (areturn))
                                      (endofcode 2))
                                   (Exceptions )
                                   (StackMap )))
                        (method "toString"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 111)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokevirtual (methodCP "getActions" "java.security.Permission" () (class "java.lang.String")))) 
                                      (4 (astore_1)) 
                                      (5 (aload_1)) 
                                      (6 (ifnull 16))  ;;to TAG_0
                                      (9 (aload_1)) 
                                      (10 (invokevirtual (methodCP "length" "java.lang.String" () int))) 
                                      (13 (ifne 59)) ;;to TAG_1
                                      (16 (new (class "java.lang.StringBuilder"))) ;;at TAG_0
                                      (19 (dup)) 
                                      (20 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (23 (ldc 1)) ;;STRING:: "(\""
                                      (25 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (28 (aload_0)) 
                                      (29 (invokevirtual (methodCP "getClass" "java.lang.Object" () (class "java.lang.Class")))) 
                                      (32 (invokevirtual (methodCP "getName" "java.lang.Class" () (class "java.lang.String")))) 
                                      (35 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (38 (ldc 2)) ;;STRING:: "\" \""
                                      (40 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (43 (aload_0)) 
                                      (44 (getfield (fieldCP "name" "java.security.Permission" (class "java.lang.String")))) 
                                      (47 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (50 (ldc 3)) ;;STRING:: "\")"
                                      (52 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (55 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (58 (areturn)) 
                                      (59 (new (class "java.lang.StringBuilder"))) ;;at TAG_1
                                      (62 (dup)) 
                                      (63 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (66 (ldc 1)) ;;STRING:: "(\""
                                      (68 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (71 (aload_0)) 
                                      (72 (invokevirtual (methodCP "getClass" "java.lang.Object" () (class "java.lang.Class")))) 
                                      (75 (invokevirtual (methodCP "getName" "java.lang.Class" () (class "java.lang.String")))) 
                                      (78 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (81 (ldc 2)) ;;STRING:: "\" \""
                                      (83 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (86 (aload_0)) 
                                      (87 (getfield (fieldCP "name" "java.security.Permission" (class "java.lang.String")))) 
                                      (90 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (93 (ldc 2)) ;;STRING:: "\" \""
                                      (95 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (98 (aload_1)) 
                                      (99 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (102 (ldc 3)) ;;STRING:: "\")"
                                      (104 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (107 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (110 (areturn)) 
                                      (endofcode 111))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.security.Guard" "java.io.Serializable")
            (accessflags  *abstract*  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *Permission-class-table*
  (make-static-class-decls 
   *java.security.Permission*))

(defconst *package-name-map* 
  ("java.security.Permission" . "java.security"))
