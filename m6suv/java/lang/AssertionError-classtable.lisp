; AssertionError-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:33 CDT 2014.
;

(defconst *java.lang.AssertionError*
 (make-class-def
      '(class "java.lang.AssertionError"
            "java.lang.Error"
            (constant_pool
                        (LONG -5013299493970297370)
                        (STRING  ""))
            (fields
                        (field "serialVersionUID" long (accessflags  *class*  *final*  *private*  *static* ) 0))
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Error" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.lang.String"))
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 6)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (invokespecial
					(methodCP "<init>" "java.lang.Error" ((class "java.lang.String")) void)))
                                      (5 (return))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.lang.Object"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 40)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (new (class "java.lang.StringBuilder"))) 
                                      (4 (dup)) 
                                      (5 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (8 (ldc 1)) ;;STRING:: ""
                                      (10 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (13 (aload_1)) 
                                      (14 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.Object")) (class "java.lang.StringBuilder")))) 
                                      (17 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (20 (invokespecial (methodCP "<init>" "java.lang.AssertionError" ((class "java.lang.String")) void))) 
                                      (23 (aload_1)) 
                                      (24 (instanceof (class "java.lang.Throwable"))) 
                                      (27 (ifeq 39))  ;;to TAG_0
                                      (30 (aload_0)) 
                                      (31 (aload_1)) 
                                      (32 (checkcast (class "java.lang.Throwable"))) 
                                      (35 (invokevirtual (methodCP "initCause" "java.lang.AssertionError" ((class "java.lang.Throwable")) (class "java.lang.Throwable")))) 
                                      (38 (pop)) 
                                      (39 (return)) ;;at TAG_0
                                      (endofcode 40))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters boolean)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 24)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (new (class "java.lang.StringBuilder")))
                                      (4 (dup))
                                      (5 (invokespecial
					(methodCP "<init>" "java.lang.StringBuilder" () void)))
                                      (8 (ldc 1))         ;;STRING:: ""
                                      (10 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder"))))
                                      (13 (iload_1))
                                      (14 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" (boolean) (class "java.lang.StringBuilder"))))
                                      (17 (invokevirtual
					(methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String"))))
                                      (20 (invokespecial
					(methodCP "<init>" "java.lang.AssertionError" ((class "java.lang.String")) void)))
                                      (23 (return))
                                      (endofcode 24))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters char)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 24)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (new (class "java.lang.StringBuilder")))
                                      (4 (dup))
                                      (5 (invokespecial
					(methodCP "<init>" "java.lang.StringBuilder" () void)))
                                      (8 (ldc 1))         ;;STRING:: ""
                                      (10 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder"))))
                                      (13 (iload_1))
                                      (14 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" (char) (class "java.lang.StringBuilder"))))
                                      (17 (invokevirtual
					(methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String"))))
                                      (20 (invokespecial
					(methodCP "<init>" "java.lang.AssertionError" ((class "java.lang.String")) void)))
                                      (23 (return))
                                      (endofcode 24))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters int)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 24)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (new (class "java.lang.StringBuilder")))
                                      (4 (dup))
                                      (5 (invokespecial
					(methodCP "<init>" "java.lang.StringBuilder" () void)))
                                      (8 (ldc 1))         ;;STRING:: ""
                                      (10 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder"))))
                                      (13 (iload_1))
                                      (14 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" (int) (class "java.lang.StringBuilder"))))
                                      (17 (invokevirtual
					(methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String"))))
                                      (20 (invokespecial
					(methodCP "<init>" "java.lang.AssertionError" ((class "java.lang.String")) void)))
                                      (23 (return))
                                      (endofcode 24))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters long)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 3) (code_length . 24)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (new (class "java.lang.StringBuilder")))
                                      (4 (dup))
                                      (5 (invokespecial
					(methodCP "<init>" "java.lang.StringBuilder" () void)))
                                      (8 (ldc 1))         ;;STRING:: ""
                                      (10 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder"))))
                                      (13 (lload_1))
                                      (14 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" (long) (class "java.lang.StringBuilder"))))
                                      (17 (invokevirtual
					(methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String"))))
                                      (20 (invokespecial
					(methodCP "<init>" "java.lang.AssertionError" ((class "java.lang.String")) void)))
                                      (23 (return))
                                      (endofcode 24))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters float)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 24)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (new (class "java.lang.StringBuilder")))
                                      (4 (dup))
                                      (5 (invokespecial
					(methodCP "<init>" "java.lang.StringBuilder" () void)))
                                      (8 (ldc 1))         ;;STRING:: ""
                                      (10 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder"))))
                                      (13 (fload_1))
                                      (14 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" (float) (class "java.lang.StringBuilder"))))
                                      (17 (invokevirtual
					(methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String"))))
                                      (20 (invokespecial
					(methodCP "<init>" "java.lang.AssertionError" ((class "java.lang.String")) void)))
                                      (23 (return))
                                      (endofcode 24))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters double)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 3) (code_length . 24)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (new (class "java.lang.StringBuilder")))
                                      (4 (dup))
                                      (5 (invokespecial
					(methodCP "<init>" "java.lang.StringBuilder" () void)))
                                      (8 (ldc 1))         ;;STRING:: ""
                                      (10 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder"))))
                                      (13 (dload_1))
                                      (14 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" (double) (class "java.lang.StringBuilder"))))
                                      (17 (invokevirtual
					(methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String"))))
                                      (20 (invokespecial
					(methodCP "<init>" "java.lang.AssertionError" ((class "java.lang.String")) void)))
                                      (23 (return))
                                      (endofcode 24))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.lang.String") (class "java.lang.Throwable"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 7)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (aload_2))
                                      (3 (invokespecial
					(methodCP "<init>" "java.lang.Error" ((class "java.lang.String") (class "java.lang.Throwable")) void)))
                                      (6 (return))
                                      (endofcode 7))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *AssertionError-class-table*
  (make-static-class-decls 
   *java.lang.AssertionError*))

(defconst *package-name-map* 
  ("java.lang.AssertionError" . "java.lang"))

