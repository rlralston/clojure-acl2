; ServerSocket$1-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:37 CDT 2014.
;

(defconst *java.net.ServerSocket$1*
 (make-class-def
      '(class "java.net.ServerSocket$1"
            "java.lang.Object"
            (constant_pool
                        (STRING  "connect"))
            (fields
                        (field "this$0" (class "java.net.ServerSocket") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.net.ServerSocket"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "this$0" "java.net.ServerSocket$1" (class "java.net.ServerSocket"))))
                                      (5 (aload_0))
                                      (6 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "run"
                              (parameters )
                              (returntype . (class "java.lang.Void"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 36)
                                   (parsedcode
                                      (0 (iconst_2))
                                      (1 (anewarray (class "java.lang.Class")))
                                      (4 (astore_1))
                                      (5 (aload_1))
                                      (6 (iconst_0))
                                      (7 (ldc_w ))
                                      (10 (aastore))
                                      (11 (aload_1))
                                      (12 (iconst_1))
                                      (13 (getstatic (fieldCP "TYPE" "java.lang.Integer" (class "java.lang.Class"))))
                                      (16 (aastore))
                                      (17 (aload_0))
                                      (18 (getfield (fieldCP "this$0" "java.net.ServerSocket$1" (class "java.net.ServerSocket"))))
                                      (21 (invokestatic
					(methodCP "access$000" "java.net.ServerSocket" ((class "java.net.ServerSocket")) (class "java.net.SocketImpl"))))
                                      (24 (invokevirtual
					(methodCP "getClass" "java.lang.Object" () (class "java.lang.Class"))))
                                      (27 (ldc 0))        ;;STRING:: "connect"
                                      (29 (aload_1))
                                      (30 (invokevirtual
					(methodCP "getDeclaredMethod" "java.lang.Class" ((class "java.lang.String") (array (class "java.lang.Class"))) (class "java.lang.reflect.Method"))))
                                      (33 (pop))
                                      (34 (aconst_null))
                                      (35 (areturn))
                                      (endofcode 36))
                                   (Exceptions )
                                   (StackMap )))
                        (method "run"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public*  *volatile* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokevirtual
					(methodCP "run" "java.net.ServerSocket$1" () (class "java.lang.Void"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.security.PrivilegedExceptionAction")
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "EnclosingMethod")
              (attribute "Signature")
              (attribute "InnerClasses")))))


(defconst *ServerSocket$1-class-table*
  (make-static-class-decls 
   *java.net.ServerSocket$1*))

(defconst *package-name-map* 
  ("java.net.ServerSocket$1" . "java.net"))

