; SocksSocketImpl$6-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:37 CDT 2014.
;

(defconst *java.net.SocksSocketImpl$6*
 (make-class-def
      '(class "java.net.SocksSocketImpl$6"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "this$0" (class "java.net.SocksSocketImpl") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.net.SocksSocketImpl"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "this$0" "java.net.SocksSocketImpl$6" (class "java.net.SocksSocketImpl"))))
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
                                   (max_stack . 5) (max_locals . 1) (code_length . 91)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "this$0" "java.net.SocksSocketImpl$6" (class "java.net.SocksSocketImpl"))))
                                      (4 (new (class "java.net.Socket")))
                                      (7 (dup))
                                      (8 (new (class "java.net.PlainSocketImpl")))
                                      (11 (dup))
                                      (12 (invokespecial
					(methodCP "<init>" "java.net.PlainSocketImpl" () void)))
                                      (15 (invokespecial
					(methodCP "<init>" "java.net.Socket" ((class "java.net.SocketImpl")) void)))
                                      (18 (invokestatic
					(methodCP "access$502" "java.net.SocksSocketImpl" ((class "java.net.SocksSocketImpl") (class "java.net.Socket")) (class "java.net.Socket"))))
                                      (21 (pop))
                                      (22 (aload_0))
                                      (23 (getfield (fieldCP "this$0" "java.net.SocksSocketImpl$6" (class "java.net.SocksSocketImpl"))))
                                      (26 (invokestatic
					(methodCP "access$500" "java.net.SocksSocketImpl" ((class "java.net.SocksSocketImpl")) (class "java.net.Socket"))))
                                      (29 (new (class "java.net.InetSocketAddress")))
                                      (32 (dup))
                                      (33 (aload_0))
                                      (34 (getfield (fieldCP "this$0" "java.net.SocksSocketImpl$6" (class "java.net.SocksSocketImpl"))))
                                      (37 (invokestatic
					(methodCP "access$300" "java.net.SocksSocketImpl" ((class "java.net.SocksSocketImpl")) (class "java.lang.String"))))
                                      (40 (aload_0))
                                      (41 (getfield (fieldCP "this$0" "java.net.SocksSocketImpl$6" (class "java.net.SocksSocketImpl"))))
                                      (44 (invokestatic
					(methodCP "access$400" "java.net.SocksSocketImpl" ((class "java.net.SocksSocketImpl")) int)))
                                      (47 (invokespecial
					(methodCP "<init>" "java.net.InetSocketAddress" ((class "java.lang.String") int) void)))
                                      (50 (invokevirtual
					(methodCP "connect" "java.net.Socket" ((class "java.net.SocketAddress")) void)))
                                      (53 (aload_0))
                                      (54 (getfield (fieldCP "this$0" "java.net.SocksSocketImpl$6" (class "java.net.SocksSocketImpl"))))
                                      (57 (aload_0))
                                      (58 (getfield (fieldCP "this$0" "java.net.SocksSocketImpl$6" (class "java.net.SocksSocketImpl"))))
                                      (61 (invokestatic
					(methodCP "access$500" "java.net.SocksSocketImpl" ((class "java.net.SocksSocketImpl")) (class "java.net.Socket"))))
                                      (64 (invokevirtual
					(methodCP "getInputStream" "java.net.Socket" () (class "java.io.InputStream"))))
                                      (67 (invokestatic
					(methodCP "access$102" "java.net.SocksSocketImpl" ((class "java.net.SocksSocketImpl") (class "java.io.InputStream")) (class "java.io.InputStream"))))
                                      (70 (pop))
                                      (71 (aload_0))
                                      (72 (getfield (fieldCP "this$0" "java.net.SocksSocketImpl$6" (class "java.net.SocksSocketImpl"))))
                                      (75 (aload_0))
                                      (76 (getfield (fieldCP "this$0" "java.net.SocksSocketImpl$6" (class "java.net.SocksSocketImpl"))))
                                      (79 (invokestatic
					(methodCP "access$500" "java.net.SocksSocketImpl" ((class "java.net.SocksSocketImpl")) (class "java.net.Socket"))))
                                      (82 (invokevirtual
					(methodCP "getOutputStream" "java.net.Socket" () (class "java.io.OutputStream"))))
                                      (85 (invokestatic
					(methodCP "access$202" "java.net.SocksSocketImpl" ((class "java.net.SocksSocketImpl") (class "java.io.OutputStream")) (class "java.io.OutputStream"))))
                                      (88 (pop))
                                      (89 (aconst_null))
                                      (90 (areturn))
                                      (endofcode 91))
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
					(methodCP "run" "java.net.SocksSocketImpl$6" () (class "java.lang.Void"))))
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


(defconst *SocksSocketImpl$6-class-table*
  (make-static-class-decls 
   *java.net.SocksSocketImpl$6*))

(defconst *package-name-map* 
  ("java.net.SocksSocketImpl$6" . "java.net"))
