; SdpSocketImpl-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:37 CDT 2014.
;

(defconst *java.net.SdpSocketImpl*
 (make-class-def
      '(class "java.net.SdpSocketImpl"
            "java.net.PlainSocketImpl"
            (constant_pool
                        (STRING  "Must be a stream socket"))
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
					(methodCP "<init>" "java.net.PlainSocketImpl" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "create"
                              (parameters boolean)
                              (returntype . void)
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 50)
                                   (parsedcode
                                      (0 (iload_1)) 
                                      (1 (ifne 14)) ;;to TAG_0
                                      (4 (new (class "java.lang.UnsupportedOperationException"))) 
                                      (7 (dup)) 
                                      (8 (ldc 0)) ;;STRING:: "Must be a stream socket"
                                      (10 (invokespecial (methodCP "<init>" "java.lang.UnsupportedOperationException" ((class "java.lang.String")) void))) 
                                      (13 (athrow)) 
                                      (14 (aload_0)) ;;at TAG_0
                                      (15 (invokestatic (methodCP "createSocket" "sun.net.sdp.SdpSupport" () (class "java.io.FileDescriptor")))) 
                                      (18 (putfield (fieldCP "fd" "java.net.SdpSocketImpl" (class "java.io.FileDescriptor")))) 
                                      (21 (aload_0)) 
                                      (22 (getfield (fieldCP "socket" "java.net.SdpSocketImpl" (class "java.net.Socket")))) 
                                      (25 (ifnull 35)) ;;to TAG_1
                                      (28 (aload_0)) 
                                      (29 (getfield (fieldCP "socket" "java.net.SdpSocketImpl" (class "java.net.Socket")))) 
                                      (32 (invokevirtual (methodCP "setCreated" "java.net.Socket" () void))) 
                                      (35 (aload_0)) ;;at TAG_1
                                      (36 (getfield (fieldCP "serverSocket" "java.net.SdpSocketImpl" (class "java.net.ServerSocket")))) 
                                      (39 (ifnull 49))  ;;to TAG_2
                                      (42 (aload_0)) 
                                      (43 (getfield (fieldCP "serverSocket" "java.net.SdpSocketImpl" (class "java.net.ServerSocket")))) 
                                      (46 (invokevirtual (methodCP "setCreated" "java.net.ServerSocket" () void))) 
                                      (49 (return)) ;;at TAG_2
                                      (endofcode 50))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *SdpSocketImpl-class-table*
  (make-static-class-decls 
   *java.net.SdpSocketImpl*))

(defconst *package-name-map* 
  ("java.net.SdpSocketImpl" . "java.net"))

