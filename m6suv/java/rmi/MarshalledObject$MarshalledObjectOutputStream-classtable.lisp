; MarshalledObject$MarshalledObjectOutputStream-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:39 CDT 2014.
;

(defconst *java.rmi.MarshalledObject$MarshalledObjectOutputStream*
 (make-class-def
      '(class "java.rmi.MarshalledObject$MarshalledObjectOutputStream"
            "sun.rmi.server.MarshalOutputStream"
            (constant_pool)
            (fields
                        (field "locOut" (class "java.io.ObjectOutputStream") (accessflags  *class*  *private* ) -1)
                        (field "hadAnnotations" boolean (accessflags  *class*  *private* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.io.OutputStream") (class "java.io.OutputStream"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 4) (max_locals . 3) (code_length . 28)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (invokespecial
					(methodCP "<init>" "sun.rmi.server.MarshalOutputStream" ((class "java.io.OutputStream")) void)))
                                      (5 (aload_0))
                                      (6 (iconst_2))
                                      (7 (invokevirtual
					(methodCP "useProtocolVersion" "java.rmi.MarshalledObject$MarshalledObjectOutputStream" (int) void)))
                                      (10 (aload_0))
                                      (11 (new (class "java.io.ObjectOutputStream")))
                                      (14 (dup))
                                      (15 (aload_2))
                                      (16 (invokespecial
					(methodCP "<init>" "java.io.ObjectOutputStream" ((class "java.io.OutputStream")) void)))
                                      (19 (putfield (fieldCP "locOut" "java.rmi.MarshalledObject$MarshalledObjectOutputStream" (class "java.io.ObjectOutputStream"))))
                                      (22 (aload_0))
                                      (23 (iconst_0))
                                      (24 (putfield (fieldCP "hadAnnotations" "java.rmi.MarshalledObject$MarshalledObjectOutputStream" boolean)))
                                      (27 (return))
                                      (endofcode 28))
                                   (Exceptions )
                                   (StackMap )))
                        (method "hadAnnotations"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "hadAnnotations" "java.rmi.MarshalledObject$MarshalledObjectOutputStream" boolean)))
                                      (4 (ireturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "writeLocation"
                              (parameters (class "java.lang.String"))
                              (returntype . void)
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 27)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (dup)) 
                                      (2 (getfield (fieldCP "hadAnnotations" "java.rmi.MarshalledObject$MarshalledObjectOutputStream" boolean))) 
                                      (5 (aload_1)) 
                                      (6 (ifnull 13))  ;;to TAG_0
                                      (9 (iconst_1)) 
                                      (10 (goto 14)) ;;to TAG_1
                                      (13 (iconst_0)) ;;at TAG_0
                                      (14 (ior)) ;;at TAG_1
                                      (15 (putfield (fieldCP "hadAnnotations" "java.rmi.MarshalledObject$MarshalledObjectOutputStream" boolean))) 
                                      (18 (aload_0)) 
                                      (19 (getfield (fieldCP "locOut" "java.rmi.MarshalledObject$MarshalledObjectOutputStream" (class "java.io.ObjectOutputStream")))) 
                                      (22 (aload_1)) 
                                      (23 (invokevirtual (methodCP "writeObject" "java.io.ObjectOutputStream" ((class "java.lang.Object")) void))) 
                                      (26 (return)) 
                                      (endofcode 27))
                                   (Exceptions )
                                   (StackMap )))
                        (method "flush"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 12)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "flush" "sun.rmi.server.MarshalOutputStream" () void)))
                                      (4 (aload_0))
                                      (5 (getfield (fieldCP "locOut" "java.rmi.MarshalledObject$MarshalledObjectOutputStream" (class "java.io.ObjectOutputStream"))))
                                      (8 (invokevirtual
					(methodCP "flush" "java.io.ObjectOutputStream" () void)))
                                      (11 (return))
                                      (endofcode 12))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *MarshalledObject$MarshalledObjectOutputStream-class-table*
  (make-static-class-decls 
   *java.rmi.MarshalledObject$MarshalledObjectOutputStream*))

(defconst *package-name-map* 
  ("java.rmi.MarshalledObject$MarshalledObjectOutputStream" . "java.rmi"))

