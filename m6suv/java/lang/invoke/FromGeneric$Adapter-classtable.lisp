; FromGeneric$Adapter-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:34 CDT 2014.
;

(defconst *java.lang.invoke.FromGeneric$Adapter*
 (make-class-def
      '(class "java.lang.invoke.FromGeneric$Adapter"
            "java.lang.invoke.BoundMethodHandle"
            (constant_pool)
            (fields
                        (field "invoker" (class "java.lang.invoke.MethodHandle") (accessflags  *class*  *final*  *protected* ) -1)
                        (field "convert" (class "java.lang.invoke.MethodHandle") (accessflags  *class*  *final*  *protected* ) -1)
                        (field "target" (class "java.lang.invoke.MethodHandle") (accessflags  *class*  *final*  *protected* ) -1)
                        (field "CLASS_PREFIX" (class "java.lang.String") (accessflags  *class*  *final*  *private*  *static* ) -1)
                        (field "$assertionsDisabled" boolean (accessflags  *class*  *final*  *static* ) -1))
            (methods
                        (method "debugString"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 9)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "target" "java.lang.invoke.FromGeneric$Adapter" (class "java.lang.invoke.MethodHandle"))))
                                      (4 (aload_0))
                                      (5 (invokestatic
					(methodCP "addTypeString" "java.lang.invoke.MethodHandleStatics" ((class "java.lang.Object") (class "java.lang.invoke.MethodHandle")) (class "java.lang.String"))))
                                      (8 (areturn))
                                      (endofcode 9))
                                   (Exceptions )
                                   (StackMap )))
                        (method "isPrototype"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 13)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "target" "java.lang.invoke.FromGeneric$Adapter" (class "java.lang.invoke.MethodHandle")))) 
                                      (4 (ifnonnull 11))  ;;to TAG_0
                                      (7 (iconst_1)) 
                                      (8 (goto 12)) ;;to TAG_1
                                      (11 (iconst_0)) ;;at TAG_0
                                      (12 (ireturn)) ;;at TAG_1
                                      (endofcode 13))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.lang.invoke.MethodHandle"))
                              (returntype . void)
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 5) (max_locals . 2) (code_length . 30)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (aload_1)) 
                                      (2 (aconst_null)) 
                                      (3 (aload_1)) 
                                      (4 (aconst_null)) 
                                      (5 (invokespecial (methodCP "<init>" "java.lang.invoke.FromGeneric$Adapter" ((class "java.lang.invoke.MethodHandle") (class "java.lang.invoke.MethodHandle") (class "java.lang.invoke.MethodHandle") (class "java.lang.invoke.MethodHandle")) void))) 
                                      (8 (getstatic (fieldCP "$assertionsDisabled" "java.lang.invoke.FromGeneric$Adapter" boolean))) 
                                      (11 (ifne 29))  ;;to TAG_0
                                      (14 (aload_0)) 
                                      (15 (invokevirtual (methodCP "isPrototype" "java.lang.invoke.FromGeneric$Adapter" () boolean))) 
                                      (18 (ifne 29))  ;;to TAG_0
                                      (21 (new (class "java.lang.AssertionError"))) 
                                      (24 (dup)) 
                                      (25 (invokespecial (methodCP "<init>" "java.lang.AssertionError" () void))) 
                                      (28 (athrow)) 
                                      (29 (return)) ;;at TAG_0
                                      (endofcode 30))
                                   (Exceptions )
                                   (StackMap )))
                        (method "prototypeEntryPoint"
                              (parameters )
                              (returntype . (class "java.lang.invoke.MethodHandle"))
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 20)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokevirtual (methodCP "isPrototype" "java.lang.invoke.FromGeneric$Adapter" () boolean))) 
                                      (4 (ifne 15))  ;;to TAG_0
                                      (7 (new (class "java.lang.InternalError"))) 
                                      (10 (dup)) 
                                      (11 (invokespecial (methodCP "<init>" "java.lang.InternalError" () void))) 
                                      (14 (athrow)) 
                                      (15 (aload_0)) ;;at TAG_0
                                      (16 (getfield (fieldCP "convert" "java.lang.invoke.FromGeneric$Adapter" (class "java.lang.invoke.MethodHandle")))) 
                                      (19 (areturn)) 
                                      (endofcode 20))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.lang.invoke.MethodHandle") (class "java.lang.invoke.MethodHandle") (class "java.lang.invoke.MethodHandle") (class "java.lang.invoke.MethodHandle"))
                              (returntype . void)
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 2) (max_locals . 5) (code_length . 22)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (invokespecial
					(methodCP "<init>" "java.lang.invoke.BoundMethodHandle" ((class "java.lang.invoke.MethodHandle")) void)))
                                      (5 (aload_0))
                                      (6 (aload_2))
                                      (7 (putfield (fieldCP "invoker" "java.lang.invoke.FromGeneric$Adapter" (class "java.lang.invoke.MethodHandle"))))
                                      (10 (aload_0))
                                      (11 (aload_3))
                                      (12 (putfield (fieldCP "convert" "java.lang.invoke.FromGeneric$Adapter" (class "java.lang.invoke.MethodHandle"))))
                                      (15 (aload_0))
                                      (16 (aload 4))
                                      (18 (putfield (fieldCP "target" "java.lang.invoke.FromGeneric$Adapter" (class "java.lang.invoke.MethodHandle"))))
                                      (21 (return))
                                      (endofcode 22))
                                   (Exceptions )
                                   (StackMap )))
                        (method "makeInstance"
                              (parameters (class "java.lang.invoke.MethodHandle") (class "java.lang.invoke.MethodHandle") (class "java.lang.invoke.MethodHandle") (class "java.lang.invoke.MethodHandle"))
                              (returntype . (class "java.lang.invoke.FromGeneric$Adapter"))
                              (accessflags  *abstract*  *class*  *protected* )
                              (code))
                        (method "convert_L"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 9)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "convert" "java.lang.invoke.FromGeneric$Adapter" (class "java.lang.invoke.MethodHandle"))))
                                      (4 (aload_1))
                                      (5 (invokevirtual
					(methodCP "invokeExact" "java.lang.invoke.MethodHandle" ((class "java.lang.Object")) (class "java.lang.Object"))))
                                      (8 (areturn))
                                      (endofcode 9))
                                   (Exceptions )
                                   (StackMap )))
                        (method "convert_I"
                              (parameters int)
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 9)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "convert" "java.lang.invoke.FromGeneric$Adapter" (class "java.lang.invoke.MethodHandle"))))
                                      (4 (iload_1))
                                      (5 (invokevirtual
					(methodCP "invokeExact" "java.lang.invoke.MethodHandle" (int) (class "java.lang.Object"))))
                                      (8 (areturn))
                                      (endofcode 9))
                                   (Exceptions )
                                   (StackMap )))
                        (method "convert_J"
                              (parameters long)
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 9)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "convert" "java.lang.invoke.FromGeneric$Adapter" (class "java.lang.invoke.MethodHandle"))))
                                      (4 (lload_1))
                                      (5 (invokevirtual
					(methodCP "invokeExact" "java.lang.invoke.MethodHandle" (long) (class "java.lang.Object"))))
                                      (8 (areturn))
                                      (endofcode 9))
                                   (Exceptions )
                                   (StackMap )))
                        (method "convert_F"
                              (parameters float)
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 9)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "convert" "java.lang.invoke.FromGeneric$Adapter" (class "java.lang.invoke.MethodHandle"))))
                                      (4 (fload_1))
                                      (5 (invokevirtual
					(methodCP "invokeExact" "java.lang.invoke.MethodHandle" (float) (class "java.lang.Object"))))
                                      (8 (areturn))
                                      (endofcode 9))
                                   (Exceptions )
                                   (StackMap )))
                        (method "convert_D"
                              (parameters double)
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 9)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "convert" "java.lang.invoke.FromGeneric$Adapter" (class "java.lang.invoke.MethodHandle"))))
                                      (4 (dload_1))
                                      (5 (invokevirtual
					(methodCP "invokeExact" "java.lang.invoke.MethodHandle" (double) (class "java.lang.Object"))))
                                      (8 (areturn))
                                      (endofcode 9))
                                   (Exceptions )
                                   (StackMap )))
                        (method "findSubClass"
                              (parameters (class "java.lang.String"))
                              (returntype . (class "java.lang.Class"))
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 38)
                                   (parsedcode
                                      (0 (new (class "java.lang.StringBuilder"))) 
                                      (3 (dup)) 
                                      (4 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (7 (getstatic (fieldCP "CLASS_PREFIX" "java.lang.invoke.FromGeneric$Adapter" (class "java.lang.String")))) 
                                      (10 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (13 (aload_0)) 
                                      (14 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (17 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (20 (astore_1)) 
                                      (21 (aload_1)) ;;at TAG_0
                                      (22 (invokestatic (methodCP "forName" "java.lang.Class" ((class "java.lang.String")) (class "java.lang.Class")))) 
                                      (25 (ldc_w )) 
                                      (28 (invokevirtual (methodCP "asSubclass" "java.lang.Class" ((class "java.lang.Class")) (class "java.lang.Class")))) 
                                      (31 (areturn)) ;;at TAG_1
                                      (32 (astore_2)) ;;at TAG_2
                                      (33 (aconst_null)) 
                                      (34 (areturn)) 
                                      (35 (astore_2)) ;;at TAG_3
                                      (36 (aconst_null)) 
                                      (37 (areturn)) 
                                      (endofcode 38))
                                   (Exceptions 
                                     (handler 21 31  32 (class "java.lang.ClassNotFoundException"))
                                     (handler 21 31  35 (class "java.lang.ClassCastException")))
                                   (StackMap )))
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 4) (max_locals . 2) (code_length . 65)
                                   (parsedcode
                                      (0 (ldc_w )) 
                                      (3 (invokevirtual (methodCP "desiredAssertionStatus" "java.lang.Class" () boolean))) 
                                      (6 (ifne 13)) ;;to TAG_0
                                      (9 (iconst_1)) 
                                      (10 (goto 14)) ;;to TAG_1
                                      (13 (iconst_0)) ;;at TAG_0
                                      (14 (putstatic (fieldCP "$assertionsDisabled" "java.lang.invoke.FromGeneric$Adapter" boolean))) ;;at TAG_1
                                      (17 (ldc_w )) 
                                      (20 (invokevirtual (methodCP "getName" "java.lang.Class" () (class "java.lang.String")))) 
                                      (23 (astore_0)) 
                                      (24 (ldc_w )) 
                                      (27 (invokevirtual (methodCP "getSimpleName" "java.lang.Class" () (class "java.lang.String")))) 
                                      (30 (astore_1)) 
                                      (31 (aload_0)) 
                                      (32 (aload_1)) 
                                      (33 (invokevirtual (methodCP "endsWith" "java.lang.String" ((class "java.lang.String")) boolean))) 
                                      (36 (ifne 47))  ;;to TAG_2
                                      (39 (new (class "java.lang.InternalError"))) 
                                      (42 (dup)) 
                                      (43 (invokespecial (methodCP "<init>" "java.lang.InternalError" () void))) 
                                      (46 (athrow)) 
                                      (47 (aload_0)) ;;at TAG_2
                                      (48 (iconst_0)) 
                                      (49 (aload_0)) 
                                      (50 (invokevirtual (methodCP "length" "java.lang.String" () int))) 
                                      (53 (aload_1)) 
                                      (54 (invokevirtual (methodCP "length" "java.lang.String" () int))) 
                                      (57 (isub)) 
                                      (58 (invokevirtual (methodCP "substring" "java.lang.String" (int int) (class "java.lang.String")))) 
                                      (61 (putstatic (fieldCP "CLASS_PREFIX" "java.lang.invoke.FromGeneric$Adapter" (class "java.lang.String")))) 
                                      (64 (return)) 
                                      (endofcode 65))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *abstract*  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *FromGeneric$Adapter-class-table*
  (make-static-class-decls 
   *java.lang.invoke.FromGeneric$Adapter*))

(defconst *package-name-map* 
  ("java.lang.invoke.FromGeneric$Adapter" . "java.lang.invoke"))

