; Compiler$InstanceFieldExpr-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:50 CDT 2014.
;

(defconst *clojure.lang.Compiler$InstanceFieldExpr*
 (make-class-def
      '(class "clojure.lang.Compiler$InstanceFieldExpr"
            "clojure.lang.Compiler$FieldExpr"
            (constant_pool
                        (STRING  "Reflection warning, %s:%d:%d - reference to field %s can\nt be resolved.\n")
                        (STRING  "Unboxed emit of unknown member")
                        (STRING  "Object invokeNoArgInstanceMember(Object,String)")
                        (STRING  "Object setInstanceField(Object,String,Object)"))
            (fields
                        (field "target" (class "clojure.lang.Compiler$Expr") (accessflags  *class*  *final*  *public* ) -1)
                        (field "targetClass" (class "java.lang.Class") (accessflags  *class*  *final*  *public* ) -1)
                        (field "field" (class "java.lang.reflect.Field") (accessflags  *class*  *final*  *public* ) -1)
                        (field "fieldName" (class "java.lang.String") (accessflags  *class*  *final*  *public* ) -1)
                        (field "line" int (accessflags  *class*  *final*  *public* ) -1)
                        (field "column" int (accessflags  *class*  *final*  *public* ) -1)
                        (field "tag" (class "clojure.lang.Symbol") (accessflags  *class*  *final*  *public* ) -1)
                        (field "invokeNoArgInstanceMember" (class "clojure.asm.commons.Method") (accessflags  *class*  *final*  *static* ) -1)
                        (field "setInstanceFieldMethod" (class "clojure.asm.commons.Method") (accessflags  *class*  *final*  *static* ) -1))
            (methods
                        (method "<init>"
                              (parameters int int (class "clojure.lang.Compiler$Expr") (class "java.lang.String") (class "clojure.lang.Symbol"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 6) (code_length . 140)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokespecial (methodCP "<init>" "clojure.lang.Compiler$FieldExpr" () void))) 
                                      (4 (aload_0)) 
                                      (5 (aload_3)) 
                                      (6 (putfield (fieldCP "target" "clojure.lang.Compiler$InstanceFieldExpr" (class "clojure.lang.Compiler$Expr")))) 
                                      (9 (aload_0)) 
                                      (10 (aload_3)) 
                                      (11 (invokeinterface (methodCP "hasJavaClass" "clojure.lang.Compiler$Expr" () boolean) 1)) 
                                      (16 (ifeq 28)) ;;to TAG_0
                                      (19 (aload_3)) 
                                      (20 (invokeinterface (methodCP "getJavaClass" "clojure.lang.Compiler$Expr" () (class "java.lang.Class")) 1)) 
                                      (25 (goto 29)) ;;to TAG_1
                                      (28 (aconst_null)) ;;at TAG_0
                                      (29 (putfield (fieldCP "targetClass" "clojure.lang.Compiler$InstanceFieldExpr" (class "java.lang.Class")))) ;;at TAG_1
                                      (32 (aload_0)) 
                                      (33 (aload_0)) 
                                      (34 (getfield (fieldCP "targetClass" "clojure.lang.Compiler$InstanceFieldExpr" (class "java.lang.Class")))) 
                                      (37 (ifnull 53))  ;;to TAG_2
                                      (40 (aload_0)) 
                                      (41 (getfield (fieldCP "targetClass" "clojure.lang.Compiler$InstanceFieldExpr" (class "java.lang.Class")))) 
                                      (44 (aload 4)) 
                                      (46 (iconst_0)) 
                                      (47 (invokestatic (methodCP "getField" "clojure.lang.Reflector" ((class "java.lang.Class") (class "java.lang.String") boolean) (class "java.lang.reflect.Field")))) 
                                      (50 (goto 54)) ;;to TAG_3
                                      (53 (aconst_null)) ;;at TAG_2
                                      (54 (putfield (fieldCP "field" "clojure.lang.Compiler$InstanceFieldExpr" (class "java.lang.reflect.Field")))) ;;at TAG_3
                                      (57 (aload_0)) 
                                      (58 (aload 4)) 
                                      (60 (putfield (fieldCP "fieldName" "clojure.lang.Compiler$InstanceFieldExpr" (class "java.lang.String")))) 
                                      (63 (aload_0)) 
                                      (64 (iload_1)) 
                                      (65 (putfield (fieldCP "line" "clojure.lang.Compiler$InstanceFieldExpr" int))) 
                                      (68 (aload_0)) 
                                      (69 (iload_2)) 
                                      (70 (putfield (fieldCP "column" "clojure.lang.Compiler$InstanceFieldExpr" int))) 
                                      (73 (aload_0)) 
                                      (74 (aload 5)) 
                                      (76 (putfield (fieldCP "tag" "clojure.lang.Compiler$InstanceFieldExpr" (class "clojure.lang.Symbol")))) 
                                      (79 (aload_0)) 
                                      (80 (getfield (fieldCP "field" "clojure.lang.Compiler$InstanceFieldExpr" (class "java.lang.reflect.Field")))) 
                                      (83 (ifnonnull 139)) ;;to TAG_4
                                      (86 (getstatic (fieldCP "WARN_ON_REFLECTION" "clojure.lang.RT" (class "clojure.lang.Var")))) 
                                      (89 (invokevirtual (methodCP "deref" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (92 (invokestatic (methodCP "booleanCast" "clojure.lang.RT" ((class "java.lang.Object")) boolean))) 
                                      (95 (ifeq 139)) ;;to TAG_4
                                      (98 (invokestatic (methodCP "errPrintWriter" "clojure.lang.RT" () (class "java.io.PrintWriter")))) 
                                      (101 (ldc 0)) ;;STRING:: "Reflection warning, %s:%d:%d - reference to field %s can\nt be resolved.\n"
                                      (103 (iconst_4)) 
                                      (104 (anewarray (class "java.lang.Object"))) 
                                      (107 (dup)) 
                                      (108 (iconst_0)) 
                                      (109 (getstatic (fieldCP "SOURCE_PATH" "clojure.lang.Compiler" (class "clojure.lang.Var")))) 
                                      (112 (invokevirtual (methodCP "deref" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (115 (aastore)) 
                                      (116 (dup)) 
                                      (117 (iconst_1)) 
                                      (118 (iload_1)) 
                                      (119 (invokestatic (methodCP "valueOf" "java.lang.Integer" (int) (class "java.lang.Integer")))) 
                                      (122 (aastore)) 
                                      (123 (dup)) 
                                      (124 (iconst_2)) 
                                      (125 (iload_2)) 
                                      (126 (invokestatic (methodCP "valueOf" "java.lang.Integer" (int) (class "java.lang.Integer")))) 
                                      (129 (aastore)) 
                                      (130 (dup)) 
                                      (131 (iconst_3)) 
                                      (132 (aload 4)) 
                                      (134 (aastore)) 
                                      (135 (invokevirtual (methodCP "format" "java.io.PrintWriter" ((class "java.lang.String") (array (class "java.lang.Object"))) (class "java.io.PrintWriter")))) 
                                      (138 (pop)) 
                                      (139 (return)) ;;at TAG_4
                                      (endofcode 140))
                                   (Exceptions )
                                   (StackMap )))
                        (method "eval"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 17)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "target" "clojure.lang.Compiler$InstanceFieldExpr" (class "clojure.lang.Compiler$Expr"))))
                                      (4 (invokeinterface
					(methodCP "eval" "clojure.lang.Compiler$Expr" () (class "java.lang.Object")) 1))
                                      (9 (aload_0))
                                      (10 (getfield (fieldCP "fieldName" "clojure.lang.Compiler$InstanceFieldExpr" (class "java.lang.String"))))
                                      (13 (invokestatic
					(methodCP "invokeNoArgInstanceMember" "clojure.lang.Reflector" ((class "java.lang.Object") (class "java.lang.String")) (class "java.lang.Object"))))
                                      (16 (areturn))
                                      (endofcode 17))
                                   (Exceptions )
                                   (StackMap )))
                        (method "canEmitPrimitive"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 33)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "targetClass" "clojure.lang.Compiler$InstanceFieldExpr" (class "java.lang.Class")))) 
                                      (4 (ifnull 31))  ;;to TAG_0
                                      (7 (aload_0)) 
                                      (8 (getfield (fieldCP "field" "clojure.lang.Compiler$InstanceFieldExpr" (class "java.lang.reflect.Field")))) 
                                      (11 (ifnull 31))  ;;to TAG_0
                                      (14 (aload_0)) 
                                      (15 (getfield (fieldCP "field" "clojure.lang.Compiler$InstanceFieldExpr" (class "java.lang.reflect.Field")))) 
                                      (18 (invokevirtual (methodCP "getType" "java.lang.reflect.Field" () (class "java.lang.Class")))) 
                                      (21 (invokestatic (methodCP "isPrimitive" "clojure.lang.Util" ((class "java.lang.Class")) boolean))) 
                                      (24 (ifeq 31))  ;;to TAG_0
                                      (27 (iconst_1)) 
                                      (28 (goto 32)) ;;to TAG_1
                                      (31 (iconst_0)) ;;at TAG_0
                                      (32 (ireturn)) ;;at TAG_1
                                      (endofcode 33))
                                   (Exceptions )
                                   (StackMap )))
                        (method "emitUnboxed"
                              (parameters (class "clojure.lang.Compiler$C") (class "clojure.lang.Compiler$ObjExpr") (class "clojure.asm.commons.GeneratorAdapter"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 4) (code_length . 90)
                                   (parsedcode
                                      (0 (aload_3)) 
                                      (1 (aload_0)) 
                                      (2 (getfield (fieldCP "line" "clojure.lang.Compiler$InstanceFieldExpr" int))) 
                                      (5 (aload_3)) 
                                      (6 (invokevirtual (methodCP "mark" "clojure.asm.commons.GeneratorAdapter" () (class "clojure.asm.Label")))) 
                                      (9 (invokevirtual (methodCP "visitLineNumber" "clojure.asm.commons.GeneratorAdapter" (int (class "clojure.asm.Label")) void))) 
                                      (12 (aload_0)) 
                                      (13 (getfield (fieldCP "targetClass" "clojure.lang.Compiler$InstanceFieldExpr" (class "java.lang.Class")))) 
                                      (16 (ifnull 79))  ;;to TAG_0
                                      (19 (aload_0)) 
                                      (20 (getfield (fieldCP "field" "clojure.lang.Compiler$InstanceFieldExpr" (class "java.lang.reflect.Field")))) 
                                      (23 (ifnull 79))  ;;to TAG_0
                                      (26 (aload_0)) 
                                      (27 (getfield (fieldCP "target" "clojure.lang.Compiler$InstanceFieldExpr" (class "clojure.lang.Compiler$Expr")))) 
                                      (30 (getstatic (fieldCP "EXPRESSION" "clojure.lang.Compiler$C" (class "clojure.lang.Compiler$C")))) 
                                      (33 (aload_2)) 
                                      (34 (aload_3)) 
                                      (35 (invokeinterface (methodCP "emit" "clojure.lang.Compiler$Expr" ((class "clojure.lang.Compiler$C") (class "clojure.lang.Compiler$ObjExpr") (class "clojure.asm.commons.GeneratorAdapter")) void) 4)) 
                                      (40 (aload_3)) 
                                      (41 (aload_0)) 
                                      (42 (getfield (fieldCP "targetClass" "clojure.lang.Compiler$InstanceFieldExpr" (class "java.lang.Class")))) 
                                      (45 (invokestatic (methodCP "getType" "clojure.lang.Compiler" ((class "java.lang.Class")) (class "clojure.asm.Type")))) 
                                      (48 (invokevirtual (methodCP "checkCast" "clojure.asm.commons.GeneratorAdapter" ((class "clojure.asm.Type")) void))) 
                                      (51 (aload_3)) 
                                      (52 (aload_0)) 
                                      (53 (getfield (fieldCP "targetClass" "clojure.lang.Compiler$InstanceFieldExpr" (class "java.lang.Class")))) 
                                      (56 (invokestatic (methodCP "getType" "clojure.lang.Compiler" ((class "java.lang.Class")) (class "clojure.asm.Type")))) 
                                      (59 (aload_0)) 
                                      (60 (getfield (fieldCP "fieldName" "clojure.lang.Compiler$InstanceFieldExpr" (class "java.lang.String")))) 
                                      (63 (aload_0)) 
                                      (64 (getfield (fieldCP "field" "clojure.lang.Compiler$InstanceFieldExpr" (class "java.lang.reflect.Field")))) 
                                      (67 (invokevirtual (methodCP "getType" "java.lang.reflect.Field" () (class "java.lang.Class")))) 
                                      (70 (invokestatic (methodCP "getType" "clojure.asm.Type" ((class "java.lang.Class")) (class "clojure.asm.Type")))) 
                                      (73 (invokevirtual (methodCP "getField" "clojure.asm.commons.GeneratorAdapter" ((class "clojure.asm.Type") (class "java.lang.String") (class "clojure.asm.Type")) void))) 
                                      (76 (goto 89)) ;;to TAG_1
                                      (79 (new (class "java.lang.UnsupportedOperationException"))) ;;at TAG_0
                                      (82 (dup)) 
                                      (83 (ldc 1)) ;;STRING:: "Unboxed emit of unknown member"
                                      (85 (invokespecial (methodCP "<init>" "java.lang.UnsupportedOperationException" ((class "java.lang.String")) void))) 
                                      (88 (athrow)) 
                                      (89 (return)) ;;at TAG_1
                                      (endofcode 90))
                                   (Exceptions )
                                   (StackMap )))
                        (method "emit"
                              (parameters (class "clojure.lang.Compiler$C") (class "clojure.lang.Compiler$ObjExpr") (class "clojure.asm.commons.GeneratorAdapter"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 4) (code_length . 146)
                                   (parsedcode
                                      (0 (aload_3)) 
                                      (1 (aload_0)) 
                                      (2 (getfield (fieldCP "line" "clojure.lang.Compiler$InstanceFieldExpr" int))) 
                                      (5 (aload_3)) 
                                      (6 (invokevirtual (methodCP "mark" "clojure.asm.commons.GeneratorAdapter" () (class "clojure.asm.Label")))) 
                                      (9 (invokevirtual (methodCP "visitLineNumber" "clojure.asm.commons.GeneratorAdapter" (int (class "clojure.asm.Label")) void))) 
                                      (12 (aload_0)) 
                                      (13 (getfield (fieldCP "targetClass" "clojure.lang.Compiler$InstanceFieldExpr" (class "java.lang.Class")))) 
                                      (16 (ifnull 102))  ;;to TAG_0
                                      (19 (aload_0)) 
                                      (20 (getfield (fieldCP "field" "clojure.lang.Compiler$InstanceFieldExpr" (class "java.lang.reflect.Field")))) 
                                      (23 (ifnull 102))  ;;to TAG_0
                                      (26 (aload_0)) 
                                      (27 (getfield (fieldCP "target" "clojure.lang.Compiler$InstanceFieldExpr" (class "clojure.lang.Compiler$Expr")))) 
                                      (30 (getstatic (fieldCP "EXPRESSION" "clojure.lang.Compiler$C" (class "clojure.lang.Compiler$C")))) 
                                      (33 (aload_2)) 
                                      (34 (aload_3)) 
                                      (35 (invokeinterface (methodCP "emit" "clojure.lang.Compiler$Expr" ((class "clojure.lang.Compiler$C") (class "clojure.lang.Compiler$ObjExpr") (class "clojure.asm.commons.GeneratorAdapter")) void) 4)) 
                                      (40 (aload_3)) 
                                      (41 (aload_0)) 
                                      (42 (getfield (fieldCP "targetClass" "clojure.lang.Compiler$InstanceFieldExpr" (class "java.lang.Class")))) 
                                      (45 (invokestatic (methodCP "getType" "clojure.lang.Compiler" ((class "java.lang.Class")) (class "clojure.asm.Type")))) 
                                      (48 (invokevirtual (methodCP "checkCast" "clojure.asm.commons.GeneratorAdapter" ((class "clojure.asm.Type")) void))) 
                                      (51 (aload_3)) 
                                      (52 (aload_0)) 
                                      (53 (getfield (fieldCP "targetClass" "clojure.lang.Compiler$InstanceFieldExpr" (class "java.lang.Class")))) 
                                      (56 (invokestatic (methodCP "getType" "clojure.lang.Compiler" ((class "java.lang.Class")) (class "clojure.asm.Type")))) 
                                      (59 (aload_0)) 
                                      (60 (getfield (fieldCP "fieldName" "clojure.lang.Compiler$InstanceFieldExpr" (class "java.lang.String")))) 
                                      (63 (aload_0)) 
                                      (64 (getfield (fieldCP "field" "clojure.lang.Compiler$InstanceFieldExpr" (class "java.lang.reflect.Field")))) 
                                      (67 (invokevirtual (methodCP "getType" "java.lang.reflect.Field" () (class "java.lang.Class")))) 
                                      (70 (invokestatic (methodCP "getType" "clojure.asm.Type" ((class "java.lang.Class")) (class "clojure.asm.Type")))) 
                                      (73 (invokevirtual (methodCP "getField" "clojure.asm.commons.GeneratorAdapter" ((class "clojure.asm.Type") (class "java.lang.String") (class "clojure.asm.Type")) void))) 
                                      (76 (aload_2)) 
                                      (77 (aload_3)) 
                                      (78 (aload_0)) 
                                      (79 (getfield (fieldCP "field" "clojure.lang.Compiler$InstanceFieldExpr" (class "java.lang.reflect.Field")))) 
                                      (82 (invokevirtual (methodCP "getType" "java.lang.reflect.Field" () (class "java.lang.Class")))) 
                                      (85 (invokestatic (methodCP "emitBoxReturn" "clojure.lang.Compiler$HostExpr" ((class "clojure.lang.Compiler$ObjExpr") (class "clojure.asm.commons.GeneratorAdapter") (class "java.lang.Class")) void))) 
                                      (88 (aload_1)) 
                                      (89 (getstatic (fieldCP "STATEMENT" "clojure.lang.Compiler$C" (class "clojure.lang.Compiler$C")))) 
                                      (92 (if_acmpne 145)) ;;to TAG_1
                                      (95 (aload_3)) 
                                      (96 (invokevirtual (methodCP "pop" "clojure.asm.commons.GeneratorAdapter" () void))) 
                                      (99 (goto 145)) ;;to TAG_1
                                      (102 (aload_0)) ;;at TAG_0
                                      (103 (getfield (fieldCP "target" "clojure.lang.Compiler$InstanceFieldExpr" (class "clojure.lang.Compiler$Expr")))) 
                                      (106 (getstatic (fieldCP "EXPRESSION" "clojure.lang.Compiler$C" (class "clojure.lang.Compiler$C")))) 
                                      (109 (aload_2)) 
                                      (110 (aload_3)) 
                                      (111 (invokeinterface (methodCP "emit" "clojure.lang.Compiler$Expr" ((class "clojure.lang.Compiler$C") (class "clojure.lang.Compiler$ObjExpr") (class "clojure.asm.commons.GeneratorAdapter")) void) 4)) 
                                      (116 (aload_3)) 
                                      (117 (aload_0)) 
                                      (118 (getfield (fieldCP "fieldName" "clojure.lang.Compiler$InstanceFieldExpr" (class "java.lang.String")))) 
                                      (121 (invokevirtual (methodCP "push" "clojure.asm.commons.GeneratorAdapter" ((class "java.lang.String")) void))) 
                                      (124 (aload_3)) 
                                      (125 (getstatic (fieldCP "REFLECTOR_TYPE" "clojure.lang.Compiler" (class "clojure.asm.Type")))) 
                                      (128 (getstatic (fieldCP "invokeNoArgInstanceMember" "clojure.lang.Compiler$InstanceFieldExpr" (class "clojure.asm.commons.Method")))) 
                                      (131 (invokevirtual (methodCP "invokeStatic" "clojure.asm.commons.GeneratorAdapter" ((class "clojure.asm.Type") (class "clojure.asm.commons.Method")) void))) 
                                      (134 (aload_1)) 
                                      (135 (getstatic (fieldCP "STATEMENT" "clojure.lang.Compiler$C" (class "clojure.lang.Compiler$C")))) 
                                      (138 (if_acmpne 145)) ;;to TAG_1
                                      (141 (aload_3)) 
                                      (142 (invokevirtual (methodCP "pop" "clojure.asm.commons.GeneratorAdapter" () void))) 
                                      (145 (return)) ;;at TAG_1
                                      (endofcode 146))
                                   (Exceptions )
                                   (StackMap )))
                        (method "hasJavaClass"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 20)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "field" "clojure.lang.Compiler$InstanceFieldExpr" (class "java.lang.reflect.Field")))) 
                                      (4 (ifnonnull 14)) ;;to TAG_0
                                      (7 (aload_0)) 
                                      (8 (getfield (fieldCP "tag" "clojure.lang.Compiler$InstanceFieldExpr" (class "clojure.lang.Symbol")))) 
                                      (11 (ifnull 18)) ;;to TAG_1
                                      (14 (iconst_1)) ;;at TAG_0
                                      (15 (goto 19))  ;;to TAG_2
                                      (18 (iconst_0)) ;;at TAG_1
                                      (19 (ireturn)) ;;at TAG_2
                                      (endofcode 20))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getJavaClass"
                              (parameters )
                              (returntype . (class "java.lang.Class"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 25)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "tag" "clojure.lang.Compiler$InstanceFieldExpr" (class "clojure.lang.Symbol")))) 
                                      (4 (ifnull 17))  ;;to TAG_0
                                      (7 (aload_0)) 
                                      (8 (getfield (fieldCP "tag" "clojure.lang.Compiler$InstanceFieldExpr" (class "clojure.lang.Symbol")))) 
                                      (11 (invokestatic (methodCP "tagToClass" "clojure.lang.Compiler$HostExpr" ((class "java.lang.Object")) (class "java.lang.Class")))) 
                                      (14 (goto 24)) ;;to TAG_1
                                      (17 (aload_0)) ;;at TAG_0
                                      (18 (getfield (fieldCP "field" "clojure.lang.Compiler$InstanceFieldExpr" (class "java.lang.reflect.Field")))) 
                                      (21 (invokevirtual (methodCP "getType" "java.lang.reflect.Field" () (class "java.lang.Class")))) 
                                      (24 (areturn)) ;;at TAG_1
                                      (endofcode 25))
                                   (Exceptions )
                                   (StackMap )))
                        (method "evalAssign"
                              (parameters (class "clojure.lang.Compiler$Expr"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 23)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "target" "clojure.lang.Compiler$InstanceFieldExpr" (class "clojure.lang.Compiler$Expr"))))
                                      (4 (invokeinterface
					(methodCP "eval" "clojure.lang.Compiler$Expr" () (class "java.lang.Object")) 1))
                                      (9 (aload_0))
                                      (10 (getfield (fieldCP "fieldName" "clojure.lang.Compiler$InstanceFieldExpr" (class "java.lang.String"))))
                                      (13 (aload_1))
                                      (14 (invokeinterface
					(methodCP "eval" "clojure.lang.Compiler$Expr" () (class "java.lang.Object")) 1))
                                      (19 (invokestatic
					(methodCP "setInstanceField" "clojure.lang.Reflector" ((class "java.lang.Object") (class "java.lang.String") (class "java.lang.Object")) (class "java.lang.Object"))))
                                      (22 (areturn))
                                      (endofcode 23))
                                   (Exceptions )
                                   (StackMap )))
                        (method "emitAssign"
                              (parameters (class "clojure.lang.Compiler$C") (class "clojure.lang.Compiler$ObjExpr") (class "clojure.asm.commons.GeneratorAdapter") (class "clojure.lang.Compiler$Expr"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 5) (code_length . 163)
                                   (parsedcode
                                      (0 (aload_3)) 
                                      (1 (aload_0)) 
                                      (2 (getfield (fieldCP "line" "clojure.lang.Compiler$InstanceFieldExpr" int))) 
                                      (5 (aload_3)) 
                                      (6 (invokevirtual (methodCP "mark" "clojure.asm.commons.GeneratorAdapter" () (class "clojure.asm.Label")))) 
                                      (9 (invokevirtual (methodCP "visitLineNumber" "clojure.asm.commons.GeneratorAdapter" (int (class "clojure.asm.Label")) void))) 
                                      (12 (aload_0)) 
                                      (13 (getfield (fieldCP "targetClass" "clojure.lang.Compiler$InstanceFieldExpr" (class "java.lang.Class")))) 
                                      (16 (ifnull 107)) ;;to TAG_0
                                      (19 (aload_0)) 
                                      (20 (getfield (fieldCP "field" "clojure.lang.Compiler$InstanceFieldExpr" (class "java.lang.reflect.Field")))) 
                                      (23 (ifnull 107)) ;;to TAG_0
                                      (26 (aload_0)) 
                                      (27 (getfield (fieldCP "target" "clojure.lang.Compiler$InstanceFieldExpr" (class "clojure.lang.Compiler$Expr")))) 
                                      (30 (getstatic (fieldCP "EXPRESSION" "clojure.lang.Compiler$C" (class "clojure.lang.Compiler$C")))) 
                                      (33 (aload_2)) 
                                      (34 (aload_3)) 
                                      (35 (invokeinterface (methodCP "emit" "clojure.lang.Compiler$Expr" ((class "clojure.lang.Compiler$C") (class "clojure.lang.Compiler$ObjExpr") (class "clojure.asm.commons.GeneratorAdapter")) void) 4)) 
                                      (40 (aload_3)) 
                                      (41 (aload_0)) 
                                      (42 (getfield (fieldCP "targetClass" "clojure.lang.Compiler$InstanceFieldExpr" (class "java.lang.Class")))) 
                                      (45 (invokestatic (methodCP "getType" "clojure.asm.Type" ((class "java.lang.Class")) (class "clojure.asm.Type")))) 
                                      (48 (invokevirtual (methodCP "checkCast" "clojure.asm.commons.GeneratorAdapter" ((class "clojure.asm.Type")) void))) 
                                      (51 (aload 4)) 
                                      (53 (getstatic (fieldCP "EXPRESSION" "clojure.lang.Compiler$C" (class "clojure.lang.Compiler$C")))) 
                                      (56 (aload_2)) 
                                      (57 (aload_3)) 
                                      (58 (invokeinterface (methodCP "emit" "clojure.lang.Compiler$Expr" ((class "clojure.lang.Compiler$C") (class "clojure.lang.Compiler$ObjExpr") (class "clojure.asm.commons.GeneratorAdapter")) void) 4)) 
                                      (63 (aload_3)) 
                                      (64 (invokevirtual (methodCP "dupX1" "clojure.asm.commons.GeneratorAdapter" () void))) 
                                      (67 (aload_2)) 
                                      (68 (aload_3)) 
                                      (69 (aload_0)) 
                                      (70 (getfield (fieldCP "field" "clojure.lang.Compiler$InstanceFieldExpr" (class "java.lang.reflect.Field")))) 
                                      (73 (invokevirtual (methodCP "getType" "java.lang.reflect.Field" () (class "java.lang.Class")))) 
                                      (76 (invokestatic (methodCP "emitUnboxArg" "clojure.lang.Compiler$HostExpr" ((class "clojure.lang.Compiler$ObjExpr") (class "clojure.asm.commons.GeneratorAdapter") (class "java.lang.Class")) void))) 
                                      (79 (aload_3)) 
                                      (80 (aload_0)) 
                                      (81 (getfield (fieldCP "targetClass" "clojure.lang.Compiler$InstanceFieldExpr" (class "java.lang.Class")))) 
                                      (84 (invokestatic (methodCP "getType" "clojure.asm.Type" ((class "java.lang.Class")) (class "clojure.asm.Type")))) 
                                      (87 (aload_0)) 
                                      (88 (getfield (fieldCP "fieldName" "clojure.lang.Compiler$InstanceFieldExpr" (class "java.lang.String")))) 
                                      (91 (aload_0)) 
                                      (92 (getfield (fieldCP "field" "clojure.lang.Compiler$InstanceFieldExpr" (class "java.lang.reflect.Field")))) 
                                      (95 (invokevirtual (methodCP "getType" "java.lang.reflect.Field" () (class "java.lang.Class")))) 
                                      (98 (invokestatic (methodCP "getType" "clojure.asm.Type" ((class "java.lang.Class")) (class "clojure.asm.Type")))) 
                                      (101 (invokevirtual (methodCP "putField" "clojure.asm.commons.GeneratorAdapter" ((class "clojure.asm.Type") (class "java.lang.String") (class "clojure.asm.Type")) void))) 
                                      (104 (goto 151)) ;;to TAG_1
                                      (107 (aload_0)) ;;at TAG_0
                                      (108 (getfield (fieldCP "target" "clojure.lang.Compiler$InstanceFieldExpr" (class "clojure.lang.Compiler$Expr")))) 
                                      (111 (getstatic (fieldCP "EXPRESSION" "clojure.lang.Compiler$C" (class "clojure.lang.Compiler$C")))) 
                                      (114 (aload_2)) 
                                      (115 (aload_3)) 
                                      (116 (invokeinterface (methodCP "emit" "clojure.lang.Compiler$Expr" ((class "clojure.lang.Compiler$C") (class "clojure.lang.Compiler$ObjExpr") (class "clojure.asm.commons.GeneratorAdapter")) void) 4)) 
                                      (121 (aload_3)) 
                                      (122 (aload_0)) 
                                      (123 (getfield (fieldCP "fieldName" "clojure.lang.Compiler$InstanceFieldExpr" (class "java.lang.String")))) 
                                      (126 (invokevirtual (methodCP "push" "clojure.asm.commons.GeneratorAdapter" ((class "java.lang.String")) void))) 
                                      (129 (aload 4)) 
                                      (131 (getstatic (fieldCP "EXPRESSION" "clojure.lang.Compiler$C" (class "clojure.lang.Compiler$C")))) 
                                      (134 (aload_2)) 
                                      (135 (aload_3)) 
                                      (136 (invokeinterface (methodCP "emit" "clojure.lang.Compiler$Expr" ((class "clojure.lang.Compiler$C") (class "clojure.lang.Compiler$ObjExpr") (class "clojure.asm.commons.GeneratorAdapter")) void) 4)) 
                                      (141 (aload_3)) 
                                      (142 (getstatic (fieldCP "REFLECTOR_TYPE" "clojure.lang.Compiler" (class "clojure.asm.Type")))) 
                                      (145 (getstatic (fieldCP "setInstanceFieldMethod" "clojure.lang.Compiler$InstanceFieldExpr" (class "clojure.asm.commons.Method")))) 
                                      (148 (invokevirtual (methodCP "invokeStatic" "clojure.asm.commons.GeneratorAdapter" ((class "clojure.asm.Type") (class "clojure.asm.commons.Method")) void))) 
                                      (151 (aload_1)) ;;at TAG_1
                                      (152 (getstatic (fieldCP "STATEMENT" "clojure.lang.Compiler$C" (class "clojure.lang.Compiler$C")))) 
                                      (155 (if_acmpne 162))  ;;to TAG_2
                                      (158 (aload_3)) 
                                      (159 (invokevirtual (methodCP "pop" "clojure.asm.commons.GeneratorAdapter" () void))) 
                                      (162 (return)) ;;at TAG_2
                                      (endofcode 163))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 1) (max_locals . 0) (code_length . 17)
                                   (parsedcode
                                      (0 (ldc 2))         ;;STRING:: "Object invokeNoArgInstanceMember(Object,String)"
                                      (2 (invokestatic
					(methodCP "getMethod" "clojure.asm.commons.Method" ((class "java.lang.String")) (class "clojure.asm.commons.Method"))))
                                      (5 (putstatic (fieldCP "invokeNoArgInstanceMember" "clojure.lang.Compiler$InstanceFieldExpr" (class "clojure.asm.commons.Method"))))
                                      (8 (ldc 3))         ;;STRING:: "Object setInstanceField(Object,String,Object)"
                                      (10 (invokestatic
					(methodCP "getMethod" "clojure.asm.commons.Method" ((class "java.lang.String")) (class "clojure.asm.commons.Method"))))
                                      (13 (putstatic (fieldCP "setInstanceFieldMethod" "clojure.lang.Compiler$InstanceFieldExpr" (class "clojure.asm.commons.Method"))))
                                      (16 (return))
                                      (endofcode 17))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "clojure.lang.Compiler$AssignableExpr")
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *Compiler$InstanceFieldExpr-class-table*
  (make-static-class-decls 
   *clojure.lang.Compiler$InstanceFieldExpr*))

(defconst *package-name-map* 
  ("clojure.lang.Compiler$InstanceFieldExpr" . "clojure.lang"))

