; Compiler$IfExpr-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:50 CDT 2014.
;

(defconst *clojure.lang.Compiler$IfExpr*
 (make-class-def
      '(class "clojure.lang.Compiler$IfExpr"
            "java.lang.Object"
            (constant_pool
                        (STRING  "FALSE"))
            (fields
                        (field "testExpr" (class "clojure.lang.Compiler$Expr") (accessflags  *class*  *final*  *public* ) -1)
                        (field "thenExpr" (class "clojure.lang.Compiler$Expr") (accessflags  *class*  *final*  *public* ) -1)
                        (field "elseExpr" (class "clojure.lang.Compiler$Expr") (accessflags  *class*  *final*  *public* ) -1)
                        (field "line" int (accessflags  *class*  *final*  *public* ) -1)
                        (field "column" int (accessflags  *class*  *final*  *public* ) -1))
            (methods
                        (method "<init>"
                              (parameters int int (class "clojure.lang.Compiler$Expr") (class "clojure.lang.Compiler$Expr") (class "clojure.lang.Compiler$Expr"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 6) (code_length . 32)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (aload_0))
                                      (5 (aload_3))
                                      (6 (putfield (fieldCP "testExpr" "clojure.lang.Compiler$IfExpr" (class "clojure.lang.Compiler$Expr"))))
                                      (9 (aload_0))
                                      (10 (aload 4))
                                      (12 (putfield (fieldCP "thenExpr" "clojure.lang.Compiler$IfExpr" (class "clojure.lang.Compiler$Expr"))))
                                      (15 (aload_0))
                                      (16 (aload 5))
                                      (18 (putfield (fieldCP "elseExpr" "clojure.lang.Compiler$IfExpr" (class "clojure.lang.Compiler$Expr"))))
                                      (21 (aload_0))
                                      (22 (iload_1))
                                      (23 (putfield (fieldCP "line" "clojure.lang.Compiler$IfExpr" int)))
                                      (26 (aload_0))
                                      (27 (iload_2))
                                      (28 (putfield (fieldCP "column" "clojure.lang.Compiler$IfExpr" int)))
                                      (31 (return))
                                      (endofcode 32))
                                   (Exceptions )
                                   (StackMap )))
                        (method "eval"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 41)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "testExpr" "clojure.lang.Compiler$IfExpr" (class "clojure.lang.Compiler$Expr")))) 
                                      (4 (invokeinterface (methodCP "eval" "clojure.lang.Compiler$Expr" () (class "java.lang.Object")) 1)) 
                                      (9 (astore_1)) 
                                      (10 (aload_1)) 
                                      (11 (ifnull 31))  ;;to TAG_0
                                      (14 (aload_1)) 
                                      (15 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (18 (if_acmpeq 31))  ;;to TAG_0
                                      (21 (aload_0)) 
                                      (22 (getfield (fieldCP "thenExpr" "clojure.lang.Compiler$IfExpr" (class "clojure.lang.Compiler$Expr")))) 
                                      (25 (invokeinterface (methodCP "eval" "clojure.lang.Compiler$Expr" () (class "java.lang.Object")) 1)) 
                                      (30 (areturn)) 
                                      (31 (aload_0)) ;;at TAG_0
                                      (32 (getfield (fieldCP "elseExpr" "clojure.lang.Compiler$IfExpr" (class "clojure.lang.Compiler$Expr")))) 
                                      (35 (invokeinterface (methodCP "eval" "clojure.lang.Compiler$Expr" () (class "java.lang.Object")) 1)) 
                                      (40 (areturn)) 
                                      (endofcode 41))
                                   (Exceptions )
                                   (StackMap )))
                        (method "emit"
                              (parameters (class "clojure.lang.Compiler$C") (class "clojure.lang.Compiler$ObjExpr") (class "clojure.asm.commons.GeneratorAdapter"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 4) (code_length . 9)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (aload_2))
                                      (3 (aload_3))
                                      (4 (iconst_0))
                                      (5 (invokevirtual
					(methodCP "doEmit" "clojure.lang.Compiler$IfExpr" ((class "clojure.lang.Compiler$C") (class "clojure.lang.Compiler$ObjExpr") (class "clojure.asm.commons.GeneratorAdapter") boolean) void)))
                                      (8 (return))
                                      (endofcode 9))
                                   (Exceptions )
                                   (StackMap )))
                        (method "emitUnboxed"
                              (parameters (class "clojure.lang.Compiler$C") (class "clojure.lang.Compiler$ObjExpr") (class "clojure.asm.commons.GeneratorAdapter"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 4) (code_length . 9)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (aload_2))
                                      (3 (aload_3))
                                      (4 (iconst_1))
                                      (5 (invokevirtual
					(methodCP "doEmit" "clojure.lang.Compiler$IfExpr" ((class "clojure.lang.Compiler$C") (class "clojure.lang.Compiler$ObjExpr") (class "clojure.asm.commons.GeneratorAdapter") boolean) void)))
                                      (8 (return))
                                      (endofcode 9))
                                   (Exceptions )
                                   (StackMap )))
                        (method "doEmit"
                              (parameters (class "clojure.lang.Compiler$C") (class "clojure.lang.Compiler$ObjExpr") (class "clojure.asm.commons.GeneratorAdapter") boolean)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 9) (code_length . 272)
                                   (parsedcode
                                      (0 (aload_3)) 
                                      (1 (invokevirtual (methodCP "newLabel" "clojure.asm.commons.GeneratorAdapter" () (class "clojure.asm.Label")))) 
                                      (4 (astore 5)) 
                                      (6 (aload_3)) 
                                      (7 (invokevirtual (methodCP "newLabel" "clojure.asm.commons.GeneratorAdapter" () (class "clojure.asm.Label")))) 
                                      (10 (astore 6)) 
                                      (12 (aload_3)) 
                                      (13 (invokevirtual (methodCP "newLabel" "clojure.asm.commons.GeneratorAdapter" () (class "clojure.asm.Label")))) 
                                      (16 (astore 7)) 
                                      (18 (aload_3)) 
                                      (19 (aload_0)) 
                                      (20 (getfield (fieldCP "line" "clojure.lang.Compiler$IfExpr" int))) 
                                      (23 (aload_3)) 
                                      (24 (invokevirtual (methodCP "mark" "clojure.asm.commons.GeneratorAdapter" () (class "clojure.asm.Label")))) 
                                      (27 (invokevirtual (methodCP "visitLineNumber" "clojure.asm.commons.GeneratorAdapter" (int (class "clojure.asm.Label")) void))) 
                                      (30 (aload_0)) ;;at TAG_8
                                      (31 (getfield (fieldCP "testExpr" "clojure.lang.Compiler$IfExpr" (class "clojure.lang.Compiler$Expr")))) 
                                      (34 (instanceof (class "clojure.lang.Compiler$StaticMethodExpr"))) 
                                      (37 (ifeq 73)) ;;to TAG_0
                                      (40 (aload_0)) 
                                      (41 (getfield (fieldCP "testExpr" "clojure.lang.Compiler$IfExpr" (class "clojure.lang.Compiler$Expr")))) 
                                      (44 (checkcast (class "clojure.lang.Compiler$StaticMethodExpr"))) 
                                      (47 (invokevirtual (methodCP "canEmitIntrinsicPredicate" "clojure.lang.Compiler$StaticMethodExpr" () boolean))) 
                                      (50 (ifeq 73)) ;;to TAG_0
                                      (53 (aload_0)) 
                                      (54 (getfield (fieldCP "testExpr" "clojure.lang.Compiler$IfExpr" (class "clojure.lang.Compiler$Expr")))) 
                                      (57 (checkcast (class "clojure.lang.Compiler$StaticMethodExpr"))) 
                                      (60 (getstatic (fieldCP "EXPRESSION" "clojure.lang.Compiler$C" (class "clojure.lang.Compiler$C")))) 
                                      (63 (aload_2)) 
                                      (64 (aload_3)) 
                                      (65 (aload 6)) 
                                      (67 (invokevirtual (methodCP "emitIntrinsicPredicate" "clojure.lang.Compiler$StaticMethodExpr" ((class "clojure.lang.Compiler$C") (class "clojure.lang.Compiler$ObjExpr") (class "clojure.asm.commons.GeneratorAdapter") (class "clojure.asm.Label")) void))) 
                                      (70 (goto 162))  ;;to TAG_1
                                      (73 (aload_0)) ;;at TAG_0
                                      (74 (getfield (fieldCP "testExpr" "clojure.lang.Compiler$IfExpr" (class "clojure.lang.Compiler$Expr")))) 
                                      (77 (invokestatic (methodCP "maybePrimitiveType" "clojure.lang.Compiler" ((class "clojure.lang.Compiler$Expr")) (class "java.lang.Class")))) 
                                      (80 (getstatic (fieldCP "TYPE" "java.lang.Boolean" (class "java.lang.Class")))) 
                                      (83 (if_acmpne 117)) ;;to TAG_2
                                      (86 (aload_0)) 
                                      (87 (getfield (fieldCP "testExpr" "clojure.lang.Compiler$IfExpr" (class "clojure.lang.Compiler$Expr")))) 
                                      (90 (checkcast (class "clojure.lang.Compiler$MaybePrimitiveExpr"))) 
                                      (93 (getstatic (fieldCP "EXPRESSION" "clojure.lang.Compiler$C" (class "clojure.lang.Compiler$C")))) 
                                      (96 (aload_2)) 
                                      (97 (aload_3)) 
                                      (98 (invokeinterface (methodCP "emitUnboxed" "clojure.lang.Compiler$MaybePrimitiveExpr" ((class "clojure.lang.Compiler$C") (class "clojure.lang.Compiler$ObjExpr") (class "clojure.asm.commons.GeneratorAdapter")) void) 4)) 
                                      (103 (aload_3)) 
                                      (104 (aload_3)) 
                                      (105 (pop)) 
                                      (106 (sipush 153)) 
                                      (109 (aload 6)) 
                                      (111 (invokevirtual (methodCP "ifZCmp" "clojure.asm.commons.GeneratorAdapter" (int (class "clojure.asm.Label")) void))) 
                                      (114 (goto 162))  ;;to TAG_1
                                      (117 (aload_0)) ;;at TAG_2
                                      (118 (getfield (fieldCP "testExpr" "clojure.lang.Compiler$IfExpr" (class "clojure.lang.Compiler$Expr")))) 
                                      (121 (getstatic (fieldCP "EXPRESSION" "clojure.lang.Compiler$C" (class "clojure.lang.Compiler$C")))) 
                                      (124 (aload_2)) 
                                      (125 (aload_3)) 
                                      (126 (invokeinterface (methodCP "emit" "clojure.lang.Compiler$Expr" ((class "clojure.lang.Compiler$C") (class "clojure.lang.Compiler$ObjExpr") (class "clojure.asm.commons.GeneratorAdapter")) void) 4)) 
                                      (131 (aload_3)) 
                                      (132 (invokevirtual (methodCP "dup" "clojure.asm.commons.GeneratorAdapter" () void))) 
                                      (135 (aload_3)) 
                                      (136 (aload 5)) 
                                      (138 (invokevirtual (methodCP "ifNull" "clojure.asm.commons.GeneratorAdapter" ((class "clojure.asm.Label")) void))) 
                                      (141 (aload_3)) 
                                      (142 (getstatic (fieldCP "BOOLEAN_OBJECT_TYPE" "clojure.lang.Compiler" (class "clojure.asm.Type")))) 
                                      (145 (ldc 0)) ;;STRING:: "FALSE"
                                      (147 (getstatic (fieldCP "BOOLEAN_OBJECT_TYPE" "clojure.lang.Compiler" (class "clojure.asm.Type")))) 
                                      (150 (invokevirtual (methodCP "getStatic" "clojure.asm.commons.GeneratorAdapter" ((class "clojure.asm.Type") (class "java.lang.String") (class "clojure.asm.Type")) void))) 
                                      (153 (aload_3)) 
                                      (154 (sipush 165)) 
                                      (157 (aload 6)) 
                                      (159 (invokevirtual (methodCP "visitJumpInsn" "clojure.asm.commons.GeneratorAdapter" (int (class "clojure.asm.Label")) void))) 
                                      (162 (goto 173)) ;;to TAG_3;;at TAG_1
                                      (165 (astore 8)) ;;at TAG_9
                                      (167 (aload 8)) 
                                      (169 (invokestatic (methodCP "sneakyThrow" "clojure.lang.Util" ((class "java.lang.Throwable")) (class "java.lang.RuntimeException")))) 
                                      (172 (athrow)) 
                                      (173 (iload 4)) ;;at TAG_3
                                      (175 (ifeq 196)) ;;to TAG_4
                                      (178 (aload_0)) 
                                      (179 (getfield (fieldCP "thenExpr" "clojure.lang.Compiler$IfExpr" (class "clojure.lang.Compiler$Expr")))) 
                                      (182 (checkcast (class "clojure.lang.Compiler$MaybePrimitiveExpr"))) 
                                      (185 (aload_1)) 
                                      (186 (aload_2)) 
                                      (187 (aload_3)) 
                                      (188 (invokeinterface (methodCP "emitUnboxed" "clojure.lang.Compiler$MaybePrimitiveExpr" ((class "clojure.lang.Compiler$C") (class "clojure.lang.Compiler$ObjExpr") (class "clojure.asm.commons.GeneratorAdapter")) void) 4)) 
                                      (193 (goto 208)) ;;to TAG_5
                                      (196 (aload_0)) ;;at TAG_4
                                      (197 (getfield (fieldCP "thenExpr" "clojure.lang.Compiler$IfExpr" (class "clojure.lang.Compiler$Expr")))) 
                                      (200 (aload_1)) 
                                      (201 (aload_2)) 
                                      (202 (aload_3)) 
                                      (203 (invokeinterface (methodCP "emit" "clojure.lang.Compiler$Expr" ((class "clojure.lang.Compiler$C") (class "clojure.lang.Compiler$ObjExpr") (class "clojure.asm.commons.GeneratorAdapter")) void) 4)) 
                                      (208 (aload_3)) ;;at TAG_5
                                      (209 (aload 7)) 
                                      (211 (invokevirtual (methodCP "goTo" "clojure.asm.commons.GeneratorAdapter" ((class "clojure.asm.Label")) void))) 
                                      (214 (aload_3)) 
                                      (215 (aload 5)) 
                                      (217 (invokevirtual (methodCP "mark" "clojure.asm.commons.GeneratorAdapter" ((class "clojure.asm.Label")) void))) 
                                      (220 (aload_3)) 
                                      (221 (invokevirtual (methodCP "pop" "clojure.asm.commons.GeneratorAdapter" () void))) 
                                      (224 (aload_3)) 
                                      (225 (aload 6)) 
                                      (227 (invokevirtual (methodCP "mark" "clojure.asm.commons.GeneratorAdapter" ((class "clojure.asm.Label")) void))) 
                                      (230 (iload 4)) 
                                      (232 (ifeq 253)) ;;to TAG_6
                                      (235 (aload_0)) 
                                      (236 (getfield (fieldCP "elseExpr" "clojure.lang.Compiler$IfExpr" (class "clojure.lang.Compiler$Expr")))) 
                                      (239 (checkcast (class "clojure.lang.Compiler$MaybePrimitiveExpr"))) 
                                      (242 (aload_1)) 
                                      (243 (aload_2)) 
                                      (244 (aload_3)) 
                                      (245 (invokeinterface (methodCP "emitUnboxed" "clojure.lang.Compiler$MaybePrimitiveExpr" ((class "clojure.lang.Compiler$C") (class "clojure.lang.Compiler$ObjExpr") (class "clojure.asm.commons.GeneratorAdapter")) void) 4)) 
                                      (250 (goto 265)) ;;to TAG_7
                                      (253 (aload_0)) ;;at TAG_6
                                      (254 (getfield (fieldCP "elseExpr" "clojure.lang.Compiler$IfExpr" (class "clojure.lang.Compiler$Expr")))) 
                                      (257 (aload_1)) 
                                      (258 (aload_2)) 
                                      (259 (aload_3)) 
                                      (260 (invokeinterface (methodCP "emit" "clojure.lang.Compiler$Expr" ((class "clojure.lang.Compiler$C") (class "clojure.lang.Compiler$ObjExpr") (class "clojure.asm.commons.GeneratorAdapter")) void) 4)) 
                                      (265 (aload_3)) ;;at TAG_7
                                      (266 (aload 7)) 
                                      (268 (invokevirtual (methodCP "mark" "clojure.asm.commons.GeneratorAdapter" ((class "clojure.asm.Label")) void))) 
                                      (271 (return)) 
                                      (endofcode 272))
                                   (Exceptions 
                                     (handler 30 162  165 (class "java.lang.Exception")))
                                   (StackMap )))
                        (method "hasJavaClass"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 135)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "thenExpr" "clojure.lang.Compiler$IfExpr" (class "clojure.lang.Compiler$Expr")))) 
                                      (4 (invokeinterface (methodCP "hasJavaClass" "clojure.lang.Compiler$Expr" () boolean) 1)) 
                                      (9 (ifeq 133)) ;;to TAG_0
                                      (12 (aload_0)) 
                                      (13 (getfield (fieldCP "elseExpr" "clojure.lang.Compiler$IfExpr" (class "clojure.lang.Compiler$Expr")))) 
                                      (16 (invokeinterface (methodCP "hasJavaClass" "clojure.lang.Compiler$Expr" () boolean) 1)) 
                                      (21 (ifeq 133)) ;;to TAG_0
                                      (24 (aload_0)) 
                                      (25 (getfield (fieldCP "thenExpr" "clojure.lang.Compiler$IfExpr" (class "clojure.lang.Compiler$Expr")))) 
                                      (28 (invokeinterface (methodCP "getJavaClass" "clojure.lang.Compiler$Expr" () (class "java.lang.Class")) 1)) 
                                      (33 (aload_0)) 
                                      (34 (getfield (fieldCP "elseExpr" "clojure.lang.Compiler$IfExpr" (class "clojure.lang.Compiler$Expr")))) 
                                      (37 (invokeinterface (methodCP "getJavaClass" "clojure.lang.Compiler$Expr" () (class "java.lang.Class")) 1)) 
                                      (42 (if_acmpeq 129)) ;;to TAG_1
                                      (45 (aload_0)) 
                                      (46 (getfield (fieldCP "thenExpr" "clojure.lang.Compiler$IfExpr" (class "clojure.lang.Compiler$Expr")))) 
                                      (49 (invokeinterface (methodCP "getJavaClass" "clojure.lang.Compiler$Expr" () (class "java.lang.Class")) 1)) 
                                      (54 (getstatic (fieldCP "RECUR_CLASS" "clojure.lang.Compiler" (class "java.lang.Class")))) 
                                      (57 (if_acmpeq 129)) ;;to TAG_1
                                      (60 (aload_0)) 
                                      (61 (getfield (fieldCP "elseExpr" "clojure.lang.Compiler$IfExpr" (class "clojure.lang.Compiler$Expr")))) 
                                      (64 (invokeinterface (methodCP "getJavaClass" "clojure.lang.Compiler$Expr" () (class "java.lang.Class")) 1)) 
                                      (69 (getstatic (fieldCP "RECUR_CLASS" "clojure.lang.Compiler" (class "java.lang.Class")))) 
                                      (72 (if_acmpeq 129)) ;;to TAG_1
                                      (75 (aload_0)) 
                                      (76 (getfield (fieldCP "thenExpr" "clojure.lang.Compiler$IfExpr" (class "clojure.lang.Compiler$Expr")))) 
                                      (79 (invokeinterface (methodCP "getJavaClass" "clojure.lang.Compiler$Expr" () (class "java.lang.Class")) 1)) 
                                      (84 (ifnonnull 102))  ;;to TAG_2
                                      (87 (aload_0)) 
                                      (88 (getfield (fieldCP "elseExpr" "clojure.lang.Compiler$IfExpr" (class "clojure.lang.Compiler$Expr")))) 
                                      (91 (invokeinterface (methodCP "getJavaClass" "clojure.lang.Compiler$Expr" () (class "java.lang.Class")) 1)) 
                                      (96 (invokevirtual (methodCP "isPrimitive" "java.lang.Class" () boolean))) 
                                      (99 (ifeq 129)) ;;to TAG_1
                                      (102 (aload_0)) ;;at TAG_2
                                      (103 (getfield (fieldCP "elseExpr" "clojure.lang.Compiler$IfExpr" (class "clojure.lang.Compiler$Expr")))) 
                                      (106 (invokeinterface (methodCP "getJavaClass" "clojure.lang.Compiler$Expr" () (class "java.lang.Class")) 1)) 
                                      (111 (ifnonnull 133)) ;;to TAG_0
                                      (114 (aload_0)) 
                                      (115 (getfield (fieldCP "thenExpr" "clojure.lang.Compiler$IfExpr" (class "clojure.lang.Compiler$Expr")))) 
                                      (118 (invokeinterface (methodCP "getJavaClass" "clojure.lang.Compiler$Expr" () (class "java.lang.Class")) 1)) 
                                      (123 (invokevirtual (methodCP "isPrimitive" "java.lang.Class" () boolean))) 
                                      (126 (ifne 133)) ;;to TAG_0
                                      (129 (iconst_1)) ;;at TAG_1
                                      (130 (goto 134)) ;;to TAG_3
                                      (133 (iconst_0)) ;;at TAG_0
                                      (134 (ireturn)) ;;at TAG_3
                                      (endofcode 135))
                                   (Exceptions )
                                   (StackMap )))
                        (method "canEmitPrimitive"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 110)
                                   (parsedcode
                                      (0 (aload_0)) ;;at TAG_3
                                      (1 (getfield (fieldCP "thenExpr" "clojure.lang.Compiler$IfExpr" (class "clojure.lang.Compiler$Expr")))) 
                                      (4 (instanceof (class "clojure.lang.Compiler$MaybePrimitiveExpr"))) 
                                      (7 (ifeq 105)) ;;to TAG_0
                                      (10 (aload_0)) 
                                      (11 (getfield (fieldCP "elseExpr" "clojure.lang.Compiler$IfExpr" (class "clojure.lang.Compiler$Expr")))) 
                                      (14 (instanceof (class "clojure.lang.Compiler$MaybePrimitiveExpr"))) 
                                      (17 (ifeq 105)) ;;to TAG_0
                                      (20 (aload_0)) 
                                      (21 (getfield (fieldCP "thenExpr" "clojure.lang.Compiler$IfExpr" (class "clojure.lang.Compiler$Expr")))) 
                                      (24 (invokeinterface (methodCP "getJavaClass" "clojure.lang.Compiler$Expr" () (class "java.lang.Class")) 1)) 
                                      (29 (aload_0)) 
                                      (30 (getfield (fieldCP "elseExpr" "clojure.lang.Compiler$IfExpr" (class "clojure.lang.Compiler$Expr")))) 
                                      (33 (invokeinterface (methodCP "getJavaClass" "clojure.lang.Compiler$Expr" () (class "java.lang.Class")) 1)) 
                                      (38 (if_acmpeq 71)) ;;to TAG_1
                                      (41 (aload_0)) 
                                      (42 (getfield (fieldCP "thenExpr" "clojure.lang.Compiler$IfExpr" (class "clojure.lang.Compiler$Expr")))) 
                                      (45 (invokeinterface (methodCP "getJavaClass" "clojure.lang.Compiler$Expr" () (class "java.lang.Class")) 1)) 
                                      (50 (getstatic (fieldCP "RECUR_CLASS" "clojure.lang.Compiler" (class "java.lang.Class")))) 
                                      (53 (if_acmpeq 71)) ;;to TAG_1
                                      (56 (aload_0)) 
                                      (57 (getfield (fieldCP "elseExpr" "clojure.lang.Compiler$IfExpr" (class "clojure.lang.Compiler$Expr")))) 
                                      (60 (invokeinterface (methodCP "getJavaClass" "clojure.lang.Compiler$Expr" () (class "java.lang.Class")) 1)) 
                                      (65 (getstatic (fieldCP "RECUR_CLASS" "clojure.lang.Compiler" (class "java.lang.Class")))) 
                                      (68 (if_acmpne 105)) ;;to TAG_0
                                      (71 (aload_0)) ;;at TAG_1
                                      (72 (getfield (fieldCP "thenExpr" "clojure.lang.Compiler$IfExpr" (class "clojure.lang.Compiler$Expr")))) 
                                      (75 (checkcast (class "clojure.lang.Compiler$MaybePrimitiveExpr"))) 
                                      (78 (invokeinterface (methodCP "canEmitPrimitive" "clojure.lang.Compiler$MaybePrimitiveExpr" () boolean) 1)) 
                                      (83 (ifeq 105)) ;;to TAG_0
                                      (86 (aload_0)) 
                                      (87 (getfield (fieldCP "elseExpr" "clojure.lang.Compiler$IfExpr" (class "clojure.lang.Compiler$Expr")))) 
                                      (90 (checkcast (class "clojure.lang.Compiler$MaybePrimitiveExpr"))) 
                                      (93 (invokeinterface (methodCP "canEmitPrimitive" "clojure.lang.Compiler$MaybePrimitiveExpr" () boolean) 1)) 
                                      (98 (ifeq 105)) ;;to TAG_0
                                      (101 (iconst_1)) 
                                      (102 (goto 106))  ;;to TAG_2
                                      (105 (iconst_0)) ;;at TAG_0
                                      (106 (ireturn)) ;;at TAG_2
                                      (107 (astore_1)) ;;at TAG_4
                                      (108 (iconst_0)) 
                                      (109 (ireturn)) 
                                      (endofcode 110))
                                   (Exceptions 
                                     (handler 0 106  107 (class "java.lang.Exception")))
                                   (StackMap )))
                        (method "getJavaClass"
                              (parameters )
                              (returntype . (class "java.lang.Class"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 33)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "thenExpr" "clojure.lang.Compiler$IfExpr" (class "clojure.lang.Compiler$Expr")))) 
                                      (4 (invokeinterface (methodCP "getJavaClass" "clojure.lang.Compiler$Expr" () (class "java.lang.Class")) 1)) 
                                      (9 (astore_1)) 
                                      (10 (aload_1)) 
                                      (11 (ifnull 23))  ;;to TAG_0
                                      (14 (aload_1)) 
                                      (15 (getstatic (fieldCP "RECUR_CLASS" "clojure.lang.Compiler" (class "java.lang.Class")))) 
                                      (18 (if_acmpeq 23))  ;;to TAG_0
                                      (21 (aload_1)) 
                                      (22 (areturn)) 
                                      (23 (aload_0)) ;;at TAG_0
                                      (24 (getfield (fieldCP "elseExpr" "clojure.lang.Compiler$IfExpr" (class "clojure.lang.Compiler$Expr")))) 
                                      (27 (invokeinterface (methodCP "getJavaClass" "clojure.lang.Compiler$Expr" () (class "java.lang.Class")) 1)) 
                                      (32 (areturn)) 
                                      (endofcode 33))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "clojure.lang.Compiler$Expr" "clojure.lang.Compiler$MaybePrimitiveExpr")
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *Compiler$IfExpr-class-table*
  (make-static-class-decls 
   *clojure.lang.Compiler$IfExpr*))

(defconst *package-name-map* 
  ("clojure.lang.Compiler$IfExpr" . "clojure.lang"))

