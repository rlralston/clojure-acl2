; Compiler$KeywordInvokeExpr-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:50 CDT 2014.
;

(defconst *clojure.lang.Compiler$KeywordInvokeExpr*
 (make-class-def
      '(class "clojure.lang.Compiler$KeywordInvokeExpr"
            "java.lang.Object"
            (constant_pool
                        (STRING  "Object get(Object)")
                        (STRING  "clojure.lang.ILookupThunk fault(Object)"))
            (fields
                        (field "kw" (class "clojure.lang.Compiler$KeywordExpr") (accessflags  *class*  *final*  *public* ) -1)
                        (field "tag" (class "java.lang.Object") (accessflags  *class*  *final*  *public* ) -1)
                        (field "target" (class "clojure.lang.Compiler$Expr") (accessflags  *class*  *final*  *public* ) -1)
                        (field "line" int (accessflags  *class*  *final*  *public* ) -1)
                        (field "column" int (accessflags  *class*  *final*  *public* ) -1)
                        (field "siteIndex" int (accessflags  *class*  *final*  *public* ) -1)
                        (field "source" (class "java.lang.String") (accessflags  *class*  *final*  *public* ) -1)
                        (field "ILOOKUP_TYPE" (class "clojure.asm.Type") (accessflags  *class*  *static* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.lang.String") int int (class "clojure.lang.Symbol") (class "clojure.lang.Compiler$KeywordExpr") (class "clojure.lang.Compiler$Expr"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 7) (code_length . 50)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (putfield (fieldCP "source" "clojure.lang.Compiler$KeywordInvokeExpr" (class "java.lang.String"))))
                                      (9 (aload_0))
                                      (10 (aload 5))
                                      (12 (putfield (fieldCP "kw" "clojure.lang.Compiler$KeywordInvokeExpr" (class "clojure.lang.Compiler$KeywordExpr"))))
                                      (15 (aload_0))
                                      (16 (aload 6))
                                      (18 (putfield (fieldCP "target" "clojure.lang.Compiler$KeywordInvokeExpr" (class "clojure.lang.Compiler$Expr"))))
                                      (21 (aload_0))
                                      (22 (iload_2))
                                      (23 (putfield (fieldCP "line" "clojure.lang.Compiler$KeywordInvokeExpr" int)))
                                      (26 (aload_0))
                                      (27 (iload_3))
                                      (28 (putfield (fieldCP "column" "clojure.lang.Compiler$KeywordInvokeExpr" int)))
                                      (31 (aload_0))
                                      (32 (aload 4))
                                      (34 (putfield (fieldCP "tag" "clojure.lang.Compiler$KeywordInvokeExpr" (class "java.lang.Object"))))
                                      (37 (aload_0))
                                      (38 (aload 5))
                                      (40 (getfield (fieldCP "k" "clojure.lang.Compiler$KeywordExpr" (class "clojure.lang.Keyword"))))
                                      (43 (invokestatic
					(methodCP "access$1000" "clojure.lang.Compiler" ((class "clojure.lang.Keyword")) int)))
                                      (46 (putfield (fieldCP "siteIndex" "clojure.lang.Compiler$KeywordInvokeExpr" int)))
                                      (49 (return))
                                      (endofcode 50))
                                   (Exceptions )
                                   (StackMap )))
                        (method "eval"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 2) (code_length . 54)
                                   (parsedcode
                                      (0 (aload_0)) ;;at TAG_1
                                      (1 (getfield (fieldCP "kw" "clojure.lang.Compiler$KeywordInvokeExpr" (class "clojure.lang.Compiler$KeywordExpr")))) 
                                      (4 (getfield (fieldCP "k" "clojure.lang.Compiler$KeywordExpr" (class "clojure.lang.Keyword")))) 
                                      (7 (aload_0)) 
                                      (8 (getfield (fieldCP "target" "clojure.lang.Compiler$KeywordInvokeExpr" (class "clojure.lang.Compiler$Expr")))) 
                                      (11 (invokeinterface (methodCP "eval" "clojure.lang.Compiler$Expr" () (class "java.lang.Object")) 1)) 
                                      (16 (invokevirtual (methodCP "invoke" "clojure.lang.Keyword" ((class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (19 (areturn)) ;;at TAG_2
                                      (20 (astore_1)) ;;at TAG_3
                                      (21 (aload_1)) 
                                      (22 (instanceof (class "clojure.lang.Compiler$CompilerException"))) 
                                      (25 (ifne 49)) ;;to TAG_0
                                      (28 (new (class "clojure.lang.Compiler$CompilerException"))) 
                                      (31 (dup)) 
                                      (32 (aload_0)) 
                                      (33 (getfield (fieldCP "source" "clojure.lang.Compiler$KeywordInvokeExpr" (class "java.lang.String")))) 
                                      (36 (aload_0)) 
                                      (37 (getfield (fieldCP "line" "clojure.lang.Compiler$KeywordInvokeExpr" int))) 
                                      (40 (aload_0)) 
                                      (41 (getfield (fieldCP "column" "clojure.lang.Compiler$KeywordInvokeExpr" int))) 
                                      (44 (aload_1)) 
                                      (45 (invokespecial (methodCP "<init>" "clojure.lang.Compiler$CompilerException" ((class "java.lang.String") int int (class "java.lang.Throwable")) void))) 
                                      (48 (athrow)) 
                                      (49 (aload_1)) ;;at TAG_0
                                      (50 (checkcast (class "clojure.lang.Compiler$CompilerException"))) 
                                      (53 (athrow)) 
                                      (endofcode 54))
                                   (Exceptions 
                                     (handler 0 19  20 (class "java.lang.Throwable")))
                                   (StackMap )))
                        (method "emit"
                              (parameters (class "clojure.lang.Compiler$C") (class "clojure.lang.Compiler$ObjExpr") (class "clojure.asm.commons.GeneratorAdapter"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 6) (code_length . 210)
                                   (parsedcode
                                      (0 (aload_3)) 
                                      (1 (invokevirtual (methodCP "newLabel" "clojure.asm.commons.GeneratorAdapter" () (class "clojure.asm.Label")))) 
                                      (4 (astore 4)) 
                                      (6 (aload_3)) 
                                      (7 (invokevirtual (methodCP "newLabel" "clojure.asm.commons.GeneratorAdapter" () (class "clojure.asm.Label")))) 
                                      (10 (astore 5)) 
                                      (12 (aload_3)) 
                                      (13 (aload_0)) 
                                      (14 (getfield (fieldCP "line" "clojure.lang.Compiler$KeywordInvokeExpr" int))) 
                                      (17 (aload_3)) 
                                      (18 (invokevirtual (methodCP "mark" "clojure.asm.commons.GeneratorAdapter" () (class "clojure.asm.Label")))) 
                                      (21 (invokevirtual (methodCP "visitLineNumber" "clojure.asm.commons.GeneratorAdapter" (int (class "clojure.asm.Label")) void))) 
                                      (24 (aload_3)) 
                                      (25 (aload_2)) 
                                      (26 (getfield (fieldCP "objtype" "clojure.lang.Compiler$ObjExpr" (class "clojure.asm.Type")))) 
                                      (29 (aload_2)) 
                                      (30 (aload_0)) 
                                      (31 (getfield (fieldCP "siteIndex" "clojure.lang.Compiler$KeywordInvokeExpr" int))) 
                                      (34 (invokevirtual (methodCP "thunkNameStatic" "clojure.lang.Compiler$ObjExpr" (int) (class "java.lang.String")))) 
                                      (37 (getstatic (fieldCP "ILOOKUP_THUNK_TYPE" "clojure.lang.Compiler$ObjExpr" (class "clojure.asm.Type")))) 
                                      (40 (invokevirtual (methodCP "getStatic" "clojure.asm.commons.GeneratorAdapter" ((class "clojure.asm.Type") (class "java.lang.String") (class "clojure.asm.Type")) void))) 
                                      (43 (aload_3)) 
                                      (44 (invokevirtual (methodCP "dup" "clojure.asm.commons.GeneratorAdapter" () void))) 
                                      (47 (aload_0)) 
                                      (48 (getfield (fieldCP "target" "clojure.lang.Compiler$KeywordInvokeExpr" (class "clojure.lang.Compiler$Expr")))) 
                                      (51 (getstatic (fieldCP "EXPRESSION" "clojure.lang.Compiler$C" (class "clojure.lang.Compiler$C")))) 
                                      (54 (aload_2)) 
                                      (55 (aload_3)) 
                                      (56 (invokeinterface (methodCP "emit" "clojure.lang.Compiler$Expr" ((class "clojure.lang.Compiler$C") (class "clojure.lang.Compiler$ObjExpr") (class "clojure.asm.commons.GeneratorAdapter")) void) 4)) 
                                      (61 (aload_3)) 
                                      (62 (invokevirtual (methodCP "dupX2" "clojure.asm.commons.GeneratorAdapter" () void))) 
                                      (65 (aload_3)) 
                                      (66 (getstatic (fieldCP "ILOOKUP_THUNK_TYPE" "clojure.lang.Compiler$ObjExpr" (class "clojure.asm.Type")))) 
                                      (69 (ldc 0)) ;;STRING:: "Object get(Object)"
                                      (71 (invokestatic (methodCP "getMethod" "clojure.asm.commons.Method" ((class "java.lang.String")) (class "clojure.asm.commons.Method")))) 
                                      (74 (invokevirtual (methodCP "invokeInterface" "clojure.asm.commons.GeneratorAdapter" ((class "clojure.asm.Type") (class "clojure.asm.commons.Method")) void))) 
                                      (77 (aload_3)) 
                                      (78 (invokevirtual (methodCP "dupX2" "clojure.asm.commons.GeneratorAdapter" () void))) 
                                      (81 (aload_3)) 
                                      (82 (sipush 165)) 
                                      (85 (aload 5)) 
                                      (87 (invokevirtual (methodCP "visitJumpInsn" "clojure.asm.commons.GeneratorAdapter" (int (class "clojure.asm.Label")) void))) 
                                      (90 (aload_3)) 
                                      (91 (invokevirtual (methodCP "pop" "clojure.asm.commons.GeneratorAdapter" () void))) 
                                      (94 (aload_3)) 
                                      (95 (aload 4)) 
                                      (97 (invokevirtual (methodCP "goTo" "clojure.asm.commons.GeneratorAdapter" ((class "clojure.asm.Label")) void))) 
                                      (100 (aload_3)) 
                                      (101 (aload 5)) 
                                      (103 (invokevirtual (methodCP "mark" "clojure.asm.commons.GeneratorAdapter" ((class "clojure.asm.Label")) void))) 
                                      (106 (aload_3)) 
                                      (107 (invokevirtual (methodCP "swap" "clojure.asm.commons.GeneratorAdapter" () void))) 
                                      (110 (aload_3)) 
                                      (111 (invokevirtual (methodCP "pop" "clojure.asm.commons.GeneratorAdapter" () void))) 
                                      (114 (aload_3)) 
                                      (115 (invokevirtual (methodCP "dup" "clojure.asm.commons.GeneratorAdapter" () void))) 
                                      (118 (aload_3)) 
                                      (119 (aload_2)) 
                                      (120 (getfield (fieldCP "objtype" "clojure.lang.Compiler$ObjExpr" (class "clojure.asm.Type")))) 
                                      (123 (aload_2)) 
                                      (124 (aload_0)) 
                                      (125 (getfield (fieldCP "siteIndex" "clojure.lang.Compiler$KeywordInvokeExpr" int))) 
                                      (128 (invokevirtual (methodCP "siteNameStatic" "clojure.lang.Compiler$ObjExpr" (int) (class "java.lang.String")))) 
                                      (131 (getstatic (fieldCP "KEYWORD_LOOKUPSITE_TYPE" "clojure.lang.Compiler$ObjExpr" (class "clojure.asm.Type")))) 
                                      (134 (invokevirtual (methodCP "getStatic" "clojure.asm.commons.GeneratorAdapter" ((class "clojure.asm.Type") (class "java.lang.String") (class "clojure.asm.Type")) void))) 
                                      (137 (aload_3)) 
                                      (138 (invokevirtual (methodCP "swap" "clojure.asm.commons.GeneratorAdapter" () void))) 
                                      (141 (aload_3)) 
                                      (142 (getstatic (fieldCP "ILOOKUP_SITE_TYPE" "clojure.lang.Compiler$ObjExpr" (class "clojure.asm.Type")))) 
                                      (145 (ldc 1)) ;;STRING:: "clojure.lang.ILookupThunk fault(Object)"
                                      (147 (invokestatic (methodCP "getMethod" "clojure.asm.commons.Method" ((class "java.lang.String")) (class "clojure.asm.commons.Method")))) 
                                      (150 (invokevirtual (methodCP "invokeInterface" "clojure.asm.commons.GeneratorAdapter" ((class "clojure.asm.Type") (class "clojure.asm.commons.Method")) void))) 
                                      (153 (aload_3)) 
                                      (154 (invokevirtual (methodCP "dup" "clojure.asm.commons.GeneratorAdapter" () void))) 
                                      (157 (aload_3)) 
                                      (158 (aload_2)) 
                                      (159 (getfield (fieldCP "objtype" "clojure.lang.Compiler$ObjExpr" (class "clojure.asm.Type")))) 
                                      (162 (aload_2)) 
                                      (163 (aload_0)) 
                                      (164 (getfield (fieldCP "siteIndex" "clojure.lang.Compiler$KeywordInvokeExpr" int))) 
                                      (167 (invokevirtual (methodCP "thunkNameStatic" "clojure.lang.Compiler$ObjExpr" (int) (class "java.lang.String")))) 
                                      (170 (getstatic (fieldCP "ILOOKUP_THUNK_TYPE" "clojure.lang.Compiler$ObjExpr" (class "clojure.asm.Type")))) 
                                      (173 (invokevirtual (methodCP "putStatic" "clojure.asm.commons.GeneratorAdapter" ((class "clojure.asm.Type") (class "java.lang.String") (class "clojure.asm.Type")) void))) 
                                      (176 (aload_3)) 
                                      (177 (invokevirtual (methodCP "swap" "clojure.asm.commons.GeneratorAdapter" () void))) 
                                      (180 (aload_3)) 
                                      (181 (getstatic (fieldCP "ILOOKUP_THUNK_TYPE" "clojure.lang.Compiler$ObjExpr" (class "clojure.asm.Type")))) 
                                      (184 (ldc 0)) ;;STRING:: "Object get(Object)"
                                      (186 (invokestatic (methodCP "getMethod" "clojure.asm.commons.Method" ((class "java.lang.String")) (class "clojure.asm.commons.Method")))) 
                                      (189 (invokevirtual (methodCP "invokeInterface" "clojure.asm.commons.GeneratorAdapter" ((class "clojure.asm.Type") (class "clojure.asm.commons.Method")) void))) 
                                      (192 (aload_3)) 
                                      (193 (aload 4)) 
                                      (195 (invokevirtual (methodCP "mark" "clojure.asm.commons.GeneratorAdapter" ((class "clojure.asm.Label")) void))) 
                                      (198 (aload_1)) 
                                      (199 (getstatic (fieldCP "STATEMENT" "clojure.lang.Compiler$C" (class "clojure.lang.Compiler$C")))) 
                                      (202 (if_acmpne 209))  ;;to TAG_0
                                      (205 (aload_3)) 
                                      (206 (invokevirtual (methodCP "pop" "clojure.asm.commons.GeneratorAdapter" () void))) 
                                      (209 (return)) ;;at TAG_0
                                      (endofcode 210))
                                   (Exceptions )
                                   (StackMap )))
                        (method "hasJavaClass"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 13)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "tag" "clojure.lang.Compiler$KeywordInvokeExpr" (class "java.lang.Object")))) 
                                      (4 (ifnull 11))  ;;to TAG_0
                                      (7 (iconst_1)) 
                                      (8 (goto 12)) ;;to TAG_1
                                      (11 (iconst_0)) ;;at TAG_0
                                      (12 (ireturn)) ;;at TAG_1
                                      (endofcode 13))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getJavaClass"
                              (parameters )
                              (returntype . (class "java.lang.Class"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 8)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "tag" "clojure.lang.Compiler$KeywordInvokeExpr" (class "java.lang.Object"))))
                                      (4 (invokestatic
					(methodCP "tagToClass" "clojure.lang.Compiler$HostExpr" ((class "java.lang.Object")) (class "java.lang.Class"))))
                                      (7 (areturn))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 1) (max_locals . 0) (code_length . 10)
                                   (parsedcode
                                      (0 (ldc_w ))
                                      (3 (invokestatic
					(methodCP "getType" "clojure.asm.Type" ((class "java.lang.Class")) (class "clojure.asm.Type"))))
                                      (6 (putstatic (fieldCP "ILOOKUP_TYPE" "clojure.lang.Compiler$KeywordInvokeExpr" (class "clojure.asm.Type"))))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "clojure.lang.Compiler$Expr")
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *Compiler$KeywordInvokeExpr-class-table*
  (make-static-class-decls 
   *clojure.lang.Compiler$KeywordInvokeExpr*))

(defconst *package-name-map* 
  ("clojure.lang.Compiler$KeywordInvokeExpr" . "clojure.lang"))
