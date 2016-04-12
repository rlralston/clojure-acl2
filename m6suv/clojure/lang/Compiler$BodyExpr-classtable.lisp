; Compiler$BodyExpr-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:50 CDT 2014.
;

(defconst *clojure.lang.Compiler$BodyExpr*
 (make-class-def
      '(class "clojure.lang.Compiler$BodyExpr"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "exprs" (class "clojure.lang.PersistentVector") (accessflags  *class* ) -1))
            (methods
                        (method "exprs"
                              (parameters )
                              (returntype . (class "clojure.lang.PersistentVector"))
                              (accessflags  *class*  *final*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "exprs" "clojure.lang.Compiler$BodyExpr" (class "clojure.lang.PersistentVector"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "clojure.lang.PersistentVector"))
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
                                      (6 (putfield (fieldCP "exprs" "clojure.lang.Compiler$BodyExpr" (class "clojure.lang.PersistentVector"))))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "eval"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 5) (code_length . 45)
                                   (parsedcode
                                      (0 (aconst_null)) 
                                      (1 (astore_1)) 
                                      (2 (aload_0)) 
                                      (3 (getfield (fieldCP "exprs" "clojure.lang.Compiler$BodyExpr" (class "clojure.lang.PersistentVector")))) 
                                      (6 (invokevirtual (methodCP "iterator" "clojure.lang.PersistentVector" () (class "java.util.Iterator")))) 
                                      (9 (astore_2)) 
                                      (10 (aload_2)) ;;at TAG_1
                                      (11 (invokeinterface (methodCP "hasNext" "java.util.Iterator" () boolean) 1)) 
                                      (16 (ifeq 43))  ;;to TAG_0
                                      (19 (aload_2)) 
                                      (20 (invokeinterface (methodCP "next" "java.util.Iterator" () (class "java.lang.Object")) 1)) 
                                      (25 (astore_3)) 
                                      (26 (aload_3)) 
                                      (27 (checkcast (class "clojure.lang.Compiler$Expr"))) 
                                      (30 (astore 4)) 
                                      (32 (aload 4)) 
                                      (34 (invokeinterface (methodCP "eval" "clojure.lang.Compiler$Expr" () (class "java.lang.Object")) 1)) 
                                      (39 (astore_1)) 
                                      (40 (goto 10)) ;;to TAG_1
                                      (43 (aload_1)) ;;at TAG_0
                                      (44 (areturn)) 
                                      (endofcode 45))
                                   (Exceptions )
                                   (StackMap )))
                        (method "canEmitPrimitive"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 31)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokespecial (methodCP "lastExpr" "clojure.lang.Compiler$BodyExpr" () (class "clojure.lang.Compiler$Expr")))) 
                                      (4 (instanceof (class "clojure.lang.Compiler$MaybePrimitiveExpr"))) 
                                      (7 (ifeq 29))  ;;to TAG_0
                                      (10 (aload_0)) 
                                      (11 (invokespecial (methodCP "lastExpr" "clojure.lang.Compiler$BodyExpr" () (class "clojure.lang.Compiler$Expr")))) 
                                      (14 (checkcast (class "clojure.lang.Compiler$MaybePrimitiveExpr"))) 
                                      (17 (invokeinterface (methodCP "canEmitPrimitive" "clojure.lang.Compiler$MaybePrimitiveExpr" () boolean) 1)) 
                                      (22 (ifeq 29))  ;;to TAG_0
                                      (25 (iconst_1)) 
                                      (26 (goto 30)) ;;to TAG_1
                                      (29 (iconst_0)) ;;at TAG_0
                                      (30 (ireturn)) ;;at TAG_1
                                      (endofcode 31))
                                   (Exceptions )
                                   (StackMap )))
                        (method "emitUnboxed"
                              (parameters (class "clojure.lang.Compiler$C") (class "clojure.lang.Compiler$ObjExpr") (class "clojure.asm.commons.GeneratorAdapter"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 6) (code_length . 81)
                                   (parsedcode
                                      (0 (iconst_0)) 
                                      (1 (istore 4)) 
                                      (3 (iload 4)) ;;at TAG_1
                                      (5 (aload_0)) 
                                      (6 (getfield (fieldCP "exprs" "clojure.lang.Compiler$BodyExpr" (class "clojure.lang.PersistentVector")))) 
                                      (9 (invokevirtual (methodCP "count" "clojure.lang.PersistentVector" () int))) 
                                      (12 (iconst_1)) 
                                      (13 (isub)) 
                                      (14 (if_icmpge 49))  ;;to TAG_0
                                      (17 (aload_0)) 
                                      (18 (getfield (fieldCP "exprs" "clojure.lang.Compiler$BodyExpr" (class "clojure.lang.PersistentVector")))) 
                                      (21 (iload 4)) 
                                      (23 (invokevirtual (methodCP "nth" "clojure.lang.PersistentVector" (int) (class "java.lang.Object")))) 
                                      (26 (checkcast (class "clojure.lang.Compiler$Expr"))) 
                                      (29 (astore 5)) 
                                      (31 (aload 5)) 
                                      (33 (getstatic (fieldCP "STATEMENT" "clojure.lang.Compiler$C" (class "clojure.lang.Compiler$C")))) 
                                      (36 (aload_2)) 
                                      (37 (aload_3)) 
                                      (38 (invokeinterface (methodCP "emit" "clojure.lang.Compiler$Expr" ((class "clojure.lang.Compiler$C") (class "clojure.lang.Compiler$ObjExpr") (class "clojure.asm.commons.GeneratorAdapter")) void) 4)) 
                                      (43 (iinc 4 1)) 
                                      (46 (goto 3)) ;;to TAG_1
                                      (49 (aload_0)) ;;at TAG_0
                                      (50 (getfield (fieldCP "exprs" "clojure.lang.Compiler$BodyExpr" (class "clojure.lang.PersistentVector")))) 
                                      (53 (aload_0)) 
                                      (54 (getfield (fieldCP "exprs" "clojure.lang.Compiler$BodyExpr" (class "clojure.lang.PersistentVector")))) 
                                      (57 (invokevirtual (methodCP "count" "clojure.lang.PersistentVector" () int))) 
                                      (60 (iconst_1)) 
                                      (61 (isub)) 
                                      (62 (invokevirtual (methodCP "nth" "clojure.lang.PersistentVector" (int) (class "java.lang.Object")))) 
                                      (65 (checkcast (class "clojure.lang.Compiler$MaybePrimitiveExpr"))) 
                                      (68 (astore 4)) 
                                      (70 (aload 4)) 
                                      (72 (aload_1)) 
                                      (73 (aload_2)) 
                                      (74 (aload_3)) 
                                      (75 (invokeinterface (methodCP "emitUnboxed" "clojure.lang.Compiler$MaybePrimitiveExpr" ((class "clojure.lang.Compiler$C") (class "clojure.lang.Compiler$ObjExpr") (class "clojure.asm.commons.GeneratorAdapter")) void) 4)) 
                                      (80 (return)) 
                                      (endofcode 81))
                                   (Exceptions )
                                   (StackMap )))
                        (method "emit"
                              (parameters (class "clojure.lang.Compiler$C") (class "clojure.lang.Compiler$ObjExpr") (class "clojure.asm.commons.GeneratorAdapter"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 6) (code_length . 81)
                                   (parsedcode
                                      (0 (iconst_0)) 
                                      (1 (istore 4)) 
                                      (3 (iload 4)) ;;at TAG_1
                                      (5 (aload_0)) 
                                      (6 (getfield (fieldCP "exprs" "clojure.lang.Compiler$BodyExpr" (class "clojure.lang.PersistentVector")))) 
                                      (9 (invokevirtual (methodCP "count" "clojure.lang.PersistentVector" () int))) 
                                      (12 (iconst_1)) 
                                      (13 (isub)) 
                                      (14 (if_icmpge 49))  ;;to TAG_0
                                      (17 (aload_0)) 
                                      (18 (getfield (fieldCP "exprs" "clojure.lang.Compiler$BodyExpr" (class "clojure.lang.PersistentVector")))) 
                                      (21 (iload 4)) 
                                      (23 (invokevirtual (methodCP "nth" "clojure.lang.PersistentVector" (int) (class "java.lang.Object")))) 
                                      (26 (checkcast (class "clojure.lang.Compiler$Expr"))) 
                                      (29 (astore 5)) 
                                      (31 (aload 5)) 
                                      (33 (getstatic (fieldCP "STATEMENT" "clojure.lang.Compiler$C" (class "clojure.lang.Compiler$C")))) 
                                      (36 (aload_2)) 
                                      (37 (aload_3)) 
                                      (38 (invokeinterface (methodCP "emit" "clojure.lang.Compiler$Expr" ((class "clojure.lang.Compiler$C") (class "clojure.lang.Compiler$ObjExpr") (class "clojure.asm.commons.GeneratorAdapter")) void) 4)) 
                                      (43 (iinc 4 1)) 
                                      (46 (goto 3)) ;;to TAG_1
                                      (49 (aload_0)) ;;at TAG_0
                                      (50 (getfield (fieldCP "exprs" "clojure.lang.Compiler$BodyExpr" (class "clojure.lang.PersistentVector")))) 
                                      (53 (aload_0)) 
                                      (54 (getfield (fieldCP "exprs" "clojure.lang.Compiler$BodyExpr" (class "clojure.lang.PersistentVector")))) 
                                      (57 (invokevirtual (methodCP "count" "clojure.lang.PersistentVector" () int))) 
                                      (60 (iconst_1)) 
                                      (61 (isub)) 
                                      (62 (invokevirtual (methodCP "nth" "clojure.lang.PersistentVector" (int) (class "java.lang.Object")))) 
                                      (65 (checkcast (class "clojure.lang.Compiler$Expr"))) 
                                      (68 (astore 4)) 
                                      (70 (aload 4)) 
                                      (72 (aload_1)) 
                                      (73 (aload_2)) 
                                      (74 (aload_3)) 
                                      (75 (invokeinterface (methodCP "emit" "clojure.lang.Compiler$Expr" ((class "clojure.lang.Compiler$C") (class "clojure.lang.Compiler$ObjExpr") (class "clojure.asm.commons.GeneratorAdapter")) void) 4)) 
                                      (80 (return)) 
                                      (endofcode 81))
                                   (Exceptions )
                                   (StackMap )))
                        (method "hasJavaClass"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "lastExpr" "clojure.lang.Compiler$BodyExpr" () (class "clojure.lang.Compiler$Expr"))))
                                      (4 (invokeinterface
					(methodCP "hasJavaClass" "clojure.lang.Compiler$Expr" () boolean) 1))
                                      (9 (ireturn))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getJavaClass"
                              (parameters )
                              (returntype . (class "java.lang.Class"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "lastExpr" "clojure.lang.Compiler$BodyExpr" () (class "clojure.lang.Compiler$Expr"))))
                                      (4 (invokeinterface
					(methodCP "getJavaClass" "clojure.lang.Compiler$Expr" () (class "java.lang.Class")) 1))
                                      (9 (areturn))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "lastExpr"
                              (parameters )
                              (returntype . (class "clojure.lang.Compiler$Expr"))
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 3) (max_locals . 1) (code_length . 20)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "exprs" "clojure.lang.Compiler$BodyExpr" (class "clojure.lang.PersistentVector"))))
                                      (4 (aload_0))
                                      (5 (getfield (fieldCP "exprs" "clojure.lang.Compiler$BodyExpr" (class "clojure.lang.PersistentVector"))))
                                      (8 (invokevirtual
					(methodCP "count" "clojure.lang.PersistentVector" () int)))
                                      (11 (iconst_1))
                                      (12 (isub))
                                      (13 (invokevirtual
					(methodCP "nth" "clojure.lang.PersistentVector" (int) (class "java.lang.Object"))))
                                      (16 (checkcast (class "clojure.lang.Compiler$Expr")))
                                      (19 (areturn))
                                      (endofcode 20))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "clojure.lang.Compiler$Expr" "clojure.lang.Compiler$MaybePrimitiveExpr")
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *Compiler$BodyExpr-class-table*
  (make-static-class-decls 
   *clojure.lang.Compiler$BodyExpr*))

(defconst *package-name-map* 
  ("clojure.lang.Compiler$BodyExpr" . "clojure.lang"))

