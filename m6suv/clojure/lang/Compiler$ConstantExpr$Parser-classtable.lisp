; Compiler$ConstantExpr$Parser-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:50 CDT 2014.
;

(defconst *clojure.lang.Compiler$ConstantExpr$Parser*
 (make-class-def
      '(class "clojure.lang.Compiler$ConstantExpr$Parser"
            "java.lang.Object"
            (constant_pool)
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
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "parse"
                              (parameters (class "clojure.lang.Compiler$C") (class "java.lang.Object"))
                              (returntype . (class "clojure.lang.Compiler$Expr"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 4) (code_length . 106)
                                   (parsedcode
                                      (0 (aload_2)) 
                                      (1 (invokestatic (methodCP "second" "clojure.lang.RT" ((class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (4 (astore_3)) 
                                      (5 (aload_3)) 
                                      (6 (ifnonnull 13)) ;;to TAG_0
                                      (9 (getstatic (fieldCP "NIL_EXPR" "clojure.lang.Compiler" (class "clojure.lang.Compiler$NilExpr")))) 
                                      (12 (areturn)) 
                                      (13 (aload_3)) ;;at TAG_0
                                      (14 (getstatic (fieldCP "TRUE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (17 (if_acmpne 24)) ;;to TAG_1
                                      (20 (getstatic (fieldCP "TRUE_EXPR" "clojure.lang.Compiler" (class "clojure.lang.Compiler$BooleanExpr")))) 
                                      (23 (areturn)) 
                                      (24 (aload_3)) ;;at TAG_1
                                      (25 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (28 (if_acmpne 35))  ;;to TAG_2
                                      (31 (getstatic (fieldCP "FALSE_EXPR" "clojure.lang.Compiler" (class "clojure.lang.Compiler$BooleanExpr")))) 
                                      (34 (areturn)) 
                                      (35 (aload_3)) ;;at TAG_2
                                      (36 (instanceof (class "java.lang.Number"))) 
                                      (39 (ifeq 50)) ;;to TAG_3
                                      (42 (aload_3)) 
                                      (43 (checkcast (class "java.lang.Number"))) 
                                      (46 (invokestatic (methodCP "parse" "clojure.lang.Compiler$NumberExpr" ((class "java.lang.Number")) (class "clojure.lang.Compiler$Expr")))) 
                                      (49 (areturn)) 
                                      (50 (aload_3)) ;;at TAG_3
                                      (51 (instanceof (class "java.lang.String"))) 
                                      (54 (ifeq 69)) ;;to TAG_4
                                      (57 (new (class "clojure.lang.Compiler$StringExpr"))) 
                                      (60 (dup)) 
                                      (61 (aload_3)) 
                                      (62 (checkcast (class "java.lang.String"))) 
                                      (65 (invokespecial (methodCP "<init>" "clojure.lang.Compiler$StringExpr" ((class "java.lang.String")) void))) 
                                      (68 (areturn)) 
                                      (69 (aload_3)) ;;at TAG_4
                                      (70 (instanceof (class "clojure.lang.IPersistentCollection"))) 
                                      (73 (ifeq 97)) ;;to TAG_5
                                      (76 (aload_3)) 
                                      (77 (checkcast (class "clojure.lang.IPersistentCollection"))) 
                                      (80 (invokeinterface (methodCP "count" "clojure.lang.IPersistentCollection" () int) 1)) 
                                      (85 (ifne 97)) ;;to TAG_5
                                      (88 (new (class "clojure.lang.Compiler$EmptyExpr"))) 
                                      (91 (dup)) 
                                      (92 (aload_3)) 
                                      (93 (invokespecial (methodCP "<init>" "clojure.lang.Compiler$EmptyExpr" ((class "java.lang.Object")) void))) 
                                      (96 (areturn)) 
                                      (97 (new (class "clojure.lang.Compiler$ConstantExpr"))) ;;at TAG_5
                                      (100 (dup)) 
                                      (101 (aload_3)) 
                                      (102 (invokespecial (methodCP "<init>" "clojure.lang.Compiler$ConstantExpr" ((class "java.lang.Object")) void))) 
                                      (105 (areturn)) 
                                      (endofcode 106))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "clojure.lang.Compiler$IParser")
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *Compiler$ConstantExpr$Parser-class-table*
  (make-static-class-decls 
   *clojure.lang.Compiler$ConstantExpr$Parser*))

(defconst *package-name-map* 
  ("clojure.lang.Compiler$ConstantExpr$Parser" . "clojure.lang"))

