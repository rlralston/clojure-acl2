; Compiler$EmptyExpr-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:50 CDT 2014.
;

(defconst *clojure.lang.Compiler$EmptyExpr*
 (make-class-def
      '(class "clojure.lang.Compiler$EmptyExpr"
            "java.lang.Object"
            (constant_pool
                        (STRING  "EMPTY")
                        (STRING  "Unknown Collection type"))
            (fields
                        (field "coll" (class "java.lang.Object") (accessflags  *class*  *final*  *public* ) -1)
                        (field "HASHMAP_TYPE" (class "clojure.asm.Type") (accessflags  *class*  *final*  *static* ) -1)
                        (field "HASHSET_TYPE" (class "clojure.asm.Type") (accessflags  *class*  *final*  *static* ) -1)
                        (field "VECTOR_TYPE" (class "clojure.asm.Type") (accessflags  *class*  *final*  *static* ) -1)
                        (field "LIST_TYPE" (class "clojure.asm.Type") (accessflags  *class*  *final*  *static* ) -1)
                        (field "EMPTY_LIST_TYPE" (class "clojure.asm.Type") (accessflags  *class*  *final*  *static* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.lang.Object"))
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
                                      (6 (putfield (fieldCP "coll" "clojure.lang.Compiler$EmptyExpr" (class "java.lang.Object"))))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "eval"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "coll" "clojure.lang.Compiler$EmptyExpr" (class "java.lang.Object"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "emit"
                              (parameters (class "clojure.lang.Compiler$C") (class "clojure.lang.Compiler$ObjExpr") (class "clojure.asm.commons.GeneratorAdapter"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 4) (code_length . 122)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "coll" "clojure.lang.Compiler$EmptyExpr" (class "java.lang.Object")))) 
                                      (4 (instanceof (class "clojure.lang.IPersistentList"))) 
                                      (7 (ifeq 25)) ;;to TAG_0
                                      (10 (aload_3)) 
                                      (11 (getstatic (fieldCP "LIST_TYPE" "clojure.lang.Compiler$EmptyExpr" (class "clojure.asm.Type")))) 
                                      (14 (ldc 0)) ;;STRING:: "EMPTY"
                                      (16 (getstatic (fieldCP "EMPTY_LIST_TYPE" "clojure.lang.Compiler$EmptyExpr" (class "clojure.asm.Type")))) 
                                      (19 (invokevirtual (methodCP "getStatic" "clojure.asm.commons.GeneratorAdapter" ((class "clojure.asm.Type") (class "java.lang.String") (class "clojure.asm.Type")) void))) 
                                      (22 (goto 110)) ;;to TAG_1
                                      (25 (aload_0)) ;;at TAG_0
                                      (26 (getfield (fieldCP "coll" "clojure.lang.Compiler$EmptyExpr" (class "java.lang.Object")))) 
                                      (29 (instanceof (class "clojure.lang.IPersistentVector"))) 
                                      (32 (ifeq 50))  ;;to TAG_2
                                      (35 (aload_3)) 
                                      (36 (getstatic (fieldCP "VECTOR_TYPE" "clojure.lang.Compiler$EmptyExpr" (class "clojure.asm.Type")))) 
                                      (39 (ldc 0)) ;;STRING:: "EMPTY"
                                      (41 (getstatic (fieldCP "VECTOR_TYPE" "clojure.lang.Compiler$EmptyExpr" (class "clojure.asm.Type")))) 
                                      (44 (invokevirtual (methodCP "getStatic" "clojure.asm.commons.GeneratorAdapter" ((class "clojure.asm.Type") (class "java.lang.String") (class "clojure.asm.Type")) void))) 
                                      (47 (goto 110)) ;;to TAG_1
                                      (50 (aload_0)) ;;at TAG_2
                                      (51 (getfield (fieldCP "coll" "clojure.lang.Compiler$EmptyExpr" (class "java.lang.Object")))) 
                                      (54 (instanceof (class "clojure.lang.IPersistentMap"))) 
                                      (57 (ifeq 75)) ;;to TAG_3
                                      (60 (aload_3)) 
                                      (61 (getstatic (fieldCP "HASHMAP_TYPE" "clojure.lang.Compiler$EmptyExpr" (class "clojure.asm.Type")))) 
                                      (64 (ldc 0)) ;;STRING:: "EMPTY"
                                      (66 (getstatic (fieldCP "HASHMAP_TYPE" "clojure.lang.Compiler$EmptyExpr" (class "clojure.asm.Type")))) 
                                      (69 (invokevirtual (methodCP "getStatic" "clojure.asm.commons.GeneratorAdapter" ((class "clojure.asm.Type") (class "java.lang.String") (class "clojure.asm.Type")) void))) 
                                      (72 (goto 110)) ;;to TAG_1
                                      (75 (aload_0)) ;;at TAG_3
                                      (76 (getfield (fieldCP "coll" "clojure.lang.Compiler$EmptyExpr" (class "java.lang.Object")))) 
                                      (79 (instanceof (class "clojure.lang.IPersistentSet"))) 
                                      (82 (ifeq 100)) ;;to TAG_4
                                      (85 (aload_3)) 
                                      (86 (getstatic (fieldCP "HASHSET_TYPE" "clojure.lang.Compiler$EmptyExpr" (class "clojure.asm.Type")))) 
                                      (89 (ldc 0)) ;;STRING:: "EMPTY"
                                      (91 (getstatic (fieldCP "HASHSET_TYPE" "clojure.lang.Compiler$EmptyExpr" (class "clojure.asm.Type")))) 
                                      (94 (invokevirtual (methodCP "getStatic" "clojure.asm.commons.GeneratorAdapter" ((class "clojure.asm.Type") (class "java.lang.String") (class "clojure.asm.Type")) void))) 
                                      (97 (goto 110)) ;;to TAG_1
                                      (100 (new (class "java.lang.UnsupportedOperationException"))) ;;at TAG_4
                                      (103 (dup)) 
                                      (104 (ldc 1)) ;;STRING:: "Unknown Collection type"
                                      (106 (invokespecial (methodCP "<init>" "java.lang.UnsupportedOperationException" ((class "java.lang.String")) void))) 
                                      (109 (athrow)) 
                                      (110 (aload_1)) ;;at TAG_1
                                      (111 (getstatic (fieldCP "STATEMENT" "clojure.lang.Compiler$C" (class "clojure.lang.Compiler$C")))) 
                                      (114 (if_acmpne 121)) ;;to TAG_5
                                      (117 (aload_3)) 
                                      (118 (invokevirtual (methodCP "pop" "clojure.asm.commons.GeneratorAdapter" () void))) 
                                      (121 (return)) ;;at TAG_5
                                      (endofcode 122))
                                   (Exceptions )
                                   (StackMap )))
                        (method "hasJavaClass"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 2)
                                   (parsedcode
                                      (0 (iconst_1))
                                      (1 (ireturn))
                                      (endofcode 2))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getJavaClass"
                              (parameters )
                              (returntype . (class "java.lang.Class"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 1) (code_length . 66)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "coll" "clojure.lang.Compiler$EmptyExpr" (class "java.lang.Object")))) 
                                      (4 (instanceof (class "clojure.lang.IPersistentList"))) 
                                      (7 (ifeq 14)) ;;to TAG_0
                                      (10 (ldc_w )) 
                                      (13 (areturn)) 
                                      (14 (aload_0)) ;;at TAG_0
                                      (15 (getfield (fieldCP "coll" "clojure.lang.Compiler$EmptyExpr" (class "java.lang.Object")))) 
                                      (18 (instanceof (class "clojure.lang.IPersistentVector"))) 
                                      (21 (ifeq 28)) ;;to TAG_1
                                      (24 (ldc_w )) 
                                      (27 (areturn)) 
                                      (28 (aload_0)) ;;at TAG_1
                                      (29 (getfield (fieldCP "coll" "clojure.lang.Compiler$EmptyExpr" (class "java.lang.Object")))) 
                                      (32 (instanceof (class "clojure.lang.IPersistentMap"))) 
                                      (35 (ifeq 42))  ;;to TAG_2
                                      (38 (ldc_w )) 
                                      (41 (areturn)) 
                                      (42 (aload_0)) ;;at TAG_2
                                      (43 (getfield (fieldCP "coll" "clojure.lang.Compiler$EmptyExpr" (class "java.lang.Object")))) 
                                      (46 (instanceof (class "clojure.lang.IPersistentSet"))) 
                                      (49 (ifeq 56)) ;;to TAG_3
                                      (52 (ldc_w )) 
                                      (55 (areturn)) 
                                      (56 (new (class "java.lang.UnsupportedOperationException"))) ;;at TAG_3
                                      (59 (dup)) 
                                      (60 (ldc 1)) ;;STRING:: "Unknown Collection type"
                                      (62 (invokespecial (methodCP "<init>" "java.lang.UnsupportedOperationException" ((class "java.lang.String")) void))) 
                                      (65 (athrow)) 
                                      (endofcode 66))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 1) (max_locals . 0) (code_length . 46)
                                   (parsedcode
                                      (0 (ldc_w ))
                                      (3 (invokestatic
					(methodCP "getType" "clojure.asm.Type" ((class "java.lang.Class")) (class "clojure.asm.Type"))))
                                      (6 (putstatic (fieldCP "HASHMAP_TYPE" "clojure.lang.Compiler$EmptyExpr" (class "clojure.asm.Type"))))
                                      (9 (ldc_w ))
                                      (12 (invokestatic
					(methodCP "getType" "clojure.asm.Type" ((class "java.lang.Class")) (class "clojure.asm.Type"))))
                                      (15 (putstatic (fieldCP "HASHSET_TYPE" "clojure.lang.Compiler$EmptyExpr" (class "clojure.asm.Type"))))
                                      (18 (ldc_w ))
                                      (21 (invokestatic
					(methodCP "getType" "clojure.asm.Type" ((class "java.lang.Class")) (class "clojure.asm.Type"))))
                                      (24 (putstatic (fieldCP "VECTOR_TYPE" "clojure.lang.Compiler$EmptyExpr" (class "clojure.asm.Type"))))
                                      (27 (ldc_w ))
                                      (30 (invokestatic
					(methodCP "getType" "clojure.asm.Type" ((class "java.lang.Class")) (class "clojure.asm.Type"))))
                                      (33 (putstatic (fieldCP "LIST_TYPE" "clojure.lang.Compiler$EmptyExpr" (class "clojure.asm.Type"))))
                                      (36 (ldc_w ))
                                      (39 (invokestatic
					(methodCP "getType" "clojure.asm.Type" ((class "java.lang.Class")) (class "clojure.asm.Type"))))
                                      (42 (putstatic (fieldCP "EMPTY_LIST_TYPE" "clojure.lang.Compiler$EmptyExpr" (class "clojure.asm.Type"))))
                                      (45 (return))
                                      (endofcode 46))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "clojure.lang.Compiler$Expr")
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *Compiler$EmptyExpr-class-table*
  (make-static-class-decls 
   *clojure.lang.Compiler$EmptyExpr*))

(defconst *package-name-map* 
  ("clojure.lang.Compiler$EmptyExpr" . "clojure.lang"))
