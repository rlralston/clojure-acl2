; instant$zero_fill_right-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:49 CDT 2014.
;

(defconst *clojure.instant$zero_fill_right*
 (make-class-def
      '(class "clojure.instant$zero_fill_right"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "=")
                        (STRING  "count")
                        (STRING  "<")
                        (STRING  "else"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 67)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "="
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.instant$zero_fill_right" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "count"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.instant$zero_fill_right" (class "clojure.lang.Var"))))
                                      (26 (ldc 0))        ;;STRING:: "clojure.core"
                                      (28 (ldc 3))        ;;STRING:: "<"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.instant$zero_fill_right" (class "clojure.lang.Var"))))
                                      (39 (lconst_0))
                                      (40 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (43 (putstatic (fieldCP "const__3" "clojure.instant$zero_fill_right" (class "java.lang.Object"))))
                                      (46 (aconst_null))
                                      (47 (ldc 4))        ;;STRING:: "else"
                                      (49 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (52 (checkcast (class "clojure.lang.Keyword")))
                                      (55 (putstatic (fieldCP "const__4" "clojure.instant$zero_fill_right" (class "clojure.lang.Keyword"))))
                                      (58 (bipush 48))
                                      (60 (invokestatic
					(methodCP "valueOf" "java.lang.Character" (char) (class "java.lang.Character"))))
                                      (63 (putstatic (fieldCP "const__5" "clojure.instant$zero_fill_right" (class "java.lang.Object"))))
                                      (66 (return))
                                      (endofcode 67))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "clojure.lang.AFunction" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 4) (code_length . 130)
                                   (parsedcode
                                      (0 (aload_2)) 
                                      (1 (aload_1)) 
                                      (2 (invokestatic (methodCP "count" "clojure.lang.RT" ((class "java.lang.Object")) int))) 
                                      (5 (i2l)) 
                                      (6 (invokestatic (methodCP "equiv" "clojure.lang.Util" ((class "java.lang.Object") long) boolean))) 
                                      (9 (ifeq 19)) ;;to TAG_0
                                      (12 (aload_1)) 
                                      (13 (aconst_null)) 
                                      (14 (astore_1)) 
                                      (15 (goto 129)) ;;to TAG_1
                                      (18 (pop)) 
                                      (19 (aload_2)) ;;at TAG_0
                                      (20 (aload_1)) 
                                      (21 (invokestatic (methodCP "count" "clojure.lang.RT" ((class "java.lang.Object")) int))) 
                                      (24 (i2l)) 
                                      (25 (invokestatic (methodCP "lt" "clojure.lang.Numbers" ((class "java.lang.Object") long) boolean))) 
                                      (28 (ifeq 57))  ;;to TAG_2
                                      (31 (aload_1)) 
                                      (32 (aconst_null)) 
                                      (33 (astore_1)) 
                                      (34 (checkcast (class "java.lang.String"))) 
                                      (37 (lconst_0)) 
                                      (38 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (41 (aload_2)) 
                                      (42 (aconst_null)) 
                                      (43 (astore_2)) 
                                      (44 (checkcast (class "java.lang.Number"))) 
                                      (47 (invokestatic (methodCP "intCast" "clojure.lang.RT" ((class "java.lang.Object")) int))) 
                                      (50 (invokevirtual (methodCP "substring" "java.lang.String" (int int) (class "java.lang.String")))) 
                                      (53 (goto 129)) ;;to TAG_1
                                      (56 (pop)) 
                                      (57 (getstatic (fieldCP "const__4" "clojure.instant$zero_fill_right" (class "clojure.lang.Keyword")))) ;;at TAG_2
                                      (60 (dup)) 
                                      (61 (ifnull 127)) ;;to TAG_3
                                      (64 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (67 (if_acmpeq 128)) ;;to TAG_4
                                      (70 (new (class "java.lang.StringBuilder"))) 
                                      (73 (dup)) 
                                      (74 (aload_1)) 
                                      (75 (aconst_null)) 
                                      (76 (astore_1)) 
                                      (77 (checkcast (class "java.lang.String"))) 
                                      (80 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" ((class "java.lang.String")) void))) 
                                      (83 (astore_3)) 
                                      (84 (aload_3)) ;;at TAG_6
                                      (85 (checkcast (class "java.lang.StringBuilder"))) 
                                      (88 (invokevirtual (methodCP "length" "java.lang.StringBuilder" () int))) 
                                      (91 (i2l)) 
                                      (92 (aload_2)) 
                                      (93 (invokestatic (methodCP "lt" "clojure.lang.Numbers" (long (class "java.lang.Object")) boolean))) 
                                      (96 (ifeq 117)) ;;to TAG_5
                                      (99 (aload_3)) 
                                      (100 (checkcast (class "java.lang.StringBuilder"))) 
                                      (103 (getstatic (fieldCP "const__5" "clojure.instant$zero_fill_right" (class "java.lang.Object")))) 
                                      (106 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.Object")) (class "java.lang.StringBuilder")))) 
                                      (109 (astore_3)) 
                                      (110 (goto 84)) ;;to TAG_6
                                      (113 (goto 124)) ;;to TAG_7
                                      (116 (pop)) 
                                      (117 (aload_3)) ;;at TAG_5
                                      (118 (checkcast (class "java.lang.StringBuilder"))) 
                                      (121 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (124 (goto 129)) ;;to TAG_1;;at TAG_7
                                      (127 (pop)) ;;at TAG_3
                                      (128 (aconst_null)) ;;at TAG_4
                                      (129 (areturn)) ;;at TAG_1
                                      (endofcode 130))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *instant$zero_fill_right-class-table*
  (make-static-class-decls 
   *clojure.instant$zero_fill_right*))

(defconst *package-name-map* 
  ("clojure.instant$zero_fill_right" . "clojure"))
