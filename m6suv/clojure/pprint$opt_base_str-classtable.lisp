; pprint$opt_base_str-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:56 CDT 2014.
;

(defconst *clojure.pprint$opt_base_str*
 (make-class-def
      '(class "clojure.pprint$opt_base_str"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "get")
                        (STRING  "clojure.pprint")
                        (STRING  "java-base-formats")
                        (STRING  "integer?")
                        (STRING  "not")
                        (STRING  "instance?")
                        (STRING  "format")
                        (STRING  "base-str"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__6" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 92)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "get"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.pprint$opt_base_str" (class "clojure.lang.Var"))))
                                      (13 (ldc 2))        ;;STRING:: "clojure.pprint"
                                      (15 (ldc 3))        ;;STRING:: "java-base-formats"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.pprint$opt_base_str" (class "clojure.lang.Var"))))
                                      (26 (ldc 0))        ;;STRING:: "clojure.core"
                                      (28 (ldc 4))        ;;STRING:: "integer?"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.pprint$opt_base_str" (class "clojure.lang.Var"))))
                                      (39 (ldc 0))        ;;STRING:: "clojure.core"
                                      (41 (ldc 5))        ;;STRING:: "not"
                                      (43 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (46 (checkcast (class "clojure.lang.Var")))
                                      (49 (putstatic (fieldCP "const__3" "clojure.pprint$opt_base_str" (class "clojure.lang.Var"))))
                                      (52 (ldc 0))        ;;STRING:: "clojure.core"
                                      (54 (ldc 6))        ;;STRING:: "instance?"
                                      (56 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (59 (checkcast (class "clojure.lang.Var")))
                                      (62 (putstatic (fieldCP "const__4" "clojure.pprint$opt_base_str" (class "clojure.lang.Var"))))
                                      (65 (ldc 0))        ;;STRING:: "clojure.core"
                                      (67 (ldc 7))        ;;STRING:: "format"
                                      (69 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (72 (checkcast (class "clojure.lang.Var")))
                                      (75 (putstatic (fieldCP "const__5" "clojure.pprint$opt_base_str" (class "clojure.lang.Var"))))
                                      (78 (ldc 2))        ;;STRING:: "clojure.pprint"
                                      (80 (ldc 8))        ;;STRING:: "base-str"
                                      (82 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (85 (checkcast (class "clojure.lang.Var")))
                                      (88 (putstatic (fieldCP "const__6" "clojure.pprint$opt_base_str" (class "clojure.lang.Var"))))
                                      (91 (return))
                                      (endofcode 92))
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
                                   (max_stack . 4) (max_locals . 6) (code_length . 158)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__1" "clojure.pprint$opt_base_str" (class "clojure.lang.Var")))) 
                                      (3 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (6 (aload_1)) 
                                      (7 (invokestatic (methodCP "get" "clojure.lang.RT" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (10 (astore_3)) 
                                      (11 (aload_3)) 
                                      (12 (astore 4)) 
                                      (14 (aload 4)) 
                                      (16 (dup)) 
                                      (17 (ifnull 97)) ;;to TAG_0
                                      (20 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (23 (if_acmpeq 98))  ;;to TAG_1
                                      (26 (getstatic (fieldCP "const__2" "clojure.pprint$opt_base_str" (class "clojure.lang.Var")))) 
                                      (29 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (32 (checkcast (class "clojure.lang.IFn"))) 
                                      (35 (aload_2)) 
                                      (36 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (41 (astore 5)) 
                                      (43 (aload 5)) 
                                      (45 (dup)) 
                                      (46 (ifnull 88)) ;;to TAG_2
                                      (49 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (52 (if_acmpeq 89)) ;;to TAG_3
                                      (55 (getstatic (fieldCP "const__3" "clojure.pprint$opt_base_str" (class "clojure.lang.Var")))) 
                                      (58 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (61 (checkcast (class "clojure.lang.IFn"))) 
                                      (64 (aload_2)) 
                                      (65 (instanceof (class "clojure.lang.BigInt"))) 
                                      (68 (ifeq 77)) ;;to TAG_4
                                      (71 (getstatic (fieldCP "TRUE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (74 (goto 80)) ;;to TAG_5
                                      (77 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) ;;at TAG_4
                                      (80 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) ;;at TAG_5
                                      (85 (goto 94)) ;;to TAG_6
                                      (88 (pop)) ;;at TAG_2
                                      (89 (aload 5)) ;;at TAG_3
                                      (91 (aconst_null)) 
                                      (92 (astore 5)) 
                                      (94 (goto 103)) ;;to TAG_7;;at TAG_6
                                      (97 (pop)) ;;at TAG_0
                                      (98 (aload 4)) ;;at TAG_1
                                      (100 (aconst_null)) 
                                      (101 (astore 4)) 
                                      (103 (dup)) ;;at TAG_7
                                      (104 (ifnull 136)) ;;to TAG_8
                                      (107 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (110 (if_acmpeq 137)) ;;to TAG_9
                                      (113 (getstatic (fieldCP "const__5" "clojure.pprint$opt_base_str" (class "clojure.lang.Var")))) 
                                      (116 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (119 (checkcast (class "clojure.lang.IFn"))) 
                                      (122 (aload_3)) 
                                      (123 (aconst_null)) 
                                      (124 (astore_3)) 
                                      (125 (aload_2)) 
                                      (126 (aconst_null)) 
                                      (127 (astore_2)) 
                                      (128 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (133 (goto 157)) ;;to TAG_10
                                      (136 (pop)) ;;at TAG_8
                                      (137 (getstatic (fieldCP "const__6" "clojure.pprint$opt_base_str" (class "clojure.lang.Var")))) ;;at TAG_9
                                      (140 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (143 (checkcast (class "clojure.lang.IFn"))) 
                                      (146 (aload_1)) 
                                      (147 (aconst_null)) 
                                      (148 (astore_1)) 
                                      (149 (aload_2)) 
                                      (150 (aconst_null)) 
                                      (151 (astore_2)) 
                                      (152 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (157 (areturn)) ;;at TAG_10
                                      (endofcode 158))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *pprint$opt_base_str-class-table*
  (make-static-class-decls 
   *clojure.pprint$opt_base_str*))

(defconst *package-name-map* 
  ("clojure.pprint$opt_base_str" . "clojure"))

