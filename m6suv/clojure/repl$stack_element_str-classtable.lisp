; repl$stack_element_str-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:58 CDT 2014.
;

(defconst *clojure.repl$stack_element_str*
 (make-class-def
      '(class "clojure.repl$stack_element_str"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "=")
                        (STRING  "str")
                        (STRING  "clojure.repl")
                        (STRING  "demunge")
                        (STRING  ".clj")
                        (STRING  "NO_SOURCE_FILE")
                        (STRING  ".")
                        (STRING  " (")
                        (STRING  ":")
                        (STRING  ")"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 40)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "="
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.repl$stack_element_str" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "str"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.repl$stack_element_str" (class "clojure.lang.Var"))))
                                      (26 (ldc 3))        ;;STRING:: "clojure.repl"
                                      (28 (ldc 4))        ;;STRING:: "demunge"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.repl$stack_element_str" (class "clojure.lang.Var"))))
                                      (39 (return))
                                      (endofcode 40))
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
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 7) (max_locals . 5) (code_length . 194)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (checkcast (class "java.lang.StackTraceElement"))) 
                                      (4 (invokevirtual (methodCP "getFileName" "java.lang.StackTraceElement" () (class "java.lang.String")))) 
                                      (7 (astore_2)) 
                                      (8 (aload_2)) 
                                      (9 (astore_3)) 
                                      (10 (aload_3)) 
                                      (11 (dup)) 
                                      (12 (ifnull 81)) ;;to TAG_0
                                      (15 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (18 (if_acmpeq 82))  ;;to TAG_1
                                      (21 (aload_2)) 
                                      (22 (checkcast (class "java.lang.String"))) 
                                      (25 (ldc 5)) ;;STRING:: ".clj"
                                      (27 (checkcast (class "java.lang.String"))) 
                                      (30 (invokevirtual (methodCP "endsWith" "java.lang.String" ((class "java.lang.String")) boolean))) 
                                      (33 (istore 4)) 
                                      (35 (iload 4)) 
                                      (37 (ifeq 58)) ;;to TAG_2
                                      (40 (iload 4)) 
                                      (42 (ifeq 51)) ;;to TAG_3
                                      (45 (getstatic (fieldCP "TRUE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (48 (goto 54)) ;;to TAG_4
                                      (51 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) ;;at TAG_3
                                      (54 (goto 78)) ;;to TAG_5;;at TAG_4
                                      (57 (pop)) 
                                      (58 (aload_2)) ;;at TAG_2
                                      (59 (aconst_null)) 
                                      (60 (astore_2)) 
                                      (61 (ldc 6)) ;;STRING:: "NO_SOURCE_FILE"
                                      (63 (invokestatic (methodCP "equiv" "clojure.lang.Util" ((class "java.lang.Object") (class "java.lang.Object")) boolean))) 
                                      (66 (ifeq 75)) ;;to TAG_6
                                      (69 (getstatic (fieldCP "TRUE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (72 (goto 78)) ;;to TAG_5
                                      (75 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) ;;at TAG_6
                                      (78 (goto 85)) ;;to TAG_7;;at TAG_5
                                      (81 (pop)) ;;at TAG_0
                                      (82 (aload_3)) ;;at TAG_1
                                      (83 (aconst_null)) 
                                      (84 (astore_3)) 
                                      (85 (astore_3)) ;;at TAG_7
                                      (86 (getstatic (fieldCP "const__1" "clojure.repl$stack_element_str" (class "clojure.lang.Var")))) 
                                      (89 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (92 (checkcast (class "clojure.lang.IFn"))) 
                                      (95 (aload_3)) 
                                      (96 (aconst_null)) 
                                      (97 (astore_3)) 
                                      (98 (dup)) 
                                      (99 (ifnull 132)) ;;to TAG_8
                                      (102 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (105 (if_acmpeq 133)) ;;to TAG_9
                                      (108 (getstatic (fieldCP "const__2" "clojure.repl$stack_element_str" (class "clojure.lang.Var")))) 
                                      (111 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (114 (checkcast (class "clojure.lang.IFn"))) 
                                      (117 (aload_1)) 
                                      (118 (checkcast (class "java.lang.StackTraceElement"))) 
                                      (121 (invokevirtual (methodCP "getClassName" "java.lang.StackTraceElement" () (class "java.lang.String")))) 
                                      (124 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (129 (goto 163)) ;;to TAG_10
                                      (132 (pop)) ;;at TAG_8
                                      (133 (getstatic (fieldCP "const__1" "clojure.repl$stack_element_str" (class "clojure.lang.Var")))) ;;at TAG_9
                                      (136 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (139 (checkcast (class "clojure.lang.IFn"))) 
                                      (142 (aload_1)) 
                                      (143 (checkcast (class "java.lang.StackTraceElement"))) 
                                      (146 (invokevirtual (methodCP "getClassName" "java.lang.StackTraceElement" () (class "java.lang.String")))) 
                                      (149 (ldc 7)) ;;STRING:: "."
                                      (151 (aload_1)) 
                                      (152 (checkcast (class "java.lang.StackTraceElement"))) 
                                      (155 (invokevirtual (methodCP "getMethodName" "java.lang.StackTraceElement" () (class "java.lang.String")))) 
                                      (158 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4)) 
                                      (163 (ldc 8)) ;;at TAG_10;;STRING:: " ("
                                      (165 (aload_1)) 
                                      (166 (checkcast (class "java.lang.StackTraceElement"))) 
                                      (169 (invokevirtual (methodCP "getFileName" "java.lang.StackTraceElement" () (class "java.lang.String")))) 
                                      (172 (ldc 9)) ;;STRING:: ":"
                                      (174 (aload_1)) 
                                      (175 (aconst_null)) 
                                      (176 (astore_1)) 
                                      (177 (checkcast (class "java.lang.StackTraceElement"))) 
                                      (180 (invokevirtual (methodCP "getLineNumber" "java.lang.StackTraceElement" () int))) 
                                      (183 (invokestatic (methodCP "valueOf" "java.lang.Integer" (int) (class "java.lang.Integer")))) 
                                      (186 (ldc 10)) ;;STRING:: ")"
                                      (188 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 7)) 
                                      (193 (areturn)) 
                                      (endofcode 194))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *repl$stack_element_str-class-table*
  (make-static-class-decls 
   *clojure.repl$stack_element_str*))

(defconst *package-name-map* 
  ("clojure.repl$stack_element_str" . "clojure"))

