; core$seque-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:45 CDT 2014.
;

(defconst *clojure.core$seque*
 (make-class-def
      '(class "clojure.core$seque"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "seque")
                        (LONG 100)
                        (STRING  "instance?")
                        (STRING  "int")
                        (STRING  "agent")
                        (STRING  "send-off"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 75)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "seque"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$seque" (class "clojure.lang.Var"))))
                                      (13 (ldc2_w 2))     ;; LONG:: "100"
                                      (16 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (19 (putstatic (fieldCP "const__1" "clojure.core$seque" (class "java.lang.Object"))))
                                      (22 (ldc 0))        ;;STRING:: "clojure.core"
                                      (24 (ldc 3))        ;;STRING:: "instance?"
                                      (26 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (29 (checkcast (class "clojure.lang.Var")))
                                      (32 (putstatic (fieldCP "const__2" "clojure.core$seque" (class "clojure.lang.Var"))))
                                      (35 (ldc 0))        ;;STRING:: "clojure.core"
                                      (37 (ldc 4))        ;;STRING:: "int"
                                      (39 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (42 (checkcast (class "clojure.lang.Var")))
                                      (45 (putstatic (fieldCP "const__3" "clojure.core$seque" (class "clojure.lang.Var"))))
                                      (48 (ldc 0))        ;;STRING:: "clojure.core"
                                      (50 (ldc 5))        ;;STRING:: "agent"
                                      (52 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (55 (checkcast (class "clojure.lang.Var")))
                                      (58 (putstatic (fieldCP "const__4" "clojure.core$seque" (class "clojure.lang.Var"))))
                                      (61 (ldc 0))        ;;STRING:: "clojure.core"
                                      (63 (ldc 6))        ;;STRING:: "send-off"
                                      (65 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (68 (checkcast (class "clojure.lang.Var")))
                                      (71 (putstatic (fieldCP "const__5" "clojure.core$seque" (class "clojure.lang.Var"))))
                                      (74 (return))
                                      (endofcode 75))
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
                                   (max_stack . 7) (max_locals . 9) (code_length . 159)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (instanceof (class "java.util.concurrent.BlockingQueue"))) 
                                      (4 (ifeq 14))  ;;to TAG_0
                                      (7 (aload_1)) 
                                      (8 (aconst_null)) 
                                      (9 (astore_1)) 
                                      (10 (goto 27)) ;;to TAG_1
                                      (13 (pop)) 
                                      (14 (new (class "java.util.concurrent.LinkedBlockingQueue"))) ;;at TAG_0
                                      (17 (dup)) 
                                      (18 (aload_1)) 
                                      (19 (aconst_null)) 
                                      (20 (astore_1)) 
                                      (21 (invokestatic (methodCP "intCast" "clojure.lang.RT" ((class "java.lang.Object")) int))) 
                                      (24 (invokespecial (methodCP "<init>" "java.util.concurrent.LinkedBlockingQueue" (int) void))) 
                                      (27 (astore_3)) ;;at TAG_1
                                      (28 (new (class "java.lang.Object"))) 
                                      (31 (dup)) 
                                      (32 (invokespecial (methodCP "<init>" "java.lang.Object" () void))) 
                                      (35 (astore 4)) 
                                      (37 (getstatic (fieldCP "const__4" "clojure.core$seque" (class "clojure.lang.Var")))) 
                                      (40 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (43 (checkcast (class "clojure.lang.IFn"))) 
                                      (46 (new (class "clojure.lang.LazySeq"))) 
                                      (49 (dup)) 
                                      (50 (new (class "clojure.core$seque$fn__4785"))) 
                                      (53 (dup)) 
                                      (54 (aload_2)) 
                                      (55 (aconst_null)) 
                                      (56 (astore_2)) 
                                      (57 (invokespecial (methodCP "<init>" "clojure.core$seque$fn__4785" ((class "java.lang.Object")) void))) 
                                      (60 (checkcast (class "clojure.lang.IFn"))) 
                                      (63 (invokespecial (methodCP "<init>" "clojure.lang.LazySeq" ((class "clojure.lang.IFn")) void))) 
                                      (66 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (71 (astore 5)) 
                                      (73 (new (class "clojure.core$seque$log_error__4787"))) 
                                      (76 (dup)) 
                                      (77 (invokespecial (methodCP "<init>" "clojure.core$seque$log_error__4787" () void))) 
                                      (80 (astore 6)) 
                                      (82 (new (class "clojure.core$seque$fill__4789"))) 
                                      (85 (dup)) 
                                      (86 (aload 4)) 
                                      (88 (aload_3)) 
                                      (89 (aload 6)) 
                                      (91 (aconst_null)) 
                                      (92 (astore 6)) 
                                      (94 (invokespecial (methodCP "<init>" "clojure.core$seque$fill__4789" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) void))) 
                                      (97 (astore 7)) 
                                      (99 (new (class "clojure.core$seque$drain__4795"))) 
                                      (102 (dup)) 
                                      (103 (aload 4)) 
                                      (105 (aconst_null)) 
                                      (106 (astore 4)) 
                                      (108 (aload 7)) 
                                      (110 (aload_3)) 
                                      (111 (aconst_null)) 
                                      (112 (astore_3)) 
                                      (113 (aload 5)) 
                                      (115 (invokespecial (methodCP "<init>" "clojure.core$seque$drain__4795" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) void))) 
                                      (118 (astore 8)) 
                                      (120 (getstatic (fieldCP "const__5" "clojure.core$seque" (class "clojure.lang.Var")))) 
                                      (123 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (126 (checkcast (class "clojure.lang.IFn"))) 
                                      (129 (aload 5)) 
                                      (131 (aconst_null)) 
                                      (132 (astore 5)) 
                                      (134 (aload 7)) 
                                      (136 (aconst_null)) 
                                      (137 (astore 7)) 
                                      (139 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (144 (pop)) 
                                      (145 (aload 8)) 
                                      (147 (aconst_null)) 
                                      (148 (astore 8)) 
                                      (150 (checkcast (class "clojure.lang.IFn"))) 
                                      (153 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" () (class "java.lang.Object")) 1)) 
                                      (158 (areturn)) 
                                      (endofcode 159))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 2) (code_length . 21)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.core$seque" (class "clojure.lang.Var"))))
                                      (3 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (6 (checkcast (class "clojure.lang.IFn")))
                                      (9 (getstatic (fieldCP "const__1" "clojure.core$seque" (class "java.lang.Object"))))
                                      (12 (aload_1))
                                      (13 (aconst_null))
                                      (14 (astore_1))
                                      (15 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3))
                                      (20 (areturn))
                                      (endofcode 21))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$seque-class-table*
  (make-static-class-decls 
   *clojure.core$seque*))

(defconst *package-name-map* 
  ("clojure.core$seque" . "clojure"))

