; core$range$fn__4269-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:45 CDT 2014.
;

(defconst *clojure.core$range$fn__4269*
 (make-class-def
      '(class "clojure.core$range$fn__4269"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "chunk-buffer")
                        (LONG 32)
                        (STRING  "pos?")
                        (STRING  "<")
                        (STRING  ">")
                        (STRING  "count")
                        (STRING  "chunk-append")
                        (STRING  "+")
                        (STRING  "chunk-cons")
                        (STRING  "chunk")
                        (STRING  "range"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__6" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__7" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__8" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__9" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__10" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "start" (class "java.lang.Object") (accessflags  *class* ) -1)
                        (field "step" (class "java.lang.Object") (accessflags  *class* ) -1)
                        (field "end" (class "java.lang.Object") (accessflags  *class* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 140)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "chunk-buffer"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$range$fn__4269" (class "clojure.lang.Var"))))
                                      (13 (ldc2_w 2))     ;; LONG:: "32"
                                      (16 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (19 (putstatic (fieldCP "const__1" "clojure.core$range$fn__4269" (class "java.lang.Object"))))
                                      (22 (ldc 0))        ;;STRING:: "clojure.core"
                                      (24 (ldc 3))        ;;STRING:: "pos?"
                                      (26 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (29 (checkcast (class "clojure.lang.Var")))
                                      (32 (putstatic (fieldCP "const__2" "clojure.core$range$fn__4269" (class "clojure.lang.Var"))))
                                      (35 (ldc 0))        ;;STRING:: "clojure.core"
                                      (37 (ldc 4))        ;;STRING:: "<"
                                      (39 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (42 (checkcast (class "clojure.lang.Var")))
                                      (45 (putstatic (fieldCP "const__3" "clojure.core$range$fn__4269" (class "clojure.lang.Var"))))
                                      (48 (ldc 0))        ;;STRING:: "clojure.core"
                                      (50 (ldc 5))        ;;STRING:: ">"
                                      (52 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (55 (checkcast (class "clojure.lang.Var")))
                                      (58 (putstatic (fieldCP "const__4" "clojure.core$range$fn__4269" (class "clojure.lang.Var"))))
                                      (61 (ldc 0))        ;;STRING:: "clojure.core"
                                      (63 (ldc 6))        ;;STRING:: "count"
                                      (65 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (68 (checkcast (class "clojure.lang.Var")))
                                      (71 (putstatic (fieldCP "const__5" "clojure.core$range$fn__4269" (class "clojure.lang.Var"))))
                                      (74 (ldc 0))        ;;STRING:: "clojure.core"
                                      (76 (ldc 7))        ;;STRING:: "chunk-append"
                                      (78 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (81 (checkcast (class "clojure.lang.Var")))
                                      (84 (putstatic (fieldCP "const__6" "clojure.core$range$fn__4269" (class "clojure.lang.Var"))))
                                      (87 (ldc 0))        ;;STRING:: "clojure.core"
                                      (89 (ldc 8))        ;;STRING:: "+"
                                      (91 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (94 (checkcast (class "clojure.lang.Var")))
                                      (97 (putstatic (fieldCP "const__7" "clojure.core$range$fn__4269" (class "clojure.lang.Var"))))
                                      (100 (ldc 0))       ;;STRING:: "clojure.core"
                                      (102 (ldc 9))       ;;STRING:: "chunk-cons"
                                      (104 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (107 (checkcast (class "clojure.lang.Var")))
                                      (110 (putstatic (fieldCP "const__8" "clojure.core$range$fn__4269" (class "clojure.lang.Var"))))
                                      (113 (ldc 0))       ;;STRING:: "clojure.core"
                                      (115 (ldc 10))      ;;STRING:: "chunk"
                                      (117 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (120 (checkcast (class "clojure.lang.Var")))
                                      (123 (putstatic (fieldCP "const__9" "clojure.core$range$fn__4269" (class "clojure.lang.Var"))))
                                      (126 (ldc 0))       ;;STRING:: "clojure.core"
                                      (128 (ldc 11))      ;;STRING:: "range"
                                      (130 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (133 (checkcast (class "clojure.lang.Var")))
                                      (136 (putstatic (fieldCP "const__10" "clojure.core$range$fn__4269" (class "clojure.lang.Var"))))
                                      (139 (return))
                                      (endofcode 140))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 4) (code_length . 20)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "clojure.lang.AFunction" () void)))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (putfield (fieldCP "start" "clojure.core$range$fn__4269" (class "java.lang.Object"))))
                                      (9 (aload_0))
                                      (10 (aload_2))
                                      (11 (putfield (fieldCP "step" "clojure.core$range$fn__4269" (class "java.lang.Object"))))
                                      (14 (aload_0))
                                      (15 (aload_3))
                                      (16 (putfield (fieldCP "end" "clojure.core$range$fn__4269" (class "java.lang.Object"))))
                                      (19 (return))
                                      (endofcode 20))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 5) (code_length . 230)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.core$range$fn__4269" (class "clojure.lang.Var")))) 
                                      (3 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (6 (checkcast (class "clojure.lang.IFn"))) 
                                      (9 (getstatic (fieldCP "const__1" "clojure.core$range$fn__4269" (class "java.lang.Object")))) 
                                      (12 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (17 (astore_1)) 
                                      (18 (aload_0)) 
                                      (19 (getfield (fieldCP "step" "clojure.core$range$fn__4269" (class "java.lang.Object")))) 
                                      (22 (invokestatic (methodCP "isPos" "clojure.lang.Numbers" ((class "java.lang.Object")) boolean))) 
                                      (25 (ifeq 38)) ;;to TAG_0
                                      (28 (getstatic (fieldCP "const__3" "clojure.core$range$fn__4269" (class "clojure.lang.Var")))) 
                                      (31 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (34 (goto 44))  ;;to TAG_1
                                      (37 (pop)) 
                                      (38 (getstatic (fieldCP "const__4" "clojure.core$range$fn__4269" (class "clojure.lang.Var")))) ;;at TAG_0
                                      (41 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (44 (astore_2)) ;;at TAG_1
                                      (45 (aload_0)) 
                                      (46 (getfield (fieldCP "start" "clojure.core$range$fn__4269" (class "java.lang.Object")))) 
                                      (49 (aload_0)) 
                                      (50 (aconst_null)) 
                                      (51 (putfield (fieldCP "start" "clojure.core$range$fn__4269" (class "java.lang.Object")))) 
                                      (54 (astore_3)) 
                                      (55 (aload_1)) ;;at TAG_7
                                      (56 (invokestatic (methodCP "count" "clojure.lang.RT" ((class "java.lang.Object")) int))) 
                                      (59 (i2l)) 
                                      (60 (ldc2_w 2)) ;; LONG:: "32"
                                      (63 (invokestatic (methodCP "lt" "clojure.lang.Numbers" (long long) boolean))) 
                                      (66 (istore 4)) 
                                      (68 (iload 4)) 
                                      (70 (ifeq 91)) ;;to TAG_2
                                      (73 (aload_2)) 
                                      (74 (checkcast (class "clojure.lang.IFn"))) 
                                      (77 (aload_3)) 
                                      (78 (aload_0)) 
                                      (79 (getfield (fieldCP "end" "clojure.core$range$fn__4269" (class "java.lang.Object")))) 
                                      (82 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (87 (goto 105)) ;;to TAG_3
                                      (90 (pop)) 
                                      (91 (iload 4)) ;;at TAG_2
                                      (93 (ifeq 102)) ;;to TAG_4
                                      (96 (getstatic (fieldCP "TRUE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (99 (goto 105)) ;;to TAG_3
                                      (102 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) ;;at TAG_4
                                      (105 (dup)) ;;at TAG_3
                                      (106 (ifnull 147)) ;;to TAG_5
                                      (109 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (112 (if_acmpeq 148)) ;;to TAG_6
                                      (115 (getstatic (fieldCP "const__6" "clojure.core$range$fn__4269" (class "clojure.lang.Var")))) 
                                      (118 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (121 (checkcast (class "clojure.lang.IFn"))) 
                                      (124 (aload_1)) 
                                      (125 (aload_3)) 
                                      (126 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (131 (pop)) 
                                      (132 (aload_3)) 
                                      (133 (aload_0)) 
                                      (134 (getfield (fieldCP "step" "clojure.core$range$fn__4269" (class "java.lang.Object")))) 
                                      (137 (invokestatic (methodCP "add" "clojure.lang.Numbers" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Number")))) 
                                      (140 (astore_3)) 
                                      (141 (goto 55)) ;;to TAG_7
                                      (144 (goto 229)) ;;to TAG_8
                                      (147 (pop)) ;;at TAG_5
                                      (148 (getstatic (fieldCP "const__8" "clojure.core$range$fn__4269" (class "clojure.lang.Var")))) ;;at TAG_6
                                      (151 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (154 (checkcast (class "clojure.lang.IFn"))) 
                                      (157 (getstatic (fieldCP "const__9" "clojure.core$range$fn__4269" (class "clojure.lang.Var")))) 
                                      (160 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (163 (checkcast (class "clojure.lang.IFn"))) 
                                      (166 (aload_1)) 
                                      (167 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (172 (aload_2)) 
                                      (173 (checkcast (class "clojure.lang.IFn"))) 
                                      (176 (aload_3)) 
                                      (177 (aload_0)) 
                                      (178 (getfield (fieldCP "end" "clojure.core$range$fn__4269" (class "java.lang.Object")))) 
                                      (181 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (186 (dup)) 
                                      (187 (ifnull 222)) ;;to TAG_9
                                      (190 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (193 (if_acmpeq 223)) ;;to TAG_10
                                      (196 (getstatic (fieldCP "const__10" "clojure.core$range$fn__4269" (class "clojure.lang.Var")))) 
                                      (199 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (202 (checkcast (class "clojure.lang.IFn"))) 
                                      (205 (aload_3)) 
                                      (206 (aload_0)) 
                                      (207 (getfield (fieldCP "end" "clojure.core$range$fn__4269" (class "java.lang.Object")))) 
                                      (210 (aload_0)) 
                                      (211 (getfield (fieldCP "step" "clojure.core$range$fn__4269" (class "java.lang.Object")))) 
                                      (214 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4)) 
                                      (219 (goto 224)) ;;to TAG_11
                                      (222 (pop)) ;;at TAG_9
                                      (223 (aconst_null)) ;;at TAG_10
                                      (224 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) ;;at TAG_11
                                      (229 (areturn)) ;;at TAG_8
                                      (endofcode 230))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$range$fn__4269-class-table*
  (make-static-class-decls 
   *clojure.core$range$fn__4269*))

(defconst *package-name-map* 
  ("clojure.core$range$fn__4269" . "clojure"))
