; core$await_for-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:40 CDT 2014.
;

(defconst *clojure.core$await_for*
 (make-class-def
      '(class "clojure.core$await_for"
            "clojure.lang.RestFn"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "*agent*")
                        (STRING  "count")
                        (STRING  "seq")
                        (STRING  "<")
                        (STRING  "send")
                        (STRING  "unchecked-inc")
                        (STRING  "chunked-seq?")
                        (STRING  "chunk-first")
                        (STRING  "chunk-rest")
                        (STRING  "int")
                        (STRING  "first")
                        (STRING  "next")
                        (STRING  "await-for in transaction")
                        (STRING  "Can\nt await in agent action"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__6" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__7" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__8" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__9" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__10" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__11" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__12" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 164)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "*agent*"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$await_for" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "count"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.core$await_for" (class "clojure.lang.Var"))))
                                      (26 (ldc 0))        ;;STRING:: "clojure.core"
                                      (28 (ldc 3))        ;;STRING:: "seq"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.core$await_for" (class "clojure.lang.Var"))))
                                      (39 (lconst_0))
                                      (40 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (43 (putstatic (fieldCP "const__3" "clojure.core$await_for" (class "java.lang.Object"))))
                                      (46 (ldc 0))        ;;STRING:: "clojure.core"
                                      (48 (ldc 4))        ;;STRING:: "<"
                                      (50 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (53 (checkcast (class "clojure.lang.Var")))
                                      (56 (putstatic (fieldCP "const__4" "clojure.core$await_for" (class "clojure.lang.Var"))))
                                      (59 (ldc 0))        ;;STRING:: "clojure.core"
                                      (61 (ldc 5))        ;;STRING:: "send"
                                      (63 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (66 (checkcast (class "clojure.lang.Var")))
                                      (69 (putstatic (fieldCP "const__5" "clojure.core$await_for" (class "clojure.lang.Var"))))
                                      (72 (ldc 0))        ;;STRING:: "clojure.core"
                                      (74 (ldc 6))        ;;STRING:: "unchecked-inc"
                                      (76 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (79 (checkcast (class "clojure.lang.Var")))
                                      (82 (putstatic (fieldCP "const__6" "clojure.core$await_for" (class "clojure.lang.Var"))))
                                      (85 (ldc 0))        ;;STRING:: "clojure.core"
                                      (87 (ldc 7))        ;;STRING:: "chunked-seq?"
                                      (89 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (92 (checkcast (class "clojure.lang.Var")))
                                      (95 (putstatic (fieldCP "const__7" "clojure.core$await_for" (class "clojure.lang.Var"))))
                                      (98 (ldc 0))        ;;STRING:: "clojure.core"
                                      (100 (ldc 8))       ;;STRING:: "chunk-first"
                                      (102 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (105 (checkcast (class "clojure.lang.Var")))
                                      (108 (putstatic (fieldCP "const__8" "clojure.core$await_for" (class "clojure.lang.Var"))))
                                      (111 (ldc 0))       ;;STRING:: "clojure.core"
                                      (113 (ldc 9))       ;;STRING:: "chunk-rest"
                                      (115 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (118 (checkcast (class "clojure.lang.Var")))
                                      (121 (putstatic (fieldCP "const__9" "clojure.core$await_for" (class "clojure.lang.Var"))))
                                      (124 (ldc 0))       ;;STRING:: "clojure.core"
                                      (126 (ldc 10))      ;;STRING:: "int"
                                      (128 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (131 (checkcast (class "clojure.lang.Var")))
                                      (134 (putstatic (fieldCP "const__10" "clojure.core$await_for" (class "clojure.lang.Var"))))
                                      (137 (ldc 0))       ;;STRING:: "clojure.core"
                                      (139 (ldc 11))      ;;STRING:: "first"
                                      (141 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (144 (checkcast (class "clojure.lang.Var")))
                                      (147 (putstatic (fieldCP "const__11" "clojure.core$await_for" (class "clojure.lang.Var"))))
                                      (150 (ldc 0))       ;;STRING:: "clojure.core"
                                      (152 (ldc 12))      ;;STRING:: "next"
                                      (154 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (157 (checkcast (class "clojure.lang.Var")))
                                      (160 (putstatic (fieldCP "const__12" "clojure.core$await_for" (class "clojure.lang.Var"))))
                                      (163 (return))
                                      (endofcode 164))
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
					(methodCP "<init>" "clojure.lang.RestFn" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "doInvoke"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 8) (max_locals . 14) (code_length . 436)
                                   (parsedcode
                                      (0 (invokestatic (methodCP "isRunning" "clojure.lang.LockingTransaction" () boolean))) 
                                      (3 (ifeq 26)) ;;to TAG_0
                                      (6 (new (class "java.lang.IllegalStateException"))) 
                                      (9 (dup)) 
                                      (10 (ldc 13)) ;;STRING:: "await-for in transaction"
                                      (12 (checkcast (class "java.lang.String"))) 
                                      (15 (invokespecial (methodCP "<init>" "java.lang.IllegalStateException" ((class "java.lang.String")) void))) 
                                      (18 (checkcast (class "java.lang.Throwable"))) 
                                      (21 (athrow)) 
                                      (22 (goto 435))  ;;to TAG_1
                                      (25 (pop)) 
                                      (26 (getstatic (fieldCP "const__0" "clojure.core$await_for" (class "clojure.lang.Var")))) ;;at TAG_0
                                      (29 (invokevirtual (methodCP "get" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (32 (dup)) 
                                      (33 (ifnull 61)) ;;to TAG_2
                                      (36 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (39 (if_acmpeq 62)) ;;to TAG_3
                                      (42 (new (class "java.lang.Exception"))) 
                                      (45 (dup)) 
                                      (46 (ldc 14)) ;;STRING:: "Can\nt await in agent action"
                                      (48 (checkcast (class "java.lang.String"))) 
                                      (51 (invokespecial (methodCP "<init>" "java.lang.Exception" ((class "java.lang.String")) void))) 
                                      (54 (checkcast (class "java.lang.Throwable"))) 
                                      (57 (athrow)) 
                                      (58 (goto 64)) ;;to TAG_4
                                      (61 (pop)) ;;at TAG_2
                                      (62 (aconst_null)) ;;at TAG_3
                                      (63 (pop)) 
                                      (64 (new (class "java.util.concurrent.CountDownLatch"))) ;;at TAG_4
                                      (67 (dup)) 
                                      (68 (aload_2)) 
                                      (69 (invokestatic (methodCP "count" "clojure.lang.RT" ((class "java.lang.Object")) int))) 
                                      (72 (invokespecial (methodCP "<init>" "java.util.concurrent.CountDownLatch" (int) void))) 
                                      (75 (astore_3)) 
                                      (76 (new (class "clojure.core$await_for$count_down__4331"))) 
                                      (79 (dup)) 
                                      (80 (aload_3)) 
                                      (81 (invokespecial (methodCP "<init>" "clojure.core$await_for$count_down__4331" ((class "java.lang.Object")) void))) 
                                      (84 (astore 4)) 
                                      (86 (getstatic (fieldCP "const__2" "clojure.core$await_for" (class "clojure.lang.Var")))) 
                                      (89 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (92 (checkcast (class "clojure.lang.IFn"))) 
                                      (95 (aload_2)) 
                                      (96 (aconst_null)) 
                                      (97 (astore_2)) 
                                      (98 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (103 (astore 5)) 
                                      (105 (aconst_null)) 
                                      (106 (astore 6)) 
                                      (108 (lconst_0)) 
                                      (109 (lstore 7)) 
                                      (111 (lconst_0)) 
                                      (112 (lstore 9)) 
                                      (114 (lload 9)) ;;at TAG_6
                                      (116 (lload 7)) 
                                      (118 (lcmp)) 
                                      (119 (ifge 186)) ;;to TAG_5
                                      (122 (aload 6)) 
                                      (124 (checkcast (class "clojure.lang.Indexed"))) 
                                      (127 (lload 9)) 
                                      (129 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (132 (invokeinterface (methodCP "nth" "clojure.lang.Indexed" (int) (class "java.lang.Object")) 2)) 
                                      (137 (astore 11)) 
                                      (139 (getstatic (fieldCP "const__5" "clojure.core$await_for" (class "clojure.lang.Var")))) 
                                      (142 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (145 (checkcast (class "clojure.lang.IFn"))) 
                                      (148 (aload 11)) 
                                      (150 (aconst_null)) 
                                      (151 (astore 11)) 
                                      (153 (aload 4)) 
                                      (155 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (160 (pop)) 
                                      (161 (aload 5)) 
                                      (163 (aload 6)) 
                                      (165 (lload 7)) 
                                      (167 (lload 9)) 
                                      (169 (lconst_1)) 
                                      (170 (ladd)) 
                                      (171 (lstore 9)) 
                                      (173 (lstore 7)) 
                                      (175 (astore 6)) 
                                      (177 (astore 5)) 
                                      (179 (goto 114)) ;;to TAG_6
                                      (182 (goto 399)) ;;to TAG_7
                                      (185 (pop)) 
                                      (186 (getstatic (fieldCP "const__2" "clojure.core$await_for" (class "clojure.lang.Var")))) ;;at TAG_5
                                      (189 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (192 (checkcast (class "clojure.lang.IFn"))) 
                                      (195 (aload 5)) 
                                      (197 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (202 (astore 11)) 
                                      (204 (aload 11)) 
                                      (206 (dup)) 
                                      (207 (ifnull 396)) ;;to TAG_8
                                      (210 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (213 (if_acmpeq 397)) ;;to TAG_9
                                      (216 (aload 11)) 
                                      (218 (aconst_null)) 
                                      (219 (astore 11)) 
                                      (221 (astore 12)) 
                                      (223 (getstatic (fieldCP "const__7" "clojure.core$await_for" (class "clojure.lang.Var")))) 
                                      (226 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (229 (checkcast (class "clojure.lang.IFn"))) 
                                      (232 (aload 12)) 
                                      (234 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (239 (dup)) 
                                      (240 (ifnull 319)) ;;to TAG_10
                                      (243 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (246 (if_acmpeq 320)) ;;to TAG_11
                                      (249 (getstatic (fieldCP "const__8" "clojure.core$await_for" (class "clojure.lang.Var")))) 
                                      (252 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (255 (checkcast (class "clojure.lang.IFn"))) 
                                      (258 (aload 12)) 
                                      (260 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (265 (astore 13)) 
                                      (267 (getstatic (fieldCP "const__9" "clojure.core$await_for" (class "clojure.lang.Var")))) 
                                      (270 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (273 (checkcast (class "clojure.lang.IFn"))) 
                                      (276 (aload 12)) 
                                      (278 (aconst_null)) 
                                      (279 (astore 12)) 
                                      (281 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (286 (aload 13)) 
                                      (288 (aload 13)) 
                                      (290 (aconst_null)) 
                                      (291 (astore 13)) 
                                      (293 (invokestatic (methodCP "count" "clojure.lang.RT" ((class "java.lang.Object")) int))) 
                                      (296 (invokestatic (methodCP "intCast" "clojure.lang.RT" (int) int))) 
                                      (299 (i2l)) 
                                      (300 (lconst_0)) 
                                      (301 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (304 (i2l)) 
                                      (305 (lstore 9)) 
                                      (307 (lstore 7)) 
                                      (309 (astore 6)) 
                                      (311 (astore 5)) 
                                      (313 (goto 114)) ;;to TAG_6
                                      (316 (goto 393)) ;;to TAG_12
                                      (319 (pop)) ;;at TAG_10
                                      (320 (getstatic (fieldCP "const__11" "clojure.core$await_for" (class "clojure.lang.Var")))) ;;at TAG_11
                                      (323 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (326 (checkcast (class "clojure.lang.IFn"))) 
                                      (329 (aload 12)) 
                                      (331 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (336 (astore 13)) 
                                      (338 (getstatic (fieldCP "const__5" "clojure.core$await_for" (class "clojure.lang.Var")))) 
                                      (341 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (344 (checkcast (class "clojure.lang.IFn"))) 
                                      (347 (aload 13)) 
                                      (349 (aconst_null)) 
                                      (350 (astore 13)) 
                                      (352 (aload 4)) 
                                      (354 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (359 (pop)) 
                                      (360 (getstatic (fieldCP "const__12" "clojure.core$await_for" (class "clojure.lang.Var")))) 
                                      (363 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (366 (checkcast (class "clojure.lang.IFn"))) 
                                      (369 (aload 12)) 
                                      (371 (aconst_null)) 
                                      (372 (astore 12)) 
                                      (374 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (379 (aconst_null)) 
                                      (380 (lconst_0)) 
                                      (381 (lconst_0)) 
                                      (382 (lstore 9)) 
                                      (384 (lstore 7)) 
                                      (386 (astore 6)) 
                                      (388 (astore 5)) 
                                      (390 (goto 114)) ;;to TAG_6
                                      (393 (goto 399)) ;;to TAG_7;;at TAG_12
                                      (396 (pop)) ;;at TAG_8
                                      (397 (aconst_null)) ;;at TAG_9
                                      (398 (pop)) 
                                      (399 (aload_3)) ;;at TAG_7
                                      (400 (aconst_null)) 
                                      (401 (astore_3)) 
                                      (402 (checkcast (class "java.util.concurrent.CountDownLatch"))) 
                                      (405 (aload_1)) 
                                      (406 (aconst_null)) 
                                      (407 (astore_1)) 
                                      (408 (checkcast (class "java.lang.Number"))) 
                                      (411 (invokestatic (methodCP "longCast" "clojure.lang.RT" ((class "java.lang.Object")) long))) 
                                      (414 (getstatic (fieldCP "MILLISECONDS" "java.util.concurrent.TimeUnit" (class "java.util.concurrent.TimeUnit")))) 
                                      (417 (checkcast (class "java.util.concurrent.TimeUnit"))) 
                                      (420 (invokevirtual (methodCP "await" "java.util.concurrent.CountDownLatch" (long (class "java.util.concurrent.TimeUnit")) boolean))) 
                                      (423 (ifeq 432)) ;;to TAG_13
                                      (426 (getstatic (fieldCP "TRUE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (429 (goto 435))  ;;to TAG_1
                                      (432 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) ;;at TAG_13
                                      (435 (areturn)) ;;at TAG_1
                                      (endofcode 436))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getRequiredArity"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 2)
                                   (parsedcode
                                      (0 (iconst_1))
                                      (1 (ireturn))
                                      (endofcode 2))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$await_for-class-table*
  (make-static-class-decls 
   *clojure.core$await_for*))

(defconst *package-name-map* 
  ("clojure.core$await_for" . "clojure"))

