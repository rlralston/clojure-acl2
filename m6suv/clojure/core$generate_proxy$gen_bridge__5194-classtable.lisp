; core$generate_proxy$gen_bridge__5194-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:43 CDT 2014.
;

(defconst *clojure.core$generate_proxy$gen_bridge__5194*
 (make-class-def
      '(class "clojure.core$generate_proxy$gen_bridge__5194"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "bit-or")
                        (STRING  "long")
                        (STRING  "count")
                        (STRING  "<")
                        (STRING  "unchecked-inc"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "cv" (class "java.lang.Object") (accessflags  *class* ) -1)
                        (field "to_types" (class "java.lang.Object") (accessflags  *class* ) -1)
                        (field "totype" (class "java.lang.Object") (accessflags  *class* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 73)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "bit-or"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$generate_proxy$gen_bridge__5194" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "long"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.core$generate_proxy$gen_bridge__5194" (class "clojure.lang.Var"))))
                                      (26 (ldc 0))        ;;STRING:: "clojure.core"
                                      (28 (ldc 3))        ;;STRING:: "count"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.core$generate_proxy$gen_bridge__5194" (class "clojure.lang.Var"))))
                                      (39 (lconst_0))
                                      (40 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (43 (putstatic (fieldCP "const__3" "clojure.core$generate_proxy$gen_bridge__5194" (class "java.lang.Object"))))
                                      (46 (ldc 0))        ;;STRING:: "clojure.core"
                                      (48 (ldc 4))        ;;STRING:: "<"
                                      (50 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (53 (checkcast (class "clojure.lang.Var")))
                                      (56 (putstatic (fieldCP "const__4" "clojure.core$generate_proxy$gen_bridge__5194" (class "clojure.lang.Var"))))
                                      (59 (ldc 0))        ;;STRING:: "clojure.core"
                                      (61 (ldc 5))        ;;STRING:: "unchecked-inc"
                                      (63 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (66 (checkcast (class "clojure.lang.Var")))
                                      (69 (putstatic (fieldCP "const__5" "clojure.core$generate_proxy$gen_bridge__5194" (class "clojure.lang.Var"))))
                                      (72 (return))
                                      (endofcode 73))
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
                                      (6 (putfield (fieldCP "cv" "clojure.core$generate_proxy$gen_bridge__5194" (class "java.lang.Object"))))
                                      (9 (aload_0))
                                      (10 (aload_2))
                                      (11 (putfield (fieldCP "to_types" "clojure.core$generate_proxy$gen_bridge__5194" (class "java.lang.Object"))))
                                      (14 (aload_0))
                                      (15 (aload_3))
                                      (16 (putfield (fieldCP "totype" "clojure.core$generate_proxy$gen_bridge__5194" (class "java.lang.Object"))))
                                      (19 (return))
                                      (endofcode 20))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 7) (max_locals . 14) (code_length . 377)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (checkcast (class "java.lang.reflect.Method"))) 
                                      (4 (invokevirtual (methodCP "getParameterTypes" "java.lang.reflect.Method" () (array (class "java.lang.Class"))))) 
                                      (7 (astore_3)) 
                                      (8 (aload_0)) 
                                      (9 (getfield (fieldCP "to_types" "clojure.core$generate_proxy$gen_bridge__5194" (class "java.lang.Object")))) 
                                      (12 (checkcast (class "clojure.lang.IFn"))) 
                                      (15 (aload_3)) 
                                      (16 (aconst_null)) 
                                      (17 (astore_3)) 
                                      (18 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (23 (astore 4)) 
                                      (25 (aload_0)) 
                                      (26 (getfield (fieldCP "totype" "clojure.core$generate_proxy$gen_bridge__5194" (class "java.lang.Object")))) 
                                      (29 (checkcast (class "clojure.lang.IFn"))) 
                                      (32 (aload_1)) 
                                      (33 (checkcast (class "java.lang.reflect.Method"))) 
                                      (36 (invokevirtual (methodCP "getReturnType" "java.lang.reflect.Method" () (class "java.lang.Class")))) 
                                      (39 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (44 (astore 5)) 
                                      (46 (new (class "clojure.asm.commons.Method"))) 
                                      (49 (dup)) 
                                      (50 (aload_1)) 
                                      (51 (aconst_null)) 
                                      (52 (astore_1)) 
                                      (53 (checkcast (class "java.lang.reflect.Method"))) 
                                      (56 (invokevirtual (methodCP "getName" "java.lang.reflect.Method" () (class "java.lang.String")))) 
                                      (59 (checkcast (class "java.lang.String"))) 
                                      (62 (aload 5)) 
                                      (64 (aconst_null)) 
                                      (65 (astore 5)) 
                                      (67 (checkcast (class "clojure.asm.Type"))) 
                                      (70 (aload 4)) 
                                      (72 (checkcast (array (class "clojure.asm.Type")))) 
                                      (75 (invokespecial (methodCP "<init>" "clojure.asm.commons.Method" ((class "java.lang.String") (class "clojure.asm.Type") (array (class "clojure.asm.Type"))) void))) 
                                      (78 (astore 6)) 
                                      (80 (aload_0)) 
                                      (81 (getfield (fieldCP "totype" "clojure.core$generate_proxy$gen_bridge__5194" (class "java.lang.Object")))) 
                                      (84 (checkcast (class "clojure.lang.IFn"))) 
                                      (87 (aload_2)) 
                                      (88 (checkcast (class "java.lang.reflect.Method"))) 
                                      (91 (invokevirtual (methodCP "getDeclaringClass" "java.lang.reflect.Method" () (class "java.lang.Class")))) 
                                      (94 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (99 (astore 7)) 
                                      (101 (new (class "clojure.asm.commons.Method"))) 
                                      (104 (dup)) 
                                      (105 (aload_2)) 
                                      (106 (checkcast (class "java.lang.reflect.Method"))) 
                                      (109 (invokevirtual (methodCP "getName" "java.lang.reflect.Method" () (class "java.lang.String")))) 
                                      (112 (checkcast (class "java.lang.String"))) 
                                      (115 (aload_0)) 
                                      (116 (getfield (fieldCP "totype" "clojure.core$generate_proxy$gen_bridge__5194" (class "java.lang.Object")))) 
                                      (119 (checkcast (class "clojure.lang.IFn"))) 
                                      (122 (aload_2)) 
                                      (123 (checkcast (class "java.lang.reflect.Method"))) 
                                      (126 (invokevirtual (methodCP "getReturnType" "java.lang.reflect.Method" () (class "java.lang.Class")))) 
                                      (129 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (134 (checkcast (class "clojure.asm.Type"))) 
                                      (137 (aload_0)) 
                                      (138 (getfield (fieldCP "to_types" "clojure.core$generate_proxy$gen_bridge__5194" (class "java.lang.Object")))) 
                                      (141 (checkcast (class "clojure.lang.IFn"))) 
                                      (144 (aload_2)) 
                                      (145 (checkcast (class "java.lang.reflect.Method"))) 
                                      (148 (invokevirtual (methodCP "getParameterTypes" "java.lang.reflect.Method" () (array (class "java.lang.Class"))))) 
                                      (151 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (156 (checkcast (array (class "clojure.asm.Type")))) 
                                      (159 (invokespecial (methodCP "<init>" "clojure.asm.commons.Method" ((class "java.lang.String") (class "clojure.asm.Type") (array (class "clojure.asm.Type"))) void))) 
                                      (162 (astore 8)) 
                                      (164 (new (class "clojure.asm.commons.GeneratorAdapter"))) 
                                      (167 (dup)) 
                                      (168 (getstatic (fieldCP "ACC_PUBLIC" "clojure.asm.Opcodes" int))) 
                                      (171 (i2l)) 
                                      (172 (getstatic (fieldCP "ACC_BRIDGE" "clojure.asm.Opcodes" int))) 
                                      (175 (i2l)) 
                                      (176 (lor)) 
                                      (177 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (180 (aload 6)) 
                                      (182 (aconst_null)) 
                                      (183 (astore 6)) 
                                      (185 (checkcast (class "clojure.asm.commons.Method"))) 
                                      (188 (aconst_null)) 
                                      (189 (checkcast (class "java.lang.String"))) 
                                      (192 (aconst_null)) 
                                      (193 (checkcast (array (class "clojure.asm.Type")))) 
                                      (196 (aload_0)) 
                                      (197 (getfield (fieldCP "cv" "clojure.core$generate_proxy$gen_bridge__5194" (class "java.lang.Object")))) 
                                      (200 (checkcast (class "clojure.asm.ClassVisitor"))) 
                                      (203 (invokespecial (methodCP "<init>" "clojure.asm.commons.GeneratorAdapter" (int (class "clojure.asm.commons.Method") (class "java.lang.String") (array (class "clojure.asm.Type")) (class "clojure.asm.ClassVisitor")) void))) 
                                      (206 (astore 9)) 
                                      (208 (aload 9)) 
                                      (210 (checkcast (class "clojure.asm.MethodAdapter"))) 
                                      (213 (invokevirtual (methodCP "visitCode" "clojure.asm.MethodAdapter" () void))) 
                                      (216 (aconst_null)) 
                                      (217 (pop)) 
                                      (218 (aload 9)) 
                                      (220 (checkcast (class "clojure.asm.commons.GeneratorAdapter"))) 
                                      (223 (invokevirtual (methodCP "loadThis" "clojure.asm.commons.GeneratorAdapter" () void))) 
                                      (226 (aconst_null)) 
                                      (227 (pop)) 
                                      (228 (aload 4)) 
                                      (230 (aconst_null)) 
                                      (231 (astore 4)) 
                                      (233 (invokestatic (methodCP "count" "clojure.lang.RT" ((class "java.lang.Object")) int))) 
                                      (236 (i2l)) 
                                      (237 (lstore 10)) 
                                      (239 (lconst_0)) 
                                      (240 (lstore 12)) 
                                      (242 (lload 12)) ;;at TAG_1
                                      (244 (lload 10)) 
                                      (246 (lcmp)) 
                                      (247 (ifge 278)) ;;to TAG_0
                                      (250 (aload 9)) 
                                      (252 (checkcast (class "clojure.asm.commons.GeneratorAdapter"))) 
                                      (255 (lload 12)) 
                                      (257 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (260 (invokevirtual (methodCP "loadArg" "clojure.asm.commons.GeneratorAdapter" (int) void))) 
                                      (263 (aconst_null)) 
                                      (264 (pop)) 
                                      (265 (lload 12)) 
                                      (267 (lconst_1)) 
                                      (268 (ladd)) 
                                      (269 (lstore 12)) 
                                      (271 (goto 242)) ;;to TAG_1
                                      (274 (goto 280))  ;;to TAG_2
                                      (277 (pop)) 
                                      (278 (aconst_null)) ;;at TAG_0
                                      (279 (pop)) 
                                      (280 (aload_2)) ;;at TAG_2
                                      (281 (aconst_null)) 
                                      (282 (astore_2)) 
                                      (283 (checkcast (class "java.lang.reflect.Method"))) 
                                      (286 (invokevirtual (methodCP "getDeclaringClass" "java.lang.reflect.Method" () (class "java.lang.Class")))) 
                                      (289 (checkcast (class "java.lang.Class"))) 
                                      (292 (invokevirtual (methodCP "isInterface" "java.lang.Class" () boolean))) 
                                      (295 (ifeq 328)) ;;to TAG_3
                                      (298 (aload 9)) 
                                      (300 (checkcast (class "clojure.asm.commons.GeneratorAdapter"))) 
                                      (303 (aload 7)) 
                                      (305 (aconst_null)) 
                                      (306 (astore 7)) 
                                      (308 (checkcast (class "clojure.asm.Type"))) 
                                      (311 (aload 8)) 
                                      (313 (aconst_null)) 
                                      (314 (astore 8)) 
                                      (316 (checkcast (class "clojure.asm.commons.Method"))) 
                                      (319 (invokevirtual (methodCP "invokeInterface" "clojure.asm.commons.GeneratorAdapter" ((class "clojure.asm.Type") (class "clojure.asm.commons.Method")) void))) 
                                      (322 (aconst_null)) 
                                      (323 (pop)) 
                                      (324 (goto 354)) ;;to TAG_4
                                      (327 (pop)) 
                                      (328 (aload 9)) ;;at TAG_3
                                      (330 (checkcast (class "clojure.asm.commons.GeneratorAdapter"))) 
                                      (333 (aload 7)) 
                                      (335 (aconst_null)) 
                                      (336 (astore 7)) 
                                      (338 (checkcast (class "clojure.asm.Type"))) 
                                      (341 (aload 8)) 
                                      (343 (aconst_null)) 
                                      (344 (astore 8)) 
                                      (346 (checkcast (class "clojure.asm.commons.Method"))) 
                                      (349 (invokevirtual (methodCP "invokeVirtual" "clojure.asm.commons.GeneratorAdapter" ((class "clojure.asm.Type") (class "clojure.asm.commons.Method")) void))) 
                                      (352 (aconst_null)) 
                                      (353 (pop)) 
                                      (354 (aload 9)) ;;at TAG_4
                                      (356 (checkcast (class "clojure.asm.commons.GeneratorAdapter"))) 
                                      (359 (invokevirtual (methodCP "returnValue" "clojure.asm.commons.GeneratorAdapter" () void))) 
                                      (362 (aconst_null)) 
                                      (363 (pop)) 
                                      (364 (aload 9)) 
                                      (366 (aconst_null)) 
                                      (367 (astore 9)) 
                                      (369 (checkcast (class "clojure.asm.commons.GeneratorAdapter"))) 
                                      (372 (invokevirtual (methodCP "endMethod" "clojure.asm.commons.GeneratorAdapter" () void))) 
                                      (375 (aconst_null)) 
                                      (376 (areturn)) 
                                      (endofcode 377))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$generate_proxy$gen_bridge__5194-class-table*
  (make-static-class-decls 
   *clojure.core$generate_proxy$gen_bridge__5194*))

(defconst *package-name-map* 
  ("clojure.core$generate_proxy$gen_bridge__5194" . "clojure"))

