; LispReader$CharacterReader-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:52 CDT 2014.
;

(defconst *clojure.lang.LispReader$CharacterReader*
 (make-class-def
      '(class "clojure.lang.LispReader$CharacterReader"
            "clojure.lang.AFn"
            (constant_pool
                        (STRING  "EOF while reading character")
                        (STRING  "newline")
                        (STRING  "space")
                        (STRING  "tab")
                        (STRING  "backspace")
                        (STRING  "formfeed")
                        (STRING  "return")
                        (STRING  "u")
                        (INT 55296)
                        (INT 57343)
                        (STRING  "Invalid character constant: \\u")
                        (STRING  "o")
                        (STRING  "Invalid octal escape sequence length: ")
                        (STRING  "Octal escape sequence must be in range [0, 377].")
                        (STRING  "Unsupported character: \\"))
            (fields)
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "clojure.lang.AFn" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 8) (code_length . 324)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (checkcast (class "java.io.PushbackReader"))) 
                                      (4 (astore_3)) 
                                      (5 (aload_3)) 
                                      (6 (invokestatic (methodCP "read1" "clojure.lang.LispReader" ((class "java.io.Reader")) int))) 
                                      (9 (istore 4)) 
                                      (11 (iload 4)) 
                                      (13 (iconst_m1)) 
                                      (14 (if_icmpne 23)) ;;to TAG_0
                                      (17 (ldc 0)) ;;STRING:: "EOF while reading character"
                                      (19 (invokestatic (methodCP "runtimeException" "clojure.lang.Util" ((class "java.lang.String")) (class "java.lang.RuntimeException")))) 
                                      (22 (athrow)) 
                                      (23 (aload_3)) ;;at TAG_0
                                      (24 (iload 4)) 
                                      (26 (i2c)) 
                                      (27 (invokestatic (methodCP "access$100" "clojure.lang.LispReader" ((class "java.io.PushbackReader") char) (class "java.lang.String")))) 
                                      (30 (astore 5)) 
                                      (32 (aload 5)) 
                                      (34 (invokevirtual (methodCP "length" "java.lang.String" () int))) 
                                      (37 (iconst_1)) 
                                      (38 (if_icmpne 51))  ;;to TAG_1
                                      (41 (aload 5)) 
                                      (43 (iconst_0)) 
                                      (44 (invokevirtual (methodCP "charAt" "java.lang.String" (int) char))) 
                                      (47 (invokestatic (methodCP "valueOf" "java.lang.Character" (char) (class "java.lang.Character")))) 
                                      (50 (areturn)) 
                                      (51 (aload 5)) ;;at TAG_1
                                      (53 (ldc 1)) ;;STRING:: "newline"
                                      (55 (invokevirtual (methodCP "equals" "java.lang.String" ((class "java.lang.Object")) boolean))) 
                                      (58 (ifeq 67)) ;;to TAG_2
                                      (61 (bipush 10)) 
                                      (63 (invokestatic (methodCP "valueOf" "java.lang.Character" (char) (class "java.lang.Character")))) 
                                      (66 (areturn)) 
                                      (67 (aload 5)) ;;at TAG_2
                                      (69 (ldc 2)) ;;STRING:: "space"
                                      (71 (invokevirtual (methodCP "equals" "java.lang.String" ((class "java.lang.Object")) boolean))) 
                                      (74 (ifeq 83)) ;;to TAG_3
                                      (77 (bipush 32)) 
                                      (79 (invokestatic (methodCP "valueOf" "java.lang.Character" (char) (class "java.lang.Character")))) 
                                      (82 (areturn)) 
                                      (83 (aload 5)) ;;at TAG_3
                                      (85 (ldc 3)) ;;STRING:: "tab"
                                      (87 (invokevirtual (methodCP "equals" "java.lang.String" ((class "java.lang.Object")) boolean))) 
                                      (90 (ifeq 99)) ;;to TAG_4
                                      (93 (bipush 9)) 
                                      (95 (invokestatic (methodCP "valueOf" "java.lang.Character" (char) (class "java.lang.Character")))) 
                                      (98 (areturn)) 
                                      (99 (aload 5)) ;;at TAG_4
                                      (101 (ldc 4)) ;;STRING:: "backspace"
                                      (103 (invokevirtual (methodCP "equals" "java.lang.String" ((class "java.lang.Object")) boolean))) 
                                      (106 (ifeq 115)) ;;to TAG_5
                                      (109 (bipush 8)) 
                                      (111 (invokestatic (methodCP "valueOf" "java.lang.Character" (char) (class "java.lang.Character")))) 
                                      (114 (areturn)) 
                                      (115 (aload 5)) ;;at TAG_5
                                      (117 (ldc 5)) ;;STRING:: "formfeed"
                                      (119 (invokevirtual (methodCP "equals" "java.lang.String" ((class "java.lang.Object")) boolean))) 
                                      (122 (ifeq 131)) ;;to TAG_6
                                      (125 (bipush 12)) 
                                      (127 (invokestatic (methodCP "valueOf" "java.lang.Character" (char) (class "java.lang.Character")))) 
                                      (130 (areturn)) 
                                      (131 (aload 5)) ;;at TAG_6
                                      (133 (ldc 6)) ;;STRING:: "return"
                                      (135 (invokevirtual (methodCP "equals" "java.lang.String" ((class "java.lang.Object")) boolean))) 
                                      (138 (ifeq 147)) ;;to TAG_7
                                      (141 (bipush 13)) 
                                      (143 (invokestatic (methodCP "valueOf" "java.lang.Character" (char) (class "java.lang.Character")))) 
                                      (146 (areturn)) 
                                      (147 (aload 5)) ;;at TAG_7
                                      (149 (ldc 7)) ;;STRING:: "u"
                                      (151 (invokevirtual (methodCP "startsWith" "java.lang.String" ((class "java.lang.String")) boolean))) 
                                      (154 (ifeq 218)) ;;to TAG_8
                                      (157 (aload 5)) 
                                      (159 (iconst_1)) 
                                      (160 (iconst_4)) 
                                      (161 (bipush 16)) 
                                      (163 (invokestatic (methodCP "access$400" "clojure.lang.LispReader" ((class "java.lang.String") int int int) int))) 
                                      (166 (i2c)) 
                                      (167 (istore 6)) 
                                      (169 (iload 6)) 
                                      (171 (ldc 8)) ;;INT:: "55296"
                                      (173 (if_icmplt 212)) ;;to TAG_9
                                      (176 (iload 6)) 
                                      (178 (ldc 9)) ;;INT:: "57343"
                                      (180 (if_icmpgt 212)) ;;to TAG_9
                                      (183 (new (class "java.lang.StringBuilder"))) 
                                      (186 (dup)) 
                                      (187 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (190 (ldc 10)) ;;STRING:: "Invalid character constant: \\u"
                                      (192 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (195 (iload 6)) 
                                      (197 (bipush 16)) 
                                      (199 (invokestatic (methodCP "toString" "java.lang.Integer" (int int) (class "java.lang.String")))) 
                                      (202 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (205 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (208 (invokestatic (methodCP "runtimeException" "clojure.lang.Util" ((class "java.lang.String")) (class "java.lang.RuntimeException")))) 
                                      (211 (athrow)) 
                                      (212 (iload 6)) ;;at TAG_9
                                      (214 (invokestatic (methodCP "valueOf" "java.lang.Character" (char) (class "java.lang.Character")))) 
                                      (217 (areturn)) 
                                      (218 (aload 5)) ;;at TAG_8
                                      (220 (ldc 11)) ;;STRING:: "o"
                                      (222 (invokevirtual (methodCP "startsWith" "java.lang.String" ((class "java.lang.String")) boolean))) 
                                      (225 (ifeq 300)) ;;to TAG_10
                                      (228 (aload 5)) 
                                      (230 (invokevirtual (methodCP "length" "java.lang.String" () int))) 
                                      (233 (iconst_1)) 
                                      (234 (isub)) 
                                      (235 (istore 6)) 
                                      (237 (iload 6)) 
                                      (239 (iconst_3)) 
                                      (240 (if_icmple 267)) ;;to TAG_11
                                      (243 (new (class "java.lang.StringBuilder"))) 
                                      (246 (dup)) 
                                      (247 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (250 (ldc 12)) ;;STRING:: "Invalid octal escape sequence length: "
                                      (252 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (255 (iload 6)) 
                                      (257 (invokevirtual (methodCP "append" "java.lang.StringBuilder" (int) (class "java.lang.StringBuilder")))) 
                                      (260 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (263 (invokestatic (methodCP "runtimeException" "clojure.lang.Util" ((class "java.lang.String")) (class "java.lang.RuntimeException")))) 
                                      (266 (athrow)) 
                                      (267 (aload 5)) ;;at TAG_11
                                      (269 (iconst_1)) 
                                      (270 (iload 6)) 
                                      (272 (bipush 8)) 
                                      (274 (invokestatic (methodCP "access$400" "clojure.lang.LispReader" ((class "java.lang.String") int int int) int))) 
                                      (277 (istore 7)) 
                                      (279 (iload 7)) 
                                      (281 (sipush 255)) 
                                      (284 (if_icmple 293)) ;;to TAG_12
                                      (287 (ldc 13)) ;;STRING:: "Octal escape sequence must be in range [0, 377]."
                                      (289 (invokestatic (methodCP "runtimeException" "clojure.lang.Util" ((class "java.lang.String")) (class "java.lang.RuntimeException")))) 
                                      (292 (athrow)) 
                                      (293 (iload 7)) ;;at TAG_12
                                      (295 (i2c)) 
                                      (296 (invokestatic (methodCP "valueOf" "java.lang.Character" (char) (class "java.lang.Character")))) 
                                      (299 (areturn)) 
                                      (300 (new (class "java.lang.StringBuilder"))) ;;at TAG_10
                                      (303 (dup)) 
                                      (304 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (307 (ldc 14)) ;;STRING:: "Unsupported character: \\"
                                      (309 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (312 (aload 5)) 
                                      (314 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (317 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (320 (invokestatic (methodCP "runtimeException" "clojure.lang.Util" ((class "java.lang.String")) (class "java.lang.RuntimeException")))) 
                                      (323 (athrow)) 
                                      (endofcode 324))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *LispReader$CharacterReader-class-table*
  (make-static-class-decls 
   *clojure.lang.LispReader$CharacterReader*))

(defconst *package-name-map* 
  ("clojure.lang.LispReader$CharacterReader" . "clojure.lang"))
