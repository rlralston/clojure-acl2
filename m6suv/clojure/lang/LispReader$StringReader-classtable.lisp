; LispReader$StringReader-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:52 CDT 2014.
;

(defconst *clojure.lang.LispReader$StringReader*
 (make-class-def
      '(class "clojure.lang.LispReader$StringReader"
            "clojure.lang.AFn"
            (constant_pool
                        (STRING  "EOF while reading string")
                        (STRING  "Invalid unicode escape: \\u")
                        (STRING  "Octal escape sequence must be in range [0, 377].")
                        (STRING  "Unsupported escape character: \\"))
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
                                   (max_stack . 5) (max_locals . 6) (code_length . 333)
                                   (parsedcode
                                      (0 (new (class "java.lang.StringBuilder"))) 
                                      (3 (dup)) 
                                      (4 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (7 (astore_3)) 
                                      (8 (aload_1)) 
                                      (9 (checkcast (class "java.io.Reader"))) 
                                      (12 (astore 4)) 
                                      (14 (aload 4)) 
                                      (16 (invokestatic (methodCP "read1" "clojure.lang.LispReader" ((class "java.io.Reader")) int))) 
                                      (19 (istore 5)) 
                                      (21 (iload 5)) ;;at TAG_15
                                      (23 (bipush 34)) 
                                      (25 (if_icmpeq 328)) ;;to TAG_0
                                      (28 (iload 5)) 
                                      (30 (iconst_m1)) 
                                      (31 (if_icmpne 40))  ;;to TAG_1
                                      (34 (ldc 0)) ;;STRING:: "EOF while reading string"
                                      (36 (invokestatic (methodCP "runtimeException" "clojure.lang.Util" ((class "java.lang.String")) (class "java.lang.RuntimeException")))) 
                                      (39 (athrow)) 
                                      (40 (iload 5)) ;;at TAG_1
                                      (42 (bipush 92)) 
                                      (44 (if_icmpne 310)) ;;to TAG_2
                                      (47 (aload 4)) 
                                      (49 (invokestatic (methodCP "read1" "clojure.lang.LispReader" ((class "java.io.Reader")) int))) 
                                      (52 (istore 5)) 
                                      (54 (iload 5)) 
                                      (56 (iconst_m1)) 
                                      (57 (if_icmpne 66)) ;;to TAG_3
                                      (60 (ldc 0)) ;;STRING:: "EOF while reading string"
                                      (62 (invokestatic (methodCP "runtimeException" "clojure.lang.Util" ((class "java.lang.String")) (class "java.lang.RuntimeException")))) 
                                      (65 (athrow)) 
                                      (66 (iload 5)) ;;at TAG_3
                                      (68 (lookupswitch (lookupswitchinfo 247 8 ((34 . 168) (92 . 165) (98 . 171) (102 . 178) (110 . 158) (114 . 151) (116 . 144) (117 . 185))))) ;;to TAG_4;;to TAG_5;;to TAG_6;;to TAG_7;;to TAG_8;;to TAG_9;;to TAG_10;;to TAG_11;;to TAG_12
                                      (144 (bipush 9)) ;;at TAG_11
                                      (146 (istore 5)) 
                                      (148 (goto 310)) ;;to TAG_2
                                      (151 (bipush 13)) ;;at TAG_10
                                      (153 (istore 5)) 
                                      (155 (goto 310)) ;;to TAG_2
                                      (158 (bipush 10)) ;;at TAG_9
                                      (160 (istore 5)) 
                                      (162 (goto 310)) ;;to TAG_2
                                      (165 (goto 310)) ;;to TAG_2;;at TAG_6
                                      (168 (goto 310)) ;;to TAG_2;;at TAG_5
                                      (171 (bipush 8)) ;;at TAG_7
                                      (173 (istore 5)) 
                                      (175 (goto 310)) ;;to TAG_2
                                      (178 (bipush 12)) ;;at TAG_8
                                      (180 (istore 5)) 
                                      (182 (goto 310)) ;;to TAG_2
                                      (185 (aload 4)) ;;at TAG_12
                                      (187 (invokestatic (methodCP "read1" "clojure.lang.LispReader" ((class "java.io.Reader")) int))) 
                                      (190 (istore 5)) 
                                      (192 (iload 5)) 
                                      (194 (bipush 16)) 
                                      (196 (invokestatic (methodCP "digit" "java.lang.Character" (int int) int))) 
                                      (199 (iconst_m1)) 
                                      (200 (if_icmpne 228)) ;;to TAG_13
                                      (203 (new (class "java.lang.StringBuilder"))) 
                                      (206 (dup)) 
                                      (207 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (210 (ldc 1)) ;;STRING:: "Invalid unicode escape: \\u"
                                      (212 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (215 (iload 5)) 
                                      (217 (i2c)) 
                                      (218 (invokevirtual (methodCP "append" "java.lang.StringBuilder" (char) (class "java.lang.StringBuilder")))) 
                                      (221 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (224 (invokestatic (methodCP "runtimeException" "clojure.lang.Util" ((class "java.lang.String")) (class "java.lang.RuntimeException")))) 
                                      (227 (athrow)) 
                                      (228 (aload 4)) ;;at TAG_13
                                      (230 (checkcast (class "java.io.PushbackReader"))) 
                                      (233 (iload 5)) 
                                      (235 (bipush 16)) 
                                      (237 (iconst_4)) 
                                      (238 (iconst_1)) 
                                      (239 (invokestatic (methodCP "access$000" "clojure.lang.LispReader" ((class "java.io.PushbackReader") int int int boolean) int))) 
                                      (242 (istore 5)) 
                                      (244 (goto 310)) ;;to TAG_2
                                      (247 (iload 5)) ;;at TAG_4
                                      (249 (invokestatic (methodCP "isDigit" "java.lang.Character" (int) boolean))) 
                                      (252 (ifeq 285)) ;;to TAG_14
                                      (255 (aload 4)) 
                                      (257 (checkcast (class "java.io.PushbackReader"))) 
                                      (260 (iload 5)) 
                                      (262 (bipush 8)) 
                                      (264 (iconst_3)) 
                                      (265 (iconst_0)) 
                                      (266 (invokestatic (methodCP "access$000" "clojure.lang.LispReader" ((class "java.io.PushbackReader") int int int boolean) int))) 
                                      (269 (istore 5)) 
                                      (271 (iload 5)) 
                                      (273 (sipush 255)) 
                                      (276 (if_icmple 310)) ;;to TAG_2
                                      (279 (ldc 2)) ;;STRING:: "Octal escape sequence must be in range [0, 377]."
                                      (281 (invokestatic (methodCP "runtimeException" "clojure.lang.Util" ((class "java.lang.String")) (class "java.lang.RuntimeException")))) 
                                      (284 (athrow)) 
                                      (285 (new (class "java.lang.StringBuilder"))) ;;at TAG_14
                                      (288 (dup)) 
                                      (289 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (292 (ldc 3)) ;;STRING:: "Unsupported escape character: \\"
                                      (294 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (297 (iload 5)) 
                                      (299 (i2c)) 
                                      (300 (invokevirtual (methodCP "append" "java.lang.StringBuilder" (char) (class "java.lang.StringBuilder")))) 
                                      (303 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (306 (invokestatic (methodCP "runtimeException" "clojure.lang.Util" ((class "java.lang.String")) (class "java.lang.RuntimeException")))) 
                                      (309 (athrow)) 
                                      (310 (aload_3)) ;;at TAG_2
                                      (311 (iload 5)) 
                                      (313 (i2c)) 
                                      (314 (invokevirtual (methodCP "append" "java.lang.StringBuilder" (char) (class "java.lang.StringBuilder")))) 
                                      (317 (pop)) 
                                      (318 (aload 4)) 
                                      (320 (invokestatic (methodCP "read1" "clojure.lang.LispReader" ((class "java.io.Reader")) int))) 
                                      (323 (istore 5)) 
                                      (325 (goto 21)) ;;to TAG_15
                                      (328 (aload_3)) ;;at TAG_0
                                      (329 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (332 (areturn)) 
                                      (endofcode 333))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *LispReader$StringReader-class-table*
  (make-static-class-decls 
   *clojure.lang.LispReader$StringReader*))

(defconst *package-name-map* 
  ("clojure.lang.LispReader$StringReader" . "clojure.lang"))
