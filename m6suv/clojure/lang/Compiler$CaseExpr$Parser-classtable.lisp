; Compiler$CaseExpr$Parser-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:50 CDT 2014.
;

(defconst *clojure.lang.Compiler$CaseExpr$Parser*
 (make-class-def
      '(class "clojure.lang.Compiler$CaseExpr$Parser"
            "java.lang.Object"
            (constant_pool)
            (fields)
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "parse"
                              (parameters (class "clojure.lang.Compiler$C") (class "java.lang.Object"))
                              (returntype . (class "clojure.lang.Compiler$Expr"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 15) (max_locals . 31) (code_length . 566)
                                   (parsedcode
                                      (0 (aload_2)) 
                                      (1 (checkcast (class "clojure.lang.ISeq"))) 
                                      (4 (astore_3)) 
                                      (5 (aload_1)) 
                                      (6 (getstatic (fieldCP "EVAL" "clojure.lang.Compiler$C" (class "clojure.lang.Compiler$C")))) 
                                      (9 (if_acmpne 30)) ;;to TAG_0
                                      (12 (aload_1)) 
                                      (13 (getstatic (fieldCP "FNONCE" "clojure.lang.Compiler" (class "clojure.lang.Symbol")))) 
                                      (16 (getstatic (fieldCP "EMPTY" "clojure.lang.PersistentVector" (class "clojure.lang.PersistentVector")))) 
                                      (19 (aload_3)) 
                                      (20 (invokestatic (methodCP "list" "clojure.lang.RT" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "clojure.lang.ISeq")))) 
                                      (23 (invokestatic (methodCP "list" "clojure.lang.RT" ((class "java.lang.Object")) (class "clojure.lang.ISeq")))) 
                                      (26 (invokestatic (methodCP "analyze" "clojure.lang.Compiler" ((class "clojure.lang.Compiler$C") (class "java.lang.Object")) (class "clojure.lang.Compiler$Expr")))) 
                                      (29 (areturn)) 
                                      (30 (aload_3)) ;;at TAG_0
                                      (31 (invokeinterface (methodCP "next" "clojure.lang.ISeq" () (class "clojure.lang.ISeq")) 1)) 
                                      (36 (invokestatic (methodCP "create" "clojure.lang.PersistentVector" ((class "clojure.lang.ISeq")) (class "clojure.lang.PersistentVector")))) 
                                      (39 (astore 4)) 
                                      (41 (aload 4)) 
                                      (43 (iconst_0)) 
                                      (44 (invokevirtual (methodCP "nth" "clojure.lang.PersistentVector" (int) (class "java.lang.Object")))) 
                                      (47 (astore 5)) 
                                      (49 (aload 4)) 
                                      (51 (iconst_1)) 
                                      (52 (invokevirtual (methodCP "nth" "clojure.lang.PersistentVector" (int) (class "java.lang.Object")))) 
                                      (55 (checkcast (class "java.lang.Number"))) 
                                      (58 (invokevirtual (methodCP "intValue" "java.lang.Number" () int))) 
                                      (61 (istore 6)) 
                                      (63 (aload 4)) 
                                      (65 (iconst_2)) 
                                      (66 (invokevirtual (methodCP "nth" "clojure.lang.PersistentVector" (int) (class "java.lang.Object")))) 
                                      (69 (checkcast (class "java.lang.Number"))) 
                                      (72 (invokevirtual (methodCP "intValue" "java.lang.Number" () int))) 
                                      (75 (istore 7)) 
                                      (77 (aload 4)) 
                                      (79 (iconst_3)) 
                                      (80 (invokevirtual (methodCP "nth" "clojure.lang.PersistentVector" (int) (class "java.lang.Object")))) 
                                      (83 (astore 8)) 
                                      (85 (aload 4)) 
                                      (87 (iconst_4)) 
                                      (88 (invokevirtual (methodCP "nth" "clojure.lang.PersistentVector" (int) (class "java.lang.Object")))) 
                                      (91 (checkcast (class "java.util.Map"))) 
                                      (94 (astore 9)) 
                                      (96 (aload 4)) 
                                      (98 (iconst_5)) 
                                      (99 (invokevirtual (methodCP "nth" "clojure.lang.PersistentVector" (int) (class "java.lang.Object")))) 
                                      (102 (checkcast (class "clojure.lang.Keyword"))) 
                                      (105 (astore 10)) 
                                      (107 (aload 4)) 
                                      (109 (bipush 6)) 
                                      (111 (invokevirtual (methodCP "nth" "clojure.lang.PersistentVector" (int) (class "java.lang.Object")))) 
                                      (114 (checkcast (class "clojure.lang.Keyword"))) 
                                      (117 (astore 11)) 
                                      (119 (aload 4)) 
                                      (121 (invokestatic (methodCP "count" "clojure.lang.RT" ((class "java.lang.Object")) int))) 
                                      (124 (bipush 8)) 
                                      (126 (if_icmpge 133)) ;;to TAG_1
                                      (129 (aconst_null)) 
                                      (130 (goto 143)) ;;to TAG_2
                                      (133 (aload 4)) ;;at TAG_1
                                      (135 (bipush 7)) 
                                      (137 (invokevirtual (methodCP "nth" "clojure.lang.PersistentVector" (int) (class "java.lang.Object")))) 
                                      (140 (checkcast (class "java.util.Set"))) 
                                      (143 (astore 12)) ;;at TAG_2
                                      (145 (aload 9)) 
                                      (147 (invokestatic (methodCP "keys" "clojure.lang.RT" ((class "java.lang.Object")) (class "clojure.lang.ISeq")))) 
                                      (150 (astore 13)) 
                                      (152 (aload 13)) 
                                      (154 (invokestatic (methodCP "first" "clojure.lang.RT" ((class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (157 (checkcast (class "java.lang.Number"))) 
                                      (160 (invokevirtual (methodCP "intValue" "java.lang.Number" () int))) 
                                      (163 (istore 14)) 
                                      (165 (aload 13)) 
                                      (167 (aload 13)) 
                                      (169 (invokestatic (methodCP "count" "clojure.lang.RT" ((class "java.lang.Object")) int))) 
                                      (172 (iconst_1)) 
                                      (173 (isub)) 
                                      (174 (invokestatic (methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int) (class "java.lang.Object")))) 
                                      (177 (checkcast (class "java.lang.Number"))) 
                                      (180 (invokevirtual (methodCP "intValue" "java.lang.Number" () int))) 
                                      (183 (istore 15)) 
                                      (185 (getstatic (fieldCP "EXPRESSION" "clojure.lang.Compiler$C" (class "clojure.lang.Compiler$C")))) 
                                      (188 (aload 5)) 
                                      (190 (invokestatic (methodCP "analyze" "clojure.lang.Compiler" ((class "clojure.lang.Compiler$C") (class "java.lang.Object")) (class "clojure.lang.Compiler$Expr")))) 
                                      (193 (checkcast (class "clojure.lang.Compiler$LocalBindingExpr"))) 
                                      (196 (astore 16)) 
                                      (198 (aload 16)) 
                                      (200 (iconst_0)) 
                                      (201 (putfield (fieldCP "shouldClear" "clojure.lang.Compiler$LocalBindingExpr" boolean))) 
                                      (204 (new (class "java.util.TreeMap"))) 
                                      (207 (dup)) 
                                      (208 (invokespecial (methodCP "<init>" "java.util.TreeMap" () void))) 
                                      (211 (astore 17)) 
                                      (213 (new (class "java.util.HashMap"))) 
                                      (216 (dup)) 
                                      (217 (invokespecial (methodCP "<init>" "java.util.HashMap" () void))) 
                                      (220 (astore 18)) 
                                      (222 (new (class "clojure.lang.Compiler$PathNode"))) 
                                      (225 (dup)) 
                                      (226 (getstatic (fieldCP "BRANCH" "clojure.lang.Compiler$PATHTYPE" (class "clojure.lang.Compiler$PATHTYPE")))) 
                                      (229 (getstatic (fieldCP "CLEAR_PATH" "clojure.lang.Compiler" (class "clojure.lang.Var")))) 
                                      (232 (invokevirtual (methodCP "get" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (235 (checkcast (class "clojure.lang.Compiler$PathNode"))) 
                                      (238 (invokespecial (methodCP "<init>" "clojure.lang.Compiler$PathNode" ((class "clojure.lang.Compiler$PATHTYPE") (class "clojure.lang.Compiler$PathNode")) void))) 
                                      (241 (astore 19)) 
                                      (243 (aload 9)) 
                                      (245 (invokeinterface (methodCP "entrySet" "java.util.Map" () (class "java.util.Set")) 1)) 
                                      (250 (invokeinterface (methodCP "iterator" "java.util.Set" () (class "java.util.Iterator")) 1)) 
                                      (255 (astore 20)) 
                                      (257 (aload 20)) ;;at TAG_8
                                      (259 (invokeinterface (methodCP "hasNext" "java.util.Iterator" () boolean) 1)) 
                                      (264 (ifeq 440)) ;;to TAG_3
                                      (267 (aload 20)) 
                                      (269 (invokeinterface (methodCP "next" "java.util.Iterator" () (class "java.lang.Object")) 1)) 
                                      (274 (astore 21)) 
                                      (276 (aload 21)) 
                                      (278 (checkcast (class "java.util.Map$Entry"))) 
                                      (281 (astore 22)) 
                                      (283 (aload 22)) 
                                      (285 (invokeinterface (methodCP "getKey" "java.util.Map$Entry" () (class "java.lang.Object")) 1)) 
                                      (290 (checkcast (class "java.lang.Number"))) 
                                      (293 (invokevirtual (methodCP "intValue" "java.lang.Number" () int))) 
                                      (296 (invokestatic (methodCP "valueOf" "java.lang.Integer" (int) (class "java.lang.Integer")))) 
                                      (299 (astore 23)) 
                                      (301 (aload 22)) 
                                      (303 (invokeinterface (methodCP "getValue" "java.util.Map$Entry" () (class "java.lang.Object")) 1)) 
                                      (308 (astore 24)) 
                                      (310 (aload 11)) 
                                      (312 (getstatic (fieldCP "intKey" "clojure.lang.Compiler$CaseExpr" (class "clojure.lang.Keyword")))) 
                                      (315 (if_acmpne 338)) ;;to TAG_4
                                      (318 (aload 24)) 
                                      (320 (invokestatic (methodCP "first" "clojure.lang.RT" ((class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (323 (checkcast (class "java.lang.Number"))) 
                                      (326 (invokevirtual (methodCP "intValue" "java.lang.Number" () int))) 
                                      (329 (invokestatic (methodCP "valueOf" "java.lang.Integer" (int) (class "java.lang.Integer")))) 
                                      (332 (invokestatic (methodCP "parse" "clojure.lang.Compiler$NumberExpr" ((class "java.lang.Number")) (class "clojure.lang.Compiler$Expr")))) 
                                      (335 (goto 350)) ;;to TAG_5
                                      (338 (new (class "clojure.lang.Compiler$ConstantExpr"))) ;;at TAG_4
                                      (341 (dup)) 
                                      (342 (aload 24)) 
                                      (344 (invokestatic (methodCP "first" "clojure.lang.RT" ((class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (347 (invokespecial (methodCP "<init>" "clojure.lang.Compiler$ConstantExpr" ((class "java.lang.Object")) void))) 
                                      (350 (astore 25)) ;;at TAG_5
                                      (352 (aload 17)) 
                                      (354 (aload 23)) 
                                      (356 (aload 25)) 
                                      (358 (invokeinterface (methodCP "put" "java.util.SortedMap" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (363 (pop)) 
                                      (364 (iconst_2)) ;;at TAG_11
                                      (365 (anewarray (class "java.lang.Object"))) 
                                      (368 (dup)) 
                                      (369 (iconst_0)) 
                                      (370 (getstatic (fieldCP "CLEAR_PATH" "clojure.lang.Compiler" (class "clojure.lang.Var")))) 
                                      (373 (aastore)) 
                                      (374 (dup)) 
                                      (375 (iconst_1)) 
                                      (376 (new (class "clojure.lang.Compiler$PathNode"))) 
                                      (379 (dup)) 
                                      (380 (getstatic (fieldCP "PATH" "clojure.lang.Compiler$PATHTYPE" (class "clojure.lang.Compiler$PATHTYPE")))) 
                                      (383 (aload 19)) 
                                      (385 (invokespecial (methodCP "<init>" "clojure.lang.Compiler$PathNode" ((class "clojure.lang.Compiler$PATHTYPE") (class "clojure.lang.Compiler$PathNode")) void))) 
                                      (388 (aastore)) 
                                      (389 (invokestatic (methodCP "map" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentMap")))) 
                                      (392 (invokestatic (methodCP "pushThreadBindings" "clojure.lang.Var" ((class "clojure.lang.Associative")) void))) 
                                      (395 (aload_1)) 
                                      (396 (aload 24)) 
                                      (398 (invokestatic (methodCP "second" "clojure.lang.RT" ((class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (401 (invokestatic (methodCP "analyze" "clojure.lang.Compiler" ((class "clojure.lang.Compiler$C") (class "java.lang.Object")) (class "clojure.lang.Compiler$Expr")))) 
                                      (404 (astore 26)) 
                                      (406 (jsr 420)) ;;to TAG_6
                                      (409 (goto 427)) ;;to TAG_7;;at TAG_12
                                      (412 (astore 27)) ;;at TAG_13
                                      (414 (jsr 420)) ;;to TAG_6
                                      (417 (aload 27)) ;;at TAG_14
                                      (419 (athrow)) 
                                      (420 (astore 28)) ;;at TAG_6
                                      (422 (invokestatic (methodCP "popThreadBindings" "clojure.lang.Var" () void))) 
                                      (425 (ret 28)) 
                                      (427 (aload 18)) ;;at TAG_7
                                      (429 (aload 23)) 
                                      (431 (aload 26)) 
                                      (433 (invokevirtual (methodCP "put" "java.util.HashMap" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (436 (pop)) 
                                      (437 (goto 257)) ;;to TAG_8
                                      (440 (iconst_2)) ;;at TAG_3
                                      (441 (anewarray (class "java.lang.Object"))) 
                                      (444 (dup)) 
                                      (445 (iconst_0)) 
                                      (446 (getstatic (fieldCP "CLEAR_PATH" "clojure.lang.Compiler" (class "clojure.lang.Var")))) 
                                      (449 (aastore)) 
                                      (450 (dup)) 
                                      (451 (iconst_1)) 
                                      (452 (new (class "clojure.lang.Compiler$PathNode"))) 
                                      (455 (dup)) 
                                      (456 (getstatic (fieldCP "PATH" "clojure.lang.Compiler$PATHTYPE" (class "clojure.lang.Compiler$PATHTYPE")))) 
                                      (459 (aload 19)) 
                                      (461 (invokespecial (methodCP "<init>" "clojure.lang.Compiler$PathNode" ((class "clojure.lang.Compiler$PATHTYPE") (class "clojure.lang.Compiler$PathNode")) void))) 
                                      (464 (aastore)) 
                                      (465 (invokestatic (methodCP "map" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentMap")))) 
                                      (468 (invokestatic (methodCP "pushThreadBindings" "clojure.lang.Var" ((class "clojure.lang.Associative")) void))) 
                                      (471 (aload_1)) 
                                      (472 (aload 4)) 
                                      (474 (iconst_3)) 
                                      (475 (invokevirtual (methodCP "nth" "clojure.lang.PersistentVector" (int) (class "java.lang.Object")))) 
                                      (478 (invokestatic (methodCP "analyze" "clojure.lang.Compiler" ((class "clojure.lang.Compiler$C") (class "java.lang.Object")) (class "clojure.lang.Compiler$Expr")))) 
                                      (481 (astore 20)) 
                                      (483 (jsr 497)) ;;to TAG_9
                                      (486 (goto 504)) ;;to TAG_10;;at TAG_15
                                      (489 (astore 29)) ;;at TAG_16
                                      (491 (jsr 497)) ;;to TAG_9
                                      (494 (aload 29)) ;;at TAG_17
                                      (496 (athrow)) 
                                      (497 (astore 30)) ;;at TAG_9
                                      (499 (invokestatic (methodCP "popThreadBindings" "clojure.lang.Var" () void))) 
                                      (502 (ret 30)) 
                                      (504 (getstatic (fieldCP "LINE" "clojure.lang.Compiler" (class "clojure.lang.Var")))) ;;at TAG_10
                                      (507 (invokevirtual (methodCP "deref" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (510 (checkcast (class "java.lang.Number"))) 
                                      (513 (invokevirtual (methodCP "intValue" "java.lang.Number" () int))) 
                                      (516 (istore 21)) 
                                      (518 (getstatic (fieldCP "COLUMN" "clojure.lang.Compiler" (class "clojure.lang.Var")))) 
                                      (521 (invokevirtual (methodCP "deref" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (524 (checkcast (class "java.lang.Number"))) 
                                      (527 (invokevirtual (methodCP "intValue" "java.lang.Number" () int))) 
                                      (530 (istore 22)) 
                                      (532 (new (class "clojure.lang.Compiler$CaseExpr"))) 
                                      (535 (dup)) 
                                      (536 (iload 21)) 
                                      (538 (iload 22)) 
                                      (540 (aload 16)) 
                                      (542 (iload 6)) 
                                      (544 (iload 7)) 
                                      (546 (iload 14)) 
                                      (548 (iload 15)) 
                                      (550 (aload 20)) 
                                      (552 (aload 17)) 
                                      (554 (aload 18)) 
                                      (556 (aload 10)) 
                                      (558 (aload 11)) 
                                      (560 (aload 12)) 
                                      (562 (invokespecial (methodCP "<init>" "clojure.lang.Compiler$CaseExpr" (int int (class "clojure.lang.Compiler$LocalBindingExpr") int int int int (class "clojure.lang.Compiler$Expr") (class "java.util.SortedMap") (class "java.util.HashMap") (class "clojure.lang.Keyword") (class "clojure.lang.Keyword") (class "java.util.Set")) void))) 
                                      (565 (areturn)) 
                                      (endofcode 566))
                                   (Exceptions 
                                     (handler 364 409  412 (class "java.lang.Throwable"))
                                     (handler 412 417  412 (class "java.lang.Throwable"))
                                     (handler 440 486  489 (class "java.lang.Throwable"))
                                     (handler 489 494  489 (class "java.lang.Throwable")))
                                   (StackMap ))))
            (interfaces "clojure.lang.Compiler$IParser")
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *Compiler$CaseExpr$Parser-class-table*
  (make-static-class-decls 
   *clojure.lang.Compiler$CaseExpr$Parser*))

(defconst *package-name-map* 
  ("clojure.lang.Compiler$CaseExpr$Parser" . "clojure.lang"))

