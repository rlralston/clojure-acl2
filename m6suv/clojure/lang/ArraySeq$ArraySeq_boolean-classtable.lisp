; ArraySeq$ArraySeq_boolean-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:50 CDT 2014.
;

(defconst *clojure.lang.ArraySeq$ArraySeq_boolean*
 (make-class-def
      '(class "clojure.lang.ArraySeq$ArraySeq_boolean"
            "clojure.lang.ASeq"
            (constant_pool)
            (fields
                        (field "array" (array boolean) (accessflags  *class*  *final*  *public* ) -1)
                        (field "i" int (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "clojure.lang.IPersistentMap") (array boolean) int)
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 4) (code_length . 16)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (invokespecial
					(methodCP "<init>" "clojure.lang.ASeq" ((class "clojure.lang.IPersistentMap")) void)))
                                      (5 (aload_0))
                                      (6 (aload_2))
                                      (7 (putfield (fieldCP "array" "clojure.lang.ArraySeq$ArraySeq_boolean" (array boolean))))
                                      (10 (aload_0))
                                      (11 (iload_3))
                                      (12 (putfield (fieldCP "i" "clojure.lang.ArraySeq$ArraySeq_boolean" int)))
                                      (15 (return))
                                      (endofcode 16))
                                   (Exceptions )
                                   (StackMap )))
                        (method "first"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 13)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "array" "clojure.lang.ArraySeq$ArraySeq_boolean" (array boolean))))
                                      (4 (aload_0))
                                      (5 (getfield (fieldCP "i" "clojure.lang.ArraySeq$ArraySeq_boolean" int)))
                                      (8 (baload))
                                      (9 (invokestatic
					(methodCP "valueOf" "java.lang.Boolean" (boolean) (class "java.lang.Boolean"))))
                                      (12 (areturn))
                                      (endofcode 13))
                                   (Exceptions )
                                   (StackMap )))
                        (method "next"
                              (parameters )
                              (returntype . (class "clojure.lang.ISeq"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 1) (code_length . 38)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "i" "clojure.lang.ArraySeq$ArraySeq_boolean" int))) 
                                      (4 (iconst_1)) 
                                      (5 (iadd)) 
                                      (6 (aload_0)) 
                                      (7 (getfield (fieldCP "array" "clojure.lang.ArraySeq$ArraySeq_boolean" (array boolean)))) 
                                      (10 (arraylength)) 
                                      (11 (if_icmpge 36))  ;;to TAG_0
                                      (14 (new (class "clojure.lang.ArraySeq$ArraySeq_boolean"))) 
                                      (17 (dup)) 
                                      (18 (aload_0)) 
                                      (19 (invokevirtual (methodCP "meta" "clojure.lang.ArraySeq$ArraySeq_boolean" () (class "clojure.lang.IPersistentMap")))) 
                                      (22 (aload_0)) 
                                      (23 (getfield (fieldCP "array" "clojure.lang.ArraySeq$ArraySeq_boolean" (array boolean)))) 
                                      (26 (aload_0)) 
                                      (27 (getfield (fieldCP "i" "clojure.lang.ArraySeq$ArraySeq_boolean" int))) 
                                      (30 (iconst_1)) 
                                      (31 (iadd)) 
                                      (32 (invokespecial (methodCP "<init>" "clojure.lang.ArraySeq$ArraySeq_boolean" ((class "clojure.lang.IPersistentMap") (array boolean) int) void))) 
                                      (35 (areturn)) 
                                      (36 (aconst_null)) ;;at TAG_0
                                      (37 (areturn)) 
                                      (endofcode 38))
                                   (Exceptions )
                                   (StackMap )))
                        (method "count"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 11)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "array" "clojure.lang.ArraySeq$ArraySeq_boolean" (array boolean))))
                                      (4 (arraylength))
                                      (5 (aload_0))
                                      (6 (getfield (fieldCP "i" "clojure.lang.ArraySeq$ArraySeq_boolean" int)))
                                      (9 (isub))
                                      (10 (ireturn))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap )))
                        (method "index"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "i" "clojure.lang.ArraySeq$ArraySeq_boolean" int)))
                                      (4 (ireturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "withMeta"
                              (parameters (class "clojure.lang.IPersistentMap"))
                              (returntype . (class "clojure.lang.ArraySeq$ArraySeq_boolean"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 2) (code_length . 17)
                                   (parsedcode
                                      (0 (new (class "clojure.lang.ArraySeq$ArraySeq_boolean")))
                                      (3 (dup))
                                      (4 (aload_1))
                                      (5 (aload_0))
                                      (6 (getfield (fieldCP "array" "clojure.lang.ArraySeq$ArraySeq_boolean" (array boolean))))
                                      (9 (aload_0))
                                      (10 (getfield (fieldCP "i" "clojure.lang.ArraySeq$ArraySeq_boolean" int)))
                                      (13 (invokespecial
					(methodCP "<init>" "clojure.lang.ArraySeq$ArraySeq_boolean" ((class "clojure.lang.IPersistentMap") (array boolean) int) void)))
                                      (16 (areturn))
                                      (endofcode 17))
                                   (Exceptions )
                                   (StackMap )))
                        (method "reduce"
                              (parameters (class "clojure.lang.IFn"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 4) (code_length . 54)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "array" "clojure.lang.ArraySeq$ArraySeq_boolean" (array boolean)))) 
                                      (4 (aload_0)) 
                                      (5 (getfield (fieldCP "i" "clojure.lang.ArraySeq$ArraySeq_boolean" int))) 
                                      (8 (baload)) 
                                      (9 (invokestatic (methodCP "valueOf" "java.lang.Boolean" (boolean) (class "java.lang.Boolean")))) 
                                      (12 (astore_2)) 
                                      (13 (aload_0)) 
                                      (14 (getfield (fieldCP "i" "clojure.lang.ArraySeq$ArraySeq_boolean" int))) 
                                      (17 (iconst_1)) 
                                      (18 (iadd)) 
                                      (19 (istore_3)) 
                                      (20 (iload_3)) ;;at TAG_1
                                      (21 (aload_0)) 
                                      (22 (getfield (fieldCP "array" "clojure.lang.ArraySeq$ArraySeq_boolean" (array boolean)))) 
                                      (25 (arraylength)) 
                                      (26 (if_icmpge 52))  ;;to TAG_0
                                      (29 (aload_1)) 
                                      (30 (aload_2)) 
                                      (31 (aload_0)) 
                                      (32 (getfield (fieldCP "array" "clojure.lang.ArraySeq$ArraySeq_boolean" (array boolean)))) 
                                      (35 (iload_3)) 
                                      (36 (baload)) 
                                      (37 (invokestatic (methodCP "valueOf" "java.lang.Boolean" (boolean) (class "java.lang.Boolean")))) 
                                      (40 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (45 (astore_2)) 
                                      (46 (iinc 3 1)) 
                                      (49 (goto 20)) ;;to TAG_1
                                      (52 (aload_2)) ;;at TAG_0
                                      (53 (areturn)) 
                                      (endofcode 54))
                                   (Exceptions )
                                   (StackMap )))
                        (method "reduce"
                              (parameters (class "clojure.lang.IFn") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 5) (code_length . 64)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (aload_2)) 
                                      (2 (aload_0)) 
                                      (3 (getfield (fieldCP "array" "clojure.lang.ArraySeq$ArraySeq_boolean" (array boolean)))) 
                                      (6 (aload_0)) 
                                      (7 (getfield (fieldCP "i" "clojure.lang.ArraySeq$ArraySeq_boolean" int))) 
                                      (10 (baload)) 
                                      (11 (invokestatic (methodCP "valueOf" "java.lang.Boolean" (boolean) (class "java.lang.Boolean")))) 
                                      (14 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (19 (astore_3)) 
                                      (20 (aload_0)) 
                                      (21 (getfield (fieldCP "i" "clojure.lang.ArraySeq$ArraySeq_boolean" int))) 
                                      (24 (iconst_1)) 
                                      (25 (iadd)) 
                                      (26 (istore 4)) 
                                      (28 (iload 4)) ;;at TAG_1
                                      (30 (aload_0)) 
                                      (31 (getfield (fieldCP "array" "clojure.lang.ArraySeq$ArraySeq_boolean" (array boolean)))) 
                                      (34 (arraylength)) 
                                      (35 (if_icmpge 62))  ;;to TAG_0
                                      (38 (aload_1)) 
                                      (39 (aload_3)) 
                                      (40 (aload_0)) 
                                      (41 (getfield (fieldCP "array" "clojure.lang.ArraySeq$ArraySeq_boolean" (array boolean)))) 
                                      (44 (iload 4)) 
                                      (46 (baload)) 
                                      (47 (invokestatic (methodCP "valueOf" "java.lang.Boolean" (boolean) (class "java.lang.Boolean")))) 
                                      (50 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (55 (astore_3)) 
                                      (56 (iinc 4 1)) 
                                      (59 (goto 28)) ;;to TAG_1
                                      (62 (aload_3)) ;;at TAG_0
                                      (63 (areturn)) 
                                      (endofcode 64))
                                   (Exceptions )
                                   (StackMap )))
                        (method "indexOf"
                              (parameters (class "java.lang.Object"))
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 4) (code_length . 103)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (instanceof (class "java.lang.Boolean"))) 
                                      (4 (ifeq 52)) ;;to TAG_0
                                      (7 (aload_1)) 
                                      (8 (checkcast (class "java.lang.Boolean"))) 
                                      (11 (invokevirtual (methodCP "booleanValue" "java.lang.Boolean" () boolean))) 
                                      (14 (istore_2)) 
                                      (15 (aload_0)) 
                                      (16 (getfield (fieldCP "i" "clojure.lang.ArraySeq$ArraySeq_boolean" int))) 
                                      (19 (istore_3)) 
                                      (20 (iload_3)) ;;at TAG_2
                                      (21 (aload_0)) 
                                      (22 (getfield (fieldCP "array" "clojure.lang.ArraySeq$ArraySeq_boolean" (array boolean)))) 
                                      (25 (arraylength)) 
                                      (26 (if_icmpge 52)) ;;to TAG_0
                                      (29 (iload_2)) 
                                      (30 (aload_0)) 
                                      (31 (getfield (fieldCP "array" "clojure.lang.ArraySeq$ArraySeq_boolean" (array boolean)))) 
                                      (34 (iload_3)) 
                                      (35 (baload)) 
                                      (36 (if_icmpne 46)) ;;to TAG_1
                                      (39 (iload_3)) 
                                      (40 (aload_0)) 
                                      (41 (getfield (fieldCP "i" "clojure.lang.ArraySeq$ArraySeq_boolean" int))) 
                                      (44 (isub)) 
                                      (45 (ireturn)) 
                                      (46 (iinc 3 1)) ;;at TAG_1
                                      (49 (goto 20))  ;;to TAG_2
                                      (52 (aload_1)) ;;at TAG_0
                                      (53 (ifnonnull 58)) ;;to TAG_3
                                      (56 (iconst_m1)) 
                                      (57 (ireturn)) 
                                      (58 (aload_0)) ;;at TAG_3
                                      (59 (getfield (fieldCP "i" "clojure.lang.ArraySeq$ArraySeq_boolean" int))) 
                                      (62 (istore_2)) 
                                      (63 (iload_2)) ;;at TAG_6
                                      (64 (aload_0)) 
                                      (65 (getfield (fieldCP "array" "clojure.lang.ArraySeq$ArraySeq_boolean" (array boolean)))) 
                                      (68 (arraylength)) 
                                      (69 (if_icmpge 101)) ;;to TAG_4
                                      (72 (aload_1)) 
                                      (73 (aload_0)) 
                                      (74 (getfield (fieldCP "array" "clojure.lang.ArraySeq$ArraySeq_boolean" (array boolean)))) 
                                      (77 (iload_2)) 
                                      (78 (baload)) 
                                      (79 (invokestatic (methodCP "valueOf" "java.lang.Boolean" (boolean) (class "java.lang.Boolean")))) 
                                      (82 (invokevirtual (methodCP "equals" "java.lang.Object" ((class "java.lang.Object")) boolean))) 
                                      (85 (ifeq 95)) ;;to TAG_5
                                      (88 (iload_2)) 
                                      (89 (aload_0)) 
                                      (90 (getfield (fieldCP "i" "clojure.lang.ArraySeq$ArraySeq_boolean" int))) 
                                      (93 (isub)) 
                                      (94 (ireturn)) 
                                      (95 (iinc 2 1)) ;;at TAG_5
                                      (98 (goto 63)) ;;to TAG_6
                                      (101 (iconst_m1)) ;;at TAG_4
                                      (102 (ireturn)) 
                                      (endofcode 103))
                                   (Exceptions )
                                   (StackMap )))
                        (method "lastIndexOf"
                              (parameters (class "java.lang.Object"))
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 4) (code_length . 107)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (instanceof (class "java.lang.Boolean"))) 
                                      (4 (ifeq 54)) ;;to TAG_0
                                      (7 (aload_1)) 
                                      (8 (checkcast (class "java.lang.Boolean"))) 
                                      (11 (invokevirtual (methodCP "booleanValue" "java.lang.Boolean" () boolean))) 
                                      (14 (istore_2)) 
                                      (15 (aload_0)) 
                                      (16 (getfield (fieldCP "array" "clojure.lang.ArraySeq$ArraySeq_boolean" (array boolean)))) 
                                      (19 (arraylength)) 
                                      (20 (iconst_1)) 
                                      (21 (isub)) 
                                      (22 (istore_3)) 
                                      (23 (iload_3)) ;;at TAG_2
                                      (24 (aload_0)) 
                                      (25 (getfield (fieldCP "i" "clojure.lang.ArraySeq$ArraySeq_boolean" int))) 
                                      (28 (if_icmplt 54)) ;;to TAG_0
                                      (31 (iload_2)) 
                                      (32 (aload_0)) 
                                      (33 (getfield (fieldCP "array" "clojure.lang.ArraySeq$ArraySeq_boolean" (array boolean)))) 
                                      (36 (iload_3)) 
                                      (37 (baload)) 
                                      (38 (if_icmpne 48)) ;;to TAG_1
                                      (41 (iload_3)) 
                                      (42 (aload_0)) 
                                      (43 (getfield (fieldCP "i" "clojure.lang.ArraySeq$ArraySeq_boolean" int))) 
                                      (46 (isub)) 
                                      (47 (ireturn)) 
                                      (48 (iinc 3 -1)) ;;at TAG_1
                                      (51 (goto 23))  ;;to TAG_2
                                      (54 (aload_1)) ;;at TAG_0
                                      (55 (ifnonnull 60)) ;;to TAG_3
                                      (58 (iconst_m1)) 
                                      (59 (ireturn)) 
                                      (60 (aload_0)) ;;at TAG_3
                                      (61 (getfield (fieldCP "array" "clojure.lang.ArraySeq$ArraySeq_boolean" (array boolean)))) 
                                      (64 (arraylength)) 
                                      (65 (iconst_1)) 
                                      (66 (isub)) 
                                      (67 (istore_2)) 
                                      (68 (iload_2)) ;;at TAG_6
                                      (69 (aload_0)) 
                                      (70 (getfield (fieldCP "i" "clojure.lang.ArraySeq$ArraySeq_boolean" int))) 
                                      (73 (if_icmplt 105)) ;;to TAG_4
                                      (76 (aload_1)) 
                                      (77 (aload_0)) 
                                      (78 (getfield (fieldCP "array" "clojure.lang.ArraySeq$ArraySeq_boolean" (array boolean)))) 
                                      (81 (iload_2)) 
                                      (82 (baload)) 
                                      (83 (invokestatic (methodCP "valueOf" "java.lang.Boolean" (boolean) (class "java.lang.Boolean")))) 
                                      (86 (invokevirtual (methodCP "equals" "java.lang.Object" ((class "java.lang.Object")) boolean))) 
                                      (89 (ifeq 99)) ;;to TAG_5
                                      (92 (iload_2)) 
                                      (93 (aload_0)) 
                                      (94 (getfield (fieldCP "i" "clojure.lang.ArraySeq$ArraySeq_boolean" int))) 
                                      (97 (isub)) 
                                      (98 (ireturn)) 
                                      (99 (iinc 2 -1)) ;;at TAG_5
                                      (102 (goto 68)) ;;to TAG_6
                                      (105 (iconst_m1)) ;;at TAG_4
                                      (106 (ireturn)) 
                                      (endofcode 107))
                                   (Exceptions )
                                   (StackMap )))
                        (method "withMeta"
                              (parameters (class "clojure.lang.IPersistentMap"))
                              (returntype . (class "clojure.lang.Obj"))
                              (accessflags  *class*  *public*  *volatile* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 6)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (invokevirtual
					(methodCP "withMeta" "clojure.lang.ArraySeq$ArraySeq_boolean" ((class "clojure.lang.IPersistentMap")) (class "clojure.lang.ArraySeq$ArraySeq_boolean"))))
                                      (5 (areturn))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap )))
                        (method "withMeta"
                              (parameters (class "clojure.lang.IPersistentMap"))
                              (returntype . (class "clojure.lang.IObj"))
                              (accessflags  *class*  *public*  *volatile* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 6)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (invokevirtual
					(methodCP "withMeta" "clojure.lang.ArraySeq$ArraySeq_boolean" ((class "clojure.lang.IPersistentMap")) (class "clojure.lang.ArraySeq$ArraySeq_boolean"))))
                                      (5 (areturn))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "clojure.lang.IndexedSeq" "clojure.lang.IReduce")
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *ArraySeq$ArraySeq_boolean-class-table*
  (make-static-class-decls 
   *clojure.lang.ArraySeq$ArraySeq_boolean*))

(defconst *package-name-map* 
  ("clojure.lang.ArraySeq$ArraySeq_boolean" . "clojure.lang"))

