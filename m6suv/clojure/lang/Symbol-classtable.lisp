; Symbol-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:53 CDT 2014.
;

(defconst *clojure.lang.Symbol*
 (make-class-def
      '(class "clojure.lang.Symbol"
            "clojure.lang.AFn"
            (constant_pool
                        (STRING  "/"))
            (fields
                        (field "ns" (class "java.lang.String") (accessflags  *class*  *final* ) -1)
                        (field "name" (class "java.lang.String") (accessflags  *class*  *final* ) -1)
                        (field "hash" int (accessflags  *class*  *final* ) -1)
                        (field "_meta" (class "clojure.lang.IPersistentMap") (accessflags  *class*  *final* ) -1)
                        (field "_str" (class "java.lang.String") (accessflags  *class* ) -1))
            (methods
                        (method "toString"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 1) (code_length . 66)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "_str" "clojure.lang.Symbol" (class "java.lang.String")))) 
                                      (4 (ifnonnull 61))  ;;to TAG_0
                                      (7 (aload_0)) 
                                      (8 (getfield (fieldCP "ns" "clojure.lang.Symbol" (class "java.lang.String")))) 
                                      (11 (ifnull 53)) ;;to TAG_1
                                      (14 (aload_0)) 
                                      (15 (new (class "java.lang.StringBuilder"))) 
                                      (18 (dup)) 
                                      (19 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (22 (aload_0)) 
                                      (23 (getfield (fieldCP "ns" "clojure.lang.Symbol" (class "java.lang.String")))) 
                                      (26 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (29 (ldc 0)) ;;STRING:: "/"
                                      (31 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (34 (aload_0)) 
                                      (35 (getfield (fieldCP "name" "clojure.lang.Symbol" (class "java.lang.String")))) 
                                      (38 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (41 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (44 (invokevirtual (methodCP "intern" "java.lang.String" () (class "java.lang.String")))) 
                                      (47 (putfield (fieldCP "_str" "clojure.lang.Symbol" (class "java.lang.String")))) 
                                      (50 (goto 61))  ;;to TAG_0
                                      (53 (aload_0)) ;;at TAG_1
                                      (54 (aload_0)) 
                                      (55 (getfield (fieldCP "name" "clojure.lang.Symbol" (class "java.lang.String")))) 
                                      (58 (putfield (fieldCP "_str" "clojure.lang.Symbol" (class "java.lang.String")))) 
                                      (61 (aload_0)) ;;at TAG_0
                                      (62 (getfield (fieldCP "_str" "clojure.lang.Symbol" (class "java.lang.String")))) 
                                      (65 (areturn)) 
                                      (endofcode 66))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getNamespace"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "ns" "clojure.lang.Symbol" (class "java.lang.String"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getName"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "name" "clojure.lang.Symbol" (class "java.lang.String"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "create"
                              (parameters (class "java.lang.String") (class "java.lang.String"))
                              (returntype . (class "clojure.lang.Symbol"))
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 6)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (invokestatic
					(methodCP "intern" "clojure.lang.Symbol" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Symbol"))))
                                      (5 (areturn))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap )))
                        (method "create"
                              (parameters (class "java.lang.String"))
                              (returntype . (class "clojure.lang.Symbol"))
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokestatic
					(methodCP "intern" "clojure.lang.Symbol" ((class "java.lang.String")) (class "clojure.lang.Symbol"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "intern"
                              (parameters (class "java.lang.String") (class "java.lang.String"))
                              (returntype . (class "clojure.lang.Symbol"))
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 4) (max_locals . 2) (code_length . 24)
                                   (parsedcode
                                      (0 (new (class "clojure.lang.Symbol"))) 
                                      (3 (dup)) 
                                      (4 (aload_0)) 
                                      (5 (ifnonnull 12))  ;;to TAG_0
                                      (8 (aconst_null)) 
                                      (9 (goto 16)) ;;to TAG_1
                                      (12 (aload_0)) ;;at TAG_0
                                      (13 (invokevirtual (methodCP "intern" "java.lang.String" () (class "java.lang.String")))) 
                                      (16 (aload_1)) ;;at TAG_1
                                      (17 (invokevirtual (methodCP "intern" "java.lang.String" () (class "java.lang.String")))) 
                                      (20 (invokespecial (methodCP "<init>" "clojure.lang.Symbol" ((class "java.lang.String") (class "java.lang.String")) void))) 
                                      (23 (areturn)) 
                                      (endofcode 24))
                                   (Exceptions )
                                   (StackMap )))
                        (method "intern"
                              (parameters (class "java.lang.String"))
                              (returntype . (class "clojure.lang.Symbol"))
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 6) (max_locals . 2) (code_length . 61)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (bipush 47)) 
                                      (3 (invokevirtual (methodCP "lastIndexOf" "java.lang.String" (int) int))) 
                                      (6 (istore_1)) 
                                      (7 (iload_1)) 
                                      (8 (iconst_m1)) 
                                      (9 (if_icmpeq 21))  ;;to TAG_0
                                      (12 (aload_0)) 
                                      (13 (ldc 0)) ;;STRING:: "/"
                                      (15 (invokevirtual (methodCP "equals" "java.lang.String" ((class "java.lang.Object")) boolean))) 
                                      (18 (ifeq 34)) ;;to TAG_1
                                      (21 (new (class "clojure.lang.Symbol"))) ;;at TAG_0
                                      (24 (dup)) 
                                      (25 (aconst_null)) 
                                      (26 (aload_0)) 
                                      (27 (invokevirtual (methodCP "intern" "java.lang.String" () (class "java.lang.String")))) 
                                      (30 (invokespecial (methodCP "<init>" "clojure.lang.Symbol" ((class "java.lang.String") (class "java.lang.String")) void))) 
                                      (33 (areturn)) 
                                      (34 (new (class "clojure.lang.Symbol"))) ;;at TAG_1
                                      (37 (dup)) 
                                      (38 (aload_0)) 
                                      (39 (iconst_0)) 
                                      (40 (iload_1)) 
                                      (41 (invokevirtual (methodCP "substring" "java.lang.String" (int int) (class "java.lang.String")))) 
                                      (44 (invokevirtual (methodCP "intern" "java.lang.String" () (class "java.lang.String")))) 
                                      (47 (aload_0)) 
                                      (48 (iload_1)) 
                                      (49 (iconst_1)) 
                                      (50 (iadd)) 
                                      (51 (invokevirtual (methodCP "substring" "java.lang.String" (int) (class "java.lang.String")))) 
                                      (54 (invokevirtual (methodCP "intern" "java.lang.String" () (class "java.lang.String")))) 
                                      (57 (invokespecial (methodCP "<init>" "clojure.lang.Symbol" ((class "java.lang.String") (class "java.lang.String")) void))) 
                                      (60 (areturn)) 
                                      (endofcode 61))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.lang.String") (class "java.lang.String"))
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 41)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "clojure.lang.AFn" () void)))
                                      (4 (aload_0))
                                      (5 (aload_2))
                                      (6 (putfield (fieldCP "name" "clojure.lang.Symbol" (class "java.lang.String"))))
                                      (9 (aload_0))
                                      (10 (aload_1))
                                      (11 (putfield (fieldCP "ns" "clojure.lang.Symbol" (class "java.lang.String"))))
                                      (14 (aload_0))
                                      (15 (aload_0))
                                      (16 (getfield (fieldCP "name" "clojure.lang.Symbol" (class "java.lang.String"))))
                                      (19 (invokevirtual
					(methodCP "hashCode" "java.lang.String" () int)))
                                      (22 (aload_0))
                                      (23 (getfield (fieldCP "ns" "clojure.lang.Symbol" (class "java.lang.String"))))
                                      (26 (invokestatic
					(methodCP "hash" "clojure.lang.Util" ((class "java.lang.Object")) int)))
                                      (29 (invokestatic
					(methodCP "hashCombine" "clojure.lang.Util" (int int) int)))
                                      (32 (putfield (fieldCP "hash" "clojure.lang.Symbol" int)))
                                      (35 (aload_0))
                                      (36 (aconst_null))
                                      (37 (putfield (fieldCP "_meta" "clojure.lang.Symbol" (class "clojure.lang.IPersistentMap"))))
                                      (40 (return))
                                      (endofcode 41))
                                   (Exceptions )
                                   (StackMap )))
                        (method "equals"
                              (parameters (class "java.lang.Object"))
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 49)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (aload_1)) 
                                      (2 (if_acmpne 7)) ;;to TAG_0
                                      (5 (iconst_1)) 
                                      (6 (ireturn)) 
                                      (7 (aload_1)) ;;at TAG_0
                                      (8 (instanceof (class "clojure.lang.Symbol"))) 
                                      (11 (ifne 16)) ;;to TAG_1
                                      (14 (iconst_0)) 
                                      (15 (ireturn)) 
                                      (16 (aload_1)) ;;at TAG_1
                                      (17 (checkcast (class "clojure.lang.Symbol"))) 
                                      (20 (astore_2)) 
                                      (21 (aload_0)) 
                                      (22 (getfield (fieldCP "name" "clojure.lang.Symbol" (class "java.lang.String")))) 
                                      (25 (aload_2)) 
                                      (26 (getfield (fieldCP "name" "clojure.lang.Symbol" (class "java.lang.String")))) 
                                      (29 (if_acmpne 47))  ;;to TAG_2
                                      (32 (aload_0)) 
                                      (33 (getfield (fieldCP "ns" "clojure.lang.Symbol" (class "java.lang.String")))) 
                                      (36 (aload_2)) 
                                      (37 (getfield (fieldCP "ns" "clojure.lang.Symbol" (class "java.lang.String")))) 
                                      (40 (if_acmpne 47))  ;;to TAG_2
                                      (43 (iconst_1)) 
                                      (44 (goto 48)) ;;to TAG_3
                                      (47 (iconst_0)) ;;at TAG_2
                                      (48 (ireturn)) ;;at TAG_3
                                      (endofcode 49))
                                   (Exceptions )
                                   (StackMap )))
                        (method "hashCode"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "hash" "clojure.lang.Symbol" int)))
                                      (4 (ireturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "hasheq"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "hash" "clojure.lang.Symbol" int)))
                                      (4 (ireturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "withMeta"
                              (parameters (class "clojure.lang.IPersistentMap"))
                              (returntype . (class "clojure.lang.IObj"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 2) (code_length . 17)
                                   (parsedcode
                                      (0 (new (class "clojure.lang.Symbol")))
                                      (3 (dup))
                                      (4 (aload_1))
                                      (5 (aload_0))
                                      (6 (getfield (fieldCP "ns" "clojure.lang.Symbol" (class "java.lang.String"))))
                                      (9 (aload_0))
                                      (10 (getfield (fieldCP "name" "clojure.lang.Symbol" (class "java.lang.String"))))
                                      (13 (invokespecial
					(methodCP "<init>" "clojure.lang.Symbol" ((class "clojure.lang.IPersistentMap") (class "java.lang.String") (class "java.lang.String")) void)))
                                      (16 (areturn))
                                      (endofcode 17))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "clojure.lang.IPersistentMap") (class "java.lang.String") (class "java.lang.String"))
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 3) (max_locals . 4) (code_length . 35)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "clojure.lang.AFn" () void)))
                                      (4 (aload_0))
                                      (5 (aload_3))
                                      (6 (putfield (fieldCP "name" "clojure.lang.Symbol" (class "java.lang.String"))))
                                      (9 (aload_0))
                                      (10 (aload_2))
                                      (11 (putfield (fieldCP "ns" "clojure.lang.Symbol" (class "java.lang.String"))))
                                      (14 (aload_0))
                                      (15 (aload_1))
                                      (16 (putfield (fieldCP "_meta" "clojure.lang.Symbol" (class "clojure.lang.IPersistentMap"))))
                                      (19 (aload_0))
                                      (20 (aload_3))
                                      (21 (invokevirtual
					(methodCP "hashCode" "java.lang.String" () int)))
                                      (24 (aload_2))
                                      (25 (invokestatic
					(methodCP "hash" "clojure.lang.Util" ((class "java.lang.Object")) int)))
                                      (28 (invokestatic
					(methodCP "hashCombine" "clojure.lang.Util" (int int) int)))
                                      (31 (putfield (fieldCP "hash" "clojure.lang.Symbol" int)))
                                      (34 (return))
                                      (endofcode 35))
                                   (Exceptions )
                                   (StackMap )))
                        (method "compareTo"
                              (parameters (class "java.lang.Object"))
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 4) (code_length . 77)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (checkcast (class "clojure.lang.Symbol"))) 
                                      (4 (astore_2)) 
                                      (5 (aload_0)) 
                                      (6 (aload_1)) 
                                      (7 (invokevirtual (methodCP "equals" "clojure.lang.Symbol" ((class "java.lang.Object")) boolean))) 
                                      (10 (ifeq 15)) ;;to TAG_0
                                      (13 (iconst_0)) 
                                      (14 (ireturn)) 
                                      (15 (aload_0)) ;;at TAG_0
                                      (16 (getfield (fieldCP "ns" "clojure.lang.Symbol" (class "java.lang.String")))) 
                                      (19 (ifnonnull 31)) ;;to TAG_1
                                      (22 (aload_2)) 
                                      (23 (getfield (fieldCP "ns" "clojure.lang.Symbol" (class "java.lang.String")))) 
                                      (26 (ifnull 31)) ;;to TAG_1
                                      (29 (iconst_m1)) 
                                      (30 (ireturn)) 
                                      (31 (aload_0)) ;;at TAG_1
                                      (32 (getfield (fieldCP "ns" "clojure.lang.Symbol" (class "java.lang.String")))) 
                                      (35 (ifnull 65))  ;;to TAG_2
                                      (38 (aload_2)) 
                                      (39 (getfield (fieldCP "ns" "clojure.lang.Symbol" (class "java.lang.String")))) 
                                      (42 (ifnonnull 47)) ;;to TAG_3
                                      (45 (iconst_1)) 
                                      (46 (ireturn)) 
                                      (47 (aload_0)) ;;at TAG_3
                                      (48 (getfield (fieldCP "ns" "clojure.lang.Symbol" (class "java.lang.String")))) 
                                      (51 (aload_2)) 
                                      (52 (getfield (fieldCP "ns" "clojure.lang.Symbol" (class "java.lang.String")))) 
                                      (55 (invokevirtual (methodCP "compareTo" "java.lang.String" ((class "java.lang.String")) int))) 
                                      (58 (istore_3)) 
                                      (59 (iload_3)) 
                                      (60 (ifeq 65))  ;;to TAG_2
                                      (63 (iload_3)) 
                                      (64 (ireturn)) 
                                      (65 (aload_0)) ;;at TAG_2
                                      (66 (getfield (fieldCP "name" "clojure.lang.Symbol" (class "java.lang.String")))) 
                                      (69 (aload_2)) 
                                      (70 (getfield (fieldCP "name" "clojure.lang.Symbol" (class "java.lang.String")))) 
                                      (73 (invokevirtual (methodCP "compareTo" "java.lang.String" ((class "java.lang.String")) int))) 
                                      (76 (ireturn)) 
                                      (endofcode 77))
                                   (Exceptions )
                                   (StackMap )))
                        (method "readResolve"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 12)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "ns" "clojure.lang.Symbol" (class "java.lang.String"))))
                                      (4 (aload_0))
                                      (5 (getfield (fieldCP "name" "clojure.lang.Symbol" (class "java.lang.String"))))
                                      (8 (invokestatic
					(methodCP "intern" "clojure.lang.Symbol" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Symbol"))))
                                      (11 (areturn))
                                      (endofcode 12))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 6)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (aload_0))
                                      (2 (invokestatic
					(methodCP "get" "clojure.lang.RT" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object"))))
                                      (5 (areturn))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 7)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (aload_0))
                                      (2 (aload_2))
                                      (3 (invokestatic
					(methodCP "get" "clojure.lang.RT" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object"))))
                                      (6 (areturn))
                                      (endofcode 7))
                                   (Exceptions )
                                   (StackMap )))
                        (method "meta"
                              (parameters )
                              (returntype . (class "clojure.lang.IPersistentMap"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "_meta" "clojure.lang.Symbol" (class "clojure.lang.IPersistentMap"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "clojure.lang.IObj" "java.lang.Comparable" "clojure.lang.Named" "java.io.Serializable" "clojure.lang.IHashEq")
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *Symbol-class-table*
  (make-static-class-decls 
   *clojure.lang.Symbol*))

(defconst *package-name-map* 
  ("clojure.lang.Symbol" . "clojure.lang"))

