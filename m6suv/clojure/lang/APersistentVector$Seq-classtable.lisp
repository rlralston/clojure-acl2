; APersistentVector$Seq-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:50 CDT 2014.
;

(defconst *clojure.lang.APersistentVector$Seq*
 (make-class-def
      '(class "clojure.lang.APersistentVector$Seq"
            "clojure.lang.ASeq"
            (constant_pool)
            (fields
                        (field "v" (class "clojure.lang.IPersistentVector") (accessflags  *class*  *final* ) -1)
                        (field "i" int (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "clojure.lang.IPersistentVector") int)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 15)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "clojure.lang.ASeq" () void)))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (putfield (fieldCP "v" "clojure.lang.APersistentVector$Seq" (class "clojure.lang.IPersistentVector"))))
                                      (9 (aload_0))
                                      (10 (iload_2))
                                      (11 (putfield (fieldCP "i" "clojure.lang.APersistentVector$Seq" int)))
                                      (14 (return))
                                      (endofcode 15))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "clojure.lang.IPersistentMap") (class "clojure.lang.IPersistentVector") int)
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
                                      (7 (putfield (fieldCP "v" "clojure.lang.APersistentVector$Seq" (class "clojure.lang.IPersistentVector"))))
                                      (10 (aload_0))
                                      (11 (iload_3))
                                      (12 (putfield (fieldCP "i" "clojure.lang.APersistentVector$Seq" int)))
                                      (15 (return))
                                      (endofcode 16))
                                   (Exceptions )
                                   (StackMap )))
                        (method "first"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 14)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "v" "clojure.lang.APersistentVector$Seq" (class "clojure.lang.IPersistentVector"))))
                                      (4 (aload_0))
                                      (5 (getfield (fieldCP "i" "clojure.lang.APersistentVector$Seq" int)))
                                      (8 (invokeinterface
					(methodCP "nth" "clojure.lang.IPersistentVector" (int) (class "java.lang.Object")) 2))
                                      (13 (areturn))
                                      (endofcode 14))
                                   (Exceptions )
                                   (StackMap )))
                        (method "next"
                              (parameters )
                              (returntype . (class "clojure.lang.ISeq"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 1) (code_length . 38)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "i" "clojure.lang.APersistentVector$Seq" int))) 
                                      (4 (iconst_1)) 
                                      (5 (iadd)) 
                                      (6 (aload_0)) 
                                      (7 (getfield (fieldCP "v" "clojure.lang.APersistentVector$Seq" (class "clojure.lang.IPersistentVector")))) 
                                      (10 (invokeinterface (methodCP "count" "clojure.lang.IPersistentVector" () int) 1)) 
                                      (15 (if_icmpge 36))  ;;to TAG_0
                                      (18 (new (class "clojure.lang.APersistentVector$Seq"))) 
                                      (21 (dup)) 
                                      (22 (aload_0)) 
                                      (23 (getfield (fieldCP "v" "clojure.lang.APersistentVector$Seq" (class "clojure.lang.IPersistentVector")))) 
                                      (26 (aload_0)) 
                                      (27 (getfield (fieldCP "i" "clojure.lang.APersistentVector$Seq" int))) 
                                      (30 (iconst_1)) 
                                      (31 (iadd)) 
                                      (32 (invokespecial (methodCP "<init>" "clojure.lang.APersistentVector$Seq" ((class "clojure.lang.IPersistentVector") int) void))) 
                                      (35 (areturn)) 
                                      (36 (aconst_null)) ;;at TAG_0
                                      (37 (areturn)) 
                                      (endofcode 38))
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
                                      (1 (getfield (fieldCP "i" "clojure.lang.APersistentVector$Seq" int)))
                                      (4 (ireturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "count"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 15)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "v" "clojure.lang.APersistentVector$Seq" (class "clojure.lang.IPersistentVector"))))
                                      (4 (invokeinterface
					(methodCP "count" "clojure.lang.IPersistentVector" () int) 1))
                                      (9 (aload_0))
                                      (10 (getfield (fieldCP "i" "clojure.lang.APersistentVector$Seq" int)))
                                      (13 (isub))
                                      (14 (ireturn))
                                      (endofcode 15))
                                   (Exceptions )
                                   (StackMap )))
                        (method "withMeta"
                              (parameters (class "clojure.lang.IPersistentMap"))
                              (returntype . (class "clojure.lang.APersistentVector$Seq"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 2) (code_length . 17)
                                   (parsedcode
                                      (0 (new (class "clojure.lang.APersistentVector$Seq")))
                                      (3 (dup))
                                      (4 (aload_1))
                                      (5 (aload_0))
                                      (6 (getfield (fieldCP "v" "clojure.lang.APersistentVector$Seq" (class "clojure.lang.IPersistentVector"))))
                                      (9 (aload_0))
                                      (10 (getfield (fieldCP "i" "clojure.lang.APersistentVector$Seq" int)))
                                      (13 (invokespecial
					(methodCP "<init>" "clojure.lang.APersistentVector$Seq" ((class "clojure.lang.IPersistentMap") (class "clojure.lang.IPersistentVector") int) void)))
                                      (16 (areturn))
                                      (endofcode 17))
                                   (Exceptions )
                                   (StackMap )))
                        (method "reduce"
                              (parameters (class "clojure.lang.IFn"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 4) (code_length . 60)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "v" "clojure.lang.APersistentVector$Seq" (class "clojure.lang.IPersistentVector")))) 
                                      (4 (aload_0)) 
                                      (5 (getfield (fieldCP "i" "clojure.lang.APersistentVector$Seq" int))) 
                                      (8 (invokeinterface (methodCP "nth" "clojure.lang.IPersistentVector" (int) (class "java.lang.Object")) 2)) 
                                      (13 (astore_2)) 
                                      (14 (aload_0)) 
                                      (15 (getfield (fieldCP "i" "clojure.lang.APersistentVector$Seq" int))) 
                                      (18 (iconst_1)) 
                                      (19 (iadd)) 
                                      (20 (istore_3)) 
                                      (21 (iload_3)) ;;at TAG_1
                                      (22 (aload_0)) 
                                      (23 (getfield (fieldCP "v" "clojure.lang.APersistentVector$Seq" (class "clojure.lang.IPersistentVector")))) 
                                      (26 (invokeinterface (methodCP "count" "clojure.lang.IPersistentVector" () int) 1)) 
                                      (31 (if_icmpge 58))  ;;to TAG_0
                                      (34 (aload_1)) 
                                      (35 (aload_2)) 
                                      (36 (aload_0)) 
                                      (37 (getfield (fieldCP "v" "clojure.lang.APersistentVector$Seq" (class "clojure.lang.IPersistentVector")))) 
                                      (40 (iload_3)) 
                                      (41 (invokeinterface (methodCP "nth" "clojure.lang.IPersistentVector" (int) (class "java.lang.Object")) 2)) 
                                      (46 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (51 (astore_2)) 
                                      (52 (iinc 3 1)) 
                                      (55 (goto 21)) ;;to TAG_1
                                      (58 (aload_2)) ;;at TAG_0
                                      (59 (areturn)) 
                                      (endofcode 60))
                                   (Exceptions )
                                   (StackMap )))
                        (method "reduce"
                              (parameters (class "clojure.lang.IFn") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 5) (code_length . 70)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (aload_2)) 
                                      (2 (aload_0)) 
                                      (3 (getfield (fieldCP "v" "clojure.lang.APersistentVector$Seq" (class "clojure.lang.IPersistentVector")))) 
                                      (6 (aload_0)) 
                                      (7 (getfield (fieldCP "i" "clojure.lang.APersistentVector$Seq" int))) 
                                      (10 (invokeinterface (methodCP "nth" "clojure.lang.IPersistentVector" (int) (class "java.lang.Object")) 2)) 
                                      (15 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (20 (astore_3)) 
                                      (21 (aload_0)) 
                                      (22 (getfield (fieldCP "i" "clojure.lang.APersistentVector$Seq" int))) 
                                      (25 (iconst_1)) 
                                      (26 (iadd)) 
                                      (27 (istore 4)) 
                                      (29 (iload 4)) ;;at TAG_1
                                      (31 (aload_0)) 
                                      (32 (getfield (fieldCP "v" "clojure.lang.APersistentVector$Seq" (class "clojure.lang.IPersistentVector")))) 
                                      (35 (invokeinterface (methodCP "count" "clojure.lang.IPersistentVector" () int) 1)) 
                                      (40 (if_icmpge 68))  ;;to TAG_0
                                      (43 (aload_1)) 
                                      (44 (aload_3)) 
                                      (45 (aload_0)) 
                                      (46 (getfield (fieldCP "v" "clojure.lang.APersistentVector$Seq" (class "clojure.lang.IPersistentVector")))) 
                                      (49 (iload 4)) 
                                      (51 (invokeinterface (methodCP "nth" "clojure.lang.IPersistentVector" (int) (class "java.lang.Object")) 2)) 
                                      (56 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (61 (astore_3)) 
                                      (62 (iinc 4 1)) 
                                      (65 (goto 29)) ;;to TAG_1
                                      (68 (aload_3)) ;;at TAG_0
                                      (69 (areturn)) 
                                      (endofcode 70))
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
					(methodCP "withMeta" "clojure.lang.APersistentVector$Seq" ((class "clojure.lang.IPersistentMap")) (class "clojure.lang.APersistentVector$Seq"))))
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
					(methodCP "withMeta" "clojure.lang.APersistentVector$Seq" ((class "clojure.lang.IPersistentMap")) (class "clojure.lang.APersistentVector$Seq"))))
                                      (5 (areturn))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "clojure.lang.IndexedSeq" "clojure.lang.IReduce")
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *APersistentVector$Seq-class-table*
  (make-static-class-decls 
   *clojure.lang.APersistentVector$Seq*))

(defconst *package-name-map* 
  ("clojure.lang.APersistentVector$Seq" . "clojure.lang"))
