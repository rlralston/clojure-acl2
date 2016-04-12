; PersistentList$1-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:52 CDT 2014.
;

(defconst *clojure.lang.PersistentList$1*
 (make-class-def
      '(class "clojure.lang.PersistentList$1"
            "clojure.lang.RestFn"
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
					(methodCP "<init>" "clojure.lang.RestFn" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getRequiredArity"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *final*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 2)
                                   (parsedcode
                                      (0 (iconst_0))
                                      (1 (ireturn))
                                      (endofcode 2))
                                   (Exceptions )
                                   (StackMap )))
                        (method "doInvoke"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *final*  *protected* )
                              (code
                                   (max_stack . 3) (max_locals . 5) (code_length . 101)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (instanceof (class "clojure.lang.ArraySeq"))) 
                                      (4 (ifeq 58)) ;;to TAG_0
                                      (7 (aload_1)) 
                                      (8 (checkcast (class "clojure.lang.ArraySeq"))) 
                                      (11 (getfield (fieldCP "array" "clojure.lang.ArraySeq" (class "java.lang.Object")))) 
                                      (14 (checkcast (array (class "java.lang.Object")))) 
                                      (17 (checkcast (array (class "java.lang.Object")))) 
                                      (20 (astore_2)) 
                                      (21 (getstatic (fieldCP "EMPTY" "clojure.lang.PersistentList" (class "clojure.lang.PersistentList$EmptyList")))) 
                                      (24 (astore_3)) 
                                      (25 (aload_2)) 
                                      (26 (arraylength)) 
                                      (27 (iconst_1)) 
                                      (28 (isub)) 
                                      (29 (istore 4)) 
                                      (31 (iload 4)) ;;at TAG_2
                                      (33 (iflt 56)) ;;to TAG_1
                                      (36 (aload_3)) 
                                      (37 (aload_2)) 
                                      (38 (iload 4)) 
                                      (40 (aaload)) 
                                      (41 (invokeinterface (methodCP "cons" "clojure.lang.IPersistentList" ((class "java.lang.Object")) (class "clojure.lang.IPersistentCollection")) 2)) 
                                      (46 (checkcast (class "clojure.lang.IPersistentList"))) 
                                      (49 (astore_3)) 
                                      (50 (iinc 4 -1)) 
                                      (53 (goto 31))  ;;to TAG_2
                                      (56 (aload_3)) ;;at TAG_1
                                      (57 (areturn)) 
                                      (58 (new (class "java.util.LinkedList"))) ;;at TAG_0
                                      (61 (dup)) 
                                      (62 (invokespecial (methodCP "<init>" "java.util.LinkedList" () void))) 
                                      (65 (astore_2)) 
                                      (66 (aload_1)) 
                                      (67 (invokestatic (methodCP "seq" "clojure.lang.RT" ((class "java.lang.Object")) (class "clojure.lang.ISeq")))) 
                                      (70 (astore_3)) 
                                      (71 (aload_3)) ;;at TAG_4
                                      (72 (ifnull 96)) ;;to TAG_3
                                      (75 (aload_2)) 
                                      (76 (aload_3)) 
                                      (77 (invokeinterface (methodCP "first" "clojure.lang.ISeq" () (class "java.lang.Object")) 1)) 
                                      (82 (invokevirtual (methodCP "add" "java.util.LinkedList" ((class "java.lang.Object")) boolean))) 
                                      (85 (pop)) 
                                      (86 (aload_3)) 
                                      (87 (invokeinterface (methodCP "next" "clojure.lang.ISeq" () (class "clojure.lang.ISeq")) 1)) 
                                      (92 (astore_3)) 
                                      (93 (goto 71)) ;;to TAG_4
                                      (96 (aload_2)) ;;at TAG_3
                                      (97 (invokestatic (methodCP "create" "clojure.lang.PersistentList" ((class "java.util.List")) (class "clojure.lang.IPersistentList")))) 
                                      (100 (areturn)) 
                                      (endofcode 101))
                                   (Exceptions )
                                   (StackMap )))
                        (method "withMeta"
                              (parameters (class "clojure.lang.IPersistentMap"))
                              (returntype . (class "clojure.lang.IObj"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 8)
                                   (parsedcode
                                      (0 (new (class "java.lang.UnsupportedOperationException")))
                                      (3 (dup))
                                      (4 (invokespecial
					(methodCP "<init>" "java.lang.UnsupportedOperationException" () void)))
                                      (7 (athrow))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "meta"
                              (parameters )
                              (returntype . (class "clojure.lang.IPersistentMap"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 2)
                                   (parsedcode
                                      (0 (aconst_null))
                                      (1 (areturn))
                                      (endofcode 2))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "EnclosingMethod")
              (attribute "InnerClasses")))))


(defconst *PersistentList$1-class-table*
  (make-static-class-decls 
   *clojure.lang.PersistentList$1*))

(defconst *package-name-map* 
  ("clojure.lang.PersistentList$1" . "clojure.lang"))
