; EnumerationSeq-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:51 CDT 2014.
;

(defconst *clojure.lang.EnumerationSeq*
 (make-class-def
      '(class "clojure.lang.EnumerationSeq"
            "clojure.lang.ASeq"
            (constant_pool)
            (fields
                        (field "iter" (class "java.util.Enumeration") (accessflags  *class*  *final* ) -1)
                        (field "state" (class "clojure.lang.EnumerationSeq$State") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "create"
                              (parameters (class "java.util.Enumeration"))
                              (returntype . (class "clojure.lang.EnumerationSeq"))
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 3) (max_locals . 1) (code_length . 20)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokeinterface (methodCP "hasMoreElements" "java.util.Enumeration" () boolean) 1)) 
                                      (6 (ifeq 18))  ;;to TAG_0
                                      (9 (new (class "clojure.lang.EnumerationSeq"))) 
                                      (12 (dup)) 
                                      (13 (aload_0)) 
                                      (14 (invokespecial (methodCP "<init>" "clojure.lang.EnumerationSeq" ((class "java.util.Enumeration")) void))) 
                                      (17 (areturn)) 
                                      (18 (aconst_null)) ;;at TAG_0
                                      (19 (areturn)) 
                                      (endofcode 20))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.util.Enumeration"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 43)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "clojure.lang.ASeq" () void)))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (putfield (fieldCP "iter" "clojure.lang.EnumerationSeq" (class "java.util.Enumeration"))))
                                      (9 (aload_0))
                                      (10 (new (class "clojure.lang.EnumerationSeq$State")))
                                      (13 (dup))
                                      (14 (invokespecial
					(methodCP "<init>" "clojure.lang.EnumerationSeq$State" () void)))
                                      (17 (putfield (fieldCP "state" "clojure.lang.EnumerationSeq" (class "clojure.lang.EnumerationSeq$State"))))
                                      (20 (aload_0))
                                      (21 (getfield (fieldCP "state" "clojure.lang.EnumerationSeq" (class "clojure.lang.EnumerationSeq$State"))))
                                      (24 (aload_0))
                                      (25 (getfield (fieldCP "state" "clojure.lang.EnumerationSeq" (class "clojure.lang.EnumerationSeq$State"))))
                                      (28 (putfield (fieldCP "val" "clojure.lang.EnumerationSeq$State" (class "java.lang.Object"))))
                                      (31 (aload_0))
                                      (32 (getfield (fieldCP "state" "clojure.lang.EnumerationSeq" (class "clojure.lang.EnumerationSeq$State"))))
                                      (35 (aload_0))
                                      (36 (getfield (fieldCP "state" "clojure.lang.EnumerationSeq" (class "clojure.lang.EnumerationSeq$State"))))
                                      (39 (putfield (fieldCP "_rest" "clojure.lang.EnumerationSeq$State" (class "java.lang.Object"))))
                                      (42 (return))
                                      (endofcode 43))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "clojure.lang.IPersistentMap") (class "java.util.Enumeration") (class "clojure.lang.EnumerationSeq$State"))
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
                                      (7 (putfield (fieldCP "iter" "clojure.lang.EnumerationSeq" (class "java.util.Enumeration"))))
                                      (10 (aload_0))
                                      (11 (aload_3))
                                      (12 (putfield (fieldCP "state" "clojure.lang.EnumerationSeq" (class "clojure.lang.EnumerationSeq$State"))))
                                      (15 (return))
                                      (endofcode 16))
                                   (Exceptions )
                                   (StackMap )))
                        (method "first"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 69)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "state" "clojure.lang.EnumerationSeq" (class "clojure.lang.EnumerationSeq$State")))) 
                                      (4 (getfield (fieldCP "val" "clojure.lang.EnumerationSeq$State" (class "java.lang.Object")))) 
                                      (7 (aload_0)) 
                                      (8 (getfield (fieldCP "state" "clojure.lang.EnumerationSeq" (class "clojure.lang.EnumerationSeq$State")))) 
                                      (11 (if_acmpne 61)) ;;to TAG_0
                                      (14 (aload_0)) 
                                      (15 (getfield (fieldCP "state" "clojure.lang.EnumerationSeq" (class "clojure.lang.EnumerationSeq$State")))) 
                                      (18 (dup)) 
                                      (19 (astore_1)) 
                                      (20 (monitorenter)) 
                                      (21 (aload_0)) ;;at TAG_2
                                      (22 (getfield (fieldCP "state" "clojure.lang.EnumerationSeq" (class "clojure.lang.EnumerationSeq$State")))) 
                                      (25 (getfield (fieldCP "val" "clojure.lang.EnumerationSeq$State" (class "java.lang.Object")))) 
                                      (28 (aload_0)) 
                                      (29 (getfield (fieldCP "state" "clojure.lang.EnumerationSeq" (class "clojure.lang.EnumerationSeq$State")))) 
                                      (32 (if_acmpne 51)) ;;to TAG_1
                                      (35 (aload_0)) 
                                      (36 (getfield (fieldCP "state" "clojure.lang.EnumerationSeq" (class "clojure.lang.EnumerationSeq$State")))) 
                                      (39 (aload_0)) 
                                      (40 (getfield (fieldCP "iter" "clojure.lang.EnumerationSeq" (class "java.util.Enumeration")))) 
                                      (43 (invokeinterface (methodCP "nextElement" "java.util.Enumeration" () (class "java.lang.Object")) 1)) 
                                      (48 (putfield (fieldCP "val" "clojure.lang.EnumerationSeq$State" (class "java.lang.Object")))) 
                                      (51 (aload_1)) ;;at TAG_1
                                      (52 (monitorexit)) 
                                      (53 (goto 61)) ;;to TAG_0;;at TAG_3
                                      (56 (astore_2)) ;;at TAG_4
                                      (57 (aload_1)) 
                                      (58 (monitorexit)) 
                                      (59 (aload_2)) ;;at TAG_5
                                      (60 (athrow)) 
                                      (61 (aload_0)) ;;at TAG_0
                                      (62 (getfield (fieldCP "state" "clojure.lang.EnumerationSeq" (class "clojure.lang.EnumerationSeq$State")))) 
                                      (65 (getfield (fieldCP "val" "clojure.lang.EnumerationSeq$State" (class "java.lang.Object")))) 
                                      (68 (areturn)) 
                                      (endofcode 69))
                                   (Exceptions 
                                     (handler 21 53  56 (class "java.lang.Throwable"))
                                     (handler 56 59  56 (class "java.lang.Throwable")))
                                   (StackMap )))
                        (method "next"
                              (parameters )
                              (returntype . (class "clojure.lang.ISeq"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 75)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "state" "clojure.lang.EnumerationSeq" (class "clojure.lang.EnumerationSeq$State")))) 
                                      (4 (getfield (fieldCP "_rest" "clojure.lang.EnumerationSeq$State" (class "java.lang.Object")))) 
                                      (7 (aload_0)) 
                                      (8 (getfield (fieldCP "state" "clojure.lang.EnumerationSeq" (class "clojure.lang.EnumerationSeq$State")))) 
                                      (11 (if_acmpne 64)) ;;to TAG_0
                                      (14 (aload_0)) 
                                      (15 (getfield (fieldCP "state" "clojure.lang.EnumerationSeq" (class "clojure.lang.EnumerationSeq$State")))) 
                                      (18 (dup)) 
                                      (19 (astore_1)) 
                                      (20 (monitorenter)) 
                                      (21 (aload_0)) ;;at TAG_2
                                      (22 (getfield (fieldCP "state" "clojure.lang.EnumerationSeq" (class "clojure.lang.EnumerationSeq$State")))) 
                                      (25 (getfield (fieldCP "_rest" "clojure.lang.EnumerationSeq$State" (class "java.lang.Object")))) 
                                      (28 (aload_0)) 
                                      (29 (getfield (fieldCP "state" "clojure.lang.EnumerationSeq" (class "clojure.lang.EnumerationSeq$State")))) 
                                      (32 (if_acmpne 54)) ;;to TAG_1
                                      (35 (aload_0)) 
                                      (36 (invokevirtual (methodCP "first" "clojure.lang.EnumerationSeq" () (class "java.lang.Object")))) 
                                      (39 (pop)) 
                                      (40 (aload_0)) 
                                      (41 (getfield (fieldCP "state" "clojure.lang.EnumerationSeq" (class "clojure.lang.EnumerationSeq$State")))) 
                                      (44 (aload_0)) 
                                      (45 (getfield (fieldCP "iter" "clojure.lang.EnumerationSeq" (class "java.util.Enumeration")))) 
                                      (48 (invokestatic (methodCP "create" "clojure.lang.EnumerationSeq" ((class "java.util.Enumeration")) (class "clojure.lang.EnumerationSeq")))) 
                                      (51 (putfield (fieldCP "_rest" "clojure.lang.EnumerationSeq$State" (class "java.lang.Object")))) 
                                      (54 (aload_1)) ;;at TAG_1
                                      (55 (monitorexit)) 
                                      (56 (goto 64)) ;;to TAG_0;;at TAG_3
                                      (59 (astore_2)) ;;at TAG_4
                                      (60 (aload_1)) 
                                      (61 (monitorexit)) 
                                      (62 (aload_2)) ;;at TAG_5
                                      (63 (athrow)) 
                                      (64 (aload_0)) ;;at TAG_0
                                      (65 (getfield (fieldCP "state" "clojure.lang.EnumerationSeq" (class "clojure.lang.EnumerationSeq$State")))) 
                                      (68 (getfield (fieldCP "_rest" "clojure.lang.EnumerationSeq$State" (class "java.lang.Object")))) 
                                      (71 (checkcast (class "clojure.lang.ISeq"))) 
                                      (74 (areturn)) 
                                      (endofcode 75))
                                   (Exceptions 
                                     (handler 21 56  59 (class "java.lang.Throwable"))
                                     (handler 59 62  59 (class "java.lang.Throwable")))
                                   (StackMap )))
                        (method "withMeta"
                              (parameters (class "clojure.lang.IPersistentMap"))
                              (returntype . (class "clojure.lang.EnumerationSeq"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 2) (code_length . 17)
                                   (parsedcode
                                      (0 (new (class "clojure.lang.EnumerationSeq")))
                                      (3 (dup))
                                      (4 (aload_1))
                                      (5 (aload_0))
                                      (6 (getfield (fieldCP "iter" "clojure.lang.EnumerationSeq" (class "java.util.Enumeration"))))
                                      (9 (aload_0))
                                      (10 (getfield (fieldCP "state" "clojure.lang.EnumerationSeq" (class "clojure.lang.EnumerationSeq$State"))))
                                      (13 (invokespecial
					(methodCP "<init>" "clojure.lang.EnumerationSeq" ((class "clojure.lang.IPersistentMap") (class "java.util.Enumeration") (class "clojure.lang.EnumerationSeq$State")) void)))
                                      (16 (areturn))
                                      (endofcode 17))
                                   (Exceptions )
                                   (StackMap )))
                        (method "writeObject"
                              (parameters (class "java.io.ObjectOutputStream"))
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 15)
                                   (parsedcode
                                      (0 (new (class "java.io.NotSerializableException")))
                                      (3 (dup))
                                      (4 (aload_0))
                                      (5 (invokevirtual
					(methodCP "getClass" "java.lang.Object" () (class "java.lang.Class"))))
                                      (8 (invokevirtual
					(methodCP "getName" "java.lang.Class" () (class "java.lang.String"))))
                                      (11 (invokespecial
					(methodCP "<init>" "java.io.NotSerializableException" ((class "java.lang.String")) void)))
                                      (14 (athrow))
                                      (endofcode 15))
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
					(methodCP "withMeta" "clojure.lang.EnumerationSeq" ((class "clojure.lang.IPersistentMap")) (class "clojure.lang.EnumerationSeq"))))
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
					(methodCP "withMeta" "clojure.lang.EnumerationSeq" ((class "clojure.lang.IPersistentMap")) (class "clojure.lang.EnumerationSeq"))))
                                      (5 (areturn))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *EnumerationSeq-class-table*
  (make-static-class-decls 
   *clojure.lang.EnumerationSeq*))

(defconst *package-name-map* 
  ("clojure.lang.EnumerationSeq" . "clojure.lang"))
