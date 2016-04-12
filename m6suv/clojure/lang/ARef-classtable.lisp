; ARef-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:50 CDT 2014.
;

(defconst *clojure.lang.ARef*
 (make-class-def
      '(class "clojure.lang.ARef"
            "clojure.lang.AReference"
            (constant_pool
                        (STRING  "Invalid reference state"))
            (fields
                        (field "validator" (class "clojure.lang.IFn") (accessflags  *class*  *protected*  *volatile* ) -1)
                        (field "watches" (class "clojure.lang.IPersistentMap") (accessflags  *class*  *private*  *volatile* ) -1))
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 17)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "clojure.lang.AReference" () void)))
                                      (4 (aload_0))
                                      (5 (aconst_null))
                                      (6 (putfield (fieldCP "validator" "clojure.lang.ARef" (class "clojure.lang.IFn"))))
                                      (9 (aload_0))
                                      (10 (getstatic (fieldCP "EMPTY" "clojure.lang.PersistentHashMap" (class "clojure.lang.PersistentHashMap"))))
                                      (13 (putfield (fieldCP "watches" "clojure.lang.ARef" (class "clojure.lang.IPersistentMap"))))
                                      (16 (return))
                                      (endofcode 17))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "clojure.lang.IPersistentMap"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 18)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (invokespecial
					(methodCP "<init>" "clojure.lang.AReference" ((class "clojure.lang.IPersistentMap")) void)))
                                      (5 (aload_0))
                                      (6 (aconst_null))
                                      (7 (putfield (fieldCP "validator" "clojure.lang.ARef" (class "clojure.lang.IFn"))))
                                      (10 (aload_0))
                                      (11 (getstatic (fieldCP "EMPTY" "clojure.lang.PersistentHashMap" (class "clojure.lang.PersistentHashMap"))))
                                      (14 (putfield (fieldCP "watches" "clojure.lang.ARef" (class "clojure.lang.IPersistentMap"))))
                                      (17 (return))
                                      (endofcode 18))
                                   (Exceptions )
                                   (StackMap )))
                        (method "validate"
                              (parameters (class "clojure.lang.IFn") (class "java.lang.Object"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 4) (max_locals . 4) (code_length . 46)
                                   (parsedcode
                                      (0 (aload_1)) ;;at TAG_2
                                      (1 (ifnull 27)) ;;to TAG_0
                                      (4 (aload_1)) 
                                      (5 (aload_2)) 
                                      (6 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (11 (invokestatic (methodCP "booleanCast" "clojure.lang.RT" ((class "java.lang.Object")) boolean))) 
                                      (14 (ifne 27)) ;;to TAG_0
                                      (17 (new (class "java.lang.IllegalStateException"))) 
                                      (20 (dup)) 
                                      (21 (ldc 0)) ;;STRING:: "Invalid reference state"
                                      (23 (invokespecial (methodCP "<init>" "java.lang.IllegalStateException" ((class "java.lang.String")) void))) 
                                      (26 (athrow)) 
                                      (27 (goto 45)) ;;to TAG_1;;at TAG_0
                                      (30 (astore_3)) ;;at TAG_3
                                      (31 (aload_3)) 
                                      (32 (athrow)) 
                                      (33 (astore_3)) ;;at TAG_4
                                      (34 (new (class "java.lang.IllegalStateException"))) 
                                      (37 (dup)) 
                                      (38 (ldc 0)) ;;STRING:: "Invalid reference state"
                                      (40 (aload_3)) 
                                      (41 (invokespecial (methodCP "<init>" "java.lang.IllegalStateException" ((class "java.lang.String") (class "java.lang.Throwable")) void))) 
                                      (44 (athrow)) 
                                      (45 (return)) ;;at TAG_1
                                      (endofcode 46))
                                   (Exceptions 
                                     (handler 0 27  30 (class "java.lang.RuntimeException"))
                                     (handler 0 27  33 (class "java.lang.Exception")))
                                   (StackMap )))
                        (method "validate"
                              (parameters (class "java.lang.Object"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_0))
                                      (2 (getfield (fieldCP "validator" "clojure.lang.ARef" (class "clojure.lang.IFn"))))
                                      (5 (aload_1))
                                      (6 (invokevirtual
					(methodCP "validate" "clojure.lang.ARef" ((class "clojure.lang.IFn") (class "java.lang.Object")) void)))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "setValidator"
                              (parameters (class "clojure.lang.IFn"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 24)
                                   (parsedcode
                                      (0 (aload_0)) ;;at TAG_1
                                      (1 (aload_1)) 
                                      (2 (aload_0)) 
                                      (3 (invokevirtual (methodCP "deref" "clojure.lang.ARef" () (class "java.lang.Object")))) 
                                      (6 (invokevirtual (methodCP "validate" "clojure.lang.ARef" ((class "clojure.lang.IFn") (class "java.lang.Object")) void))) 
                                      (9 (goto 18)) ;;to TAG_0;;at TAG_2
                                      (12 (astore_2)) ;;at TAG_3
                                      (13 (aload_2)) 
                                      (14 (invokestatic (methodCP "sneakyThrow" "clojure.lang.Util" ((class "java.lang.Throwable")) (class "java.lang.RuntimeException")))) 
                                      (17 (athrow)) 
                                      (18 (aload_0)) ;;at TAG_0
                                      (19 (aload_1)) 
                                      (20 (putfield (fieldCP "validator" "clojure.lang.ARef" (class "clojure.lang.IFn")))) 
                                      (23 (return)) 
                                      (endofcode 24))
                                   (Exceptions 
                                     (handler 0 9  12 (class "java.lang.Exception")))
                                   (StackMap )))
                        (method "getValidator"
                              (parameters )
                              (returntype . (class "clojure.lang.IFn"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "validator" "clojure.lang.ARef" (class "clojure.lang.IFn"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getWatches"
                              (parameters )
                              (returntype . (class "clojure.lang.IPersistentMap"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "watches" "clojure.lang.ARef" (class "clojure.lang.IPersistentMap"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "addWatch"
                              (parameters (class "java.lang.Object") (class "clojure.lang.IFn"))
                              (returntype . (class "clojure.lang.IRef"))
                              (accessflags  *class*  *public*  *super*  *synchronized* )
                              (code
                                   (max_stack . 4) (max_locals . 3) (code_length . 17)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_0))
                                      (2 (getfield (fieldCP "watches" "clojure.lang.ARef" (class "clojure.lang.IPersistentMap"))))
                                      (5 (aload_1))
                                      (6 (aload_2))
                                      (7 (invokeinterface
					(methodCP "assoc" "clojure.lang.IPersistentMap" ((class "java.lang.Object") (class "java.lang.Object")) (class "clojure.lang.IPersistentMap")) 3))
                                      (12 (putfield (fieldCP "watches" "clojure.lang.ARef" (class "clojure.lang.IPersistentMap"))))
                                      (15 (aload_0))
                                      (16 (areturn))
                                      (endofcode 17))
                                   (Exceptions )
                                   (StackMap )))
                        (method "removeWatch"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "clojure.lang.IRef"))
                              (accessflags  *class*  *public*  *super*  *synchronized* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 25)
                                   (parsedcode
                                      (0 (aload_0)) ;;at TAG_1
                                      (1 (aload_0)) 
                                      (2 (getfield (fieldCP "watches" "clojure.lang.ARef" (class "clojure.lang.IPersistentMap")))) 
                                      (5 (aload_1)) 
                                      (6 (invokeinterface (methodCP "without" "clojure.lang.IPersistentMap" ((class "java.lang.Object")) (class "clojure.lang.IPersistentMap")) 2)) 
                                      (11 (putfield (fieldCP "watches" "clojure.lang.ARef" (class "clojure.lang.IPersistentMap")))) 
                                      (14 (goto 23)) ;;to TAG_0;;at TAG_2
                                      (17 (astore_2)) ;;at TAG_3
                                      (18 (aload_2)) 
                                      (19 (invokestatic (methodCP "sneakyThrow" "clojure.lang.Util" ((class "java.lang.Throwable")) (class "java.lang.RuntimeException")))) 
                                      (22 (athrow)) 
                                      (23 (aload_0)) ;;at TAG_0
                                      (24 (areturn)) 
                                      (endofcode 25))
                                   (Exceptions 
                                     (handler 0 14  17 (class "java.lang.Exception")))
                                   (StackMap )))
                        (method "notifyWatches"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 8) (code_length . 98)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "watches" "clojure.lang.ARef" (class "clojure.lang.IPersistentMap")))) 
                                      (4 (astore_3)) 
                                      (5 (aload_3)) 
                                      (6 (invokeinterface (methodCP "count" "clojure.lang.IPersistentMap" () int) 1)) 
                                      (11 (ifle 97)) ;;to TAG_0
                                      (14 (aload_3)) 
                                      (15 (invokeinterface (methodCP "seq" "clojure.lang.IPersistentMap" () (class "clojure.lang.ISeq")) 1)) 
                                      (20 (astore 4)) 
                                      (22 (aload 4)) ;;at TAG_3
                                      (24 (ifnull 97)) ;;to TAG_0
                                      (27 (aload 4)) 
                                      (29 (invokeinterface (methodCP "first" "clojure.lang.ISeq" () (class "java.lang.Object")) 1)) 
                                      (34 (checkcast (class "java.util.Map$Entry"))) 
                                      (37 (astore 5)) 
                                      (39 (aload 5)) 
                                      (41 (invokeinterface (methodCP "getValue" "java.util.Map$Entry" () (class "java.lang.Object")) 1)) 
                                      (46 (checkcast (class "clojure.lang.IFn"))) 
                                      (49 (astore 6)) 
                                      (51 (aload 6)) ;;at TAG_4
                                      (53 (ifnull 74)) ;;to TAG_1
                                      (56 (aload 6)) 
                                      (58 (aload 5)) 
                                      (60 (invokeinterface (methodCP "getKey" "java.util.Map$Entry" () (class "java.lang.Object")) 1)) 
                                      (65 (aload_0)) 
                                      (66 (aload_1)) 
                                      (67 (aload_2)) 
                                      (68 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 5)) 
                                      (73 (pop)) 
                                      (74 (goto 85))  ;;to TAG_2;;at TAG_1
                                      (77 (astore 7)) ;;at TAG_5
                                      (79 (aload 7)) 
                                      (81 (invokestatic (methodCP "sneakyThrow" "clojure.lang.Util" ((class "java.lang.Throwable")) (class "java.lang.RuntimeException")))) 
                                      (84 (athrow)) 
                                      (85 (aload 4)) ;;at TAG_2
                                      (87 (invokeinterface (methodCP "next" "clojure.lang.ISeq" () (class "clojure.lang.ISeq")) 1)) 
                                      (92 (astore 4)) 
                                      (94 (goto 22)) ;;to TAG_3
                                      (97 (return)) ;;at TAG_0
                                      (endofcode 98))
                                   (Exceptions 
                                     (handler 51 74  77 (class "java.lang.Exception")))
                                   (StackMap ))))
            (interfaces "clojure.lang.IRef")
            (accessflags  *abstract*  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *ARef-class-table*
  (make-static-class-decls 
   *clojure.lang.ARef*))

(defconst *package-name-map* 
  ("clojure.lang.ARef" . "clojure.lang"))

