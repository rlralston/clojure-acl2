; Encoder-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:31 CDT 2014.
;

(defconst *java.beans.Encoder*
 (make-class-def
      '(class "java.beans.Encoder"
            "java.lang.Object"
            (constant_pool
                        (STRING  "failed to evaluate: ")
                        (STRING  "Encoder: discarding statement "))
            (fields
                        (field "finder" (class "com.sun.beans.finder.PersistenceDelegateFinder") (accessflags  *class*  *final*  *private* ) -1)
                        (field "bindings" (class "java.util.Map") (accessflags  *class*  *private* ) -1)
                        (field "exceptionListener" (class "java.beans.ExceptionListener") (accessflags  *class*  *private* ) -1)
                        (field "executeStatements" boolean (accessflags  *class* ) -1)
                        (field "attributes" (class "java.util.Map") (accessflags  *class*  *private* ) -1))
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 1) (code_length . 32)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (aload_0))
                                      (5 (new (class "com.sun.beans.finder.PersistenceDelegateFinder")))
                                      (8 (dup))
                                      (9 (invokespecial
					(methodCP "<init>" "com.sun.beans.finder.PersistenceDelegateFinder" () void)))
                                      (12 (putfield (fieldCP "finder" "java.beans.Encoder" (class "com.sun.beans.finder.PersistenceDelegateFinder"))))
                                      (15 (aload_0))
                                      (16 (new (class "java.util.IdentityHashMap")))
                                      (19 (dup))
                                      (20 (invokespecial
					(methodCP "<init>" "java.util.IdentityHashMap" () void)))
                                      (23 (putfield (fieldCP "bindings" "java.beans.Encoder" (class "java.util.Map"))))
                                      (26 (aload_0))
                                      (27 (iconst_1))
                                      (28 (putfield (fieldCP "executeStatements" "java.beans.Encoder" boolean)))
                                      (31 (return))
                                      (endofcode 32))
                                   (Exceptions )
                                   (StackMap )))
                        (method "writeObject"
                              (parameters (class "java.lang.Object"))
                              (returntype . void)
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 30)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (aload_0)) 
                                      (2 (if_acmpne 6)) ;;to TAG_0
                                      (5 (return)) 
                                      (6 (aload_0)) ;;at TAG_0
                                      (7 (aload_1)) 
                                      (8 (ifnonnull 15)) ;;to TAG_1
                                      (11 (aconst_null)) 
                                      (12 (goto 19))  ;;to TAG_2
                                      (15 (aload_1)) ;;at TAG_1
                                      (16 (invokevirtual (methodCP "getClass" "java.lang.Object" () (class "java.lang.Class")))) 
                                      (19 (invokevirtual (methodCP "getPersistenceDelegate" "java.beans.Encoder" ((class "java.lang.Class")) (class "java.beans.PersistenceDelegate")))) ;;at TAG_2
                                      (22 (astore_2)) 
                                      (23 (aload_2)) 
                                      (24 (aload_1)) 
                                      (25 (aload_0)) 
                                      (26 (invokevirtual (methodCP "writeObject" "java.beans.PersistenceDelegate" ((class "java.lang.Object") (class "java.beans.Encoder")) void))) 
                                      (29 (return)) 
                                      (endofcode 30))
                                   (Exceptions )
                                   (StackMap )))
                        (method "setExceptionListener"
                              (parameters (class "java.beans.ExceptionListener"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 6)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "exceptionListener" "java.beans.Encoder" (class "java.beans.ExceptionListener"))))
                                      (5 (return))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getExceptionListener"
                              (parameters )
                              (returntype . (class "java.beans.ExceptionListener"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 18)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "exceptionListener" "java.beans.Encoder" (class "java.beans.ExceptionListener")))) 
                                      (4 (ifnull 14))  ;;to TAG_0
                                      (7 (aload_0)) 
                                      (8 (getfield (fieldCP "exceptionListener" "java.beans.Encoder" (class "java.beans.ExceptionListener")))) 
                                      (11 (goto 17)) ;;to TAG_1
                                      (14 (getstatic (fieldCP "defaultExceptionListener" "java.beans.Statement" (class "java.beans.ExceptionListener")))) ;;at TAG_0
                                      (17 (areturn)) ;;at TAG_1
                                      (endofcode 18))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getValue"
                              (parameters (class "java.beans.Expression"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class* )
                              (code
                                   (max_stack . 4) (max_locals . 3) (code_length . 54)
                                   (parsedcode
                                      (0 (aload_1)) ;;at TAG_2
                                      (1 (ifnonnull 8)) ;;to TAG_0
                                      (4 (aconst_null)) 
                                      (5 (goto 12)) ;;to TAG_1
                                      (8 (aload_1)) ;;at TAG_0
                                      (9 (invokevirtual (methodCP "getValue" "java.beans.Expression" () (class "java.lang.Object")))) 
                                      (12 (areturn)) ;;at TAG_1
                                      (13 (astore_2)) ;;at TAG_3
                                      (14 (aload_0)) 
                                      (15 (invokevirtual (methodCP "getExceptionListener" "java.beans.Encoder" () (class "java.beans.ExceptionListener")))) 
                                      (18 (aload_2)) 
                                      (19 (invokeinterface (methodCP "exceptionThrown" "java.beans.ExceptionListener" ((class "java.lang.Exception")) void) 2)) 
                                      (24 (new (class "java.lang.RuntimeException"))) 
                                      (27 (dup)) 
                                      (28 (new (class "java.lang.StringBuilder"))) 
                                      (31 (dup)) 
                                      (32 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (35 (ldc 0)) ;;STRING:: "failed to evaluate: "
                                      (37 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (40 (aload_1)) 
                                      (41 (invokevirtual (methodCP "toString" "java.beans.Expression" () (class "java.lang.String")))) 
                                      (44 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (47 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (50 (invokespecial (methodCP "<init>" "java.lang.RuntimeException" ((class "java.lang.String")) void))) 
                                      (53 (athrow)) 
                                      (endofcode 54))
                                   (Exceptions 
                                     (handler 0 12  13 (class "java.lang.Exception")))
                                   (StackMap )))
                        (method "getPersistenceDelegate"
                              (parameters (class "java.lang.Class"))
                              (returntype . (class "java.beans.PersistenceDelegate"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 22)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "finder" "java.beans.Encoder" (class "com.sun.beans.finder.PersistenceDelegateFinder")))) 
                                      (4 (aload_1)) 
                                      (5 (invokevirtual (methodCP "find" "com.sun.beans.finder.PersistenceDelegateFinder" ((class "java.lang.Class")) (class "java.beans.PersistenceDelegate")))) 
                                      (8 (astore_2)) 
                                      (9 (aload_2)) 
                                      (10 (ifnull 17))  ;;to TAG_0
                                      (13 (aload_2)) 
                                      (14 (goto 21)) ;;to TAG_1
                                      (17 (aload_1)) ;;at TAG_0
                                      (18 (invokestatic (methodCP "getPersistenceDelegate" "java.beans.MetaData" ((class "java.lang.Class")) (class "java.beans.PersistenceDelegate")))) 
                                      (21 (areturn)) ;;at TAG_1
                                      (endofcode 22))
                                   (Exceptions )
                                   (StackMap )))
                        (method "setPersistenceDelegate"
                              (parameters (class "java.lang.Class") (class "java.beans.PersistenceDelegate"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "finder" "java.beans.Encoder" (class "com.sun.beans.finder.PersistenceDelegateFinder"))))
                                      (4 (aload_1))
                                      (5 (aload_2))
                                      (6 (invokevirtual
					(methodCP "register" "com.sun.beans.finder.PersistenceDelegateFinder" ((class "java.lang.Class") (class "java.beans.PersistenceDelegate")) void)))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "remove"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 20)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "bindings" "java.beans.Encoder" (class "java.util.Map"))))
                                      (4 (aload_1))
                                      (5 (invokeinterface
					(methodCP "remove" "java.util.Map" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (10 (checkcast (class "java.beans.Expression")))
                                      (13 (astore_2))
                                      (14 (aload_0))
                                      (15 (aload_2))
                                      (16 (invokevirtual
					(methodCP "getValue" "java.beans.Encoder" ((class "java.beans.Expression")) (class "java.lang.Object"))))
                                      (19 (areturn))
                                      (endofcode 20))
                                   (Exceptions )
                                   (StackMap )))
                        (method "get"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 41)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (ifnull 19))  ;;to TAG_0
                                      (4 (aload_1)) 
                                      (5 (aload_0)) 
                                      (6 (if_acmpeq 19))  ;;to TAG_0
                                      (9 (aload_1)) 
                                      (10 (invokevirtual (methodCP "getClass" "java.lang.Object" () (class "java.lang.Class")))) 
                                      (13 (ldc_w )) 
                                      (16 (if_acmpne 21)) ;;to TAG_1
                                      (19 (aload_1)) ;;at TAG_0
                                      (20 (areturn)) 
                                      (21 (aload_0)) ;;at TAG_1
                                      (22 (getfield (fieldCP "bindings" "java.beans.Encoder" (class "java.util.Map")))) 
                                      (25 (aload_1)) 
                                      (26 (invokeinterface (methodCP "get" "java.util.Map" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (31 (checkcast (class "java.beans.Expression"))) 
                                      (34 (astore_2)) 
                                      (35 (aload_0)) 
                                      (36 (aload_2)) 
                                      (37 (invokevirtual (methodCP "getValue" "java.beans.Encoder" ((class "java.beans.Expression")) (class "java.lang.Object")))) 
                                      (40 (areturn)) 
                                      (endofcode 41))
                                   (Exceptions )
                                   (StackMap )))
                        (method "writeObject1"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 23)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (aload_1)) 
                                      (2 (invokevirtual (methodCP "get" "java.beans.Encoder" ((class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (5 (astore_2)) 
                                      (6 (aload_2)) 
                                      (7 (ifnonnull 21))  ;;to TAG_0
                                      (10 (aload_0)) 
                                      (11 (aload_1)) 
                                      (12 (invokevirtual (methodCP "writeObject" "java.beans.Encoder" ((class "java.lang.Object")) void))) 
                                      (15 (aload_0)) 
                                      (16 (aload_1)) 
                                      (17 (invokevirtual (methodCP "get" "java.beans.Encoder" ((class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (20 (astore_2)) 
                                      (21 (aload_2)) ;;at TAG_0
                                      (22 (areturn)) 
                                      (endofcode 23))
                                   (Exceptions )
                                   (StackMap )))
                        (method "cloneStatement"
                              (parameters (class "java.beans.Statement"))
                              (returntype . (class "java.beans.Statement"))
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 5) (max_locals . 7) (code_length . 114)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (invokevirtual (methodCP "getTarget" "java.beans.Statement" () (class "java.lang.Object")))) 
                                      (4 (astore_2)) 
                                      (5 (aload_0)) 
                                      (6 (aload_2)) 
                                      (7 (invokespecial (methodCP "writeObject1" "java.beans.Encoder" ((class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (10 (astore_3)) 
                                      (11 (aload_1)) 
                                      (12 (invokevirtual (methodCP "getArguments" "java.beans.Statement" () (array (class "java.lang.Object"))))) 
                                      (15 (astore 4)) 
                                      (17 (aload 4)) 
                                      (19 (arraylength)) 
                                      (20 (anewarray (class "java.lang.Object"))) 
                                      (23 (astore 5)) 
                                      (25 (iconst_0)) 
                                      (26 (istore 6)) 
                                      (28 (iload 6)) ;;at TAG_1
                                      (30 (aload 4)) 
                                      (32 (arraylength)) 
                                      (33 (if_icmpge 56)) ;;to TAG_0
                                      (36 (aload 5)) 
                                      (38 (iload 6)) 
                                      (40 (aload_0)) 
                                      (41 (aload 4)) 
                                      (43 (iload 6)) 
                                      (45 (aaload)) 
                                      (46 (invokespecial (methodCP "writeObject1" "java.beans.Encoder" ((class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (49 (aastore)) 
                                      (50 (iinc 6 1)) 
                                      (53 (goto 28)) ;;to TAG_1
                                      (56 (ldc_w )) ;;at TAG_0
                                      (59 (aload_1)) 
                                      (60 (invokevirtual (methodCP "getClass" "java.lang.Object" () (class "java.lang.Class")))) 
                                      (63 (invokevirtual (methodCP "equals" "java.lang.Object" ((class "java.lang.Object")) boolean))) 
                                      (66 (ifeq 86))  ;;to TAG_2
                                      (69 (new (class "java.beans.Statement"))) 
                                      (72 (dup)) 
                                      (73 (aload_3)) 
                                      (74 (aload_1)) 
                                      (75 (invokevirtual (methodCP "getMethodName" "java.beans.Statement" () (class "java.lang.String")))) 
                                      (78 (aload 5)) 
                                      (80 (invokespecial (methodCP "<init>" "java.beans.Statement" ((class "java.lang.Object") (class "java.lang.String") (array (class "java.lang.Object"))) void))) 
                                      (83 (goto 100)) ;;to TAG_3
                                      (86 (new (class "java.beans.Expression"))) ;;at TAG_2
                                      (89 (dup)) 
                                      (90 (aload_3)) 
                                      (91 (aload_1)) 
                                      (92 (invokevirtual (methodCP "getMethodName" "java.beans.Statement" () (class "java.lang.String")))) 
                                      (95 (aload 5)) 
                                      (97 (invokespecial (methodCP "<init>" "java.beans.Expression" ((class "java.lang.Object") (class "java.lang.String") (array (class "java.lang.Object"))) void))) 
                                      (100 (astore 6)) ;;at TAG_3
                                      (102 (aload 6)) 
                                      (104 (aload_1)) 
                                      (105 (getfield (fieldCP "loader" "java.beans.Statement" (class "java.lang.ClassLoader")))) 
                                      (108 (putfield (fieldCP "loader" "java.beans.Statement" (class "java.lang.ClassLoader")))) 
                                      (111 (aload 6)) 
                                      (113 (areturn)) 
                                      (endofcode 114))
                                   (Exceptions )
                                   (StackMap )))
                        (method "writeStatement"
                              (parameters (class "java.beans.Statement"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 4) (code_length . 66)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (aload_1)) 
                                      (2 (invokespecial (methodCP "cloneStatement" "java.beans.Encoder" ((class "java.beans.Statement")) (class "java.beans.Statement")))) 
                                      (5 (astore_2)) 
                                      (6 (aload_1)) 
                                      (7 (invokevirtual (methodCP "getTarget" "java.beans.Statement" () (class "java.lang.Object")))) 
                                      (10 (aload_0)) 
                                      (11 (if_acmpeq 65)) ;;to TAG_0
                                      (14 (aload_0)) 
                                      (15 (getfield (fieldCP "executeStatements" "java.beans.Encoder" boolean))) 
                                      (18 (ifeq 65)) ;;to TAG_0
                                      (21 (aload_2)) ;;at TAG_1
                                      (22 (invokevirtual (methodCP "execute" "java.beans.Statement" () void))) 
                                      (25 (goto 65)) ;;to TAG_0;;at TAG_2
                                      (28 (astore_3)) ;;at TAG_3
                                      (29 (aload_0)) 
                                      (30 (invokevirtual (methodCP "getExceptionListener" "java.beans.Encoder" () (class "java.beans.ExceptionListener")))) 
                                      (33 (new (class "java.lang.Exception"))) 
                                      (36 (dup)) 
                                      (37 (new (class "java.lang.StringBuilder"))) 
                                      (40 (dup)) 
                                      (41 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (44 (ldc 1)) ;;STRING:: "Encoder: discarding statement "
                                      (46 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (49 (aload_2)) 
                                      (50 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.Object")) (class "java.lang.StringBuilder")))) 
                                      (53 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (56 (aload_3)) 
                                      (57 (invokespecial (methodCP "<init>" "java.lang.Exception" ((class "java.lang.String") (class "java.lang.Throwable")) void))) 
                                      (60 (invokeinterface (methodCP "exceptionThrown" "java.beans.ExceptionListener" ((class "java.lang.Exception")) void) 2)) 
                                      (65 (return)) ;;at TAG_0
                                      (endofcode 66))
                                   (Exceptions 
                                     (handler 21 25  28 (class "java.lang.Exception")))
                                   (StackMap )))
                        (method "writeExpression"
                              (parameters (class "java.beans.Expression"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 3) (code_length . 40)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (aload_1)) 
                                      (2 (invokevirtual (methodCP "getValue" "java.beans.Encoder" ((class "java.beans.Expression")) (class "java.lang.Object")))) 
                                      (5 (astore_2)) 
                                      (6 (aload_0)) 
                                      (7 (aload_2)) 
                                      (8 (invokevirtual (methodCP "get" "java.beans.Encoder" ((class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (11 (ifnull 15))  ;;to TAG_0
                                      (14 (return)) 
                                      (15 (aload_0)) ;;at TAG_0
                                      (16 (getfield (fieldCP "bindings" "java.beans.Encoder" (class "java.util.Map")))) 
                                      (19 (aload_2)) 
                                      (20 (aload_0)) 
                                      (21 (aload_1)) 
                                      (22 (invokespecial (methodCP "cloneStatement" "java.beans.Encoder" ((class "java.beans.Statement")) (class "java.beans.Statement")))) 
                                      (25 (checkcast (class "java.beans.Expression"))) 
                                      (28 (invokeinterface (methodCP "put" "java.util.Map" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (33 (pop)) 
                                      (34 (aload_0)) 
                                      (35 (aload_2)) 
                                      (36 (invokevirtual (methodCP "writeObject" "java.beans.Encoder" ((class "java.lang.Object")) void))) 
                                      (39 (return)) 
                                      (endofcode 40))
                                   (Exceptions )
                                   (StackMap )))
                        (method "clear"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "bindings" "java.beans.Encoder" (class "java.util.Map"))))
                                      (4 (invokeinterface
					(methodCP "clear" "java.util.Map" () void) 1))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "setAttribute"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 31)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "attributes" "java.beans.Encoder" (class "java.util.Map")))) 
                                      (4 (ifnonnull 18))  ;;to TAG_0
                                      (7 (aload_0)) 
                                      (8 (new (class "java.util.HashMap"))) 
                                      (11 (dup)) 
                                      (12 (invokespecial (methodCP "<init>" "java.util.HashMap" () void))) 
                                      (15 (putfield (fieldCP "attributes" "java.beans.Encoder" (class "java.util.Map")))) 
                                      (18 (aload_0)) ;;at TAG_0
                                      (19 (getfield (fieldCP "attributes" "java.beans.Encoder" (class "java.util.Map")))) 
                                      (22 (aload_1)) 
                                      (23 (aload_2)) 
                                      (24 (invokeinterface (methodCP "put" "java.util.Map" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (29 (pop)) 
                                      (30 (return)) 
                                      (endofcode 31))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getAttribute"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 20)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "attributes" "java.beans.Encoder" (class "java.util.Map")))) 
                                      (4 (ifnonnull 9))  ;;to TAG_0
                                      (7 (aconst_null)) 
                                      (8 (areturn)) 
                                      (9 (aload_0)) ;;at TAG_0
                                      (10 (getfield (fieldCP "attributes" "java.beans.Encoder" (class "java.util.Map")))) 
                                      (13 (aload_1)) 
                                      (14 (invokeinterface (methodCP "get" "java.util.Map" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (19 (areturn)) 
                                      (endofcode 20))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *Encoder-class-table*
  (make-static-class-decls 
   *java.beans.Encoder*))

(defconst *package-name-map* 
  ("java.beans.Encoder" . "java.beans"))

