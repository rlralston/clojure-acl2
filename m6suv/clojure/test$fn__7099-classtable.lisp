; test$fn__7099-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:59 CDT 2014.
;

(defconst *clojure.test$fn__7099*
 (make-class-def
      '(class "clojure.test$fn__7099"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "sequential?")
                        (STRING  "clojure.test")
                        (STRING  "function?")
                        (STRING  "first")
                        (STRING  "assert-predicate")
                        (STRING  "assert-any"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 66)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "sequential?"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.test$fn__7099" (class "clojure.lang.Var"))))
                                      (13 (ldc 2))        ;;STRING:: "clojure.test"
                                      (15 (ldc 3))        ;;STRING:: "function?"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.test$fn__7099" (class "clojure.lang.Var"))))
                                      (26 (ldc 0))        ;;STRING:: "clojure.core"
                                      (28 (ldc 4))        ;;STRING:: "first"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.test$fn__7099" (class "clojure.lang.Var"))))
                                      (39 (ldc 2))        ;;STRING:: "clojure.test"
                                      (41 (ldc 5))        ;;STRING:: "assert-predicate"
                                      (43 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (46 (checkcast (class "clojure.lang.Var")))
                                      (49 (putstatic (fieldCP "const__3" "clojure.test$fn__7099" (class "clojure.lang.Var"))))
                                      (52 (ldc 2))        ;;STRING:: "clojure.test"
                                      (54 (ldc 6))        ;;STRING:: "assert-any"
                                      (56 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (59 (checkcast (class "clojure.lang.Var")))
                                      (62 (putstatic (fieldCP "const__4" "clojure.test$fn__7099" (class "clojure.lang.Var"))))
                                      (65 (return))
                                      (endofcode 66))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "clojure.lang.AFunction" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 4) (code_length . 118)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.test$fn__7099" (class "clojure.lang.Var")))) 
                                      (3 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (6 (checkcast (class "clojure.lang.IFn"))) 
                                      (9 (aload_2)) 
                                      (10 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (15 (astore_3)) 
                                      (16 (aload_3)) 
                                      (17 (dup)) 
                                      (18 (ifnull 59)) ;;to TAG_0
                                      (21 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (24 (if_acmpeq 60)) ;;to TAG_1
                                      (27 (getstatic (fieldCP "const__1" "clojure.test$fn__7099" (class "clojure.lang.Var")))) 
                                      (30 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (33 (checkcast (class "clojure.lang.IFn"))) 
                                      (36 (getstatic (fieldCP "const__2" "clojure.test$fn__7099" (class "clojure.lang.Var")))) 
                                      (39 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (42 (checkcast (class "clojure.lang.IFn"))) 
                                      (45 (aload_2)) 
                                      (46 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (51 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (56 (goto 63))  ;;to TAG_2
                                      (59 (pop)) ;;at TAG_0
                                      (60 (aload_3)) ;;at TAG_1
                                      (61 (aconst_null)) 
                                      (62 (astore_3)) 
                                      (63 (dup)) ;;at TAG_2
                                      (64 (ifnull 96)) ;;to TAG_3
                                      (67 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (70 (if_acmpeq 97)) ;;to TAG_4
                                      (73 (getstatic (fieldCP "const__3" "clojure.test$fn__7099" (class "clojure.lang.Var")))) 
                                      (76 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (79 (checkcast (class "clojure.lang.IFn"))) 
                                      (82 (aload_1)) 
                                      (83 (aconst_null)) 
                                      (84 (astore_1)) 
                                      (85 (aload_2)) 
                                      (86 (aconst_null)) 
                                      (87 (astore_2)) 
                                      (88 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (93 (goto 117)) ;;to TAG_5
                                      (96 (pop)) ;;at TAG_3
                                      (97 (getstatic (fieldCP "const__4" "clojure.test$fn__7099" (class "clojure.lang.Var")))) ;;at TAG_4
                                      (100 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (103 (checkcast (class "clojure.lang.IFn"))) 
                                      (106 (aload_1)) 
                                      (107 (aconst_null)) 
                                      (108 (astore_1)) 
                                      (109 (aload_2)) 
                                      (110 (aconst_null)) 
                                      (111 (astore_2)) 
                                      (112 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (117 (areturn)) ;;at TAG_5
                                      (endofcode 118))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *test$fn__7099-class-table*
  (make-static-class-decls 
   *clojure.test$fn__7099*))

(defconst *package-name-map* 
  ("clojure.test$fn__7099" . "clojure"))
