; core$case$fn__5147-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:40 CDT 2014.
;

(defconst *clojure.core$case$fn__5147*
 (make-class-def
      '(class "clojure.core$case$fn__5147"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "nth")
                        (STRING  "seq?")
                        (STRING  "reduce1"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "assoc_test" (class "java.lang.Object") (accessflags  *class* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 54)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "nth"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$case$fn__5147" (class "clojure.lang.Var"))))
                                      (13 (lconst_0))
                                      (14 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (17 (putstatic (fieldCP "const__1" "clojure.core$case$fn__5147" (class "java.lang.Object"))))
                                      (20 (lconst_1))
                                      (21 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (24 (putstatic (fieldCP "const__2" "clojure.core$case$fn__5147" (class "java.lang.Object"))))
                                      (27 (ldc 0))        ;;STRING:: "clojure.core"
                                      (29 (ldc 2))        ;;STRING:: "seq?"
                                      (31 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (34 (checkcast (class "clojure.lang.Var")))
                                      (37 (putstatic (fieldCP "const__3" "clojure.core$case$fn__5147" (class "clojure.lang.Var"))))
                                      (40 (ldc 0))        ;;STRING:: "clojure.core"
                                      (42 (ldc 3))        ;;STRING:: "reduce1"
                                      (44 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (47 (checkcast (class "clojure.lang.Var")))
                                      (50 (putstatic (fieldCP "const__4" "clojure.core$case$fn__5147" (class "clojure.lang.Var"))))
                                      (53 (return))
                                      (endofcode 54))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.lang.Object"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "clojure.lang.AFunction" () void)))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (putfield (fieldCP "assoc_test" "clojure.core$case$fn__5147" (class "java.lang.Object"))))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 6) (code_length . 122)
                                   (parsedcode
                                      (0 (aload_2)) 
                                      (1 (aconst_null)) 
                                      (2 (astore_2)) 
                                      (3 (astore_3)) 
                                      (4 (aload_3)) 
                                      (5 (lconst_0)) 
                                      (6 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (9 (aconst_null)) 
                                      (10 (invokestatic (methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (13 (astore 4)) 
                                      (15 (aload_3)) 
                                      (16 (aconst_null)) 
                                      (17 (astore_3)) 
                                      (18 (lconst_1)) 
                                      (19 (invokestatic (methodCP "intCast" "clojure.lang.RT" (long) int))) 
                                      (22 (aconst_null)) 
                                      (23 (invokestatic (methodCP "nth" "clojure.lang.RT" ((class "java.lang.Object") int (class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (26 (astore 5)) 
                                      (28 (getstatic (fieldCP "const__3" "clojure.core$case$fn__5147" (class "clojure.lang.Var")))) 
                                      (31 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (34 (checkcast (class "clojure.lang.IFn"))) 
                                      (37 (aload 4)) 
                                      (39 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (44 (dup)) 
                                      (45 (ifnull 95)) ;;to TAG_0
                                      (48 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (51 (if_acmpeq 96)) ;;to TAG_1
                                      (54 (getstatic (fieldCP "const__4" "clojure.core$case$fn__5147" (class "clojure.lang.Var")))) 
                                      (57 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (60 (checkcast (class "clojure.lang.IFn"))) 
                                      (63 (new (class "clojure.core$case$fn__5147$fn__5149"))) 
                                      (66 (dup)) 
                                      (67 (aload 5)) 
                                      (69 (aconst_null)) 
                                      (70 (astore 5)) 
                                      (72 (aload_0)) 
                                      (73 (getfield (fieldCP "assoc_test" "clojure.core$case$fn__5147" (class "java.lang.Object")))) 
                                      (76 (invokespecial (methodCP "<init>" "clojure.core$case$fn__5147$fn__5149" ((class "java.lang.Object") (class "java.lang.Object")) void))) 
                                      (79 (aload_1)) 
                                      (80 (aconst_null)) 
                                      (81 (astore_1)) 
                                      (82 (aload 4)) 
                                      (84 (aconst_null)) 
                                      (85 (astore 4)) 
                                      (87 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4)) 
                                      (92 (goto 121))  ;;to TAG_2
                                      (95 (pop)) ;;at TAG_0
                                      (96 (aload_0)) ;;at TAG_1
                                      (97 (getfield (fieldCP "assoc_test" "clojure.core$case$fn__5147" (class "java.lang.Object")))) 
                                      (100 (checkcast (class "clojure.lang.IFn"))) 
                                      (103 (aload_1)) 
                                      (104 (aconst_null)) 
                                      (105 (astore_1)) 
                                      (106 (aload 4)) 
                                      (108 (aconst_null)) 
                                      (109 (astore 4)) 
                                      (111 (aload 5)) 
                                      (113 (aconst_null)) 
                                      (114 (astore 5)) 
                                      (116 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4)) 
                                      (121 (areturn)) ;;at TAG_2
                                      (endofcode 122))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$case$fn__5147-class-table*
  (make-static-class-decls 
   *clojure.core$case$fn__5147*))

(defconst *package-name-map* 
  ("clojure.core$case$fn__5147" . "clojure"))

