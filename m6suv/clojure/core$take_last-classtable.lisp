; core$take_last-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:45 CDT 2014.
;

(defconst *clojure.core$take_last*
 (make-class-def
      '(class "clojure.core$take_last"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "seq")
                        (STRING  "drop")
                        (STRING  "next"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 40)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "seq"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$take_last" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "drop"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.core$take_last" (class "clojure.lang.Var"))))
                                      (26 (ldc 0))        ;;STRING:: "clojure.core"
                                      (28 (ldc 3))        ;;STRING:: "next"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.core$take_last" (class "clojure.lang.Var"))))
                                      (39 (return))
                                      (endofcode 40))
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
                                   (max_stack . 5) (max_locals . 5) (code_length . 107)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.core$take_last" (class "clojure.lang.Var")))) 
                                      (3 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (6 (checkcast (class "clojure.lang.IFn"))) 
                                      (9 (aload_2)) 
                                      (10 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (15 (astore_3)) 
                                      (16 (getstatic (fieldCP "const__0" "clojure.core$take_last" (class "clojure.lang.Var")))) 
                                      (19 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (22 (checkcast (class "clojure.lang.IFn"))) 
                                      (25 (getstatic (fieldCP "const__1" "clojure.core$take_last" (class "clojure.lang.Var")))) 
                                      (28 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (31 (checkcast (class "clojure.lang.IFn"))) 
                                      (34 (aload_1)) 
                                      (35 (aconst_null)) 
                                      (36 (astore_1)) 
                                      (37 (aload_2)) 
                                      (38 (aconst_null)) 
                                      (39 (astore_2)) 
                                      (40 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (45 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (50 (astore 4)) 
                                      (52 (aload 4)) ;;at TAG_2
                                      (54 (dup)) 
                                      (55 (ifnull 104)) ;;to TAG_0
                                      (58 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (61 (if_acmpeq 105)) ;;to TAG_1
                                      (64 (getstatic (fieldCP "const__2" "clojure.core$take_last" (class "clojure.lang.Var")))) 
                                      (67 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (70 (checkcast (class "clojure.lang.IFn"))) 
                                      (73 (aload_3)) 
                                      (74 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (79 (getstatic (fieldCP "const__2" "clojure.core$take_last" (class "clojure.lang.Var")))) 
                                      (82 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (85 (checkcast (class "clojure.lang.IFn"))) 
                                      (88 (aload 4)) 
                                      (90 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (95 (astore 4)) 
                                      (97 (astore_3)) 
                                      (98 (goto 52))  ;;to TAG_2
                                      (101 (goto 106)) ;;to TAG_3
                                      (104 (pop)) ;;at TAG_0
                                      (105 (aload_3)) ;;at TAG_1
                                      (106 (areturn)) ;;at TAG_3
                                      (endofcode 107))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$take_last-class-table*
  (make-static-class-decls 
   *clojure.core$take_last*))

(defconst *package-name-map* 
  ("clojure.core$take_last" . "clojure"))

