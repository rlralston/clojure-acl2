; core$fn__5441-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:42 CDT 2014.
;

(defconst *clojure.core$fn__5441*
 (make-class-def
      '(class "clojure.core$fn__5441"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "*print-readably*")
                        (STRING  "print-meta")
                        (STRING  "print-sequential")
                        (STRING  "pr-on")
                        (STRING  "print-object")
                        (STRING  "(")
                        (STRING  " ")
                        (STRING  ")"))
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
                                      (2 (ldc 1))         ;;STRING:: "*print-readably*"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$fn__5441" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "print-meta"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.core$fn__5441" (class "clojure.lang.Var"))))
                                      (26 (ldc 0))        ;;STRING:: "clojure.core"
                                      (28 (ldc 3))        ;;STRING:: "print-sequential"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.core$fn__5441" (class "clojure.lang.Var"))))
                                      (39 (ldc 0))        ;;STRING:: "clojure.core"
                                      (41 (ldc 4))        ;;STRING:: "pr-on"
                                      (43 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (46 (checkcast (class "clojure.lang.Var")))
                                      (49 (putstatic (fieldCP "const__3" "clojure.core$fn__5441" (class "clojure.lang.Var"))))
                                      (52 (ldc 0))        ;;STRING:: "clojure.core"
                                      (54 (ldc 5))        ;;STRING:: "print-object"
                                      (56 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (59 (checkcast (class "clojure.lang.Var")))
                                      (62 (putstatic (fieldCP "const__4" "clojure.core$fn__5441" (class "clojure.lang.Var"))))
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
                                   (max_stack . 8) (max_locals . 3) (code_length . 90)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.core$fn__5441" (class "clojure.lang.Var")))) 
                                      (3 (invokevirtual (methodCP "get" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (6 (dup)) 
                                      (7 (ifnull 68)) ;;to TAG_0
                                      (10 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (13 (if_acmpeq 69)) ;;to TAG_1
                                      (16 (getstatic (fieldCP "const__1" "clojure.core$fn__5441" (class "clojure.lang.Var")))) 
                                      (19 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (22 (checkcast (class "clojure.lang.IFn"))) 
                                      (25 (aload_1)) 
                                      (26 (aload_2)) 
                                      (27 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (32 (pop)) 
                                      (33 (getstatic (fieldCP "const__2" "clojure.core$fn__5441" (class "clojure.lang.Var")))) 
                                      (36 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (39 (checkcast (class "clojure.lang.IFn"))) 
                                      (42 (ldc 6)) ;;STRING:: "("
                                      (44 (getstatic (fieldCP "const__3" "clojure.core$fn__5441" (class "clojure.lang.Var")))) 
                                      (47 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (50 (ldc 7)) ;;STRING:: " "
                                      (52 (ldc 8)) ;;STRING:: ")"
                                      (54 (aload_1)) 
                                      (55 (aconst_null)) 
                                      (56 (astore_1)) 
                                      (57 (aload_2)) 
                                      (58 (aconst_null)) 
                                      (59 (astore_2)) 
                                      (60 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 7)) 
                                      (65 (goto 89))  ;;to TAG_2
                                      (68 (pop)) ;;at TAG_0
                                      (69 (getstatic (fieldCP "const__4" "clojure.core$fn__5441" (class "clojure.lang.Var")))) ;;at TAG_1
                                      (72 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (75 (checkcast (class "clojure.lang.IFn"))) 
                                      (78 (aload_1)) 
                                      (79 (aconst_null)) 
                                      (80 (astore_1)) 
                                      (81 (aload_2)) 
                                      (82 (aconst_null)) 
                                      (83 (astore_2)) 
                                      (84 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (89 (areturn)) ;;at TAG_2
                                      (endofcode 90))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$fn__5441-class-table*
  (make-static-class-decls 
   *clojure.core$fn__5441*))

(defconst *package-name-map* 
  ("clojure.core$fn__5441" . "clojure"))

