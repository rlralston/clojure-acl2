; core$pmap$step__6286$fn__6287-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:45 CDT 2014.
;

(defconst *clojure.core$pmap$step__6286$fn__6287*
 (make-class-def
      '(class "clojure.core$pmap$step__6286$fn__6287"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "map")
                        (STRING  "seq")
                        (STRING  "every?")
                        (STRING  "identity")
                        (STRING  "cons")
                        (STRING  "first")
                        (STRING  "rest"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__6" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "step" (class "java.lang.Object") (accessflags  *class* ) -1)
                        (field "cs" (class "java.lang.Object") (accessflags  *class* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 92)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "map"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$pmap$step__6286$fn__6287" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "seq"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.core$pmap$step__6286$fn__6287" (class "clojure.lang.Var"))))
                                      (26 (ldc 0))        ;;STRING:: "clojure.core"
                                      (28 (ldc 3))        ;;STRING:: "every?"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.core$pmap$step__6286$fn__6287" (class "clojure.lang.Var"))))
                                      (39 (ldc 0))        ;;STRING:: "clojure.core"
                                      (41 (ldc 4))        ;;STRING:: "identity"
                                      (43 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (46 (checkcast (class "clojure.lang.Var")))
                                      (49 (putstatic (fieldCP "const__3" "clojure.core$pmap$step__6286$fn__6287" (class "clojure.lang.Var"))))
                                      (52 (ldc 0))        ;;STRING:: "clojure.core"
                                      (54 (ldc 5))        ;;STRING:: "cons"
                                      (56 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (59 (checkcast (class "clojure.lang.Var")))
                                      (62 (putstatic (fieldCP "const__4" "clojure.core$pmap$step__6286$fn__6287" (class "clojure.lang.Var"))))
                                      (65 (ldc 0))        ;;STRING:: "clojure.core"
                                      (67 (ldc 6))        ;;STRING:: "first"
                                      (69 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (72 (checkcast (class "clojure.lang.Var")))
                                      (75 (putstatic (fieldCP "const__5" "clojure.core$pmap$step__6286$fn__6287" (class "clojure.lang.Var"))))
                                      (78 (ldc 0))        ;;STRING:: "clojure.core"
                                      (80 (ldc 7))        ;;STRING:: "rest"
                                      (82 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (85 (checkcast (class "clojure.lang.Var")))
                                      (88 (putstatic (fieldCP "const__6" "clojure.core$pmap$step__6286$fn__6287" (class "clojure.lang.Var"))))
                                      (91 (return))
                                      (endofcode 92))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 15)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "clojure.lang.AFunction" () void)))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (putfield (fieldCP "step" "clojure.core$pmap$step__6286$fn__6287" (class "java.lang.Object"))))
                                      (9 (aload_0))
                                      (10 (aload_2))
                                      (11 (putfield (fieldCP "cs" "clojure.core$pmap$step__6286$fn__6287" (class "java.lang.Object"))))
                                      (14 (return))
                                      (endofcode 15))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 7) (max_locals . 2) (code_length . 137)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.core$pmap$step__6286$fn__6287" (class "clojure.lang.Var")))) 
                                      (3 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (6 (checkcast (class "clojure.lang.IFn"))) 
                                      (9 (getstatic (fieldCP "const__1" "clojure.core$pmap$step__6286$fn__6287" (class "clojure.lang.Var")))) 
                                      (12 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (15 (aload_0)) 
                                      (16 (getfield (fieldCP "cs" "clojure.core$pmap$step__6286$fn__6287" (class "java.lang.Object")))) 
                                      (19 (aload_0)) 
                                      (20 (aconst_null)) 
                                      (21 (putfield (fieldCP "cs" "clojure.core$pmap$step__6286$fn__6287" (class "java.lang.Object")))) 
                                      (24 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (29 (astore_1)) 
                                      (30 (getstatic (fieldCP "const__2" "clojure.core$pmap$step__6286$fn__6287" (class "clojure.lang.Var")))) 
                                      (33 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (36 (checkcast (class "clojure.lang.IFn"))) 
                                      (39 (getstatic (fieldCP "const__3" "clojure.core$pmap$step__6286$fn__6287" (class "clojure.lang.Var")))) 
                                      (42 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (45 (aload_1)) 
                                      (46 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (51 (dup)) 
                                      (52 (ifnull 134)) ;;to TAG_0
                                      (55 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (58 (if_acmpeq 135)) ;;to TAG_1
                                      (61 (getstatic (fieldCP "const__4" "clojure.core$pmap$step__6286$fn__6287" (class "clojure.lang.Var")))) 
                                      (64 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (67 (checkcast (class "clojure.lang.IFn"))) 
                                      (70 (getstatic (fieldCP "const__0" "clojure.core$pmap$step__6286$fn__6287" (class "clojure.lang.Var")))) 
                                      (73 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (76 (checkcast (class "clojure.lang.IFn"))) 
                                      (79 (getstatic (fieldCP "const__5" "clojure.core$pmap$step__6286$fn__6287" (class "clojure.lang.Var")))) 
                                      (82 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (85 (aload_1)) 
                                      (86 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (91 (aload_0)) 
                                      (92 (getfield (fieldCP "step" "clojure.core$pmap$step__6286$fn__6287" (class "java.lang.Object")))) 
                                      (95 (checkcast (class "clojure.lang.IFn"))) 
                                      (98 (getstatic (fieldCP "const__0" "clojure.core$pmap$step__6286$fn__6287" (class "clojure.lang.Var")))) 
                                      (101 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (104 (checkcast (class "clojure.lang.IFn"))) 
                                      (107 (getstatic (fieldCP "const__6" "clojure.core$pmap$step__6286$fn__6287" (class "clojure.lang.Var")))) 
                                      (110 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (113 (aload_1)) 
                                      (114 (aconst_null)) 
                                      (115 (astore_1)) 
                                      (116 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (121 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (126 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (131 (goto 136))  ;;to TAG_2
                                      (134 (pop)) ;;at TAG_0
                                      (135 (aconst_null)) ;;at TAG_1
                                      (136 (areturn)) ;;at TAG_2
                                      (endofcode 137))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$pmap$step__6286$fn__6287-class-table*
  (make-static-class-decls 
   *clojure.core$pmap$step__6286$fn__6287*))

(defconst *package-name-map* 
  ("clojure.core$pmap$step__6286$fn__6287" . "clojure"))

