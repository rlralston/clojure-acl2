; core$dorun-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:41 CDT 2014.
;

(defconst *clojure.core$dorun*
 (make-class-def
      '(class "clojure.core$dorun"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "seq")
                        (STRING  "next")
                        (STRING  "pos?")
                        (STRING  "dec"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 53)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "seq"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$dorun" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "next"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.core$dorun" (class "clojure.lang.Var"))))
                                      (26 (ldc 0))        ;;STRING:: "clojure.core"
                                      (28 (ldc 3))        ;;STRING:: "pos?"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.core$dorun" (class "clojure.lang.Var"))))
                                      (39 (ldc 0))        ;;STRING:: "clojure.core"
                                      (41 (ldc 4))        ;;STRING:: "dec"
                                      (43 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (46 (checkcast (class "clojure.lang.Var")))
                                      (49 (putstatic (fieldCP "const__3" "clojure.core$dorun" (class "clojure.lang.Var"))))
                                      (52 (return))
                                      (endofcode 53))
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
                                   (max_stack . 4) (max_locals . 4) (code_length . 94)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.core$dorun" (class "clojure.lang.Var")))) ;;at TAG_7
                                      (3 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (6 (checkcast (class "clojure.lang.IFn"))) 
                                      (9 (aload_2)) 
                                      (10 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (15 (astore_3)) 
                                      (16 (aload_3)) 
                                      (17 (dup)) 
                                      (18 (ifnull 46)) ;;to TAG_0
                                      (21 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (24 (if_acmpeq 47))  ;;to TAG_1
                                      (27 (aload_1)) 
                                      (28 (invokestatic (methodCP "isPos" "clojure.lang.Numbers" ((class "java.lang.Object")) boolean))) 
                                      (31 (ifeq 40)) ;;to TAG_2
                                      (34 (getstatic (fieldCP "TRUE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (37 (goto 43)) ;;to TAG_3
                                      (40 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) ;;at TAG_2
                                      (43 (goto 50)) ;;to TAG_4;;at TAG_3
                                      (46 (pop)) ;;at TAG_0
                                      (47 (aload_3)) ;;at TAG_1
                                      (48 (aconst_null)) 
                                      (49 (astore_3)) 
                                      (50 (dup)) ;;at TAG_4
                                      (51 (ifnull 91)) ;;to TAG_5
                                      (54 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (57 (if_acmpeq 92)) ;;to TAG_6
                                      (60 (aload_1)) 
                                      (61 (aconst_null)) 
                                      (62 (astore_1)) 
                                      (63 (invokestatic (methodCP "dec" "clojure.lang.Numbers" ((class "java.lang.Object")) (class "java.lang.Number")))) 
                                      (66 (getstatic (fieldCP "const__1" "clojure.core$dorun" (class "clojure.lang.Var")))) 
                                      (69 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (72 (checkcast (class "clojure.lang.IFn"))) 
                                      (75 (aload_2)) 
                                      (76 (aconst_null)) 
                                      (77 (astore_2)) 
                                      (78 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (83 (astore_2)) 
                                      (84 (astore_1)) 
                                      (85 (goto 0)) ;;to TAG_7
                                      (88 (goto 93)) ;;to TAG_8
                                      (91 (pop)) ;;at TAG_5
                                      (92 (aconst_null)) ;;at TAG_6
                                      (93 (areturn)) ;;at TAG_8
                                      (endofcode 94))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 52)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.core$dorun" (class "clojure.lang.Var")))) ;;at TAG_2
                                      (3 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (6 (checkcast (class "clojure.lang.IFn"))) 
                                      (9 (aload_1)) 
                                      (10 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (15 (dup)) 
                                      (16 (ifnull 49)) ;;to TAG_0
                                      (19 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (22 (if_acmpeq 50)) ;;to TAG_1
                                      (25 (getstatic (fieldCP "const__1" "clojure.core$dorun" (class "clojure.lang.Var")))) 
                                      (28 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (31 (checkcast (class "clojure.lang.IFn"))) 
                                      (34 (aload_1)) 
                                      (35 (aconst_null)) 
                                      (36 (astore_1)) 
                                      (37 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (42 (astore_1)) 
                                      (43 (goto 0))  ;;to TAG_2
                                      (46 (goto 51)) ;;to TAG_3
                                      (49 (pop)) ;;at TAG_0
                                      (50 (aconst_null)) ;;at TAG_1
                                      (51 (areturn)) ;;at TAG_3
                                      (endofcode 52))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$dorun-class-table*
  (make-static-class-decls 
   *clojure.core$dorun*))

(defconst *package-name-map* 
  ("clojure.core$dorun" . "clojure"))

