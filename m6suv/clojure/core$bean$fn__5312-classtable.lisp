; core$bean$fn__5312-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:40 CDT 2014.
;

(defconst *clojure.core$bean$fn__5312*
 (make-class-def
      '(class "clojure.core$bean$fn__5312"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "zero?")
                        (STRING  "alength")
                        (STRING  "assoc")
                        (STRING  "keyword"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "x" (class "java.lang.Object") (accessflags  *class* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 53)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "zero?"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$bean$fn__5312" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "alength"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.core$bean$fn__5312" (class "clojure.lang.Var"))))
                                      (26 (ldc 0))        ;;STRING:: "clojure.core"
                                      (28 (ldc 3))        ;;STRING:: "assoc"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.core$bean$fn__5312" (class "clojure.lang.Var"))))
                                      (39 (ldc 0))        ;;STRING:: "clojure.core"
                                      (41 (ldc 4))        ;;STRING:: "keyword"
                                      (43 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (46 (checkcast (class "clojure.lang.Var")))
                                      (49 (putstatic (fieldCP "const__3" "clojure.core$bean$fn__5312" (class "clojure.lang.Var"))))
                                      (52 (return))
                                      (endofcode 53))
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
                                      (6 (putfield (fieldCP "x" "clojure.core$bean$fn__5312" (class "java.lang.Object"))))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 9) (max_locals . 6) (code_length . 141)
                                   (parsedcode
                                      (0 (aload_2)) 
                                      (1 (checkcast (class "java.beans.FeatureDescriptor"))) 
                                      (4 (invokevirtual (methodCP "getName" "java.beans.FeatureDescriptor" () (class "java.lang.String")))) 
                                      (7 (astore_3)) 
                                      (8 (aload_2)) 
                                      (9 (checkcast (class "java.beans.PropertyDescriptor"))) 
                                      (12 (invokevirtual (methodCP "getReadMethod" "java.beans.PropertyDescriptor" () (class "java.lang.reflect.Method")))) 
                                      (15 (astore 4)) 
                                      (17 (aload 4)) 
                                      (19 (astore 5)) 
                                      (21 (aload 5)) 
                                      (23 (dup)) 
                                      (24 (ifnull 64)) ;;to TAG_0
                                      (27 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (30 (if_acmpeq 65)) ;;to TAG_1
                                      (33 (aload 4)) 
                                      (35 (checkcast (class "java.lang.reflect.Method"))) 
                                      (38 (invokevirtual (methodCP "getParameterTypes" "java.lang.reflect.Method" () (array (class "java.lang.Class"))))) 
                                      (41 (checkcast (array (class "java.lang.Object")))) 
                                      (44 (arraylength)) 
                                      (45 (i2l)) 
                                      (46 (invokestatic (methodCP "isZero" "clojure.lang.Numbers" (long) boolean))) 
                                      (49 (ifeq 58))  ;;to TAG_2
                                      (52 (getstatic (fieldCP "TRUE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (55 (goto 61)) ;;to TAG_3
                                      (58 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) ;;at TAG_2
                                      (61 (goto 70)) ;;to TAG_4;;at TAG_3
                                      (64 (pop)) ;;at TAG_0
                                      (65 (aload 5)) ;;at TAG_1
                                      (67 (aconst_null)) 
                                      (68 (astore 5)) 
                                      (70 (dup)) ;;at TAG_4
                                      (71 (ifnull 136)) ;;to TAG_5
                                      (74 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (77 (if_acmpeq 137)) ;;to TAG_6
                                      (80 (getstatic (fieldCP "const__2" "clojure.core$bean$fn__5312" (class "clojure.lang.Var")))) 
                                      (83 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (86 (checkcast (class "clojure.lang.IFn"))) 
                                      (89 (aload_1)) 
                                      (90 (aconst_null)) 
                                      (91 (astore_1)) 
                                      (92 (getstatic (fieldCP "const__3" "clojure.core$bean$fn__5312" (class "clojure.lang.Var")))) 
                                      (95 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (98 (checkcast (class "clojure.lang.IFn"))) 
                                      (101 (aload_3)) 
                                      (102 (aconst_null)) 
                                      (103 (astore_3)) 
                                      (104 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (109 (new (class "clojure.core$bean$fn__5312$fn__5313"))) 
                                      (112 (dup)) 
                                      (113 (aload_0)) 
                                      (114 (getfield (fieldCP "x" "clojure.core$bean$fn__5312" (class "java.lang.Object")))) 
                                      (117 (aload 4)) 
                                      (119 (aconst_null)) 
                                      (120 (astore 4)) 
                                      (122 (aload_2)) 
                                      (123 (aconst_null)) 
                                      (124 (astore_2)) 
                                      (125 (invokespecial (methodCP "<init>" "clojure.core$bean$fn__5312$fn__5313" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) void))) 
                                      (128 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4)) 
                                      (133 (goto 140)) ;;to TAG_7
                                      (136 (pop)) ;;at TAG_5
                                      (137 (aload_1)) ;;at TAG_6
                                      (138 (aconst_null)) 
                                      (139 (astore_1)) 
                                      (140 (areturn)) ;;at TAG_7
                                      (endofcode 141))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$bean$fn__5312-class-table*
  (make-static-class-decls 
   *clojure.core$bean$fn__5312*))

(defconst *package-name-map* 
  ("clojure.core$bean$fn__5312" . "clojure"))
