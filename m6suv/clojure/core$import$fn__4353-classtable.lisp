; core$import$fn__4353-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:44 CDT 2014.
;

(defconst *clojure.core$import$fn__4353*
 (make-class-def
      '(class "clojure.core$import$fn__4353"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "seq?")
                        (STRING  "=")
                        (STRING  "quote")
                        (STRING  "first")
                        (STRING  "second"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.AFn") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 65)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "seq?"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$import$fn__4353" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "="
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.core$import$fn__4353" (class "clojure.lang.Var"))))
                                      (26 (aconst_null))
                                      (27 (ldc 3))        ;;STRING:: "quote"
                                      (29 (invokestatic
					(methodCP "intern" "clojure.lang.Symbol" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Symbol"))))
                                      (32 (checkcast (class "clojure.lang.AFn")))
                                      (35 (putstatic (fieldCP "const__2" "clojure.core$import$fn__4353" (class "clojure.lang.AFn"))))
                                      (38 (ldc 0))        ;;STRING:: "clojure.core"
                                      (40 (ldc 4))        ;;STRING:: "first"
                                      (42 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (45 (checkcast (class "clojure.lang.Var")))
                                      (48 (putstatic (fieldCP "const__3" "clojure.core$import$fn__4353" (class "clojure.lang.Var"))))
                                      (51 (ldc 0))        ;;STRING:: "clojure.core"
                                      (53 (ldc 5))        ;;STRING:: "second"
                                      (55 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (58 (checkcast (class "clojure.lang.Var")))
                                      (61 (putstatic (fieldCP "const__4" "clojure.core$import$fn__4353" (class "clojure.lang.Var"))))
                                      (64 (return))
                                      (endofcode 65))
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
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 102)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.core$import$fn__4353" (class "clojure.lang.Var")))) 
                                      (3 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (6 (checkcast (class "clojure.lang.IFn"))) 
                                      (9 (aload_1)) 
                                      (10 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (15 (astore_2)) 
                                      (16 (aload_2)) 
                                      (17 (dup)) 
                                      (18 (ifnull 63)) ;;to TAG_0
                                      (21 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (24 (if_acmpeq 64)) ;;to TAG_1
                                      (27 (getstatic (fieldCP "const__2" "clojure.core$import$fn__4353" (class "clojure.lang.AFn")))) 
                                      (30 (getstatic (fieldCP "const__3" "clojure.core$import$fn__4353" (class "clojure.lang.Var")))) 
                                      (33 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (36 (checkcast (class "clojure.lang.IFn"))) 
                                      (39 (aload_1)) 
                                      (40 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (45 (invokestatic (methodCP "equiv" "clojure.lang.Util" ((class "java.lang.Object") (class "java.lang.Object")) boolean))) 
                                      (48 (ifeq 57))  ;;to TAG_2
                                      (51 (getstatic (fieldCP "TRUE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (54 (goto 60)) ;;to TAG_3
                                      (57 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) ;;at TAG_2
                                      (60 (goto 67)) ;;to TAG_4;;at TAG_3
                                      (63 (pop)) ;;at TAG_0
                                      (64 (aload_2)) ;;at TAG_1
                                      (65 (aconst_null)) 
                                      (66 (astore_2)) 
                                      (67 (dup)) ;;at TAG_4
                                      (68 (ifnull 97)) ;;to TAG_5
                                      (71 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (74 (if_acmpeq 98)) ;;to TAG_6
                                      (77 (getstatic (fieldCP "const__4" "clojure.core$import$fn__4353" (class "clojure.lang.Var")))) 
                                      (80 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (83 (checkcast (class "clojure.lang.IFn"))) 
                                      (86 (aload_1)) 
                                      (87 (aconst_null)) 
                                      (88 (astore_1)) 
                                      (89 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (94 (goto 101)) ;;to TAG_7
                                      (97 (pop)) ;;at TAG_5
                                      (98 (aload_1)) ;;at TAG_6
                                      (99 (aconst_null)) 
                                      (100 (astore_1)) 
                                      (101 (areturn)) ;;at TAG_7
                                      (endofcode 102))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$import$fn__4353-class-table*
  (make-static-class-decls 
   *clojure.core$import$fn__4353*))

(defconst *package-name-map* 
  ("clojure.core$import$fn__4353" . "clojure"))

