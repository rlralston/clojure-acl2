; core$drop$step__4240-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:41 CDT 2014.
;

(defconst *clojure.core$drop$step__4240*
 (make-class-def
      '(class "clojure.core$drop$step__4240"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "seq")
                        (STRING  "pos?")
                        (STRING  "dec")
                        (STRING  "rest"))
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
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$drop$step__4240" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "pos?"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.core$drop$step__4240" (class "clojure.lang.Var"))))
                                      (26 (ldc 0))        ;;STRING:: "clojure.core"
                                      (28 (ldc 3))        ;;STRING:: "dec"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.core$drop$step__4240" (class "clojure.lang.Var"))))
                                      (39 (ldc 0))        ;;STRING:: "clojure.core"
                                      (41 (ldc 4))        ;;STRING:: "rest"
                                      (43 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (46 (checkcast (class "clojure.lang.Var")))
                                      (49 (putstatic (fieldCP "const__3" "clojure.core$drop$step__4240" (class "clojure.lang.Var"))))
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
                                   (max_stack . 4) (max_locals . 5) (code_length . 94)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.core$drop$step__4240" (class "clojure.lang.Var")))) ;;at TAG_5
                                      (3 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (6 (checkcast (class "clojure.lang.IFn"))) 
                                      (9 (aload_2)) 
                                      (10 (aconst_null)) 
                                      (11 (astore_2)) 
                                      (12 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (17 (astore_3)) 
                                      (18 (aload_1)) 
                                      (19 (invokestatic (methodCP "isPos" "clojure.lang.Numbers" ((class "java.lang.Object")) boolean))) 
                                      (22 (istore 4)) 
                                      (24 (iload 4)) 
                                      (26 (ifeq 34)) ;;to TAG_0
                                      (29 (aload_3)) 
                                      (30 (goto 48)) ;;to TAG_1
                                      (33 (pop)) 
                                      (34 (iload 4)) ;;at TAG_0
                                      (36 (ifeq 45))  ;;to TAG_2
                                      (39 (getstatic (fieldCP "TRUE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (42 (goto 48)) ;;to TAG_1
                                      (45 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) ;;at TAG_2
                                      (48 (dup)) ;;at TAG_1
                                      (49 (ifnull 89)) ;;to TAG_3
                                      (52 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (55 (if_acmpeq 90)) ;;to TAG_4
                                      (58 (aload_1)) 
                                      (59 (aconst_null)) 
                                      (60 (astore_1)) 
                                      (61 (invokestatic (methodCP "dec" "clojure.lang.Numbers" ((class "java.lang.Object")) (class "java.lang.Number")))) 
                                      (64 (getstatic (fieldCP "const__3" "clojure.core$drop$step__4240" (class "clojure.lang.Var")))) 
                                      (67 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (70 (checkcast (class "clojure.lang.IFn"))) 
                                      (73 (aload_3)) 
                                      (74 (aconst_null)) 
                                      (75 (astore_3)) 
                                      (76 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (81 (astore_2)) 
                                      (82 (astore_1)) 
                                      (83 (goto 0)) ;;to TAG_5
                                      (86 (goto 93)) ;;to TAG_6
                                      (89 (pop)) ;;at TAG_3
                                      (90 (aload_3)) ;;at TAG_4
                                      (91 (aconst_null)) 
                                      (92 (astore_3)) 
                                      (93 (areturn)) ;;at TAG_6
                                      (endofcode 94))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$drop$step__4240-class-table*
  (make-static-class-decls 
   *clojure.core$drop$step__4240*))

(defconst *package-name-map* 
  ("clojure.core$drop$step__4240" . "clojure"))

