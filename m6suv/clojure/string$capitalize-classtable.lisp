; string$capitalize-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:58 CDT 2014.
;

(defconst *clojure.string$capitalize*
 (make-class-def
      '(class "clojure.string$capitalize"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "<")
                        (STRING  "count")
                        (LONG 2)
                        (STRING  "str")
                        (STRING  "subs"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__6" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 76)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "<"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.string$capitalize" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "count"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.string$capitalize" (class "clojure.lang.Var"))))
                                      (26 (ldc2_w 3))     ;; LONG:: "2"
                                      (29 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (32 (putstatic (fieldCP "const__2" "clojure.string$capitalize" (class "java.lang.Object"))))
                                      (35 (ldc 0))        ;;STRING:: "clojure.core"
                                      (37 (ldc 4))        ;;STRING:: "str"
                                      (39 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (42 (checkcast (class "clojure.lang.Var")))
                                      (45 (putstatic (fieldCP "const__3" "clojure.string$capitalize" (class "clojure.lang.Var"))))
                                      (48 (ldc 0))        ;;STRING:: "clojure.core"
                                      (50 (ldc 5))        ;;STRING:: "subs"
                                      (52 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (55 (checkcast (class "clojure.lang.Var")))
                                      (58 (putstatic (fieldCP "const__4" "clojure.string$capitalize" (class "clojure.lang.Var"))))
                                      (61 (lconst_0))
                                      (62 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (65 (putstatic (fieldCP "const__5" "clojure.string$capitalize" (class "java.lang.Object"))))
                                      (68 (lconst_1))
                                      (69 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (72 (putstatic (fieldCP "const__6" "clojure.string$capitalize" (class "java.lang.Object"))))
                                      (75 (return))
                                      (endofcode 76))
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
                                   (max_stack . 5) (max_locals . 3) (code_length . 100)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (aconst_null)) 
                                      (2 (astore_1)) 
                                      (3 (invokevirtual (methodCP "toString" "java.lang.Object" () (class "java.lang.String")))) 
                                      (6 (astore_2)) 
                                      (7 (aload_2)) 
                                      (8 (invokestatic (methodCP "count" "clojure.lang.RT" ((class "java.lang.Object")) int))) 
                                      (11 (i2l)) 
                                      (12 (ldc2_w 3)) ;; LONG:: "2"
                                      (15 (lcmp)) 
                                      (16 (ifge 32))  ;;to TAG_0
                                      (19 (aload_2)) 
                                      (20 (aconst_null)) 
                                      (21 (astore_2)) 
                                      (22 (checkcast (class "java.lang.String"))) 
                                      (25 (invokevirtual (methodCP "toUpperCase" "java.lang.String" () (class "java.lang.String")))) 
                                      (28 (goto 99)) ;;to TAG_1
                                      (31 (pop)) 
                                      (32 (getstatic (fieldCP "const__3" "clojure.string$capitalize" (class "clojure.lang.Var")))) ;;at TAG_0
                                      (35 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (38 (checkcast (class "clojure.lang.IFn"))) 
                                      (41 (getstatic (fieldCP "const__4" "clojure.string$capitalize" (class "clojure.lang.Var")))) 
                                      (44 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (47 (checkcast (class "clojure.lang.IFn"))) 
                                      (50 (aload_2)) 
                                      (51 (getstatic (fieldCP "const__5" "clojure.string$capitalize" (class "java.lang.Object")))) 
                                      (54 (getstatic (fieldCP "const__6" "clojure.string$capitalize" (class "java.lang.Object")))) 
                                      (57 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4)) 
                                      (62 (checkcast (class "java.lang.String"))) 
                                      (65 (invokevirtual (methodCP "toUpperCase" "java.lang.String" () (class "java.lang.String")))) 
                                      (68 (getstatic (fieldCP "const__4" "clojure.string$capitalize" (class "clojure.lang.Var")))) 
                                      (71 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (74 (checkcast (class "clojure.lang.IFn"))) 
                                      (77 (aload_2)) 
                                      (78 (aconst_null)) 
                                      (79 (astore_2)) 
                                      (80 (getstatic (fieldCP "const__6" "clojure.string$capitalize" (class "java.lang.Object")))) 
                                      (83 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (88 (checkcast (class "java.lang.String"))) 
                                      (91 (invokevirtual (methodCP "toLowerCase" "java.lang.String" () (class "java.lang.String")))) 
                                      (94 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (99 (areturn)) ;;at TAG_1
                                      (endofcode 100))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *string$capitalize-class-table*
  (make-static-class-decls 
   *clojure.string$capitalize*))

(defconst *package-name-map* 
  ("clojure.string$capitalize" . "clojure"))

