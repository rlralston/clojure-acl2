; reflect$constructor__GT_map-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:57 CDT 2014.
;

(defconst *clojure.reflect$constructor__GT_map*
 (make-class-def
      '(class "clojure.reflect$constructor__GT_map"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "symbol")
                        (STRING  "clojure.reflect")
                        (STRING  "typesym")
                        (STRING  "vec")
                        (STRING  "map")
                        (STRING  "parse-flags")
                        (STRING  "method"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 78)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "symbol"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.reflect$constructor__GT_map" (class "clojure.lang.Var"))))
                                      (13 (ldc 2))        ;;STRING:: "clojure.reflect"
                                      (15 (ldc 3))        ;;STRING:: "typesym"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.reflect$constructor__GT_map" (class "clojure.lang.Var"))))
                                      (26 (ldc 0))        ;;STRING:: "clojure.core"
                                      (28 (ldc 4))        ;;STRING:: "vec"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.reflect$constructor__GT_map" (class "clojure.lang.Var"))))
                                      (39 (ldc 0))        ;;STRING:: "clojure.core"
                                      (41 (ldc 5))        ;;STRING:: "map"
                                      (43 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (46 (checkcast (class "clojure.lang.Var")))
                                      (49 (putstatic (fieldCP "const__3" "clojure.reflect$constructor__GT_map" (class "clojure.lang.Var"))))
                                      (52 (ldc 2))        ;;STRING:: "clojure.reflect"
                                      (54 (ldc 6))        ;;STRING:: "parse-flags"
                                      (56 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (59 (checkcast (class "clojure.lang.Var")))
                                      (62 (putstatic (fieldCP "const__4" "clojure.reflect$constructor__GT_map" (class "clojure.lang.Var"))))
                                      (65 (aconst_null))
                                      (66 (ldc 7))        ;;STRING:: "method"
                                      (68 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (71 (checkcast (class "clojure.lang.Keyword")))
                                      (74 (putstatic (fieldCP "const__5" "clojure.reflect$constructor__GT_map" (class "clojure.lang.Keyword"))))
                                      (77 (return))
                                      (endofcode 78))
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
                                   (max_stack . 9) (max_locals . 2) (code_length . 161)
                                   (parsedcode
                                      (0 (new (class "clojure.reflect.Constructor")))
                                      (3 (dup))
                                      (4 (getstatic (fieldCP "const__0" "clojure.reflect$constructor__GT_map" (class "clojure.lang.Var"))))
                                      (7 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (10 (checkcast (class "clojure.lang.IFn")))
                                      (13 (aload_1))
                                      (14 (checkcast (class "java.lang.reflect.Constructor")))
                                      (17 (invokevirtual
					(methodCP "getName" "java.lang.reflect.Constructor" () (class "java.lang.String"))))
                                      (20 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (25 (getstatic (fieldCP "const__1" "clojure.reflect$constructor__GT_map" (class "clojure.lang.Var"))))
                                      (28 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (31 (checkcast (class "clojure.lang.IFn")))
                                      (34 (aload_1))
                                      (35 (checkcast (class "java.lang.reflect.Constructor")))
                                      (38 (invokevirtual
					(methodCP "getDeclaringClass" "java.lang.reflect.Constructor" () (class "java.lang.Class"))))
                                      (41 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (46 (getstatic (fieldCP "const__2" "clojure.reflect$constructor__GT_map" (class "clojure.lang.Var"))))
                                      (49 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (52 (checkcast (class "clojure.lang.IFn")))
                                      (55 (getstatic (fieldCP "const__3" "clojure.reflect$constructor__GT_map" (class "clojure.lang.Var"))))
                                      (58 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (61 (checkcast (class "clojure.lang.IFn")))
                                      (64 (getstatic (fieldCP "const__1" "clojure.reflect$constructor__GT_map" (class "clojure.lang.Var"))))
                                      (67 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (70 (aload_1))
                                      (71 (checkcast (class "java.lang.reflect.Constructor")))
                                      (74 (invokevirtual
					(methodCP "getParameterTypes" "java.lang.reflect.Constructor" () (array (class "java.lang.Class")))))
                                      (77 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3))
                                      (82 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (87 (getstatic (fieldCP "const__2" "clojure.reflect$constructor__GT_map" (class "clojure.lang.Var"))))
                                      (90 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (93 (checkcast (class "clojure.lang.IFn")))
                                      (96 (getstatic (fieldCP "const__3" "clojure.reflect$constructor__GT_map" (class "clojure.lang.Var"))))
                                      (99 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (102 (checkcast (class "clojure.lang.IFn")))
                                      (105 (getstatic (fieldCP "const__1" "clojure.reflect$constructor__GT_map" (class "clojure.lang.Var"))))
                                      (108 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (111 (aload_1))
                                      (112 (checkcast (class "java.lang.reflect.Constructor")))
                                      (115 (invokevirtual
					(methodCP "getExceptionTypes" "java.lang.reflect.Constructor" () (array (class "java.lang.Class")))))
                                      (118 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3))
                                      (123 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (128 (getstatic (fieldCP "const__4" "clojure.reflect$constructor__GT_map" (class "clojure.lang.Var"))))
                                      (131 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (134 (checkcast (class "clojure.lang.IFn")))
                                      (137 (aload_1))
                                      (138 (aconst_null))
                                      (139 (astore_1))
                                      (140 (checkcast (class "java.lang.reflect.Constructor")))
                                      (143 (invokevirtual
					(methodCP "getModifiers" "java.lang.reflect.Constructor" () int)))
                                      (146 (invokestatic
					(methodCP "valueOf" "java.lang.Integer" (int) (class "java.lang.Integer"))))
                                      (149 (getstatic (fieldCP "const__5" "clojure.reflect$constructor__GT_map" (class "clojure.lang.Keyword"))))
                                      (152 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3))
                                      (157 (invokespecial
					(methodCP "<init>" "clojure.reflect.Constructor" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) void)))
                                      (160 (areturn))
                                      (endofcode 161))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *reflect$constructor__GT_map-class-table*
  (make-static-class-decls 
   *clojure.reflect$constructor__GT_map*))

(defconst *package-name-map* 
  ("clojure.reflect$constructor__GT_map" . "clojure"))
