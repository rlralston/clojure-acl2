; core$maybe_min_hash-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:44 CDT 2014.
;

(defconst *clojure.core$maybe_min_hash*
 (make-class-def
      '(class "clojure.core$maybe_min_hash"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "first")
                        (STRING  "filter")
                        (STRING  "map")
                        (STRING  "range")
                        (STRING  "inc")
                        (STRING  "max-mask-bits"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__6" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 86)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "first"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$maybe_min_hash" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "filter"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.core$maybe_min_hash" (class "clojure.lang.Var"))))
                                      (26 (ldc 0))        ;;STRING:: "clojure.core"
                                      (28 (ldc 3))        ;;STRING:: "map"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.core$maybe_min_hash" (class "clojure.lang.Var"))))
                                      (39 (ldc 0))        ;;STRING:: "clojure.core"
                                      (41 (ldc 4))        ;;STRING:: "range"
                                      (43 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (46 (checkcast (class "clojure.lang.Var")))
                                      (49 (putstatic (fieldCP "const__3" "clojure.core$maybe_min_hash" (class "clojure.lang.Var"))))
                                      (52 (lconst_1))
                                      (53 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (56 (putstatic (fieldCP "const__4" "clojure.core$maybe_min_hash" (class "java.lang.Object"))))
                                      (59 (ldc 0))        ;;STRING:: "clojure.core"
                                      (61 (ldc 5))        ;;STRING:: "inc"
                                      (63 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (66 (checkcast (class "clojure.lang.Var")))
                                      (69 (putstatic (fieldCP "const__5" "clojure.core$maybe_min_hash" (class "clojure.lang.Var"))))
                                      (72 (ldc 0))        ;;STRING:: "clojure.core"
                                      (74 (ldc 6))        ;;STRING:: "max-mask-bits"
                                      (76 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (79 (checkcast (class "clojure.lang.Var")))
                                      (82 (putstatic (fieldCP "const__6" "clojure.core$maybe_min_hash" (class "clojure.lang.Var"))))
                                      (85 (return))
                                      (endofcode 86))
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
                                   (max_stack . 9) (max_locals . 3) (code_length . 105)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.core$maybe_min_hash" (class "clojure.lang.Var"))))
                                      (3 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (6 (checkcast (class "clojure.lang.IFn")))
                                      (9 (getstatic (fieldCP "const__1" "clojure.core$maybe_min_hash" (class "clojure.lang.Var"))))
                                      (12 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (15 (checkcast (class "clojure.lang.IFn")))
                                      (18 (new (class "clojure.core$maybe_min_hash$fn__5077")))
                                      (21 (dup))
                                      (22 (aload_1))
                                      (23 (aconst_null))
                                      (24 (astore_1))
                                      (25 (invokespecial
					(methodCP "<init>" "clojure.core$maybe_min_hash$fn__5077" ((class "java.lang.Object")) void)))
                                      (28 (new (class "clojure.core$maybe_min_hash$iter__5082__5088")))
                                      (31 (dup))
                                      (32 (invokespecial
					(methodCP "<init>" "clojure.core$maybe_min_hash$iter__5082__5088" () void)))
                                      (35 (astore_2))
                                      (36 (aload_2))
                                      (37 (aconst_null))
                                      (38 (astore_2))
                                      (39 (checkcast (class "clojure.lang.IFn")))
                                      (42 (getstatic (fieldCP "const__2" "clojure.core$maybe_min_hash" (class "clojure.lang.Var"))))
                                      (45 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (48 (checkcast (class "clojure.lang.IFn")))
                                      (51 (new (class "clojure.core$maybe_min_hash$fn__5105")))
                                      (54 (dup))
                                      (55 (invokespecial
					(methodCP "<init>" "clojure.core$maybe_min_hash$fn__5105" () void)))
                                      (58 (getstatic (fieldCP "const__3" "clojure.core$maybe_min_hash" (class "clojure.lang.Var"))))
                                      (61 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (64 (checkcast (class "clojure.lang.IFn")))
                                      (67 (getstatic (fieldCP "const__4" "clojure.core$maybe_min_hash" (class "java.lang.Object"))))
                                      (70 (getstatic (fieldCP "const__6" "clojure.core$maybe_min_hash" (class "clojure.lang.Var"))))
                                      (73 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (76 (invokestatic
					(methodCP "inc" "clojure.lang.Numbers" ((class "java.lang.Object")) (class "java.lang.Number"))))
                                      (79 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3))
                                      (84 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3))
                                      (89 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (94 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3))
                                      (99 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (104 (areturn))
                                      (endofcode 105))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$maybe_min_hash-class-table*
  (make-static-class-decls 
   *clojure.core$maybe_min_hash*))

(defconst *package-name-map* 
  ("clojure.core$maybe_min_hash" . "clojure"))

