; core$pmap-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:45 CDT 2014.
;

(defconst *clojure.core$pmap*
 (make-class-def
      '(class "clojure.core$pmap"
            "clojure.lang.RestFn"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "+")
                        (LONG 2)
                        (STRING  "map")
                        (STRING  "drop")
                        (STRING  "pmap")
                        (STRING  "cons"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 75)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "+"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$pmap" (class "clojure.lang.Var"))))
                                      (13 (ldc2_w 2))     ;; LONG:: "2"
                                      (16 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (19 (putstatic (fieldCP "const__1" "clojure.core$pmap" (class "java.lang.Object"))))
                                      (22 (ldc 0))        ;;STRING:: "clojure.core"
                                      (24 (ldc 3))        ;;STRING:: "map"
                                      (26 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (29 (checkcast (class "clojure.lang.Var")))
                                      (32 (putstatic (fieldCP "const__2" "clojure.core$pmap" (class "clojure.lang.Var"))))
                                      (35 (ldc 0))        ;;STRING:: "clojure.core"
                                      (37 (ldc 4))        ;;STRING:: "drop"
                                      (39 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (42 (checkcast (class "clojure.lang.Var")))
                                      (45 (putstatic (fieldCP "const__3" "clojure.core$pmap" (class "clojure.lang.Var"))))
                                      (48 (ldc 0))        ;;STRING:: "clojure.core"
                                      (50 (ldc 5))        ;;STRING:: "pmap"
                                      (52 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (55 (checkcast (class "clojure.lang.Var")))
                                      (58 (putstatic (fieldCP "const__4" "clojure.core$pmap" (class "clojure.lang.Var"))))
                                      (61 (ldc 0))        ;;STRING:: "clojure.core"
                                      (63 (ldc 6))        ;;STRING:: "cons"
                                      (65 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (68 (checkcast (class "clojure.lang.Var")))
                                      (71 (putstatic (fieldCP "const__5" "clojure.core$pmap" (class "clojure.lang.Var"))))
                                      (74 (return))
                                      (endofcode 75))
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
					(methodCP "<init>" "clojure.lang.RestFn" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "doInvoke"
                              (parameters (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 7) (max_locals . 5) (code_length . 67)
                                   (parsedcode
                                      (0 (new (class "clojure.core$pmap$step__6286")))
                                      (3 (dup))
                                      (4 (invokespecial
					(methodCP "<init>" "clojure.core$pmap$step__6286" () void)))
                                      (7 (astore 4))
                                      (9 (getstatic (fieldCP "const__4" "clojure.core$pmap" (class "clojure.lang.Var"))))
                                      (12 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (15 (checkcast (class "clojure.lang.IFn")))
                                      (18 (new (class "clojure.core$pmap$fn__6290")))
                                      (21 (dup))
                                      (22 (aload_1))
                                      (23 (aconst_null))
                                      (24 (astore_1))
                                      (25 (invokespecial
					(methodCP "<init>" "clojure.core$pmap$fn__6290" ((class "java.lang.Object")) void)))
                                      (28 (aload 4))
                                      (30 (aconst_null))
                                      (31 (astore 4))
                                      (33 (checkcast (class "clojure.lang.IFn")))
                                      (36 (getstatic (fieldCP "const__5" "clojure.core$pmap" (class "clojure.lang.Var"))))
                                      (39 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (42 (checkcast (class "clojure.lang.IFn")))
                                      (45 (aload_2))
                                      (46 (aconst_null))
                                      (47 (astore_2))
                                      (48 (aload_3))
                                      (49 (aconst_null))
                                      (50 (astore_3))
                                      (51 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3))
                                      (56 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (61 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3))
                                      (66 (areturn))
                                      (endofcode 67))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 7) (code_length . 94)
                                   (parsedcode
                                      (0 (ldc2_w 2))      ;; LONG:: "2"
                                      (3 (invokestatic
					(methodCP "getRuntime" "java.lang.Runtime" () (class "java.lang.Runtime"))))
                                      (6 (checkcast (class "java.lang.Runtime")))
                                      (9 (invokevirtual
					(methodCP "availableProcessors" "java.lang.Runtime" () int)))
                                      (12 (i2l))
                                      (13 (invokestatic
					(methodCP "add" "clojure.lang.Numbers" (long long) long)))
                                      (16 (lstore_3))
                                      (17 (getstatic (fieldCP "const__2" "clojure.core$pmap" (class "clojure.lang.Var"))))
                                      (20 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (23 (checkcast (class "clojure.lang.IFn")))
                                      (26 (new (class "clojure.core$pmap$fn__6275")))
                                      (29 (dup))
                                      (30 (aload_1))
                                      (31 (aconst_null))
                                      (32 (astore_1))
                                      (33 (invokespecial
					(methodCP "<init>" "clojure.core$pmap$fn__6275" ((class "java.lang.Object")) void)))
                                      (36 (aload_2))
                                      (37 (aconst_null))
                                      (38 (astore_2))
                                      (39 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3))
                                      (44 (astore 5))
                                      (46 (new (class "clojure.core$pmap$step__6280")))
                                      (49 (dup))
                                      (50 (invokespecial
					(methodCP "<init>" "clojure.core$pmap$step__6280" () void)))
                                      (53 (astore 6))
                                      (55 (aload 6))
                                      (57 (aconst_null))
                                      (58 (astore 6))
                                      (60 (checkcast (class "clojure.lang.IFn")))
                                      (63 (aload 5))
                                      (65 (getstatic (fieldCP "const__3" "clojure.core$pmap" (class "clojure.lang.Var"))))
                                      (68 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (71 (checkcast (class "clojure.lang.IFn")))
                                      (74 (lload_3))
                                      (75 (invokestatic
					(methodCP "num" "clojure.lang.Numbers" (long) (class "java.lang.Number"))))
                                      (78 (aload 5))
                                      (80 (aconst_null))
                                      (81 (astore 5))
                                      (83 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3))
                                      (88 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3))
                                      (93 (areturn))
                                      (endofcode 94))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getRequiredArity"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 2)
                                   (parsedcode
                                      (0 (iconst_2))
                                      (1 (ireturn))
                                      (endofcode 2))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$pmap-class-table*
  (make-static-class-decls 
   *clojure.core$pmap*))

(defconst *package-name-map* 
  ("clojure.core$pmap" . "clojure"))

