; core$fn__5449-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:42 CDT 2014.
;

(defconst *clojure.core$fn__5449*
 (make-class-def
      '(class "clojure.core$fn__5449"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "print-meta")
                        (STRING  "class")
                        (STRING  "print-map")
                        (STRING  "pr-on")
                        (STRING  "#"))
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
                                      (2 (ldc 1))         ;;STRING:: "print-meta"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$fn__5449" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "class"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.core$fn__5449" (class "clojure.lang.Var"))))
                                      (26 (ldc 0))        ;;STRING:: "clojure.core"
                                      (28 (ldc 3))        ;;STRING:: "print-map"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.core$fn__5449" (class "clojure.lang.Var"))))
                                      (39 (ldc 0))        ;;STRING:: "clojure.core"
                                      (41 (ldc 4))        ;;STRING:: "pr-on"
                                      (43 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (46 (checkcast (class "clojure.lang.Var")))
                                      (49 (putstatic (fieldCP "const__3" "clojure.core$fn__5449" (class "clojure.lang.Var"))))
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
                                   (max_stack . 5) (max_locals . 3) (code_length . 91)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.core$fn__5449" (class "clojure.lang.Var"))))
                                      (3 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (6 (checkcast (class "clojure.lang.IFn")))
                                      (9 (aload_1))
                                      (10 (aload_2))
                                      (11 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3))
                                      (16 (pop))
                                      (17 (aload_2))
                                      (18 (checkcast (class "java.io.Writer")))
                                      (21 (ldc 5))        ;;STRING:: "#"
                                      (23 (checkcast (class "java.lang.String")))
                                      (26 (invokevirtual
					(methodCP "write" "java.io.Writer" ((class "java.lang.String")) void)))
                                      (29 (aconst_null))
                                      (30 (pop))
                                      (31 (aload_2))
                                      (32 (checkcast (class "java.io.Writer")))
                                      (35 (getstatic (fieldCP "const__1" "clojure.core$fn__5449" (class "clojure.lang.Var"))))
                                      (38 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (41 (checkcast (class "clojure.lang.IFn")))
                                      (44 (aload_1))
                                      (45 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (50 (checkcast (class "java.lang.Class")))
                                      (53 (invokevirtual
					(methodCP "getName" "java.lang.Class" () (class "java.lang.String"))))
                                      (56 (checkcast (class "java.lang.String")))
                                      (59 (invokevirtual
					(methodCP "write" "java.io.Writer" ((class "java.lang.String")) void)))
                                      (62 (aconst_null))
                                      (63 (pop))
                                      (64 (getstatic (fieldCP "const__2" "clojure.core$fn__5449" (class "clojure.lang.Var"))))
                                      (67 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (70 (checkcast (class "clojure.lang.IFn")))
                                      (73 (aload_1))
                                      (74 (aconst_null))
                                      (75 (astore_1))
                                      (76 (getstatic (fieldCP "const__3" "clojure.core$fn__5449" (class "clojure.lang.Var"))))
                                      (79 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (82 (aload_2))
                                      (83 (aconst_null))
                                      (84 (astore_2))
                                      (85 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4))
                                      (90 (areturn))
                                      (endofcode 91))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$fn__5449-class-table*
  (make-static-class-decls 
   *clojure.core$fn__5449*))

(defconst *package-name-map* 
  ("clojure.core$fn__5449" . "clojure"))
