; core$fn__5418-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:42 CDT 2014.
;

(defconst *clojure.core$fn__5418*
 (make-class-def
      '(class "clojure.core$fn__5418"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "print-meta")
                        (STRING  "class")
                        (STRING  "print-sequential")
                        (STRING  "print-dup")
                        (STRING  "#=(")
                        (STRING  "/create ")
                        (STRING  "[")
                        (STRING  " ")
                        (STRING  "]")
                        (STRING  ")"))
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
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$fn__5418" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "class"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.core$fn__5418" (class "clojure.lang.Var"))))
                                      (26 (ldc 0))        ;;STRING:: "clojure.core"
                                      (28 (ldc 3))        ;;STRING:: "print-sequential"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.core$fn__5418" (class "clojure.lang.Var"))))
                                      (39 (ldc 0))        ;;STRING:: "clojure.core"
                                      (41 (ldc 4))        ;;STRING:: "print-dup"
                                      (43 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (46 (checkcast (class "clojure.lang.Var")))
                                      (49 (putstatic (fieldCP "const__3" "clojure.core$fn__5418" (class "clojure.lang.Var"))))
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
                                   (max_stack . 7) (max_locals . 3) (code_length . 125)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.core$fn__5418" (class "clojure.lang.Var"))))
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
                                      (21 (ldc 5))        ;;STRING:: "#=("
                                      (23 (checkcast (class "java.lang.String")))
                                      (26 (invokevirtual
					(methodCP "write" "java.io.Writer" ((class "java.lang.String")) void)))
                                      (29 (aconst_null))
                                      (30 (pop))
                                      (31 (aload_2))
                                      (32 (checkcast (class "java.io.Writer")))
                                      (35 (getstatic (fieldCP "const__1" "clojure.core$fn__5418" (class "clojure.lang.Var"))))
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
                                      (64 (aload_2))
                                      (65 (checkcast (class "java.io.Writer")))
                                      (68 (ldc 6))        ;;STRING:: "/create "
                                      (70 (checkcast (class "java.lang.String")))
                                      (73 (invokevirtual
					(methodCP "write" "java.io.Writer" ((class "java.lang.String")) void)))
                                      (76 (aconst_null))
                                      (77 (pop))
                                      (78 (getstatic (fieldCP "const__2" "clojure.core$fn__5418" (class "clojure.lang.Var"))))
                                      (81 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (84 (checkcast (class "clojure.lang.IFn")))
                                      (87 (ldc 7))        ;;STRING:: "["
                                      (89 (getstatic (fieldCP "const__3" "clojure.core$fn__5418" (class "clojure.lang.Var"))))
                                      (92 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (95 (ldc 8))        ;;STRING:: " "
                                      (97 (ldc 9))        ;;STRING:: "]"
                                      (99 (aload_1))
                                      (100 (aconst_null))
                                      (101 (astore_1))
                                      (102 (aload_2))
                                      (103 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 7))
                                      (108 (pop))
                                      (109 (aload_2))
                                      (110 (aconst_null))
                                      (111 (astore_2))
                                      (112 (checkcast (class "java.io.Writer")))
                                      (115 (ldc 10))      ;;STRING:: ")"
                                      (117 (checkcast (class "java.lang.String")))
                                      (120 (invokevirtual
					(methodCP "write" "java.io.Writer" ((class "java.lang.String")) void)))
                                      (123 (aconst_null))
                                      (124 (areturn))
                                      (endofcode 125))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$fn__5418-class-table*
  (make-static-class-decls 
   *clojure.core$fn__5418*))

(defconst *package-name-map* 
  ("clojure.core$fn__5418" . "clojure"))

