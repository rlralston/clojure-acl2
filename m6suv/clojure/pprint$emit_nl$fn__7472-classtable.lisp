; pprint$emit_nl$fn__7472-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:55 CDT 2014.
;

(defconst *clojure.pprint$emit_nl$fn__7472*
 (make-class-def
      '(class "clojure.pprint$emit_nl$fn__7472"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "alter")
                        (STRING  "deref")
                        (STRING  "assoc")
                        (STRING  "trailing-white-space"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "this" (class "java.lang.Object") (accessflags  *class* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 52)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "alter"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.pprint$emit_nl$fn__7472" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "deref"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.pprint$emit_nl$fn__7472" (class "clojure.lang.Var"))))
                                      (26 (ldc 0))        ;;STRING:: "clojure.core"
                                      (28 (ldc 3))        ;;STRING:: "assoc"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.pprint$emit_nl$fn__7472" (class "clojure.lang.Var"))))
                                      (39 (aconst_null))
                                      (40 (ldc 4))        ;;STRING:: "trailing-white-space"
                                      (42 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (45 (checkcast (class "clojure.lang.Keyword")))
                                      (48 (putstatic (fieldCP "const__3" "clojure.pprint$emit_nl$fn__7472" (class "clojure.lang.Keyword"))))
                                      (51 (return))
                                      (endofcode 52))
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
                                      (6 (putfield (fieldCP "this" "clojure.pprint$emit_nl$fn__7472" (class "java.lang.Object"))))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 1) (code_length . 43)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.pprint$emit_nl$fn__7472" (class "clojure.lang.Var"))))
                                      (3 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (6 (checkcast (class "clojure.lang.IFn")))
                                      (9 (getstatic (fieldCP "const__1" "clojure.pprint$emit_nl$fn__7472" (class "clojure.lang.Var"))))
                                      (12 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (15 (checkcast (class "clojure.lang.IFn")))
                                      (18 (aload_0))
                                      (19 (getfield (fieldCP "this" "clojure.pprint$emit_nl$fn__7472" (class "java.lang.Object"))))
                                      (22 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (27 (getstatic (fieldCP "const__2" "clojure.pprint$emit_nl$fn__7472" (class "clojure.lang.Var"))))
                                      (30 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (33 (getstatic (fieldCP "const__3" "clojure.pprint$emit_nl$fn__7472" (class "clojure.lang.Keyword"))))
                                      (36 (aconst_null))
                                      (37 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 5))
                                      (42 (areturn))
                                      (endofcode 43))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *pprint$emit_nl$fn__7472-class-table*
  (make-static-class-decls 
   *clojure.pprint$emit_nl$fn__7472*))

(defconst *package-name-map* 
  ("clojure.pprint$emit_nl$fn__7472" . "clojure"))

