; zip$append_child-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:59 CDT 2014.
;

(defconst *clojure.zip$append_child*
 (make-class-def
      '(class "clojure.zip$append_child"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.zip")
                        (STRING  "replace")
                        (STRING  "make-node")
                        (STRING  "node")
                        (STRING  "clojure.core")
                        (STRING  "concat")
                        (STRING  "children"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 66)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.zip"
                                      (2 (ldc 1))         ;;STRING:: "replace"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.zip$append_child" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.zip"
                                      (15 (ldc 2))        ;;STRING:: "make-node"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.zip$append_child" (class "clojure.lang.Var"))))
                                      (26 (ldc 0))        ;;STRING:: "clojure.zip"
                                      (28 (ldc 3))        ;;STRING:: "node"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.zip$append_child" (class "clojure.lang.Var"))))
                                      (39 (ldc 4))        ;;STRING:: "clojure.core"
                                      (41 (ldc 5))        ;;STRING:: "concat"
                                      (43 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (46 (checkcast (class "clojure.lang.Var")))
                                      (49 (putstatic (fieldCP "const__3" "clojure.zip$append_child" (class "clojure.lang.Var"))))
                                      (52 (ldc 0))        ;;STRING:: "clojure.zip"
                                      (54 (ldc 6))        ;;STRING:: "children"
                                      (56 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (59 (checkcast (class "clojure.lang.Var")))
                                      (62 (putstatic (fieldCP "const__4" "clojure.zip$append_child" (class "clojure.lang.Var"))))
                                      (65 (return))
                                      (endofcode 66))
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
                                   (max_stack . 12) (max_locals . 3) (code_length . 90)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.zip$append_child" (class "clojure.lang.Var"))))
                                      (3 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (6 (checkcast (class "clojure.lang.IFn")))
                                      (9 (aload_1))
                                      (10 (getstatic (fieldCP "const__1" "clojure.zip$append_child" (class "clojure.lang.Var"))))
                                      (13 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (16 (checkcast (class "clojure.lang.IFn")))
                                      (19 (aload_1))
                                      (20 (getstatic (fieldCP "const__2" "clojure.zip$append_child" (class "clojure.lang.Var"))))
                                      (23 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (26 (checkcast (class "clojure.lang.IFn")))
                                      (29 (aload_1))
                                      (30 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (35 (getstatic (fieldCP "const__3" "clojure.zip$append_child" (class "clojure.lang.Var"))))
                                      (38 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (41 (checkcast (class "clojure.lang.IFn")))
                                      (44 (getstatic (fieldCP "const__4" "clojure.zip$append_child" (class "clojure.lang.Var"))))
                                      (47 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (50 (checkcast (class "clojure.lang.IFn")))
                                      (53 (aload_1))
                                      (54 (aconst_null))
                                      (55 (astore_1))
                                      (56 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (61 (iconst_1))
                                      (62 (anewarray (class "java.lang.Object")))
                                      (65 (dup))
                                      (66 (iconst_0))
                                      (67 (aload_2))
                                      (68 (aconst_null))
                                      (69 (astore_2))
                                      (70 (aastore))
                                      (71 (invokestatic
					(methodCP "vector" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentVector"))))
                                      (74 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3))
                                      (79 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4))
                                      (84 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3))
                                      (89 (areturn))
                                      (endofcode 90))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *zip$append_child-class-table*
  (make-static-class-decls 
   *clojure.zip$append_child*))

(defconst *package-name-map* 
  ("clojure.zip$append_child" . "clojure"))
