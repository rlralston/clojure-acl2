; repl$special_doc-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:58 CDT 2014.
;

(defconst *clojure.repl$special_doc*
 (make-class-def
      '(class "clojure.repl$special_doc"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "assoc")
                        (STRING  "clojure.repl")
                        (STRING  "special-doc-map")
                        (STRING  "meta")
                        (STRING  "resolve")
                        (STRING  "name")
                        (STRING  "special-form"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 77)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "assoc"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.repl$special_doc" (class "clojure.lang.Var"))))
                                      (13 (ldc 2))        ;;STRING:: "clojure.repl"
                                      (15 (ldc 3))        ;;STRING:: "special-doc-map"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.repl$special_doc" (class "clojure.lang.Var"))))
                                      (26 (ldc 0))        ;;STRING:: "clojure.core"
                                      (28 (ldc 4))        ;;STRING:: "meta"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.repl$special_doc" (class "clojure.lang.Var"))))
                                      (39 (ldc 0))        ;;STRING:: "clojure.core"
                                      (41 (ldc 5))        ;;STRING:: "resolve"
                                      (43 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (46 (checkcast (class "clojure.lang.Var")))
                                      (49 (putstatic (fieldCP "const__3" "clojure.repl$special_doc" (class "clojure.lang.Var"))))
                                      (52 (aconst_null))
                                      (53 (ldc 6))        ;;STRING:: "name"
                                      (55 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (58 (checkcast (class "clojure.lang.Keyword")))
                                      (61 (putstatic (fieldCP "const__4" "clojure.repl$special_doc" (class "clojure.lang.Keyword"))))
                                      (64 (aconst_null))
                                      (65 (ldc 7))        ;;STRING:: "special-form"
                                      (67 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (70 (checkcast (class "clojure.lang.Keyword")))
                                      (73 (putstatic (fieldCP "const__5" "clojure.repl$special_doc" (class "clojure.lang.Keyword"))))
                                      (76 (return))
                                      (endofcode 77))
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
                                   (max_stack . 6) (max_locals . 3) (code_length . 90)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.repl$special_doc" (class "clojure.lang.Var")))) 
                                      (3 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (6 (checkcast (class "clojure.lang.IFn"))) 
                                      (9 (getstatic (fieldCP "const__1" "clojure.repl$special_doc" (class "clojure.lang.Var")))) 
                                      (12 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (15 (checkcast (class "clojure.lang.IFn"))) 
                                      (18 (aload_1)) 
                                      (19 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (24 (astore_2)) 
                                      (25 (aload_2)) 
                                      (26 (dup)) 
                                      (27 (ifnull 42)) ;;to TAG_0
                                      (30 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (33 (if_acmpeq 43)) ;;to TAG_1
                                      (36 (aload_2)) 
                                      (37 (aconst_null)) 
                                      (38 (astore_2)) 
                                      (39 (goto 72))  ;;to TAG_2
                                      (42 (pop)) ;;at TAG_0
                                      (43 (getstatic (fieldCP "const__2" "clojure.repl$special_doc" (class "clojure.lang.Var")))) ;;at TAG_1
                                      (46 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (49 (checkcast (class "clojure.lang.IFn"))) 
                                      (52 (getstatic (fieldCP "const__3" "clojure.repl$special_doc" (class "clojure.lang.Var")))) 
                                      (55 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (58 (checkcast (class "clojure.lang.IFn"))) 
                                      (61 (aload_1)) 
                                      (62 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (67 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (72 (getstatic (fieldCP "const__4" "clojure.repl$special_doc" (class "clojure.lang.Keyword")))) ;;at TAG_2
                                      (75 (aload_1)) 
                                      (76 (aconst_null)) 
                                      (77 (astore_1)) 
                                      (78 (getstatic (fieldCP "const__5" "clojure.repl$special_doc" (class "clojure.lang.Keyword")))) 
                                      (81 (getstatic (fieldCP "TRUE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (84 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 6)) 
                                      (89 (areturn)) 
                                      (endofcode 90))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *repl$special_doc-class-table*
  (make-static-class-decls 
   *clojure.repl$special_doc*))

(defconst *package-name-map* 
  ("clojure.repl$special_doc" . "clojure"))

