; core$ns_imports-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:44 CDT 2014.
;

(defconst *clojure.core$ns_imports*
 (make-class-def
      '(class "clojure.core$ns_imports"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "filter-key")
                        (STRING  "val")
                        (STRING  "partial")
                        (STRING  "instance?")
                        (STRING  "java.lang.Class")
                        (STRING  "ns-map"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__5" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 74)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "filter-key"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$ns_imports" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "val"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.core$ns_imports" (class "clojure.lang.Var"))))
                                      (26 (ldc 0))        ;;STRING:: "clojure.core"
                                      (28 (ldc 3))        ;;STRING:: "partial"
                                      (30 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "clojure.core$ns_imports" (class "clojure.lang.Var"))))
                                      (39 (ldc 0))        ;;STRING:: "clojure.core"
                                      (41 (ldc 4))        ;;STRING:: "instance?"
                                      (43 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (46 (checkcast (class "clojure.lang.Var")))
                                      (49 (putstatic (fieldCP "const__3" "clojure.core$ns_imports" (class "clojure.lang.Var"))))
                                      (52 (ldc 5))        ;;STRING:: "java.lang.Class"
                                      (54 (invokestatic
					(methodCP "forName" "java.lang.Class" ((class "java.lang.String")) (class "java.lang.Class"))))
                                      (57 (putstatic (fieldCP "const__4" "clojure.core$ns_imports" (class "java.lang.Object"))))
                                      (60 (ldc 0))        ;;STRING:: "clojure.core"
                                      (62 (ldc 6))        ;;STRING:: "ns-map"
                                      (64 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (67 (checkcast (class "clojure.lang.Var")))
                                      (70 (putstatic (fieldCP "const__5" "clojure.core$ns_imports" (class "clojure.lang.Var"))))
                                      (73 (return))
                                      (endofcode 74))
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
                                   (max_stack . 6) (max_locals . 2) (code_length . 61)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.core$ns_imports" (class "clojure.lang.Var"))))
                                      (3 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (6 (checkcast (class "clojure.lang.IFn")))
                                      (9 (getstatic (fieldCP "const__1" "clojure.core$ns_imports" (class "clojure.lang.Var"))))
                                      (12 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (15 (getstatic (fieldCP "const__2" "clojure.core$ns_imports" (class "clojure.lang.Var"))))
                                      (18 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (21 (checkcast (class "clojure.lang.IFn")))
                                      (24 (getstatic (fieldCP "const__3" "clojure.core$ns_imports" (class "clojure.lang.Var"))))
                                      (27 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (30 (getstatic (fieldCP "const__4" "clojure.core$ns_imports" (class "java.lang.Object"))))
                                      (33 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3))
                                      (38 (getstatic (fieldCP "const__5" "clojure.core$ns_imports" (class "clojure.lang.Var"))))
                                      (41 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (44 (checkcast (class "clojure.lang.IFn")))
                                      (47 (aload_1))
                                      (48 (aconst_null))
                                      (49 (astore_1))
                                      (50 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (55 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4))
                                      (60 (areturn))
                                      (endofcode 61))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$ns_imports-class-table*
  (make-static-class-decls 
   *clojure.core$ns_imports*))

(defconst *package-name-map* 
  ("clojure.core$ns_imports" . "clojure"))

