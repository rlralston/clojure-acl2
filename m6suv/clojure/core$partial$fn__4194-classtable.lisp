; core$partial$fn__4194-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:44 CDT 2014.
;

(defconst *clojure.core$partial$fn__4194*
 (make-class-def
      '(class "clojure.core$partial$fn__4194"
            "clojure.lang.RestFn"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "apply"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "f" (class "java.lang.Object") (accessflags  *class* ) -1)
                        (field "arg1" (class "java.lang.Object") (accessflags  *class* ) -1)
                        (field "arg3" (class "java.lang.Object") (accessflags  *class* ) -1)
                        (field "arg2" (class "java.lang.Object") (accessflags  *class* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 14)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "apply"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$partial$fn__4194" (class "clojure.lang.Var"))))
                                      (13 (return))
                                      (endofcode 14))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 5) (code_length . 26)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "clojure.lang.RestFn" () void)))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (putfield (fieldCP "f" "clojure.core$partial$fn__4194" (class "java.lang.Object"))))
                                      (9 (aload_0))
                                      (10 (aload_2))
                                      (11 (putfield (fieldCP "arg1" "clojure.core$partial$fn__4194" (class "java.lang.Object"))))
                                      (14 (aload_0))
                                      (15 (aload_3))
                                      (16 (putfield (fieldCP "arg3" "clojure.core$partial$fn__4194" (class "java.lang.Object"))))
                                      (19 (aload_0))
                                      (20 (aload 4))
                                      (22 (putfield (fieldCP "arg2" "clojure.core$partial$fn__4194" (class "java.lang.Object"))))
                                      (25 (return))
                                      (endofcode 26))
                                   (Exceptions )
                                   (StackMap )))
                        (method "doInvoke"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 7) (max_locals . 2) (code_length . 34)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.core$partial$fn__4194" (class "clojure.lang.Var"))))
                                      (3 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (6 (checkcast (class "clojure.lang.IFn")))
                                      (9 (aload_0))
                                      (10 (getfield (fieldCP "f" "clojure.core$partial$fn__4194" (class "java.lang.Object"))))
                                      (13 (aload_0))
                                      (14 (getfield (fieldCP "arg1" "clojure.core$partial$fn__4194" (class "java.lang.Object"))))
                                      (17 (aload_0))
                                      (18 (getfield (fieldCP "arg2" "clojure.core$partial$fn__4194" (class "java.lang.Object"))))
                                      (21 (aload_0))
                                      (22 (getfield (fieldCP "arg3" "clojure.core$partial$fn__4194" (class "java.lang.Object"))))
                                      (25 (aload_1))
                                      (26 (aconst_null))
                                      (27 (astore_1))
                                      (28 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 6))
                                      (33 (areturn))
                                      (endofcode 34))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getRequiredArity"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 2)
                                   (parsedcode
                                      (0 (iconst_0))
                                      (1 (ireturn))
                                      (endofcode 2))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$partial$fn__4194-class-table*
  (make-static-class-decls 
   *clojure.core$partial$fn__4194*))

(defconst *package-name-map* 
  ("clojure.core$partial$fn__4194" . "clojure"))
