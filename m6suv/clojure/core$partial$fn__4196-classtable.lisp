; core$partial$fn__4196-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:44 CDT 2014.
;

(defconst *clojure.core$partial$fn__4196*
 (make-class-def
      '(class "clojure.core$partial$fn__4196"
            "clojure.lang.RestFn"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "apply")
                        (STRING  "concat"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "more" (class "java.lang.Object") (accessflags  *class* ) -1)
                        (field "f" (class "java.lang.Object") (accessflags  *class* ) -1)
                        (field "arg1" (class "java.lang.Object") (accessflags  *class* ) -1)
                        (field "arg2" (class "java.lang.Object") (accessflags  *class* ) -1)
                        (field "arg3" (class "java.lang.Object") (accessflags  *class* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 27)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "apply"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$partial$fn__4196" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "concat"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.core$partial$fn__4196" (class "clojure.lang.Var"))))
                                      (26 (return))
                                      (endofcode 27))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 6) (code_length . 32)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "clojure.lang.RestFn" () void)))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (putfield (fieldCP "more" "clojure.core$partial$fn__4196" (class "java.lang.Object"))))
                                      (9 (aload_0))
                                      (10 (aload_2))
                                      (11 (putfield (fieldCP "f" "clojure.core$partial$fn__4196" (class "java.lang.Object"))))
                                      (14 (aload_0))
                                      (15 (aload_3))
                                      (16 (putfield (fieldCP "arg1" "clojure.core$partial$fn__4196" (class "java.lang.Object"))))
                                      (19 (aload_0))
                                      (20 (aload 4))
                                      (22 (putfield (fieldCP "arg2" "clojure.core$partial$fn__4196" (class "java.lang.Object"))))
                                      (25 (aload_0))
                                      (26 (aload 5))
                                      (28 (putfield (fieldCP "arg3" "clojure.core$partial$fn__4196" (class "java.lang.Object"))))
                                      (31 (return))
                                      (endofcode 32))
                                   (Exceptions )
                                   (StackMap )))
                        (method "doInvoke"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 9) (max_locals . 2) (code_length . 52)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.core$partial$fn__4196" (class "clojure.lang.Var"))))
                                      (3 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (6 (checkcast (class "clojure.lang.IFn")))
                                      (9 (aload_0))
                                      (10 (getfield (fieldCP "f" "clojure.core$partial$fn__4196" (class "java.lang.Object"))))
                                      (13 (aload_0))
                                      (14 (getfield (fieldCP "arg1" "clojure.core$partial$fn__4196" (class "java.lang.Object"))))
                                      (17 (aload_0))
                                      (18 (getfield (fieldCP "arg2" "clojure.core$partial$fn__4196" (class "java.lang.Object"))))
                                      (21 (aload_0))
                                      (22 (getfield (fieldCP "arg3" "clojure.core$partial$fn__4196" (class "java.lang.Object"))))
                                      (25 (getstatic (fieldCP "const__1" "clojure.core$partial$fn__4196" (class "clojure.lang.Var"))))
                                      (28 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (31 (checkcast (class "clojure.lang.IFn")))
                                      (34 (aload_0))
                                      (35 (getfield (fieldCP "more" "clojure.core$partial$fn__4196" (class "java.lang.Object"))))
                                      (38 (aload_1))
                                      (39 (aconst_null))
                                      (40 (astore_1))
                                      (41 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3))
                                      (46 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 6))
                                      (51 (areturn))
                                      (endofcode 52))
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


(defconst *core$partial$fn__4196-class-table*
  (make-static-class-decls 
   *clojure.core$partial$fn__4196*))

(defconst *package-name-map* 
  ("clojure.core$partial$fn__4196" . "clojure"))

