; core$bit_or-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:40 CDT 2014.
;

(defconst *clojure.core$bit_or*
 (make-class-def
      '(class "clojure.core$bit_or"
            "clojure.lang.RestFn"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "reduce1")
                        (STRING  "bit-or"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 27)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "reduce1"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$bit_or" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "bit-or"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.core$bit_or" (class "clojure.lang.Var"))))
                                      (26 (return))
                                      (endofcode 27))
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
                                   (max_stack . 5) (max_locals . 4) (code_length . 36)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.core$bit_or" (class "clojure.lang.Var"))))
                                      (3 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (6 (checkcast (class "clojure.lang.IFn")))
                                      (9 (getstatic (fieldCP "const__1" "clojure.core$bit_or" (class "clojure.lang.Var"))))
                                      (12 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (15 (aload_1))
                                      (16 (aconst_null))
                                      (17 (astore_1))
                                      (18 (aload_2))
                                      (19 (aconst_null))
                                      (20 (astore_2))
                                      (21 (invokestatic
					(methodCP "or" "clojure.lang.Numbers" ((class "java.lang.Object") (class "java.lang.Object")) long)))
                                      (24 (invokestatic
					(methodCP "num" "clojure.lang.Numbers" (long) (class "java.lang.Number"))))
                                      (27 (aload_3))
                                      (28 (aconst_null))
                                      (29 (astore_3))
                                      (30 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4))
                                      (35 (areturn))
                                      (endofcode 36))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 13)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (aconst_null))
                                      (2 (astore_1))
                                      (3 (aload_2))
                                      (4 (aconst_null))
                                      (5 (astore_2))
                                      (6 (invokestatic
					(methodCP "or" "clojure.lang.Numbers" ((class "java.lang.Object") (class "java.lang.Object")) long)))
                                      (9 (invokestatic
					(methodCP "num" "clojure.lang.Numbers" (long) (class "java.lang.Number"))))
                                      (12 (areturn))
                                      (endofcode 13))
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


(defconst *core$bit_or-class-table*
  (make-static-class-decls 
   *clojure.core$bit_or*))

(defconst *package-name-map* 
  ("clojure.core$bit_or" . "clojure"))

