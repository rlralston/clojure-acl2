; core$_PLUS_-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:46 CDT 2014.
;

(defconst *clojure.core$_PLUS_*
 (make-class-def
      '(class "clojure.core$_PLUS_"
            "clojure.lang.RestFn"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "cast")
                        (STRING  "java.lang.Number")
                        (STRING  "reduce1")
                        (STRING  "+"))
            (fields
                        (field "const__0" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 55)
                                   (parsedcode
                                      (0 (lconst_0))
                                      (1 (invokestatic
					(methodCP "valueOf" "java.lang.Long" (long) (class "java.lang.Long"))))
                                      (4 (putstatic (fieldCP "const__0" "clojure.core$_PLUS_" (class "java.lang.Object"))))
                                      (7 (ldc 0))         ;;STRING:: "clojure.core"
                                      (9 (ldc 1))         ;;STRING:: "cast"
                                      (11 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (14 (checkcast (class "clojure.lang.Var")))
                                      (17 (putstatic (fieldCP "const__1" "clojure.core$_PLUS_" (class "clojure.lang.Var"))))
                                      (20 (ldc 2))        ;;STRING:: "java.lang.Number"
                                      (22 (invokestatic
					(methodCP "forName" "java.lang.Class" ((class "java.lang.String")) (class "java.lang.Class"))))
                                      (25 (putstatic (fieldCP "const__2" "clojure.core$_PLUS_" (class "java.lang.Object"))))
                                      (28 (ldc 0))        ;;STRING:: "clojure.core"
                                      (30 (ldc 3))        ;;STRING:: "reduce1"
                                      (32 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (35 (checkcast (class "clojure.lang.Var")))
                                      (38 (putstatic (fieldCP "const__3" "clojure.core$_PLUS_" (class "clojure.lang.Var"))))
                                      (41 (ldc 0))        ;;STRING:: "clojure.core"
                                      (43 (ldc 4))        ;;STRING:: "+"
                                      (45 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (48 (checkcast (class "clojure.lang.Var")))
                                      (51 (putstatic (fieldCP "const__4" "clojure.core$_PLUS_" (class "clojure.lang.Var"))))
                                      (54 (return))
                                      (endofcode 55))
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
                                   (max_stack . 5) (max_locals . 4) (code_length . 33)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__3" "clojure.core$_PLUS_" (class "clojure.lang.Var"))))
                                      (3 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (6 (checkcast (class "clojure.lang.IFn")))
                                      (9 (getstatic (fieldCP "const__4" "clojure.core$_PLUS_" (class "clojure.lang.Var"))))
                                      (12 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (15 (aload_1))
                                      (16 (aconst_null))
                                      (17 (astore_1))
                                      (18 (aload_2))
                                      (19 (aconst_null))
                                      (20 (astore_2))
                                      (21 (invokestatic
					(methodCP "add" "clojure.lang.Numbers" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Number"))))
                                      (24 (aload_3))
                                      (25 (aconst_null))
                                      (26 (astore_3))
                                      (27 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4))
                                      (32 (areturn))
                                      (endofcode 33))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (aconst_null))
                                      (2 (astore_1))
                                      (3 (aload_2))
                                      (4 (aconst_null))
                                      (5 (astore_2))
                                      (6 (invokestatic
					(methodCP "add" "clojure.lang.Numbers" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Number"))))
                                      (9 (areturn))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 2) (code_length . 21)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__1" "clojure.core$_PLUS_" (class "clojure.lang.Var"))))
                                      (3 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (6 (checkcast (class "clojure.lang.IFn")))
                                      (9 (getstatic (fieldCP "const__2" "clojure.core$_PLUS_" (class "java.lang.Object"))))
                                      (12 (aload_1))
                                      (13 (aconst_null))
                                      (14 (astore_1))
                                      (15 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3))
                                      (20 (areturn))
                                      (endofcode 21))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 4)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.core$_PLUS_" (class "java.lang.Object"))))
                                      (3 (areturn))
                                      (endofcode 4))
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


(defconst *core$_PLUS_-class-table*
  (make-static-class-decls 
   *clojure.core$_PLUS_*))

(defconst *package-name-map* 
  ("clojure.core$_PLUS_" . "clojure"))

