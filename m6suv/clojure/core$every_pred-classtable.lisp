; core$every_pred-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:42 CDT 2014.
;

(defconst *clojure.core$every_pred*
 (make-class-def
      '(class "clojure.core$every_pred"
            "clojure.lang.RestFn"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "list*"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 14)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "list*"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$every_pred" (class "clojure.lang.Var"))))
                                      (13 (return))
                                      (endofcode 14))
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
                              (parameters (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 6) (code_length . 43)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.core$every_pred" (class "clojure.lang.Var"))))
                                      (3 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (6 (checkcast (class "clojure.lang.IFn")))
                                      (9 (aload_1))
                                      (10 (aconst_null))
                                      (11 (astore_1))
                                      (12 (aload_2))
                                      (13 (aconst_null))
                                      (14 (astore_2))
                                      (15 (aload_3))
                                      (16 (aconst_null))
                                      (17 (astore_3))
                                      (18 (aload 4))
                                      (20 (aconst_null))
                                      (21 (astore 4))
                                      (23 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 5))
                                      (28 (astore 5))
                                      (30 (new (class "clojure.core$every_pred$epn__6410")))
                                      (33 (dup))
                                      (34 (aload 5))
                                      (36 (aconst_null))
                                      (37 (astore 5))
                                      (39 (invokespecial
					(methodCP "<init>" "clojure.core$every_pred$epn__6410" ((class "java.lang.Object")) void)))
                                      (42 (areturn))
                                      (endofcode 43))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 4) (code_length . 17)
                                   (parsedcode
                                      (0 (new (class "clojure.core$every_pred$ep3__6388")))
                                      (3 (dup))
                                      (4 (aload_2))
                                      (5 (aconst_null))
                                      (6 (astore_2))
                                      (7 (aload_3))
                                      (8 (aconst_null))
                                      (9 (astore_3))
                                      (10 (aload_1))
                                      (11 (aconst_null))
                                      (12 (astore_1))
                                      (13 (invokespecial
					(methodCP "<init>" "clojure.core$every_pred$ep3__6388" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) void)))
                                      (16 (areturn))
                                      (endofcode 17))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 3) (code_length . 14)
                                   (parsedcode
                                      (0 (new (class "clojure.core$every_pred$ep2__6373")))
                                      (3 (dup))
                                      (4 (aload_1))
                                      (5 (aconst_null))
                                      (6 (astore_1))
                                      (7 (aload_2))
                                      (8 (aconst_null))
                                      (9 (astore_2))
                                      (10 (invokespecial
					(methodCP "<init>" "clojure.core$every_pred$ep2__6373" ((class "java.lang.Object") (class "java.lang.Object")) void)))
                                      (13 (areturn))
                                      (endofcode 14))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 2) (code_length . 11)
                                   (parsedcode
                                      (0 (new (class "clojure.core$every_pred$ep1__6367")))
                                      (3 (dup))
                                      (4 (aload_1))
                                      (5 (aconst_null))
                                      (6 (astore_1))
                                      (7 (invokespecial
					(methodCP "<init>" "clojure.core$every_pred$ep1__6367" ((class "java.lang.Object")) void)))
                                      (10 (areturn))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getRequiredArity"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 2)
                                   (parsedcode
                                      (0 (iconst_3))
                                      (1 (ireturn))
                                      (endofcode 2))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$every_pred-class-table*
  (make-static-class-decls 
   *clojure.core$every_pred*))

(defconst *package-name-map* 
  ("clojure.core$every_pred" . "clojure"))

