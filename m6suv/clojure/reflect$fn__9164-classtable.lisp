; reflect$fn__9164-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:57 CDT 2014.
;

(defconst *clojure.reflect$fn__9164*
 (make-class-def
      '(class "clojure.reflect$fn__9164"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.reflect")
                        (STRING  "resource-name"))
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
                                      (0 (ldc 0))         ;;STRING:: "clojure.reflect"
                                      (2 (ldc 1))         ;;STRING:: "resource-name"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.reflect$fn__9164" (class "clojure.lang.Var"))))
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
                                   (max_stack . 4) (max_locals . 3) (code_length . 30)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (aconst_null))
                                      (2 (astore_1))
                                      (3 (checkcast (class "java.lang.ClassLoader")))
                                      (6 (getstatic (fieldCP "const__0" "clojure.reflect$fn__9164" (class "clojure.lang.Var"))))
                                      (9 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (12 (checkcast (class "clojure.lang.IFn")))
                                      (15 (aload_2))
                                      (16 (aconst_null))
                                      (17 (astore_2))
                                      (18 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (23 (checkcast (class "java.lang.String")))
                                      (26 (invokevirtual
					(methodCP "getResourceAsStream" "java.lang.ClassLoader" ((class "java.lang.String")) (class "java.io.InputStream"))))
                                      (29 (areturn))
                                      (endofcode 30))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *reflect$fn__9164-class-table*
  (make-static-class-decls 
   *clojure.reflect$fn__9164*))

(defconst *package-name-map* 
  ("clojure.reflect$fn__9164" . "clojure"))

