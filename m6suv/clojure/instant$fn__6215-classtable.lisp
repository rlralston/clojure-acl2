; instant$fn__6215-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:49 CDT 2014.
;

(defconst *clojure.instant$fn__6215*
 (make-class-def
      '(class "clojure.instant$fn__6215"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "init-proxy")
                        (STRING  "initialValue"))
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
                                      (2 (ldc 1))         ;;STRING:: "init-proxy"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.instant$fn__6215" (class "clojure.lang.Var"))))
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
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 7) (max_locals . 2) (code_length . 50)
                                   (parsedcode
                                      (0 (new (class "clojure.instant.proxy$java.lang.ThreadLocal$0")))
                                      (3 (dup))
                                      (4 (invokespecial
					(methodCP "<init>" "clojure.instant.proxy$java.lang.ThreadLocal$0" () void)))
                                      (7 (astore_1))
                                      (8 (getstatic (fieldCP "const__0" "clojure.instant$fn__6215" (class "clojure.lang.Var"))))
                                      (11 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (14 (checkcast (class "clojure.lang.IFn")))
                                      (17 (aload_1))
                                      (18 (iconst_2))
                                      (19 (anewarray (class "java.lang.Object")))
                                      (22 (dup))
                                      (23 (iconst_0))
                                      (24 (ldc 2))        ;;STRING:: "initialValue"
                                      (26 (aastore))
                                      (27 (dup))
                                      (28 (iconst_1))
                                      (29 (new (class "clojure.instant$fn__6215$fn__6216")))
                                      (32 (dup))
                                      (33 (invokespecial
					(methodCP "<init>" "clojure.instant$fn__6215$fn__6216" () void)))
                                      (36 (aastore))
                                      (37 (invokestatic
					(methodCP "mapUniqueKeys" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentMap"))))
                                      (40 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3))
                                      (45 (pop))
                                      (46 (aload_1))
                                      (47 (aconst_null))
                                      (48 (astore_1))
                                      (49 (areturn))
                                      (endofcode 50))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *instant$fn__6215-class-table*
  (make-static-class-decls 
   *clojure.instant$fn__6215*))

(defconst *package-name-map* 
  ("clojure.instant$fn__6215" . "clojure"))

