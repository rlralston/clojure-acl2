; walk$loading__4910__auto__-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:59 CDT 2014.
;

(defconst *clojure.walk$loading__4910__auto__*
 (make-class-def
      '(class "clojure.walk$loading__4910__auto__"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "refer"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.AFn") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 26)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "refer"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.walk$loading__4910__auto__" (class "clojure.lang.Var"))))
                                      (13 (aconst_null))
                                      (14 (ldc 0))        ;;STRING:: "clojure.core"
                                      (16 (invokestatic
					(methodCP "intern" "clojure.lang.Symbol" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Symbol"))))
                                      (19 (checkcast (class "clojure.lang.AFn")))
                                      (22 (putstatic (fieldCP "const__1" "clojure.walk$loading__4910__auto__" (class "clojure.lang.AFn"))))
                                      (25 (return))
                                      (endofcode 26))
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
                                   (max_stack . 4) (max_locals . 3) (code_length . 64)
                                   (parsedcode
                                      (0 (iconst_2)) 
                                      (1 (anewarray (class "java.lang.Object"))) 
                                      (4 (dup)) 
                                      (5 (iconst_0)) 
                                      (6 (getstatic (fieldCP "LOADER" "clojure.lang.Compiler" (class "clojure.lang.Var")))) 
                                      (9 (aastore)) 
                                      (10 (dup)) 
                                      (11 (iconst_1)) 
                                      (12 (aload_0)) 
                                      (13 (invokevirtual (methodCP "getClass" "java.lang.Object" () (class "java.lang.Class")))) 
                                      (16 (checkcast (class "java.lang.Class"))) 
                                      (19 (invokevirtual (methodCP "getClassLoader" "java.lang.Class" () (class "java.lang.ClassLoader")))) 
                                      (22 (aastore)) 
                                      (23 (invokestatic (methodCP "mapUniqueKeys" "clojure.lang.RT" ((array (class "java.lang.Object"))) (class "clojure.lang.IPersistentMap")))) 
                                      (26 (checkcast (class "clojure.lang.Associative"))) 
                                      (29 (invokestatic (methodCP "pushThreadBindings" "clojure.lang.Var" ((class "clojure.lang.Associative")) void))) 
                                      (32 (getstatic (fieldCP "const__0" "clojure.walk$loading__4910__auto__" (class "clojure.lang.Var")))) ;;at TAG_1
                                      (35 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (38 (checkcast (class "clojure.lang.IFn"))) 
                                      (41 (getstatic (fieldCP "const__1" "clojure.walk$loading__4910__auto__" (class "clojure.lang.AFn")))) 
                                      (44 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (49 (astore_1)) 
                                      (50 (invokestatic (methodCP "popThreadBindings" "clojure.lang.Var" () void))) ;;at TAG_2
                                      (53 (goto 62)) ;;to TAG_0
                                      (56 (astore_2)) ;;at TAG_3
                                      (57 (invokestatic (methodCP "popThreadBindings" "clojure.lang.Var" () void))) 
                                      (60 (aload_2)) 
                                      (61 (athrow)) 
                                      (62 (aload_1)) ;;at TAG_0
                                      (63 (areturn)) 
                                      (endofcode 64))
                                   (Exceptions 
                                     (handler 32 50  56 (class "java.lang.Throwable")))
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *walk$loading__4910__auto__-class-table*
  (make-static-class-decls 
   *clojure.walk$loading__4910__auto__*))

(defconst *package-name-map* 
  ("clojure.walk$loading__4910__auto__" . "clojure"))

