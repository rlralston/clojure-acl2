; core$trampoline-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:45 CDT 2014.
;

(defconst *clojure.core$trampoline*
 (make-class-def
      '(class "clojure.core$trampoline"
            "clojure.lang.RestFn"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "fn?")
                        (STRING  "trampoline"))
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
                                      (2 (ldc 1))         ;;STRING:: "fn?"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$trampoline" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "trampoline"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.core$trampoline" (class "clojure.lang.Var"))))
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
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 6) (max_locals . 3) (code_length . 28)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__1" "clojure.core$trampoline" (class "clojure.lang.Var"))))
                                      (3 (invokevirtual
					(methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object"))))
                                      (6 (checkcast (class "clojure.lang.IFn")))
                                      (9 (new (class "clojure.core$trampoline$fn__5044")))
                                      (12 (dup))
                                      (13 (aload_1))
                                      (14 (aconst_null))
                                      (15 (astore_1))
                                      (16 (aload_2))
                                      (17 (aconst_null))
                                      (18 (astore_2))
                                      (19 (invokespecial
					(methodCP "<init>" "clojure.core$trampoline$fn__5044" ((class "java.lang.Object") (class "java.lang.Object")) void)))
                                      (22 (invokeinterface
					(methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2))
                                      (27 (areturn))
                                      (endofcode 28))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 52)
                                   (parsedcode
                                      (0 (aload_1)) ;;at TAG_2
                                      (1 (aconst_null)) 
                                      (2 (astore_1)) 
                                      (3 (checkcast (class "clojure.lang.IFn"))) 
                                      (6 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" () (class "java.lang.Object")) 1)) 
                                      (11 (astore_2)) 
                                      (12 (getstatic (fieldCP "const__0" "clojure.core$trampoline" (class "clojure.lang.Var")))) 
                                      (15 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (18 (checkcast (class "clojure.lang.IFn"))) 
                                      (21 (aload_2)) 
                                      (22 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (27 (dup)) 
                                      (28 (ifnull 47)) ;;to TAG_0
                                      (31 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (34 (if_acmpeq 48)) ;;to TAG_1
                                      (37 (aload_2)) 
                                      (38 (aconst_null)) 
                                      (39 (astore_2)) 
                                      (40 (astore_1)) 
                                      (41 (goto 0))  ;;to TAG_2
                                      (44 (goto 51)) ;;to TAG_3
                                      (47 (pop)) ;;at TAG_0
                                      (48 (aload_2)) ;;at TAG_1
                                      (49 (aconst_null)) 
                                      (50 (astore_2)) 
                                      (51 (areturn)) ;;at TAG_3
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
                                      (0 (iconst_1))
                                      (1 (ireturn))
                                      (endofcode 2))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$trampoline-class-table*
  (make-static-class-decls 
   *clojure.core$trampoline*))

(defconst *package-name-map* 
  ("clojure.core$trampoline" . "clojure"))

