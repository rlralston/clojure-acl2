; core$disj_BANG_-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:41 CDT 2014.
;

(defconst *clojure.core$disj_BANG_*
 (make-class-def
      '(class "clojure.core$disj_BANG_"
            "clojure.lang.RestFn"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "first")
                        (STRING  "next"))
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
                                      (2 (ldc 1))         ;;STRING:: "first"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$disj_BANG_" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "next"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.core$disj_BANG_" (class "clojure.lang.Var"))))
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
                                   (max_stack . 5) (max_locals . 5) (code_length . 80)
                                   (parsedcode
                                      (0 (aload_1)) ;;at TAG_2
                                      (1 (aconst_null)) 
                                      (2 (astore_1)) 
                                      (3 (checkcast (class "clojure.lang.ITransientSet"))) 
                                      (6 (aload_2)) 
                                      (7 (aconst_null)) 
                                      (8 (astore_2)) 
                                      (9 (invokeinterface (methodCP "disjoin" "clojure.lang.ITransientSet" ((class "java.lang.Object")) (class "clojure.lang.ITransientSet")) 2)) 
                                      (14 (astore 4)) 
                                      (16 (aload_3)) 
                                      (17 (dup)) 
                                      (18 (ifnull 73)) ;;to TAG_0
                                      (21 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (24 (if_acmpeq 74)) ;;to TAG_1
                                      (27 (aload 4)) 
                                      (29 (aconst_null)) 
                                      (30 (astore 4)) 
                                      (32 (getstatic (fieldCP "const__0" "clojure.core$disj_BANG_" (class "clojure.lang.Var")))) 
                                      (35 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (38 (checkcast (class "clojure.lang.IFn"))) 
                                      (41 (aload_3)) 
                                      (42 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (47 (getstatic (fieldCP "const__1" "clojure.core$disj_BANG_" (class "clojure.lang.Var")))) 
                                      (50 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (53 (checkcast (class "clojure.lang.IFn"))) 
                                      (56 (aload_3)) 
                                      (57 (aconst_null)) 
                                      (58 (astore_3)) 
                                      (59 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (64 (astore_3)) 
                                      (65 (astore_2)) 
                                      (66 (astore_1)) 
                                      (67 (goto 0))  ;;to TAG_2
                                      (70 (goto 79)) ;;to TAG_3
                                      (73 (pop)) ;;at TAG_0
                                      (74 (aload 4)) ;;at TAG_1
                                      (76 (aconst_null)) 
                                      (77 (astore 4)) 
                                      (79 (areturn)) ;;at TAG_3
                                      (endofcode 80))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 15)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (aconst_null))
                                      (2 (astore_1))
                                      (3 (checkcast (class "clojure.lang.ITransientSet")))
                                      (6 (aload_2))
                                      (7 (aconst_null))
                                      (8 (astore_2))
                                      (9 (invokeinterface
					(methodCP "disjoin" "clojure.lang.ITransientSet" ((class "java.lang.Object")) (class "clojure.lang.ITransientSet")) 2))
                                      (14 (areturn))
                                      (endofcode 15))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 4)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (aconst_null))
                                      (2 (astore_1))
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


(defconst *core$disj_BANG_-class-table*
  (make-static-class-decls 
   *clojure.core$disj_BANG_*))

(defconst *package-name-map* 
  ("clojure.core$disj_BANG_" . "clojure"))

