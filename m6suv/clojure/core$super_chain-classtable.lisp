; core$super_chain-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:45 CDT 2014.
;

(defconst *clojure.core$super_chain*
 (make-class-def
      '(class "clojure.core$super_chain"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "cons")
                        (STRING  "super-chain"))
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
                                      (2 (ldc 1))         ;;STRING:: "cons"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$super_chain" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "super-chain"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.core$super_chain" (class "clojure.lang.Var"))))
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
					(methodCP "<init>" "clojure.lang.AFunction" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 2) (code_length . 55)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (dup)) 
                                      (2 (ifnull 52)) ;;to TAG_0
                                      (5 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (8 (if_acmpeq 53)) ;;to TAG_1
                                      (11 (getstatic (fieldCP "const__0" "clojure.core$super_chain" (class "clojure.lang.Var")))) 
                                      (14 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (17 (checkcast (class "clojure.lang.IFn"))) 
                                      (20 (aload_1)) 
                                      (21 (getstatic (fieldCP "const__1" "clojure.core$super_chain" (class "clojure.lang.Var")))) 
                                      (24 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (27 (checkcast (class "clojure.lang.IFn"))) 
                                      (30 (aload_1)) 
                                      (31 (aconst_null)) 
                                      (32 (astore_1)) 
                                      (33 (checkcast (class "java.lang.Class"))) 
                                      (36 (invokevirtual (methodCP "getSuperclass" "java.lang.Class" () (class "java.lang.Class")))) 
                                      (39 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (44 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (49 (goto 54))  ;;to TAG_2
                                      (52 (pop)) ;;at TAG_0
                                      (53 (aconst_null)) ;;at TAG_1
                                      (54 (areturn)) ;;at TAG_2
                                      (endofcode 55))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$super_chain-class-table*
  (make-static-class-decls 
   *clojure.core$super_chain*))

(defconst *package-name-map* 
  ("clojure.core$super_chain" . "clojure"))

