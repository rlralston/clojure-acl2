; data$fn__8962-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:48 CDT 2014.
;

(defconst *clojure.data$fn__8962*
 (make-class-def
      '(class "clojure.data$fn__8962"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.data")
                        (STRING  "diff-sequential")
                        (STRING  "atom-diff")
                        (STRING  "getClass")
                        (STRING  "isArray"))
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
                                      (0 (ldc 0))         ;;STRING:: "clojure.data"
                                      (2 (ldc 1))         ;;STRING:: "diff-sequential"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.data$fn__8962" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.data"
                                      (15 (ldc 2))        ;;STRING:: "atom-diff"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.data$fn__8962" (class "clojure.lang.Var"))))
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
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 3) (code_length . 52)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (ldc 3)) ;;STRING:: "getClass"
                                      (3 (invokestatic (methodCP "invokeNoArgInstanceMember" "clojure.lang.Reflector" ((class "java.lang.Object") (class "java.lang.String")) (class "java.lang.Object")))) 
                                      (6 (ldc 4)) ;;STRING:: "isArray"
                                      (8 (invokestatic (methodCP "invokeNoArgInstanceMember" "clojure.lang.Reflector" ((class "java.lang.Object") (class "java.lang.String")) (class "java.lang.Object")))) 
                                      (11 (dup)) 
                                      (12 (ifnull 30)) ;;to TAG_0
                                      (15 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (18 (if_acmpeq 31)) ;;to TAG_1
                                      (21 (getstatic (fieldCP "const__0" "clojure.data$fn__8962" (class "clojure.lang.Var")))) 
                                      (24 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (27 (goto 37))  ;;to TAG_2
                                      (30 (pop)) ;;at TAG_0
                                      (31 (getstatic (fieldCP "const__1" "clojure.data$fn__8962" (class "clojure.lang.Var")))) ;;at TAG_1
                                      (34 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (37 (checkcast (class "clojure.lang.IFn"))) ;;at TAG_2
                                      (40 (aload_1)) 
                                      (41 (aconst_null)) 
                                      (42 (astore_1)) 
                                      (43 (aload_2)) 
                                      (44 (aconst_null)) 
                                      (45 (astore_2)) 
                                      (46 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (51 (areturn)) 
                                      (endofcode 52))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *data$fn__8962-class-table*
  (make-static-class-decls 
   *clojure.data$fn__8962*))

(defconst *package-name-map* 
  ("clojure.data$fn__8962" . "clojure"))

