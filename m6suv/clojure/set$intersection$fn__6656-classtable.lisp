; set$intersection$fn__6656-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:58 CDT 2014.
;

(defconst *clojure.set$intersection$fn__6656*
 (make-class-def
      '(class "clojure.set$intersection$fn__6656"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "contains?")
                        (STRING  "disj"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "s2" (class "java.lang.Object") (accessflags  *class* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 27)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "contains?"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.set$intersection$fn__6656" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "disj"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.set$intersection$fn__6656" (class "clojure.lang.Var"))))
                                      (26 (return))
                                      (endofcode 27))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.lang.Object"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "clojure.lang.AFunction" () void)))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (putfield (fieldCP "s2" "clojure.set$intersection$fn__6656" (class "java.lang.Object"))))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 3) (code_length . 57)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.set$intersection$fn__6656" (class "clojure.lang.Var")))) 
                                      (3 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (6 (checkcast (class "clojure.lang.IFn"))) 
                                      (9 (aload_0)) 
                                      (10 (getfield (fieldCP "s2" "clojure.set$intersection$fn__6656" (class "java.lang.Object")))) 
                                      (13 (aload_2)) 
                                      (14 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (19 (dup)) 
                                      (20 (ifnull 35)) ;;to TAG_0
                                      (23 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (26 (if_acmpeq 36)) ;;to TAG_1
                                      (29 (aload_1)) 
                                      (30 (aconst_null)) 
                                      (31 (astore_1)) 
                                      (32 (goto 56))  ;;to TAG_2
                                      (35 (pop)) ;;at TAG_0
                                      (36 (getstatic (fieldCP "const__1" "clojure.set$intersection$fn__6656" (class "clojure.lang.Var")))) ;;at TAG_1
                                      (39 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (42 (checkcast (class "clojure.lang.IFn"))) 
                                      (45 (aload_1)) 
                                      (46 (aconst_null)) 
                                      (47 (astore_1)) 
                                      (48 (aload_2)) 
                                      (49 (aconst_null)) 
                                      (50 (astore_2)) 
                                      (51 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (56 (areturn)) ;;at TAG_2
                                      (endofcode 57))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *set$intersection$fn__6656-class-table*
  (make-static-class-decls 
   *clojure.set$intersection$fn__6656*))

(defconst *package-name-map* 
  ("clojure.set$intersection$fn__6656" . "clojure"))

