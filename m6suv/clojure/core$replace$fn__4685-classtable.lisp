; core$replace$fn__4685-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:45 CDT 2014.
;

(defconst *clojure.core$replace$fn__4685*
 (make-class-def
      '(class "clojure.core$replace$fn__4685"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "find")
                        (STRING  "val"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "smap" (class "java.lang.Object") (accessflags  *class* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 27)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "find"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$replace$fn__4685" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "val"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.core$replace$fn__4685" (class "clojure.lang.Var"))))
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
                                      (6 (putfield (fieldCP "smap" "clojure.core$replace$fn__4685" (class "java.lang.Object"))))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 4) (code_length . 60)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.core$replace$fn__4685" (class "clojure.lang.Var")))) 
                                      (3 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (6 (checkcast (class "clojure.lang.IFn"))) 
                                      (9 (aload_0)) 
                                      (10 (getfield (fieldCP "smap" "clojure.core$replace$fn__4685" (class "java.lang.Object")))) 
                                      (13 (aload_1)) 
                                      (14 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (19 (astore_2)) 
                                      (20 (aload_2)) 
                                      (21 (dup)) 
                                      (22 (ifnull 55)) ;;to TAG_0
                                      (25 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (28 (if_acmpeq 56)) ;;to TAG_1
                                      (31 (aload_2)) 
                                      (32 (aconst_null)) 
                                      (33 (astore_2)) 
                                      (34 (astore_3)) 
                                      (35 (getstatic (fieldCP "const__1" "clojure.core$replace$fn__4685" (class "clojure.lang.Var")))) 
                                      (38 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (41 (checkcast (class "clojure.lang.IFn"))) 
                                      (44 (aload_3)) 
                                      (45 (aconst_null)) 
                                      (46 (astore_3)) 
                                      (47 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (52 (goto 59))  ;;to TAG_2
                                      (55 (pop)) ;;at TAG_0
                                      (56 (aload_1)) ;;at TAG_1
                                      (57 (aconst_null)) 
                                      (58 (astore_1)) 
                                      (59 (areturn)) ;;at TAG_2
                                      (endofcode 60))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$replace$fn__4685-class-table*
  (make-static-class-decls 
   *clojure.core$replace$fn__4685*))

(defconst *package-name-map* 
  ("clojure.core$replace$fn__4685" . "clojure"))

