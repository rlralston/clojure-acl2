; walk$prewalk_replace$fn__7001-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:59 CDT 2014.
;

(defconst *clojure.walk$prewalk_replace$fn__7001*
 (make-class-def
      '(class "clojure.walk$prewalk_replace$fn__7001"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "contains?"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "smap" (class "java.lang.Object") (accessflags  *class* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 14)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "contains?"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.walk$prewalk_replace$fn__7001" (class "clojure.lang.Var"))))
                                      (13 (return))
                                      (endofcode 14))
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
                                      (6 (putfield (fieldCP "smap" "clojure.walk$prewalk_replace$fn__7001" (class "java.lang.Object"))))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 52)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "clojure.walk$prewalk_replace$fn__7001" (class "clojure.lang.Var")))) 
                                      (3 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (6 (checkcast (class "clojure.lang.IFn"))) 
                                      (9 (aload_0)) 
                                      (10 (getfield (fieldCP "smap" "clojure.walk$prewalk_replace$fn__7001" (class "java.lang.Object")))) 
                                      (13 (aload_1)) 
                                      (14 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (19 (dup)) 
                                      (20 (ifnull 47)) ;;to TAG_0
                                      (23 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (26 (if_acmpeq 48)) ;;to TAG_1
                                      (29 (aload_0)) 
                                      (30 (getfield (fieldCP "smap" "clojure.walk$prewalk_replace$fn__7001" (class "java.lang.Object")))) 
                                      (33 (checkcast (class "clojure.lang.IFn"))) 
                                      (36 (aload_1)) 
                                      (37 (aconst_null)) 
                                      (38 (astore_1)) 
                                      (39 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (44 (goto 51))  ;;to TAG_2
                                      (47 (pop)) ;;at TAG_0
                                      (48 (aload_1)) ;;at TAG_1
                                      (49 (aconst_null)) 
                                      (50 (astore_1)) 
                                      (51 (areturn)) ;;at TAG_2
                                      (endofcode 52))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *walk$prewalk_replace$fn__7001-class-table*
  (make-static-class-decls 
   *clojure.walk$prewalk_replace$fn__7001*))

(defconst *package-name-map* 
  ("clojure.walk$prewalk_replace$fn__7001" . "clojure"))
