; data$fn__8933$G__8928__8938-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:48 CDT 2014.
;

(defconst *clojure.data$fn__8933$G__8928__8938*
 (make-class-def
      '(class "clojure.data$fn__8933$G__8928__8938"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "-cache-protocol-fn")
                        (STRING  "clojure.data.EqualityPartition"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "java.lang.Object") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "G__8929" (class "java.lang.Object") (accessflags  *class* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 22)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "-cache-protocol-fn"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.data$fn__8933$G__8928__8938" (class "clojure.lang.Var"))))
                                      (13 (ldc 2))        ;;STRING:: "clojure.data.EqualityPartition"
                                      (15 (invokestatic
					(methodCP "forName" "java.lang.Class" ((class "java.lang.String")) (class "java.lang.Class"))))
                                      (18 (putstatic (fieldCP "const__1" "clojure.data$fn__8933$G__8928__8938" (class "java.lang.Object"))))
                                      (21 (return))
                                      (endofcode 22))
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
                                      (6 (putfield (fieldCP "G__8929" "clojure.data$fn__8933$G__8928__8938" (class "java.lang.Object"))))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 4) (code_length . 89)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (checkcast (class "clojure.lang.AFunction"))) 
                                      (4 (getfield (fieldCP "__methodImplCache" "clojure.lang.AFunction" (class "clojure.lang.MethodImplCache")))) 
                                      (7 (astore_2)) 
                                      (8 (aload_2)) 
                                      (9 (aconst_null)) 
                                      (10 (astore_2)) 
                                      (11 (checkcast (class "clojure.lang.MethodImplCache"))) 
                                      (14 (aload_1)) 
                                      (15 (invokestatic (methodCP "classOf" "clojure.lang.Util" ((class "java.lang.Object")) (class "java.lang.Class")))) 
                                      (18 (checkcast (class "java.lang.Class"))) 
                                      (21 (invokevirtual (methodCP "fnFor" "clojure.lang.MethodImplCache" ((class "java.lang.Class")) (class "clojure.lang.IFn")))) 
                                      (24 (astore_3)) 
                                      (25 (aload_3)) 
                                      (26 (dup)) 
                                      (27 (ifnull 53)) ;;to TAG_0
                                      (30 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (33 (if_acmpeq 54)) ;;to TAG_1
                                      (36 (aload_3)) 
                                      (37 (aconst_null)) 
                                      (38 (astore_3)) 
                                      (39 (checkcast (class "clojure.lang.IFn"))) 
                                      (42 (aload_1)) 
                                      (43 (aconst_null)) 
                                      (44 (astore_1)) 
                                      (45 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (50 (goto 88))  ;;to TAG_2
                                      (53 (pop)) ;;at TAG_0
                                      (54 (getstatic (fieldCP "const__0" "clojure.data$fn__8933$G__8928__8938" (class "clojure.lang.Var")))) ;;at TAG_1
                                      (57 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (60 (checkcast (class "clojure.lang.IFn"))) 
                                      (63 (aload_0)) 
                                      (64 (aload_1)) 
                                      (65 (getstatic (fieldCP "const__1" "clojure.data$fn__8933$G__8928__8938" (class "java.lang.Object")))) 
                                      (68 (aload_0)) 
                                      (69 (getfield (fieldCP "G__8929" "clojure.data$fn__8933$G__8928__8938" (class "java.lang.Object")))) 
                                      (72 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 5)) 
                                      (77 (checkcast (class "clojure.lang.IFn"))) 
                                      (80 (aload_1)) 
                                      (81 (aconst_null)) 
                                      (82 (astore_1)) 
                                      (83 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (88 (areturn)) ;;at TAG_2
                                      (endofcode 89))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *data$fn__8933$G__8928__8938-class-table*
  (make-static-class-decls 
   *clojure.data$fn__8933$G__8928__8938*))

(defconst *package-name-map* 
  ("clojure.data$fn__8933$G__8928__8938" . "clojure"))
