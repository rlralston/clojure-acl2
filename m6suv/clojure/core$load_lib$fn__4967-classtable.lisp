; core$load_lib$fn__4967-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:44 CDT 2014.
;

(defconst *clojure.core$load_lib$fn__4967*
 (make-class-def
      '(class "clojure.core$load_lib$fn__4967"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "remove-ns"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "require" (class "java.lang.Object") (accessflags  *class* ) -1)
                        (field "undefined_on_entry" (class "java.lang.Object") (accessflags  *class* ) -1)
                        (field "need_ns" (class "java.lang.Object") (accessflags  *class* ) -1)
                        (field "load" (class "java.lang.Object") (accessflags  *class* ) -1)
                        (field "lib" (class "java.lang.Object") (accessflags  *class* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 14)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "remove-ns"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$load_lib$fn__4967" (class "clojure.lang.Var"))))
                                      (13 (return))
                                      (endofcode 14))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 6) (code_length . 32)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "clojure.lang.AFunction" () void)))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (putfield (fieldCP "require" "clojure.core$load_lib$fn__4967" (class "java.lang.Object"))))
                                      (9 (aload_0))
                                      (10 (aload_2))
                                      (11 (putfield (fieldCP "undefined_on_entry" "clojure.core$load_lib$fn__4967" (class "java.lang.Object"))))
                                      (14 (aload_0))
                                      (15 (aload_3))
                                      (16 (putfield (fieldCP "need_ns" "clojure.core$load_lib$fn__4967" (class "java.lang.Object"))))
                                      (19 (aload_0))
                                      (20 (aload 4))
                                      (22 (putfield (fieldCP "load" "clojure.core$load_lib$fn__4967" (class "java.lang.Object"))))
                                      (25 (aload_0))
                                      (26 (aload 5))
                                      (28 (putfield (fieldCP "lib" "clojure.core$load_lib$fn__4967" (class "java.lang.Object"))))
                                      (31 (return))
                                      (endofcode 32))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 3) (code_length . 81)
                                   (parsedcode
                                      (0 (aload_0)) ;;at TAG_4
                                      (1 (getfield (fieldCP "load" "clojure.core$load_lib$fn__4967" (class "java.lang.Object")))) 
                                      (4 (checkcast (class "clojure.lang.IFn"))) 
                                      (7 (aload_0)) 
                                      (8 (getfield (fieldCP "lib" "clojure.core$load_lib$fn__4967" (class "java.lang.Object")))) 
                                      (11 (aload_0)) 
                                      (12 (getfield (fieldCP "need_ns" "clojure.core$load_lib$fn__4967" (class "java.lang.Object")))) 
                                      (15 (aload_0)) 
                                      (16 (getfield (fieldCP "require" "clojure.core$load_lib$fn__4967" (class "java.lang.Object")))) 
                                      (19 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 4)) 
                                      (24 (astore_1)) 
                                      (25 (goto 79)) ;;to TAG_0;;at TAG_5
                                      (28 (astore_2)) ;;at TAG_6
                                      (29 (aload_0)) 
                                      (30 (getfield (fieldCP "undefined_on_entry" "clojure.core$load_lib$fn__4967" (class "java.lang.Object")))) 
                                      (33 (dup)) 
                                      (34 (ifnull 65)) ;;to TAG_1
                                      (37 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (40 (if_acmpeq 66))  ;;to TAG_2
                                      (43 (getstatic (fieldCP "const__0" "clojure.core$load_lib$fn__4967" (class "clojure.lang.Var")))) 
                                      (46 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (49 (checkcast (class "clojure.lang.IFn"))) 
                                      (52 (aload_0)) 
                                      (53 (getfield (fieldCP "lib" "clojure.core$load_lib$fn__4967" (class "java.lang.Object")))) 
                                      (56 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (61 (pop)) 
                                      (62 (goto 68)) ;;to TAG_3
                                      (65 (pop)) ;;at TAG_1
                                      (66 (aconst_null)) ;;at TAG_2
                                      (67 (pop)) 
                                      (68 (aload_2)) ;;at TAG_3
                                      (69 (aconst_null)) 
                                      (70 (astore_2)) 
                                      (71 (checkcast (class "java.lang.Throwable"))) 
                                      (74 (athrow)) 
                                      (75 (astore_1)) 
                                      (76 (goto 79)) ;;to TAG_0
                                      (79 (aload_1)) ;;at TAG_0
                                      (80 (areturn)) 
                                      (endofcode 81))
                                   (Exceptions 
                                     (handler 0 25  28 (class "java.lang.Exception")))
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$load_lib$fn__4967-class-table*
  (make-static-class-decls 
   *clojure.core$load_lib$fn__4967*))

(defconst *package-name-map* 
  ("clojure.core$load_lib$fn__4967" . "clojure"))
