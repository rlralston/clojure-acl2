; core$protected_final_methods$not_exposable_QMARK___5513-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:45 CDT 2014.
;

(defconst *clojure.core$protected_final_methods$not_exposable_QMARK___5513*
 (make-class-def
      '(class "clojure.core$protected_final_methods$not_exposable_QMARK___5513"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "not"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 14)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "not"
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.core$protected_final_methods$not_exposable_QMARK___5513" (class "clojure.lang.Var"))))
                                      (13 (return))
                                      (endofcode 14))
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
                                   (max_stack . 3) (max_locals . 5) (code_length . 110)
                                   (parsedcode
                                      (0 (aload_1)) 
                                      (1 (aconst_null)) 
                                      (2 (astore_1)) 
                                      (3 (checkcast (class "java.lang.reflect.Method"))) 
                                      (6 (invokevirtual (methodCP "getModifiers" "java.lang.reflect.Method" () int))) 
                                      (9 (istore_2)) 
                                      (10 (getstatic (fieldCP "const__0" "clojure.core$protected_final_methods$not_exposable_QMARK___5513" (class "clojure.lang.Var")))) 
                                      (13 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (16 (checkcast (class "clojure.lang.IFn"))) 
                                      (19 (iload_2)) 
                                      (20 (invokestatic (methodCP "isProtected" "java.lang.reflect.Modifier" (int) boolean))) 
                                      (23 (istore_3)) 
                                      (24 (iload_3)) 
                                      (25 (ifeq 91)) ;;to TAG_0
                                      (28 (iload_2)) 
                                      (29 (invokestatic (methodCP "isFinal" "java.lang.reflect.Modifier" (int) boolean))) 
                                      (32 (istore 4)) 
                                      (34 (iload 4)) 
                                      (36 (ifeq 73)) ;;to TAG_1
                                      (39 (getstatic (fieldCP "const__0" "clojure.core$protected_final_methods$not_exposable_QMARK___5513" (class "clojure.lang.Var")))) 
                                      (42 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (45 (checkcast (class "clojure.lang.IFn"))) 
                                      (48 (iload_2)) 
                                      (49 (invokestatic (methodCP "isStatic" "java.lang.reflect.Modifier" (int) boolean))) 
                                      (52 (ifeq 61))  ;;to TAG_2
                                      (55 (getstatic (fieldCP "TRUE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (58 (goto 64)) ;;to TAG_3
                                      (61 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) ;;at TAG_2
                                      (64 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) ;;at TAG_3
                                      (69 (goto 87)) ;;to TAG_4
                                      (72 (pop)) 
                                      (73 (iload 4)) ;;at TAG_1
                                      (75 (ifeq 84)) ;;to TAG_5
                                      (78 (getstatic (fieldCP "TRUE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (81 (goto 87)) ;;to TAG_4
                                      (84 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) ;;at TAG_5
                                      (87 (goto 104)) ;;to TAG_6;;at TAG_4
                                      (90 (pop)) 
                                      (91 (iload_3)) ;;at TAG_0
                                      (92 (ifeq 101)) ;;to TAG_7
                                      (95 (getstatic (fieldCP "TRUE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (98 (goto 104)) ;;to TAG_6
                                      (101 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) ;;at TAG_7
                                      (104 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) ;;at TAG_6
                                      (109 (areturn)) 
                                      (endofcode 110))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *core$protected_final_methods$not_exposable_QMARK___5513-class-table*
  (make-static-class-decls 
   *clojure.core$protected_final_methods$not_exposable_QMARK___5513*))

(defconst *package-name-map* 
  ("clojure.core$protected_final_methods$not_exposable_QMARK___5513" . "clojure"))
