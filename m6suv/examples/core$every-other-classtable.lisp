; C:\Users\rralston\Repos\phd\examples\target\classes\examples-classtable.lisp
; Automatically generated by jvm2acl2 on Sun Oct 25 16:16:18 CDT 2015.
;

(include-book "../model-translation")

(defconst *examples.core$every_other*
 (make-class-def
      '(class "examples.core$every_other"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "empty?")
                        (STRING  "cons")
                        (STRING  "first")
                        (STRING  "examples.core")
                        (STRING  "every-other")
                        (STRING  "rest"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 66)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "empty?"
                                      (4 (invokestatic
                                        (methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "examples.core$every_other" (class "clojure.lang.Var"))))
                                      (13 (ldc 0))        ;;STRING:: "clojure.core"
                                      (15 (ldc 2))        ;;STRING:: "cons"
                                      (17 (invokestatic
                                        (methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "examples.core$every_other" (class "clojure.lang.Var"))))
                                      (26 (ldc 0))        ;;STRING:: "clojure.core"
                                      (28 (ldc 3))        ;;STRING:: "first"
                                      (30 (invokestatic
                                        (methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (33 (checkcast (class "clojure.lang.Var")))
                                      (36 (putstatic (fieldCP "const__2" "examples.core$every_other" (class "clojure.lang.Var"))))
                                      (39 (ldc 4))        ;;STRING:: "examples.core"
                                      (41 (ldc 5))        ;;STRING:: "every-other"
                                      (43 (invokestatic
                                        (methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (46 (checkcast (class "clojure.lang.Var")))
                                      (49 (putstatic (fieldCP "const__3" "examples.core$every_other" (class "clojure.lang.Var"))))
                                      (52 (ldc 0))        ;;STRING:: "clojure.core"
                                      (54 (ldc 6))        ;;STRING:: "rest"
                                      (56 (invokestatic
                                        (methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (59 (checkcast (class "clojure.lang.Var")))
                                      (62 (putstatic (fieldCP "const__4" "examples.core$every_other" (class "clojure.lang.Var"))))
                                      (65 (return))
                                      (endofcode 66))
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
                                   (max_stack . 7) (max_locals . 2) (code_length . 105)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__0" "examples.core$every_other" (class "clojure.lang.Var")))) 
                                      (3 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (6 (checkcast (class "clojure.lang.IFn"))) 
                                      (9 (aload_1)) 
                                      (10 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (15 (dup)) 
                                      (16 (ifnull 29)) ;;to TAG_0
                                      (19 (getstatic (fieldCP "FALSE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (22 (if_acmpeq 30)) ;;to TAG_1
                                      (25 (aconst_null)) 
                                      (26 (goto 104))  ;;to TAG_2
                                      (29 (pop)) ;;at TAG_0
                                      (30 (getstatic (fieldCP "const__1" "examples.core$every_other" (class "clojure.lang.Var")))) ;;at TAG_1
                                      (33 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (36 (checkcast (class "clojure.lang.IFn"))) 
                                      (39 (getstatic (fieldCP "const__2" "examples.core$every_other" (class "clojure.lang.Var")))) 
                                      (42 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (45 (checkcast (class "clojure.lang.IFn"))) 
                                      (48 (aload_1)) 
                                      (49 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (54 (getstatic (fieldCP "const__3" "examples.core$every_other" (class "clojure.lang.Var")))) 
                                      (57 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (60 (checkcast (class "clojure.lang.IFn"))) 
                                      (63 (getstatic (fieldCP "const__4" "examples.core$every_other" (class "clojure.lang.Var")))) 
                                      (66 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (69 (checkcast (class "clojure.lang.IFn"))) 
                                      (72 (getstatic (fieldCP "const__4" "examples.core$every_other" (class "clojure.lang.Var")))) 
                                      (75 (invokevirtual (methodCP "getRawRoot" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (78 (checkcast (class "clojure.lang.IFn"))) 
                                      (81 (aload_1)) 
                                      (82 (aconst_null)) 
                                      (83 (astore_1)) 
                                      (84 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (89 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (94 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object")) (class "java.lang.Object")) 2)) 
                                      (99 (invokeinterface (methodCP "invoke" "clojure.lang.IFn" ((class "java.lang.Object") (class "java.lang.Object")) (class "java.lang.Object")) 3)) 
                                      (104 (areturn)) ;;at TAG_2
                                      (endofcode 105))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))#|ACL2s-ToDo-Line|#



;(defconst *C:\Users\rralston\Repos\phd\examples\target\classes\examples-class-table*
;  (make-static-class-decls 
;   *examples.core$every_other*))

;(defconst *package-name-map* 
;  ("examples.core$every_other" . "examples"))

