; xml$fn__6723$fn__6736-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:59 CDT 2014.
;

(defconst *clojure.xml$fn__6723$fn__6736*
 (make-class-def
      '(class "clojure.xml$fn__6723$fn__6736"
            "clojure.lang.AFunction"
            (constant_pool
                        (STRING  "clojure.core")
                        (STRING  "=")
                        (STRING  "clojure.xml")
                        (STRING  "*state*")
                        (STRING  "chars")
                        (STRING  "*sb*")
                        (STRING  "int"))
            (fields
                        (field "const__0" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__1" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__2" (class "clojure.lang.Keyword") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__3" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "const__4" (class "clojure.lang.Var") (accessflags  *class*  *final*  *public*  *static* ) -1))
            (methods
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 65)
                                   (parsedcode
                                      (0 (ldc 0))         ;;STRING:: "clojure.core"
                                      (2 (ldc 1))         ;;STRING:: "="
                                      (4 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (7 (checkcast (class "clojure.lang.Var")))
                                      (10 (putstatic (fieldCP "const__0" "clojure.xml$fn__6723$fn__6736" (class "clojure.lang.Var"))))
                                      (13 (ldc 2))        ;;STRING:: "clojure.xml"
                                      (15 (ldc 3))        ;;STRING:: "*state*"
                                      (17 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (20 (checkcast (class "clojure.lang.Var")))
                                      (23 (putstatic (fieldCP "const__1" "clojure.xml$fn__6723$fn__6736" (class "clojure.lang.Var"))))
                                      (26 (aconst_null))
                                      (27 (ldc 4))        ;;STRING:: "chars"
                                      (29 (invokestatic
					(methodCP "keyword" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Keyword"))))
                                      (32 (checkcast (class "clojure.lang.Keyword")))
                                      (35 (putstatic (fieldCP "const__2" "clojure.xml$fn__6723$fn__6736" (class "clojure.lang.Keyword"))))
                                      (38 (ldc 2))        ;;STRING:: "clojure.xml"
                                      (40 (ldc 5))        ;;STRING:: "*sb*"
                                      (42 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (45 (checkcast (class "clojure.lang.Var")))
                                      (48 (putstatic (fieldCP "const__3" "clojure.xml$fn__6723$fn__6736" (class "clojure.lang.Var"))))
                                      (51 (ldc 0))        ;;STRING:: "clojure.core"
                                      (53 (ldc 6))        ;;STRING:: "int"
                                      (55 (invokestatic
					(methodCP "var" "clojure.lang.RT" ((class "java.lang.String") (class "java.lang.String")) (class "clojure.lang.Var"))))
                                      (58 (checkcast (class "clojure.lang.Var")))
                                      (61 (putstatic (fieldCP "const__4" "clojure.xml$fn__6723$fn__6736" (class "clojure.lang.Var"))))
                                      (64 (return))
                                      (endofcode 65))
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
                              (parameters (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 6) (code_length . 87)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "const__1" "clojure.xml$fn__6723$fn__6736" (class "clojure.lang.Var")))) 
                                      (3 (invokevirtual (methodCP "get" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (6 (getstatic (fieldCP "const__2" "clojure.xml$fn__6723$fn__6736" (class "clojure.lang.Keyword")))) 
                                      (9 (invokestatic (methodCP "equiv" "clojure.lang.Util" ((class "java.lang.Object") (class "java.lang.Object")) boolean))) 
                                      (12 (ifeq 21))  ;;to TAG_0
                                      (15 (aconst_null)) 
                                      (16 (pop)) 
                                      (17 (goto 35)) ;;to TAG_1
                                      (20 (pop)) 
                                      (21 (getstatic (fieldCP "const__3" "clojure.xml$fn__6723$fn__6736" (class "clojure.lang.Var")))) ;;at TAG_0
                                      (24 (new (class "java.lang.StringBuilder"))) 
                                      (27 (dup)) 
                                      (28 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (31 (invokevirtual (methodCP "set" "clojure.lang.Var" ((class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (34 (pop)) 
                                      (35 (getstatic (fieldCP "const__3" "clojure.xml$fn__6723$fn__6736" (class "clojure.lang.Var")))) ;;at TAG_1
                                      (38 (invokevirtual (methodCP "get" "clojure.lang.Var" () (class "java.lang.Object")))) 
                                      (41 (astore 5)) 
                                      (43 (aload 5)) 
                                      (45 (aconst_null)) 
                                      (46 (astore 5)) 
                                      (48 (checkcast (class "java.lang.StringBuilder"))) 
                                      (51 (aload_2)) 
                                      (52 (aconst_null)) 
                                      (53 (astore_2)) 
                                      (54 (checkcast (array char))) 
                                      (57 (aload_3)) 
                                      (58 (aconst_null)) 
                                      (59 (astore_3)) 
                                      (60 (invokestatic (methodCP "intCast" "clojure.lang.RT" ((class "java.lang.Object")) int))) 
                                      (63 (aload 4)) 
                                      (65 (aconst_null)) 
                                      (66 (astore 4)) 
                                      (68 (invokestatic (methodCP "intCast" "clojure.lang.RT" ((class "java.lang.Object")) int))) 
                                      (71 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((array char) int int) (class "java.lang.StringBuilder")))) 
                                      (74 (pop)) 
                                      (75 (getstatic (fieldCP "const__1" "clojure.xml$fn__6723$fn__6736" (class "clojure.lang.Var")))) 
                                      (78 (getstatic (fieldCP "const__2" "clojure.xml$fn__6723$fn__6736" (class "clojure.lang.Keyword")))) 
                                      (81 (invokevirtual (methodCP "set" "clojure.lang.Var" ((class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (84 (pop)) 
                                      (85 (aconst_null)) 
                                      (86 (areturn)) 
                                      (endofcode 87))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "SourceDebugExtension")))))


(defconst *xml$fn__6723$fn__6736-class-table*
  (make-static-class-decls 
   *clojure.xml$fn__6723$fn__6736*))

(defconst *package-name-map* 
  ("clojure.xml$fn__6723$fn__6736" . "clojure"))

