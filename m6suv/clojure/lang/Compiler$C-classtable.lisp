; Compiler$C-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:50 CDT 2014.
;

(defconst *clojure.lang.Compiler$C*
 (make-class-def
      '(class "clojure.lang.Compiler$C"
            "java.lang.Enum"
            (constant_pool
                        (STRING  "STATEMENT")
                        (STRING  "EXPRESSION")
                        (STRING  "RETURN")
                        (STRING  "EVAL"))
            (fields
                        (field "STATEMENT" (class "clojure.lang.Compiler$C") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "EXPRESSION" (class "clojure.lang.Compiler$C") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "RETURN" (class "clojure.lang.Compiler$C") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "EVAL" (class "clojure.lang.Compiler$C") (accessflags  *class*  *final*  *public*  *static* ) -1)
                        (field "$VALUES" (array (class "clojure.lang.Compiler$C")) (accessflags  *class*  *final*  *private*  *static* ) -1))
            (methods
                        (method "values"
                              (parameters )
                              (returntype . (array (class "clojure.lang.Compiler$C")))
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 1) (max_locals . 0) (code_length . 10)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "$VALUES" "clojure.lang.Compiler$C" (array (class "clojure.lang.Compiler$C")))))
                                      (3 (invokevirtual
					(methodCP "clone" "clojure.lang.Compiler$C[]" () (class "java.lang.Object"))))
                                      (6 (checkcast (array (class "clojure.lang.Compiler$C"))))
                                      (9 (areturn))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "valueOf"
                              (parameters (class "java.lang.String"))
                              (returntype . (class "clojure.lang.Compiler$C"))
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 11)
                                   (parsedcode
                                      (0 (ldc_w ))
                                      (3 (aload_0))
                                      (4 (invokestatic
					(methodCP "valueOf" "java.lang.Enum" ((class "java.lang.Class") (class "java.lang.String")) (class "java.lang.Enum"))))
                                      (7 (checkcast (class "clojure.lang.Compiler$C")))
                                      (10 (areturn))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.lang.String") int)
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 7)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (iload_2))
                                      (3 (invokespecial
					(methodCP "<init>" "java.lang.Enum" ((class "java.lang.String") int) void)))
                                      (6 (return))
                                      (endofcode 7))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 4) (max_locals . 0) (code_length . 84)
                                   (parsedcode
                                      (0 (new (class "clojure.lang.Compiler$C")))
                                      (3 (dup))
                                      (4 (ldc 0))         ;;STRING:: "STATEMENT"
                                      (6 (iconst_0))
                                      (7 (invokespecial
					(methodCP "<init>" "clojure.lang.Compiler$C" ((class "java.lang.String") int) void)))
                                      (10 (putstatic (fieldCP "STATEMENT" "clojure.lang.Compiler$C" (class "clojure.lang.Compiler$C"))))
                                      (13 (new (class "clojure.lang.Compiler$C")))
                                      (16 (dup))
                                      (17 (ldc 1))        ;;STRING:: "EXPRESSION"
                                      (19 (iconst_1))
                                      (20 (invokespecial
					(methodCP "<init>" "clojure.lang.Compiler$C" ((class "java.lang.String") int) void)))
                                      (23 (putstatic (fieldCP "EXPRESSION" "clojure.lang.Compiler$C" (class "clojure.lang.Compiler$C"))))
                                      (26 (new (class "clojure.lang.Compiler$C")))
                                      (29 (dup))
                                      (30 (ldc 2))        ;;STRING:: "RETURN"
                                      (32 (iconst_2))
                                      (33 (invokespecial
					(methodCP "<init>" "clojure.lang.Compiler$C" ((class "java.lang.String") int) void)))
                                      (36 (putstatic (fieldCP "RETURN" "clojure.lang.Compiler$C" (class "clojure.lang.Compiler$C"))))
                                      (39 (new (class "clojure.lang.Compiler$C")))
                                      (42 (dup))
                                      (43 (ldc 3))        ;;STRING:: "EVAL"
                                      (45 (iconst_3))
                                      (46 (invokespecial
					(methodCP "<init>" "clojure.lang.Compiler$C" ((class "java.lang.String") int) void)))
                                      (49 (putstatic (fieldCP "EVAL" "clojure.lang.Compiler$C" (class "clojure.lang.Compiler$C"))))
                                      (52 (iconst_4))
                                      (53 (anewarray (class "clojure.lang.Compiler$C")))
                                      (56 (dup))
                                      (57 (iconst_0))
                                      (58 (getstatic (fieldCP "STATEMENT" "clojure.lang.Compiler$C" (class "clojure.lang.Compiler$C"))))
                                      (61 (aastore))
                                      (62 (dup))
                                      (63 (iconst_1))
                                      (64 (getstatic (fieldCP "EXPRESSION" "clojure.lang.Compiler$C" (class "clojure.lang.Compiler$C"))))
                                      (67 (aastore))
                                      (68 (dup))
                                      (69 (iconst_2))
                                      (70 (getstatic (fieldCP "RETURN" "clojure.lang.Compiler$C" (class "clojure.lang.Compiler$C"))))
                                      (73 (aastore))
                                      (74 (dup))
                                      (75 (iconst_3))
                                      (76 (getstatic (fieldCP "EVAL" "clojure.lang.Compiler$C" (class "clojure.lang.Compiler$C"))))
                                      (79 (aastore))
                                      (80 (putstatic (fieldCP "$VALUES" "clojure.lang.Compiler$C" (array (class "clojure.lang.Compiler$C")))))
                                      (83 (return))
                                      (endofcode 84))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "Signature")
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *Compiler$C-class-table*
  (make-static-class-decls 
   *clojure.lang.Compiler$C*))

(defconst *package-name-map* 
  ("clojure.lang.Compiler$C" . "clojure.lang"))

