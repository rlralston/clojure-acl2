; LispReader$RegexReader-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:52 CDT 2014.
;

(defconst *clojure.lang.LispReader$RegexReader*
 (make-class-def
      '(class "clojure.lang.LispReader$RegexReader"
            "clojure.lang.AFn"
            (constant_pool
                        (STRING  "EOF while reading regex"))
            (fields
                        (field "stringrdr" (class "clojure.lang.LispReader$StringReader") (accessflags  *class*  *static* ) -1))
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "clojure.lang.AFn" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "invoke"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 6) (code_length . 100)
                                   (parsedcode
                                      (0 (new (class "java.lang.StringBuilder"))) 
                                      (3 (dup)) 
                                      (4 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (7 (astore_3)) 
                                      (8 (aload_1)) 
                                      (9 (checkcast (class "java.io.Reader"))) 
                                      (12 (astore 4)) 
                                      (14 (aload 4)) 
                                      (16 (invokestatic (methodCP "read1" "clojure.lang.LispReader" ((class "java.io.Reader")) int))) 
                                      (19 (istore 5)) 
                                      (21 (iload 5)) ;;at TAG_4
                                      (23 (bipush 34)) 
                                      (25 (if_icmpeq 92)) ;;to TAG_0
                                      (28 (iload 5)) 
                                      (30 (iconst_m1)) 
                                      (31 (if_icmpne 40)) ;;to TAG_1
                                      (34 (ldc 0)) ;;STRING:: "EOF while reading regex"
                                      (36 (invokestatic (methodCP "runtimeException" "clojure.lang.Util" ((class "java.lang.String")) (class "java.lang.RuntimeException")))) 
                                      (39 (athrow)) 
                                      (40 (aload_3)) ;;at TAG_1
                                      (41 (iload 5)) 
                                      (43 (i2c)) 
                                      (44 (invokevirtual (methodCP "append" "java.lang.StringBuilder" (char) (class "java.lang.StringBuilder")))) 
                                      (47 (pop)) 
                                      (48 (iload 5)) 
                                      (50 (bipush 92)) 
                                      (52 (if_icmpne 82))  ;;to TAG_2
                                      (55 (aload 4)) 
                                      (57 (invokestatic (methodCP "read1" "clojure.lang.LispReader" ((class "java.io.Reader")) int))) 
                                      (60 (istore 5)) 
                                      (62 (iload 5)) 
                                      (64 (iconst_m1)) 
                                      (65 (if_icmpne 74)) ;;to TAG_3
                                      (68 (ldc 0)) ;;STRING:: "EOF while reading regex"
                                      (70 (invokestatic (methodCP "runtimeException" "clojure.lang.Util" ((class "java.lang.String")) (class "java.lang.RuntimeException")))) 
                                      (73 (athrow)) 
                                      (74 (aload_3)) ;;at TAG_3
                                      (75 (iload 5)) 
                                      (77 (i2c)) 
                                      (78 (invokevirtual (methodCP "append" "java.lang.StringBuilder" (char) (class "java.lang.StringBuilder")))) 
                                      (81 (pop)) 
                                      (82 (aload 4)) ;;at TAG_2
                                      (84 (invokestatic (methodCP "read1" "clojure.lang.LispReader" ((class "java.io.Reader")) int))) 
                                      (87 (istore 5)) 
                                      (89 (goto 21)) ;;to TAG_4
                                      (92 (aload_3)) ;;at TAG_0
                                      (93 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (96 (invokestatic (methodCP "compile" "java.util.regex.Pattern" ((class "java.lang.String")) (class "java.util.regex.Pattern")))) 
                                      (99 (areturn)) 
                                      (endofcode 100))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 11)
                                   (parsedcode
                                      (0 (new (class "clojure.lang.LispReader$StringReader")))
                                      (3 (dup))
                                      (4 (invokespecial
					(methodCP "<init>" "clojure.lang.LispReader$StringReader" () void)))
                                      (7 (putstatic (fieldCP "stringrdr" "clojure.lang.LispReader$RegexReader" (class "clojure.lang.LispReader$StringReader"))))
                                      (10 (return))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *LispReader$RegexReader-class-table*
  (make-static-class-decls 
   *clojure.lang.LispReader$RegexReader*))

(defconst *package-name-map* 
  ("clojure.lang.LispReader$RegexReader" . "clojure.lang"))

