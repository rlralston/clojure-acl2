; Compiler$1-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:33 CDT 2014.
;

(defconst *java.lang.Compiler$1*
 (make-class-def
      '(class "java.lang.Compiler$1"
            "java.lang.Object"
            (constant_pool
                        (STRING  "java.compiler")
                        (STRING  "NONE")
                        (STRING  "")
                        (STRING  "Warning: JIT compiler \"")
                        (STRING  "\" not found. Will use interpreter.")
                        (STRING  "java.vm.info")
                        (STRING  ", ")
                        (STRING  ", nojit"))
            (fields)
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "run"
                              (parameters )
                              (returntype . (class "java.lang.Void"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 4) (code_length . 142)
                                   (parsedcode
                                      (0 (iconst_0)) 
                                      (1 (istore_1)) 
                                      (2 (ldc 0)) ;;STRING:: "java.compiler"
                                      (4 (invokestatic (methodCP "getProperty" "java.lang.System" ((class "java.lang.String")) (class "java.lang.String")))) 
                                      (7 (astore_2)) 
                                      (8 (aload_2)) 
                                      (9 (ifnull 73)) ;;to TAG_0
                                      (12 (aload_2)) 
                                      (13 (ldc 1)) ;;STRING:: "NONE"
                                      (15 (invokevirtual (methodCP "equals" "java.lang.String" ((class "java.lang.Object")) boolean))) 
                                      (18 (ifne 73)) ;;to TAG_0
                                      (21 (aload_2)) 
                                      (22 (ldc 2)) ;;STRING:: ""
                                      (24 (invokevirtual (methodCP "equals" "java.lang.String" ((class "java.lang.Object")) boolean))) 
                                      (27 (ifne 73)) ;;to TAG_0
                                      (30 (aload_2)) ;;at TAG_3
                                      (31 (invokestatic (methodCP "loadLibrary" "java.lang.System" ((class "java.lang.String")) void))) 
                                      (34 (invokestatic (methodCP "access$000" "java.lang.Compiler" () void))) 
                                      (37 (iconst_1)) 
                                      (38 (istore_1)) 
                                      (39 (goto 73)) ;;to TAG_0;;at TAG_4
                                      (42 (astore_3)) ;;at TAG_5
                                      (43 (getstatic (fieldCP "err" "java.lang.System" (class "java.io.PrintStream")))) 
                                      (46 (new (class "java.lang.StringBuilder"))) 
                                      (49 (dup)) 
                                      (50 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (53 (ldc 3)) ;;STRING:: "Warning: JIT compiler \""
                                      (55 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (58 (aload_2)) 
                                      (59 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (62 (ldc 4)) ;;STRING:: "\" not found. Will use interpreter."
                                      (64 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (67 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (70 (invokevirtual (methodCP "println" "java.io.PrintStream" ((class "java.lang.String")) void))) 
                                      (73 (ldc 5)) ;;at TAG_0;;STRING:: "java.vm.info"
                                      (75 (invokestatic (methodCP "getProperty" "java.lang.System" ((class "java.lang.String")) (class "java.lang.String")))) 
                                      (78 (astore_3)) 
                                      (79 (iload_1)) 
                                      (80 (ifeq 115)) ;;to TAG_1
                                      (83 (ldc 5)) ;;STRING:: "java.vm.info"
                                      (85 (new (class "java.lang.StringBuilder"))) 
                                      (88 (dup)) 
                                      (89 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (92 (aload_3)) 
                                      (93 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (96 (ldc 6)) ;;STRING:: ", "
                                      (98 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (101 (aload_2)) 
                                      (102 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (105 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (108 (invokestatic (methodCP "setProperty" "java.lang.System" ((class "java.lang.String") (class "java.lang.String")) (class "java.lang.String")))) 
                                      (111 (pop)) 
                                      (112 (goto 140))  ;;to TAG_2
                                      (115 (ldc 5)) ;;at TAG_1;;STRING:: "java.vm.info"
                                      (117 (new (class "java.lang.StringBuilder"))) 
                                      (120 (dup)) 
                                      (121 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (124 (aload_3)) 
                                      (125 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (128 (ldc 7)) ;;STRING:: ", nojit"
                                      (130 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (133 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (136 (invokestatic (methodCP "setProperty" "java.lang.System" ((class "java.lang.String") (class "java.lang.String")) (class "java.lang.String")))) 
                                      (139 (pop)) 
                                      (140 (aconst_null)) ;;at TAG_2
                                      (141 (areturn)) 
                                      (endofcode 142))
                                   (Exceptions 
                                     (handler 30 39  42 (class "java.lang.UnsatisfiedLinkError")))
                                   (StackMap )))
                        (method "run"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public*  *volatile* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokevirtual
					(methodCP "run" "java.lang.Compiler$1" () (class "java.lang.Void"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.security.PrivilegedAction")
            (accessflags  *class*  *final*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "EnclosingMethod")
              (attribute "Signature")
              (attribute "InnerClasses")))))


(defconst *Compiler$1-class-table*
  (make-static-class-decls 
   *java.lang.Compiler$1*))

(defconst *package-name-map* 
  ("java.lang.Compiler$1" . "java.lang"))

