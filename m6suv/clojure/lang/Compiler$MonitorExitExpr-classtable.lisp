; Compiler$MonitorExitExpr-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:50 CDT 2014.
;

(defconst *clojure.lang.Compiler$MonitorExitExpr*
 (make-class-def
      '(class "clojure.lang.Compiler$MonitorExitExpr"
            "clojure.lang.Compiler$UntypedExpr"
            (constant_pool
                        (STRING  "Can\nt eval monitor-exit"))
            (fields
                        (field "target" (class "clojure.lang.Compiler$Expr") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "clojure.lang.Compiler$Expr"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "clojure.lang.Compiler$UntypedExpr" () void)))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (putfield (fieldCP "target" "clojure.lang.Compiler$MonitorExitExpr" (class "clojure.lang.Compiler$Expr"))))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "eval"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 1) (code_length . 10)
                                   (parsedcode
                                      (0 (new (class "java.lang.UnsupportedOperationException")))
                                      (3 (dup))
                                      (4 (ldc 0))         ;;STRING:: "Can\nt eval monitor-exit"
                                      (6 (invokespecial
					(methodCP "<init>" "java.lang.UnsupportedOperationException" ((class "java.lang.String")) void)))
                                      (9 (athrow))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "emit"
                              (parameters (class "clojure.lang.Compiler$C") (class "clojure.lang.Compiler$ObjExpr") (class "clojure.asm.commons.GeneratorAdapter"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 4) (code_length . 28)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "target" "clojure.lang.Compiler$MonitorExitExpr" (class "clojure.lang.Compiler$Expr"))))
                                      (4 (getstatic (fieldCP "EXPRESSION" "clojure.lang.Compiler$C" (class "clojure.lang.Compiler$C"))))
                                      (7 (aload_2))
                                      (8 (aload_3))
                                      (9 (invokeinterface
					(methodCP "emit" "clojure.lang.Compiler$Expr" ((class "clojure.lang.Compiler$C") (class "clojure.lang.Compiler$ObjExpr") (class "clojure.asm.commons.GeneratorAdapter")) void) 4))
                                      (14 (aload_3))
                                      (15 (invokevirtual
					(methodCP "monitorExit" "clojure.asm.commons.GeneratorAdapter" () void)))
                                      (18 (getstatic (fieldCP "NIL_EXPR" "clojure.lang.Compiler" (class "clojure.lang.Compiler$NilExpr"))))
                                      (21 (aload_1))
                                      (22 (aload_2))
                                      (23 (aload_3))
                                      (24 (invokevirtual
					(methodCP "emit" "clojure.lang.Compiler$NilExpr" ((class "clojure.lang.Compiler$C") (class "clojure.lang.Compiler$ObjExpr") (class "clojure.asm.commons.GeneratorAdapter")) void)))
                                      (27 (return))
                                      (endofcode 28))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *Compiler$MonitorExitExpr-class-table*
  (make-static-class-decls 
   *clojure.lang.Compiler$MonitorExitExpr*))

(defconst *package-name-map* 
  ("clojure.lang.Compiler$MonitorExitExpr" . "clojure.lang"))

