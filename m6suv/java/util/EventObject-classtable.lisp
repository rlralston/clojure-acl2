; EventObject-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:45 CDT 2014.
;

(defconst *java.util.EventObject*
 (make-class-def
      '(class "java.util.EventObject"
            "java.lang.Object"
            (constant_pool
                        (LONG 5516075349620653480)
                        (STRING  "null source")
                        (STRING  "[source=")
                        (STRING  "]"))
            (fields
                        (field "serialVersionUID" long (accessflags  *class*  *final*  *private*  *static* ) 0)
                        (field "source" (class "java.lang.Object") (accessflags  *class*  *protected*  *transient* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.lang.Object"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 24)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokespecial (methodCP "<init>" "java.lang.Object" () void))) 
                                      (4 (aload_1)) 
                                      (5 (ifnonnull 18))  ;;to TAG_0
                                      (8 (new (class "java.lang.IllegalArgumentException"))) 
                                      (11 (dup)) 
                                      (12 (ldc 1)) ;;STRING:: "null source"
                                      (14 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" ((class "java.lang.String")) void))) 
                                      (17 (athrow)) 
                                      (18 (aload_0)) ;;at TAG_0
                                      (19 (aload_1)) 
                                      (20 (putfield (fieldCP "source" "java.util.EventObject" (class "java.lang.Object")))) 
                                      (23 (return)) 
                                      (endofcode 24))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getSource"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "source" "java.util.EventObject" (class "java.lang.Object"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "toString"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 38)
                                   (parsedcode
                                      (0 (new (class "java.lang.StringBuilder")))
                                      (3 (dup))
                                      (4 (invokespecial
					(methodCP "<init>" "java.lang.StringBuilder" () void)))
                                      (7 (aload_0))
                                      (8 (invokevirtual
					(methodCP "getClass" "java.lang.Object" () (class "java.lang.Class"))))
                                      (11 (invokevirtual
					(methodCP "getName" "java.lang.Class" () (class "java.lang.String"))))
                                      (14 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder"))))
                                      (17 (ldc 2))        ;;STRING:: "[source="
                                      (19 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder"))))
                                      (22 (aload_0))
                                      (23 (getfield (fieldCP "source" "java.util.EventObject" (class "java.lang.Object"))))
                                      (26 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" ((class "java.lang.Object")) (class "java.lang.StringBuilder"))))
                                      (29 (ldc 3))        ;;STRING:: "]"
                                      (31 (invokevirtual
					(methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder"))))
                                      (34 (invokevirtual
					(methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String"))))
                                      (37 (areturn))
                                      (endofcode 38))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.io.Serializable")
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *EventObject-class-table*
  (make-static-class-decls 
   *java.util.EventObject*))

(defconst *package-name-map* 
  ("java.util.EventObject" . "java.util"))
