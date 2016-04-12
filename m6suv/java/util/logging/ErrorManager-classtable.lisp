; ErrorManager-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:46 CDT 2014.
;

(defconst *java.util.logging.ErrorManager*
 (make-class-def
      '(class "java.util.logging.ErrorManager"
            "java.lang.Object"
            (constant_pool
                        (INT 0)
                        (INT 1)
                        (INT 2)
                        (INT 3)
                        (INT 4)
                        (INT 5)
                        (STRING  "java.util.logging.ErrorManager: ")
                        (STRING  ": "))
            (fields
                        (field "reported" boolean (accessflags  *class*  *private* ) -1)
                        (field "GENERIC_FAILURE" int (accessflags  *class*  *final*  *public*  *static* ) 0)
                        (field "WRITE_FAILURE" int (accessflags  *class*  *final*  *public*  *static* ) 1)
                        (field "FLUSH_FAILURE" int (accessflags  *class*  *final*  *public*  *static* ) 2)
                        (field "CLOSE_FAILURE" int (accessflags  *class*  *final*  *public*  *static* ) 3)
                        (field "OPEN_FAILURE" int (accessflags  *class*  *final*  *public*  *static* ) 4)
                        (field "FORMAT_FAILURE" int (accessflags  *class*  *final*  *public*  *static* ) 5))
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (aload_0))
                                      (5 (iconst_0))
                                      (6 (putfield (fieldCP "reported" "java.util.logging.ErrorManager" boolean)))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "error"
                              (parameters (class "java.lang.String") (class "java.lang.Exception") int)
                              (returntype . void)
                              (accessflags  *class*  *public*  *super*  *synchronized* )
                              (code
                                   (max_stack . 2) (max_locals . 5) (code_length . 81)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "reported" "java.util.logging.ErrorManager" boolean))) 
                                      (4 (ifeq 8)) ;;to TAG_0
                                      (7 (return)) 
                                      (8 (aload_0)) ;;at TAG_0
                                      (9 (iconst_1)) 
                                      (10 (putfield (fieldCP "reported" "java.util.logging.ErrorManager" boolean))) 
                                      (13 (new (class "java.lang.StringBuilder"))) 
                                      (16 (dup)) 
                                      (17 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (20 (ldc 6)) ;;STRING:: "java.util.logging.ErrorManager: "
                                      (22 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (25 (iload_3)) 
                                      (26 (invokevirtual (methodCP "append" "java.lang.StringBuilder" (int) (class "java.lang.StringBuilder")))) 
                                      (29 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (32 (astore 4)) 
                                      (34 (aload_1)) 
                                      (35 (ifnull 64)) ;;to TAG_1
                                      (38 (new (class "java.lang.StringBuilder"))) 
                                      (41 (dup)) 
                                      (42 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (45 (aload 4)) 
                                      (47 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (50 (ldc 7)) ;;STRING:: ": "
                                      (52 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (55 (aload_1)) 
                                      (56 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (59 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (62 (astore 4)) 
                                      (64 (getstatic (fieldCP "err" "java.lang.System" (class "java.io.PrintStream")))) ;;at TAG_1
                                      (67 (aload 4)) 
                                      (69 (invokevirtual (methodCP "println" "java.io.PrintStream" ((class "java.lang.String")) void))) 
                                      (72 (aload_2)) 
                                      (73 (ifnull 80))  ;;to TAG_2
                                      (76 (aload_2)) 
                                      (77 (invokevirtual (methodCP "printStackTrace" "java.lang.Exception" () void))) 
                                      (80 (return)) ;;at TAG_2
                                      (endofcode 81))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *ErrorManager-class-table*
  (make-static-class-decls 
   *java.util.logging.ErrorManager*))

(defconst *package-name-map* 
  ("java.util.logging.ErrorManager" . "java.util.logging"))

