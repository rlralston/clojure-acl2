; InvalidPathException-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:39 CDT 2014.
;

(defconst *java.nio.file.InvalidPathException*
 (make-class-def
      '(class "java.nio.file.InvalidPathException"
            "java.lang.IllegalArgumentException"
            (constant_pool
                        (LONG 4355821422286746137)
                        (STRING  " at index ")
                        (STRING  ": "))
            (fields
                        (field "serialVersionUID" long (accessflags  *class*  *final*  *static* ) 0)
                        (field "input" (class "java.lang.String") (accessflags  *class*  *private* ) -1)
                        (field "index" int (accessflags  *class*  *private* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.lang.String") (class "java.lang.String") int)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 4) (code_length . 45)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (aload_2)) 
                                      (2 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" ((class "java.lang.String")) void))) 
                                      (5 (aload_1)) 
                                      (6 (ifnull 13)) ;;to TAG_0
                                      (9 (aload_2)) 
                                      (10 (ifnonnull 21)) ;;to TAG_1
                                      (13 (new (class "java.lang.NullPointerException"))) ;;at TAG_0
                                      (16 (dup)) 
                                      (17 (invokespecial (methodCP "<init>" "java.lang.NullPointerException" () void))) 
                                      (20 (athrow)) 
                                      (21 (iload_3)) ;;at TAG_1
                                      (22 (iconst_m1)) 
                                      (23 (if_icmpge 34))  ;;to TAG_2
                                      (26 (new (class "java.lang.IllegalArgumentException"))) 
                                      (29 (dup)) 
                                      (30 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" () void))) 
                                      (33 (athrow)) 
                                      (34 (aload_0)) ;;at TAG_2
                                      (35 (aload_1)) 
                                      (36 (putfield (fieldCP "input" "java.nio.file.InvalidPathException" (class "java.lang.String")))) 
                                      (39 (aload_0)) 
                                      (40 (iload_3)) 
                                      (41 (putfield (fieldCP "index" "java.nio.file.InvalidPathException" int))) 
                                      (44 (return)) 
                                      (endofcode 45))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.lang.String") (class "java.lang.String"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 3) (code_length . 8)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (aload_2))
                                      (3 (iconst_m1))
                                      (4 (invokespecial
					(methodCP "<init>" "java.nio.file.InvalidPathException" ((class "java.lang.String") (class "java.lang.String") int) void)))
                                      (7 (return))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getInput"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "input" "java.nio.file.InvalidPathException" (class "java.lang.String"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getReason"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "getMessage" "java.lang.IllegalArgumentException" () (class "java.lang.String"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getIndex"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "index" "java.nio.file.InvalidPathException" int)))
                                      (4 (ireturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getMessage"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 62)
                                   (parsedcode
                                      (0 (new (class "java.lang.StringBuffer"))) 
                                      (3 (dup)) 
                                      (4 (invokespecial (methodCP "<init>" "java.lang.StringBuffer" () void))) 
                                      (7 (astore_1)) 
                                      (8 (aload_1)) 
                                      (9 (aload_0)) 
                                      (10 (invokevirtual (methodCP "getReason" "java.nio.file.InvalidPathException" () (class "java.lang.String")))) 
                                      (13 (invokevirtual (methodCP "append" "java.lang.StringBuffer" ((class "java.lang.String")) (class "java.lang.StringBuffer")))) 
                                      (16 (pop)) 
                                      (17 (aload_0)) 
                                      (18 (getfield (fieldCP "index" "java.nio.file.InvalidPathException" int))) 
                                      (21 (iconst_m1)) 
                                      (22 (if_icmple 41))  ;;to TAG_0
                                      (25 (aload_1)) 
                                      (26 (ldc 1)) ;;STRING:: " at index "
                                      (28 (invokevirtual (methodCP "append" "java.lang.StringBuffer" ((class "java.lang.String")) (class "java.lang.StringBuffer")))) 
                                      (31 (pop)) 
                                      (32 (aload_1)) 
                                      (33 (aload_0)) 
                                      (34 (getfield (fieldCP "index" "java.nio.file.InvalidPathException" int))) 
                                      (37 (invokevirtual (methodCP "append" "java.lang.StringBuffer" (int) (class "java.lang.StringBuffer")))) 
                                      (40 (pop)) 
                                      (41 (aload_1)) ;;at TAG_0
                                      (42 (ldc 2)) ;;STRING:: ": "
                                      (44 (invokevirtual (methodCP "append" "java.lang.StringBuffer" ((class "java.lang.String")) (class "java.lang.StringBuffer")))) 
                                      (47 (pop)) 
                                      (48 (aload_1)) 
                                      (49 (aload_0)) 
                                      (50 (getfield (fieldCP "input" "java.nio.file.InvalidPathException" (class "java.lang.String")))) 
                                      (53 (invokevirtual (methodCP "append" "java.lang.StringBuffer" ((class "java.lang.String")) (class "java.lang.StringBuffer")))) 
                                      (56 (pop)) 
                                      (57 (aload_1)) 
                                      (58 (invokevirtual (methodCP "toString" "java.lang.StringBuffer" () (class "java.lang.String")))) 
                                      (61 (areturn)) 
                                      (endofcode 62))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *InvalidPathException-class-table*
  (make-static-class-decls 
   *java.nio.file.InvalidPathException*))

(defconst *package-name-map* 
  ("java.nio.file.InvalidPathException" . "java.nio.file"))

