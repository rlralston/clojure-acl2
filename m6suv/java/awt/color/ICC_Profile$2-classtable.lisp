; ICC_Profile$2-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:24 CDT 2014.
;

(defconst *java.awt.color.ICC_Profile$2*
 (make-class-def
      '(class "java.awt.color.ICC_Profile$2"
            "java.lang.Object"
            (constant_pool
                        (STRING  "Can\nt load standard profile: "))
            (fields
                        (field "val$name" (class "java.lang.String") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.lang.String"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "val$name" "java.awt.color.ICC_Profile$2" (class "java.lang.String"))))
                                      (5 (aload_0))
                                      (6 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "run"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 3) (code_length . 46)
                                   (parsedcode
                                      (0 (aconst_null)) 
                                      (1 (astore_1)) 
                                      (2 (aload_0)) ;;at TAG_1
                                      (3 (getfield (fieldCP "val$name" "java.awt.color.ICC_Profile$2" (class "java.lang.String")))) 
                                      (6 (invokestatic (methodCP "getInstance" "java.awt.color.ICC_Profile" ((class "java.lang.String")) (class "java.awt.color.ICC_Profile")))) 
                                      (9 (astore_1)) 
                                      (10 (goto 44)) ;;to TAG_0;;at TAG_2
                                      (13 (astore_2)) ;;at TAG_3
                                      (14 (new (class "java.lang.IllegalArgumentException"))) 
                                      (17 (dup)) 
                                      (18 (new (class "java.lang.StringBuilder"))) 
                                      (21 (dup)) 
                                      (22 (invokespecial (methodCP "<init>" "java.lang.StringBuilder" () void))) 
                                      (25 (ldc 0)) ;;STRING:: "Can\nt load standard profile: "
                                      (27 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (30 (aload_0)) 
                                      (31 (getfield (fieldCP "val$name" "java.awt.color.ICC_Profile$2" (class "java.lang.String")))) 
                                      (34 (invokevirtual (methodCP "append" "java.lang.StringBuilder" ((class "java.lang.String")) (class "java.lang.StringBuilder")))) 
                                      (37 (invokevirtual (methodCP "toString" "java.lang.StringBuilder" () (class "java.lang.String")))) 
                                      (40 (invokespecial (methodCP "<init>" "java.lang.IllegalArgumentException" ((class "java.lang.String")) void))) 
                                      (43 (athrow)) 
                                      (44 (aload_1)) ;;at TAG_0
                                      (45 (areturn)) 
                                      (endofcode 46))
                                   (Exceptions 
                                     (handler 2 10  13 (class "java.io.IOException")))
                                   (StackMap ))))
            (interfaces "java.security.PrivilegedAction")
            (accessflags  *class*  *final*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "EnclosingMethod")
              (attribute "InnerClasses")))))


(defconst *ICC_Profile$2-class-table*
  (make-static-class-decls 
   *java.awt.color.ICC_Profile$2*))

(defconst *package-name-map* 
  ("java.awt.color.ICC_Profile$2" . "java.awt.color"))

