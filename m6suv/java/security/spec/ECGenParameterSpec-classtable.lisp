; ECGenParameterSpec-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:41 CDT 2014.
;

(defconst *java.security.spec.ECGenParameterSpec*
 (make-class-def
      '(class "java.security.spec.ECGenParameterSpec"
            "java.lang.Object"
            (constant_pool
                        (STRING  "stdName is null"))
            (fields
                        (field "name" (class "java.lang.String") (accessflags  *class*  *private* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.lang.String"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 24)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (invokespecial (methodCP "<init>" "java.lang.Object" () void))) 
                                      (4 (aload_1)) 
                                      (5 (ifnonnull 18))  ;;to TAG_0
                                      (8 (new (class "java.lang.NullPointerException"))) 
                                      (11 (dup)) 
                                      (12 (ldc 0)) ;;STRING:: "stdName is null"
                                      (14 (invokespecial (methodCP "<init>" "java.lang.NullPointerException" ((class "java.lang.String")) void))) 
                                      (17 (athrow)) 
                                      (18 (aload_0)) ;;at TAG_0
                                      (19 (aload_1)) 
                                      (20 (putfield (fieldCP "name" "java.security.spec.ECGenParameterSpec" (class "java.lang.String")))) 
                                      (23 (return)) 
                                      (endofcode 24))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getName"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "name" "java.security.spec.ECGenParameterSpec" (class "java.lang.String"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.security.spec.AlgorithmParameterSpec")
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *ECGenParameterSpec-class-table*
  (make-static-class-decls 
   *java.security.spec.ECGenParameterSpec*))

(defconst *package-name-map* 
  ("java.security.spec.ECGenParameterSpec" . "java.security.spec"))
