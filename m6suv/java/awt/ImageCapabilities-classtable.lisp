; ImageCapabilities-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:29 CDT 2014.
;

(defconst *java.awt.ImageCapabilities*
 (make-class-def
      '(class "java.awt.ImageCapabilities"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "accelerated" boolean (accessflags  *class*  *private* ) -1))
            (methods
                        (method "<init>"
                              (parameters boolean)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 15)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (aload_0))
                                      (5 (iconst_0))
                                      (6 (putfield (fieldCP "accelerated" "java.awt.ImageCapabilities" boolean)))
                                      (9 (aload_0))
                                      (10 (iload_1))
                                      (11 (putfield (fieldCP "accelerated" "java.awt.ImageCapabilities" boolean)))
                                      (14 (return))
                                      (endofcode 15))
                                   (Exceptions )
                                   (StackMap )))
                        (method "isAccelerated"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "accelerated" "java.awt.ImageCapabilities" boolean)))
                                      (4 (ireturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "isTrueVolatile"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 2)
                                   (parsedcode
                                      (0 (iconst_0))
                                      (1 (ireturn))
                                      (endofcode 2))
                                   (Exceptions )
                                   (StackMap )))
                        (method "clone"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 14)
                                   (parsedcode
                                      (0 (aload_0)) ;;at TAG_0
                                      (1 (invokespecial (methodCP "clone" "java.lang.Object" () (class "java.lang.Object")))) 
                                      (4 (areturn)) ;;at TAG_1
                                      (5 (astore_1)) ;;at TAG_2
                                      (6 (new (class "java.lang.InternalError"))) 
                                      (9 (dup)) 
                                      (10 (invokespecial (methodCP "<init>" "java.lang.InternalError" () void))) 
                                      (13 (athrow)) 
                                      (endofcode 14))
                                   (Exceptions 
                                     (handler 0 4  5 (class "java.lang.CloneNotSupportedException")))
                                   (StackMap ))))
            (interfaces "java.lang.Cloneable")
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *ImageCapabilities-class-table*
  (make-static-class-decls 
   *java.awt.ImageCapabilities*))

(defconst *package-name-map* 
  ("java.awt.ImageCapabilities" . "java.awt"))

