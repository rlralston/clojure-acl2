; ClassLoader$3-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:33 CDT 2014.
;

(defconst *java.lang.ClassLoader$3*
 (make-class-def
      '(class "java.lang.ClassLoader$3"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "val$file" (class "java.io.File") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.io.File"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "val$file" "java.lang.ClassLoader$3" (class "java.io.File"))))
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
                                   (max_stack . 1) (max_locals . 1) (code_length . 18)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "val$file" "java.lang.ClassLoader$3" (class "java.io.File")))) 
                                      (4 (invokevirtual (methodCP "exists" "java.io.File" () boolean))) 
                                      (7 (ifeq 16))  ;;to TAG_0
                                      (10 (getstatic (fieldCP "TRUE" "java.lang.Boolean" (class "java.lang.Boolean")))) 
                                      (13 (goto 17)) ;;to TAG_1
                                      (16 (aconst_null)) ;;at TAG_0
                                      (17 (areturn)) ;;at TAG_1
                                      (endofcode 18))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.security.PrivilegedAction")
            (accessflags  *class*  *final*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "EnclosingMethod")
              (attribute "Signature")
              (attribute "InnerClasses")))))


(defconst *ClassLoader$3-class-table*
  (make-static-class-decls 
   *java.lang.ClassLoader$3*))

(defconst *package-name-map* 
  ("java.lang.ClassLoader$3" . "java.lang"))

