; Beans$2-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:31 CDT 2014.
;

(defconst *java.beans.Beans$2*
 (make-class-def
      '(class "java.beans.Beans$2"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "val$cloader" (class "java.lang.ClassLoader") (accessflags  *class*  *final* ) -1)
                        (field "val$resourceName" (class "java.lang.String") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.lang.ClassLoader") (class "java.lang.String"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 15)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "val$cloader" "java.beans.Beans$2" (class "java.lang.ClassLoader"))))
                                      (5 (aload_0))
                                      (6 (aload_2))
                                      (7 (putfield (fieldCP "val$resourceName" "java.beans.Beans$2" (class "java.lang.String"))))
                                      (10 (aload_0))
                                      (11 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (14 (return))
                                      (endofcode 15))
                                   (Exceptions )
                                   (StackMap )))
                        (method "run"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 27)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (getfield (fieldCP "val$cloader" "java.beans.Beans$2" (class "java.lang.ClassLoader")))) 
                                      (4 (ifnonnull 15))  ;;to TAG_0
                                      (7 (aload_0)) 
                                      (8 (getfield (fieldCP "val$resourceName" "java.beans.Beans$2" (class "java.lang.String")))) 
                                      (11 (invokestatic (methodCP "getSystemResource" "java.lang.ClassLoader" ((class "java.lang.String")) (class "java.net.URL")))) 
                                      (14 (areturn)) 
                                      (15 (aload_0)) ;;at TAG_0
                                      (16 (getfield (fieldCP "val$cloader" "java.beans.Beans$2" (class "java.lang.ClassLoader")))) 
                                      (19 (aload_0)) 
                                      (20 (getfield (fieldCP "val$resourceName" "java.beans.Beans$2" (class "java.lang.String")))) 
                                      (23 (invokevirtual (methodCP "getResource" "java.lang.ClassLoader" ((class "java.lang.String")) (class "java.net.URL")))) 
                                      (26 (areturn)) 
                                      (endofcode 27))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.security.PrivilegedAction")
            (accessflags  *class*  *final*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "EnclosingMethod")
              (attribute "InnerClasses")))))


(defconst *Beans$2-class-table*
  (make-static-class-decls 
   *java.beans.Beans$2*))

(defconst *package-name-map* 
  ("java.beans.Beans$2" . "java.beans"))

