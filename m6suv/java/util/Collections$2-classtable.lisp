; Collections$2-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:42 CDT 2014.
;

(defconst *java.util.Collections$2*
 (make-class-def
      '(class "java.util.Collections$2"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "i" (class "java.util.Iterator") (accessflags  *class*  *final*  *private* ) -1)
                        (field "val$c" (class "java.util.Collection") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.util.Collection"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 23)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "val$c" "java.util.Collections$2" (class "java.util.Collection"))))
                                      (5 (aload_0))
                                      (6 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (9 (aload_0))
                                      (10 (aload_0))
                                      (11 (getfield (fieldCP "val$c" "java.util.Collections$2" (class "java.util.Collection"))))
                                      (14 (invokeinterface
					(methodCP "iterator" "java.util.Collection" () (class "java.util.Iterator")) 1))
                                      (19 (putfield (fieldCP "i" "java.util.Collections$2" (class "java.util.Iterator"))))
                                      (22 (return))
                                      (endofcode 23))
                                   (Exceptions )
                                   (StackMap )))
                        (method "hasMoreElements"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "i" "java.util.Collections$2" (class "java.util.Iterator"))))
                                      (4 (invokeinterface
					(methodCP "hasNext" "java.util.Iterator" () boolean) 1))
                                      (9 (ireturn))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "nextElement"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "i" "java.util.Collections$2" (class "java.util.Iterator"))))
                                      (4 (invokeinterface
					(methodCP "next" "java.util.Iterator" () (class "java.lang.Object")) 1))
                                      (9 (areturn))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.util.Enumeration")
            (accessflags  *class*  *final*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "EnclosingMethod")
              (attribute "Signature")
              (attribute "InnerClasses")))))


(defconst *Collections$2-class-table*
  (make-static-class-decls 
   *java.util.Collections$2*))

(defconst *package-name-map* 
  ("java.util.Collections$2" . "java.util"))
