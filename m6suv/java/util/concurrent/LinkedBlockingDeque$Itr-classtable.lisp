; LinkedBlockingDeque$Itr-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:44 CDT 2014.
;

(defconst *java.util.concurrent.LinkedBlockingDeque$Itr*
 (make-class-def
      '(class "java.util.concurrent.LinkedBlockingDeque$Itr"
            "java.util.concurrent.LinkedBlockingDeque$AbstractItr"
            (constant_pool)
            (fields
                        (field "this$0" (class "java.util.concurrent.LinkedBlockingDeque") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.util.concurrent.LinkedBlockingDeque"))
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 11)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "this$0" "java.util.concurrent.LinkedBlockingDeque$Itr" (class "java.util.concurrent.LinkedBlockingDeque"))))
                                      (5 (aload_0))
                                      (6 (aload_1))
                                      (7 (invokespecial
					(methodCP "<init>" "java.util.concurrent.LinkedBlockingDeque$AbstractItr" ((class "java.util.concurrent.LinkedBlockingDeque")) void)))
                                      (10 (return))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap )))
                        (method "firstNode"
                              (parameters )
                              (returntype . (class "java.util.concurrent.LinkedBlockingDeque$Node"))
                              (accessflags  *class* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 8)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "this$0" "java.util.concurrent.LinkedBlockingDeque$Itr" (class "java.util.concurrent.LinkedBlockingDeque"))))
                                      (4 (getfield (fieldCP "first" "java.util.concurrent.LinkedBlockingDeque" (class "java.util.concurrent.LinkedBlockingDeque$Node"))))
                                      (7 (areturn))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "nextNode"
                              (parameters (class "java.util.concurrent.LinkedBlockingDeque$Node"))
                              (returntype . (class "java.util.concurrent.LinkedBlockingDeque$Node"))
                              (accessflags  *class* )
                              (code
                                   (max_stack . 1) (max_locals . 2) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (getfield (fieldCP "next" "java.util.concurrent.LinkedBlockingDeque$Node" (class "java.util.concurrent.LinkedBlockingDeque$Node"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.util.concurrent.LinkedBlockingDeque") (class "java.util.concurrent.LinkedBlockingDeque$1"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 6)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (invokespecial
					(methodCP "<init>" "java.util.concurrent.LinkedBlockingDeque$Itr" ((class "java.util.concurrent.LinkedBlockingDeque")) void)))
                                      (5 (return))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "Signature")
              (attribute "InnerClasses")))))


(defconst *LinkedBlockingDeque$Itr-class-table*
  (make-static-class-decls 
   *java.util.concurrent.LinkedBlockingDeque$Itr*))

(defconst *package-name-map* 
  ("java.util.concurrent.LinkedBlockingDeque$Itr" . "java.util.concurrent"))
