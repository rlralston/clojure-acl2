; Exchanger$Node-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:43 CDT 2014.
;

(defconst *java.util.concurrent.Exchanger$Node*
 (make-class-def
      '(class "java.util.concurrent.Exchanger$Node"
            "java.util.concurrent.atomic.AtomicReference"
            (constant_pool)
            (fields
                        (field "item" (class "java.lang.Object") (accessflags  *class*  *final*  *public* ) -1)
                        (field "waiter" (class "java.lang.Thread") (accessflags  *class*  *public*  *volatile* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.lang.Object"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.util.concurrent.atomic.AtomicReference" () void)))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (putfield (fieldCP "item" "java.util.concurrent.Exchanger$Node" (class "java.lang.Object"))))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "Signature")
              (attribute "InnerClasses")))))


(defconst *Exchanger$Node-class-table*
  (make-static-class-decls 
   *java.util.concurrent.Exchanger$Node*))

(defconst *package-name-map* 
  ("java.util.concurrent.Exchanger$Node" . "java.util.concurrent"))

