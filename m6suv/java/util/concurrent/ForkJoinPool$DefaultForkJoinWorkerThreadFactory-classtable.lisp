; ForkJoinPool$DefaultForkJoinWorkerThreadFactory-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:44 CDT 2014.
;

(defconst *java.util.concurrent.ForkJoinPool$DefaultForkJoinWorkerThreadFactory*
 (make-class-def
      '(class "java.util.concurrent.ForkJoinPool$DefaultForkJoinWorkerThreadFactory"
            "java.lang.Object"
            (constant_pool)
            (fields)
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "newThread"
                              (parameters (class "java.util.concurrent.ForkJoinPool"))
                              (returntype . (class "java.util.concurrent.ForkJoinWorkerThread"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 2) (code_length . 9)
                                   (parsedcode
                                      (0 (new (class "java.util.concurrent.ForkJoinWorkerThread")))
                                      (3 (dup))
                                      (4 (aload_1))
                                      (5 (invokespecial
					(methodCP "<init>" "java.util.concurrent.ForkJoinWorkerThread" ((class "java.util.concurrent.ForkJoinPool")) void)))
                                      (8 (areturn))
                                      (endofcode 9))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.util.concurrent.ForkJoinPool$ForkJoinWorkerThreadFactory")
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *ForkJoinPool$DefaultForkJoinWorkerThreadFactory-class-table*
  (make-static-class-decls 
   *java.util.concurrent.ForkJoinPool$DefaultForkJoinWorkerThreadFactory*))

(defconst *package-name-map* 
  ("java.util.concurrent.ForkJoinPool$DefaultForkJoinWorkerThreadFactory" . "java.util.concurrent"))
