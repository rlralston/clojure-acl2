; ForkJoinTask$ExceptionNode-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:44 CDT 2014.
;

(defconst *java.util.concurrent.ForkJoinTask$ExceptionNode*
 (make-class-def
      '(class "java.util.concurrent.ForkJoinTask$ExceptionNode"
            "java.lang.ref.WeakReference"
            (constant_pool)
            (fields
                        (field "ex" (class "java.lang.Throwable") (accessflags  *class*  *final* ) -1)
                        (field "next" (class "java.util.concurrent.ForkJoinTask$ExceptionNode") (accessflags  *class* ) -1)
                        (field "thrower" long (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.util.concurrent.ForkJoinTask") (class "java.lang.Throwable") (class "java.util.concurrent.ForkJoinTask$ExceptionNode"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 3) (max_locals . 4) (code_length . 29)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (invokestatic
					(methodCP "access$000" "java.util.concurrent.ForkJoinTask" () (class "java.lang.ref.ReferenceQueue"))))
                                      (5 (invokespecial
					(methodCP "<init>" "java.lang.ref.WeakReference" ((class "java.lang.Object") (class "java.lang.ref.ReferenceQueue")) void)))
                                      (8 (aload_0))
                                      (9 (aload_2))
                                      (10 (putfield (fieldCP "ex" "java.util.concurrent.ForkJoinTask$ExceptionNode" (class "java.lang.Throwable"))))
                                      (13 (aload_0))
                                      (14 (aload_3))
                                      (15 (putfield (fieldCP "next" "java.util.concurrent.ForkJoinTask$ExceptionNode" (class "java.util.concurrent.ForkJoinTask$ExceptionNode"))))
                                      (18 (aload_0))
                                      (19 (invokestatic
					(methodCP "currentThread" "java.lang.Thread" () (class "java.lang.Thread"))))
                                      (22 (invokevirtual
					(methodCP "getId" "java.lang.Thread" () long)))
                                      (25 (putfield (fieldCP "thrower" "java.util.concurrent.ForkJoinTask$ExceptionNode" long)))
                                      (28 (return))
                                      (endofcode 29))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "Signature")
              (attribute "InnerClasses")))))


(defconst *ForkJoinTask$ExceptionNode-class-table*
  (make-static-class-decls 
   *java.util.concurrent.ForkJoinTask$ExceptionNode*))

(defconst *package-name-map* 
  ("java.util.concurrent.ForkJoinTask$ExceptionNode" . "java.util.concurrent"))

