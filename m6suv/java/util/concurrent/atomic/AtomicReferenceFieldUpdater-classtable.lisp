; AtomicReferenceFieldUpdater-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:43 CDT 2014.
;

(defconst *java.util.concurrent.atomic.AtomicReferenceFieldUpdater*
 (make-class-def
      '(class "java.util.concurrent.atomic.AtomicReferenceFieldUpdater"
            "java.lang.Object"
            (constant_pool)
            (fields)
            (methods
                        (method "newUpdater"
                              (parameters (class "java.lang.Class") (class "java.lang.Class") (class "java.lang.String"))
                              (returntype . (class "java.util.concurrent.atomic.AtomicReferenceFieldUpdater"))
                              (accessflags  *class*  *public*  *static* )
                              (code
                                   (max_stack . 5) (max_locals . 3) (code_length . 11)
                                   (parsedcode
                                      (0 (new (class "java.util.concurrent.atomic.AtomicReferenceFieldUpdater$AtomicReferenceFieldUpdaterImpl")))
                                      (3 (dup))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (aload_2))
                                      (7 (invokespecial
					(methodCP "<init>" "java.util.concurrent.atomic.AtomicReferenceFieldUpdater$AtomicReferenceFieldUpdaterImpl" ((class "java.lang.Class") (class "java.lang.Class") (class "java.lang.String")) void)))
                                      (10 (areturn))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *protected* )
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
                        (method "compareAndSet"
                              (parameters (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . boolean)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "weakCompareAndSet"
                              (parameters (class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . boolean)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "set"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "lazySet"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "get"
                              (parameters (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getAndSet"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 4) (code_length . 21)
                                   (parsedcode
                                      (0 (aload_0)) ;;at TAG_1
                                      (1 (aload_1)) 
                                      (2 (invokevirtual (methodCP "get" "java.util.concurrent.atomic.AtomicReferenceFieldUpdater" ((class "java.lang.Object")) (class "java.lang.Object")))) 
                                      (5 (astore_3)) 
                                      (6 (aload_0)) 
                                      (7 (aload_1)) 
                                      (8 (aload_3)) 
                                      (9 (aload_2)) 
                                      (10 (invokevirtual (methodCP "compareAndSet" "java.util.concurrent.atomic.AtomicReferenceFieldUpdater" ((class "java.lang.Object") (class "java.lang.Object") (class "java.lang.Object")) boolean))) 
                                      (13 (ifeq 18))  ;;to TAG_0
                                      (16 (aload_3)) 
                                      (17 (areturn)) 
                                      (18 (goto 0)) ;;to TAG_1;;at TAG_0
                                      (endofcode 21))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *abstract*  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "Signature")
              (attribute "InnerClasses")))))


(defconst *AtomicReferenceFieldUpdater-class-table*
  (make-static-class-decls 
   *java.util.concurrent.atomic.AtomicReferenceFieldUpdater*))

(defconst *package-name-map* 
  ("java.util.concurrent.atomic.AtomicReferenceFieldUpdater" . "java.util.concurrent.atomic"))

