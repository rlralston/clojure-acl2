; ThreadLocalRandom$1-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:44 CDT 2014.
;

(defconst *java.util.concurrent.ThreadLocalRandom$1*
 (make-class-def
      '(class "java.util.concurrent.ThreadLocalRandom$1"
            "java.lang.ThreadLocal"
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
					(methodCP "<init>" "java.lang.ThreadLocal" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "initialValue"
                              (parameters )
                              (returntype . (class "java.util.concurrent.ThreadLocalRandom"))
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 8)
                                   (parsedcode
                                      (0 (new (class "java.util.concurrent.ThreadLocalRandom")))
                                      (3 (dup))
                                      (4 (invokespecial
					(methodCP "<init>" "java.util.concurrent.ThreadLocalRandom" () void)))
                                      (7 (areturn))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "initialValue"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *protected*  *volatile* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokevirtual
					(methodCP "initialValue" "java.util.concurrent.ThreadLocalRandom$1" () (class "java.util.concurrent.ThreadLocalRandom"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "EnclosingMethod")
              (attribute "Signature")
              (attribute "InnerClasses")))))


(defconst *ThreadLocalRandom$1-class-table*
  (make-static-class-decls 
   *java.util.concurrent.ThreadLocalRandom$1*))

(defconst *package-name-map* 
  ("java.util.concurrent.ThreadLocalRandom$1" . "java.util.concurrent"))

