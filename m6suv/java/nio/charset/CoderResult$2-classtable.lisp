; CoderResult$2-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:38 CDT 2014.
;

(defconst *java.nio.charset.CoderResult$2*
 (make-class-def
      '(class "java.nio.charset.CoderResult$2"
            "java.nio.charset.CoderResult$Cache"
            (constant_pool)
            (fields)
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 6)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aconst_null))
                                      (2 (invokespecial
					(methodCP "<init>" "java.nio.charset.CoderResult$Cache" ((class "java.nio.charset.CoderResult$1")) void)))
                                      (5 (return))
                                      (endofcode 6))
                                   (Exceptions )
                                   (StackMap )))
                        (method "create"
                              (parameters int)
                              (returntype . (class "java.nio.charset.CoderResult"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 5) (max_locals . 2) (code_length . 11)
                                   (parsedcode
                                      (0 (new (class "java.nio.charset.CoderResult")))
                                      (3 (dup))
                                      (4 (iconst_3))
                                      (5 (iload_1))
                                      (6 (aconst_null))
                                      (7 (invokespecial
					(methodCP "<init>" "java.nio.charset.CoderResult" (int int (class "java.nio.charset.CoderResult$1")) void)))
                                      (10 (areturn))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "EnclosingMethod")
              (attribute "InnerClasses")))))


(defconst *CoderResult$2-class-table*
  (make-static-class-decls 
   *java.nio.charset.CoderResult$2*))

(defconst *package-name-map* 
  ("java.nio.charset.CoderResult$2" . "java.nio.charset"))

