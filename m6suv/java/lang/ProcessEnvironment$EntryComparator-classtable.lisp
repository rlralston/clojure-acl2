; ProcessEnvironment$EntryComparator-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:35 CDT 2014.
;

(defconst *java.lang.ProcessEnvironment$EntryComparator*
 (make-class-def
      '(class "java.lang.ProcessEnvironment$EntryComparator"
            "java.lang.Object"
            (constant_pool)
            (fields)
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *private* )
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
                        (method "compare"
                              (parameters (class "java.util.Map$Entry") (class "java.util.Map$Entry"))
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 25)
                                   (parsedcode
                                      (0 (invokestatic
					(methodCP "access$300" "java.lang.ProcessEnvironment" () (class "java.lang.ProcessEnvironment$NameComparator"))))
                                      (3 (aload_1))
                                      (4 (invokeinterface
					(methodCP "getKey" "java.util.Map$Entry" () (class "java.lang.Object")) 1))
                                      (9 (checkcast (class "java.lang.String")))
                                      (12 (aload_2))
                                      (13 (invokeinterface
					(methodCP "getKey" "java.util.Map$Entry" () (class "java.lang.Object")) 1))
                                      (18 (checkcast (class "java.lang.String")))
                                      (21 (invokevirtual
					(methodCP "compare" "java.lang.ProcessEnvironment$NameComparator" ((class "java.lang.String") (class "java.lang.String")) int)))
                                      (24 (ireturn))
                                      (endofcode 25))
                                   (Exceptions )
                                   (StackMap )))
                        (method "compare"
                              (parameters (class "java.lang.Object") (class "java.lang.Object"))
                              (returntype . int)
                              (accessflags  *class*  *public*  *volatile* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 13)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (checkcast (class "java.util.Map$Entry")))
                                      (5 (aload_2))
                                      (6 (checkcast (class "java.util.Map$Entry")))
                                      (9 (invokevirtual
					(methodCP "compare" "java.lang.ProcessEnvironment$EntryComparator" ((class "java.util.Map$Entry") (class "java.util.Map$Entry")) int)))
                                      (12 (ireturn))
                                      (endofcode 13))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.lang.ProcessEnvironment$1"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 1) (max_locals . 2) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.ProcessEnvironment$EntryComparator" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.util.Comparator")
            (accessflags  *class*  *final*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "Signature")
              (attribute "InnerClasses")))))


(defconst *ProcessEnvironment$EntryComparator-class-table*
  (make-static-class-decls 
   *java.lang.ProcessEnvironment$EntryComparator*))

(defconst *package-name-map* 
  ("java.lang.ProcessEnvironment$EntryComparator" . "java.lang"))

