; ProcessEnvironment$CheckedEntrySet$1-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:35 CDT 2014.
;

(defconst *java.lang.ProcessEnvironment$CheckedEntrySet$1*
 (make-class-def
      '(class "java.lang.ProcessEnvironment$CheckedEntrySet$1"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "i" (class "java.util.Iterator") (accessflags  *class* ) -1)
                        (field "this$0" (class "java.lang.ProcessEnvironment$CheckedEntrySet") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.lang.ProcessEnvironment$CheckedEntrySet"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 26)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "this$0" "java.lang.ProcessEnvironment$CheckedEntrySet$1" (class "java.lang.ProcessEnvironment$CheckedEntrySet"))))
                                      (5 (aload_0))
                                      (6 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (9 (aload_0))
                                      (10 (aload_0))
                                      (11 (getfield (fieldCP "this$0" "java.lang.ProcessEnvironment$CheckedEntrySet$1" (class "java.lang.ProcessEnvironment$CheckedEntrySet"))))
                                      (14 (invokestatic
					(methodCP "access$100" "java.lang.ProcessEnvironment$CheckedEntrySet" ((class "java.lang.ProcessEnvironment$CheckedEntrySet")) (class "java.util.Set"))))
                                      (17 (invokeinterface
					(methodCP "iterator" "java.util.Set" () (class "java.util.Iterator")) 1))
                                      (22 (putfield (fieldCP "i" "java.lang.ProcessEnvironment$CheckedEntrySet$1" (class "java.util.Iterator"))))
                                      (25 (return))
                                      (endofcode 26))
                                   (Exceptions )
                                   (StackMap )))
                        (method "hasNext"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "i" "java.lang.ProcessEnvironment$CheckedEntrySet$1" (class "java.util.Iterator"))))
                                      (4 (invokeinterface
					(methodCP "hasNext" "java.util.Iterator" () boolean) 1))
                                      (9 (ireturn))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "next"
                              (parameters )
                              (returntype . (class "java.util.Map$Entry"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 1) (code_length . 20)
                                   (parsedcode
                                      (0 (new (class "java.lang.ProcessEnvironment$CheckedEntry")))
                                      (3 (dup))
                                      (4 (aload_0))
                                      (5 (getfield (fieldCP "i" "java.lang.ProcessEnvironment$CheckedEntrySet$1" (class "java.util.Iterator"))))
                                      (8 (invokeinterface
					(methodCP "next" "java.util.Iterator" () (class "java.lang.Object")) 1))
                                      (13 (checkcast (class "java.util.Map$Entry")))
                                      (16 (invokespecial
					(methodCP "<init>" "java.lang.ProcessEnvironment$CheckedEntry" ((class "java.util.Map$Entry")) void)))
                                      (19 (areturn))
                                      (endofcode 20))
                                   (Exceptions )
                                   (StackMap )))
                        (method "remove"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "i" "java.lang.ProcessEnvironment$CheckedEntrySet$1" (class "java.util.Iterator"))))
                                      (4 (invokeinterface
					(methodCP "remove" "java.util.Iterator" () void) 1))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "next"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public*  *volatile* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokevirtual
					(methodCP "next" "java.lang.ProcessEnvironment$CheckedEntrySet$1" () (class "java.util.Map$Entry"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.util.Iterator")
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "EnclosingMethod")
              (attribute "Signature")
              (attribute "InnerClasses")))))


(defconst *ProcessEnvironment$CheckedEntrySet$1-class-table*
  (make-static-class-decls 
   *java.lang.ProcessEnvironment$CheckedEntrySet$1*))

(defconst *package-name-map* 
  ("java.lang.ProcessEnvironment$CheckedEntrySet$1" . "java.lang"))
