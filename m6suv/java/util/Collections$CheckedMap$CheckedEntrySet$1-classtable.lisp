; Collections$CheckedMap$CheckedEntrySet$1-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:42 CDT 2014.
;

(defconst *java.util.Collections$CheckedMap$CheckedEntrySet$1*
 (make-class-def
      '(class "java.util.Collections$CheckedMap$CheckedEntrySet$1"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "val$i" (class "java.util.Iterator") (accessflags  *class*  *final* ) -1)
                        (field "val$valueType" (class "java.lang.Class") (accessflags  *class*  *final* ) -1)
                        (field "this$0" (class "java.util.Collections$CheckedMap$CheckedEntrySet") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.util.Collections$CheckedMap$CheckedEntrySet") (class "java.util.Iterator") (class "java.lang.Class"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 4) (code_length . 20)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "this$0" "java.util.Collections$CheckedMap$CheckedEntrySet$1" (class "java.util.Collections$CheckedMap$CheckedEntrySet"))))
                                      (5 (aload_0))
                                      (6 (aload_2))
                                      (7 (putfield (fieldCP "val$i" "java.util.Collections$CheckedMap$CheckedEntrySet$1" (class "java.util.Iterator"))))
                                      (10 (aload_0))
                                      (11 (aload_3))
                                      (12 (putfield (fieldCP "val$valueType" "java.util.Collections$CheckedMap$CheckedEntrySet$1" (class "java.lang.Class"))))
                                      (15 (aload_0))
                                      (16 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (19 (return))
                                      (endofcode 20))
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
                                      (1 (getfield (fieldCP "val$i" "java.util.Collections$CheckedMap$CheckedEntrySet$1" (class "java.util.Iterator"))))
                                      (4 (invokeinterface
					(methodCP "hasNext" "java.util.Iterator" () boolean) 1))
                                      (9 (ireturn))
                                      (endofcode 10))
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
                                      (1 (getfield (fieldCP "val$i" "java.util.Collections$CheckedMap$CheckedEntrySet$1" (class "java.util.Iterator"))))
                                      (4 (invokeinterface
					(methodCP "remove" "java.util.Iterator" () void) 1))
                                      (9 (return))
                                      (endofcode 10))
                                   (Exceptions )
                                   (StackMap )))
                        (method "next"
                              (parameters )
                              (returntype . (class "java.util.Map$Entry"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 20)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "val$i" "java.util.Collections$CheckedMap$CheckedEntrySet$1" (class "java.util.Iterator"))))
                                      (4 (invokeinterface
					(methodCP "next" "java.util.Iterator" () (class "java.lang.Object")) 1))
                                      (9 (checkcast (class "java.util.Map$Entry")))
                                      (12 (aload_0))
                                      (13 (getfield (fieldCP "val$valueType" "java.util.Collections$CheckedMap$CheckedEntrySet$1" (class "java.lang.Class"))))
                                      (16 (invokestatic
					(methodCP "checkedEntry" "java.util.Collections$CheckedMap$CheckedEntrySet" ((class "java.util.Map$Entry") (class "java.lang.Class")) (class "java.util.Collections$CheckedMap$CheckedEntrySet$CheckedEntry"))))
                                      (19 (areturn))
                                      (endofcode 20))
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
					(methodCP "next" "java.util.Collections$CheckedMap$CheckedEntrySet$1" () (class "java.util.Map$Entry"))))
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


(defconst *Collections$CheckedMap$CheckedEntrySet$1-class-table*
  (make-static-class-decls 
   *java.util.Collections$CheckedMap$CheckedEntrySet$1*))

(defconst *package-name-map* 
  ("java.util.Collections$CheckedMap$CheckedEntrySet$1" . "java.util"))

