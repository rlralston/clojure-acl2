; FilterReader-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:32 CDT 2014.
;

(defconst *java.io.FilterReader*
 (make-class-def
      '(class "java.io.FilterReader"
            "java.io.Reader"
            (constant_pool)
            (fields
                        (field "in" (class "java.io.Reader") (accessflags  *class*  *protected* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.io.Reader"))
                              (returntype . void)
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 11)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (invokespecial
					(methodCP "<init>" "java.io.Reader" ((class "java.lang.Object")) void)))
                                      (5 (aload_0))
                                      (6 (aload_1))
                                      (7 (putfield (fieldCP "in" "java.io.FilterReader" (class "java.io.Reader"))))
                                      (10 (return))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap )))
                        (method "read"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 8)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "in" "java.io.FilterReader" (class "java.io.Reader"))))
                                      (4 (invokevirtual
					(methodCP "read" "java.io.Reader" () int)))
                                      (7 (ireturn))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "read"
                              (parameters (array char) int int)
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 4) (code_length . 11)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "in" "java.io.FilterReader" (class "java.io.Reader"))))
                                      (4 (aload_1))
                                      (5 (iload_2))
                                      (6 (iload_3))
                                      (7 (invokevirtual
					(methodCP "read" "java.io.Reader" ((array char) int int) int)))
                                      (10 (ireturn))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap )))
                        (method "skip"
                              (parameters long)
                              (returntype . long)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 3) (code_length . 9)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "in" "java.io.FilterReader" (class "java.io.Reader"))))
                                      (4 (lload_1))
                                      (5 (invokevirtual
					(methodCP "skip" "java.io.Reader" (long) long)))
                                      (8 (lreturn))
                                      (endofcode 9))
                                   (Exceptions )
                                   (StackMap )))
                        (method "ready"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 8)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "in" "java.io.FilterReader" (class "java.io.Reader"))))
                                      (4 (invokevirtual
					(methodCP "ready" "java.io.Reader" () boolean)))
                                      (7 (ireturn))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "markSupported"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 8)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "in" "java.io.FilterReader" (class "java.io.Reader"))))
                                      (4 (invokevirtual
					(methodCP "markSupported" "java.io.Reader" () boolean)))
                                      (7 (ireturn))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "mark"
                              (parameters int)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 9)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "in" "java.io.FilterReader" (class "java.io.Reader"))))
                                      (4 (iload_1))
                                      (5 (invokevirtual
					(methodCP "mark" "java.io.Reader" (int) void)))
                                      (8 (return))
                                      (endofcode 9))
                                   (Exceptions )
                                   (StackMap )))
                        (method "reset"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 8)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "in" "java.io.FilterReader" (class "java.io.Reader"))))
                                      (4 (invokevirtual
					(methodCP "reset" "java.io.Reader" () void)))
                                      (7 (return))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "close"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 8)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "in" "java.io.FilterReader" (class "java.io.Reader"))))
                                      (4 (invokevirtual
					(methodCP "close" "java.io.Reader" () void)))
                                      (7 (return))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *abstract*  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *FilterReader-class-table*
  (make-static-class-decls 
   *java.io.FilterReader*))

(defconst *package-name-map* 
  ("java.io.FilterReader" . "java.io"))

