; FilterInputStream-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:32 CDT 2014.
;

(defconst *java.io.FilterInputStream*
 (make-class-def
      '(class "java.io.FilterInputStream"
            "java.io.InputStream"
            (constant_pool)
            (fields
                        (field "in" (class "java.io.InputStream") (accessflags  *class*  *protected*  *volatile* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.io.InputStream"))
                              (returntype . void)
                              (accessflags  *class*  *protected* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 10)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.io.InputStream" () void)))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (putfield (fieldCP "in" "java.io.FilterInputStream" (class "java.io.InputStream"))))
                                      (9 (return))
                                      (endofcode 10))
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
                                      (1 (getfield (fieldCP "in" "java.io.FilterInputStream" (class "java.io.InputStream"))))
                                      (4 (invokevirtual
					(methodCP "read" "java.io.InputStream" () int)))
                                      (7 (ireturn))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "read"
                              (parameters (array byte))
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 2) (code_length . 9)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (iconst_0))
                                      (3 (aload_1))
                                      (4 (arraylength))
                                      (5 (invokevirtual
					(methodCP "read" "java.io.FilterInputStream" ((array byte) int int) int)))
                                      (8 (ireturn))
                                      (endofcode 9))
                                   (Exceptions )
                                   (StackMap )))
                        (method "read"
                              (parameters (array byte) int int)
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 4) (code_length . 11)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "in" "java.io.FilterInputStream" (class "java.io.InputStream"))))
                                      (4 (aload_1))
                                      (5 (iload_2))
                                      (6 (iload_3))
                                      (7 (invokevirtual
					(methodCP "read" "java.io.InputStream" ((array byte) int int) int)))
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
                                      (1 (getfield (fieldCP "in" "java.io.FilterInputStream" (class "java.io.InputStream"))))
                                      (4 (lload_1))
                                      (5 (invokevirtual
					(methodCP "skip" "java.io.InputStream" (long) long)))
                                      (8 (lreturn))
                                      (endofcode 9))
                                   (Exceptions )
                                   (StackMap )))
                        (method "available"
                              (parameters )
                              (returntype . int)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 8)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "in" "java.io.FilterInputStream" (class "java.io.InputStream"))))
                                      (4 (invokevirtual
					(methodCP "available" "java.io.InputStream" () int)))
                                      (7 (ireturn))
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
                                      (1 (getfield (fieldCP "in" "java.io.FilterInputStream" (class "java.io.InputStream"))))
                                      (4 (invokevirtual
					(methodCP "close" "java.io.InputStream" () void)))
                                      (7 (return))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap )))
                        (method "mark"
                              (parameters int)
                              (returntype . void)
                              (accessflags  *class*  *public*  *super*  *synchronized* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 9)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "in" "java.io.FilterInputStream" (class "java.io.InputStream"))))
                                      (4 (iload_1))
                                      (5 (invokevirtual
					(methodCP "mark" "java.io.InputStream" (int) void)))
                                      (8 (return))
                                      (endofcode 9))
                                   (Exceptions )
                                   (StackMap )))
                        (method "reset"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *public*  *super*  *synchronized* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 8)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "in" "java.io.FilterInputStream" (class "java.io.InputStream"))))
                                      (4 (invokevirtual
					(methodCP "reset" "java.io.InputStream" () void)))
                                      (7 (return))
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
                                      (1 (getfield (fieldCP "in" "java.io.FilterInputStream" (class "java.io.InputStream"))))
                                      (4 (invokevirtual
					(methodCP "markSupported" "java.io.InputStream" () boolean)))
                                      (7 (ireturn))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *FilterInputStream-class-table*
  (make-static-class-decls 
   *java.io.FilterInputStream*))

(defconst *package-name-map* 
  ("java.io.FilterInputStream" . "java.io"))

