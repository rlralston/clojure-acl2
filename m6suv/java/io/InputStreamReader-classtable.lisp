; InputStreamReader-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:32 CDT 2014.
;

(defconst *java.io.InputStreamReader*
 (make-class-def
      '(class "java.io.InputStreamReader"
            "java.io.Reader"
            (constant_pool
                        (STRING  "charsetName")
                        (STRING  "charset")
                        (STRING  "charset decoder"))
            (fields
                        (field "sd" (class "sun.nio.cs.StreamDecoder") (accessflags  *class*  *final*  *private* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.io.InputStream"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 3) (code_length . 32)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (aload_1)) 
                                      (2 (invokespecial (methodCP "<init>" "java.io.Reader" ((class "java.lang.Object")) void))) 
                                      (5 (aload_0)) ;;at TAG_1
                                      (6 (aload_1)) 
                                      (7 (aload_0)) 
                                      (8 (aconst_null)) 
                                      (9 (checkcast (class "java.lang.String"))) 
                                      (12 (invokestatic (methodCP "forInputStreamReader" "sun.nio.cs.StreamDecoder" ((class "java.io.InputStream") (class "java.lang.Object") (class "java.lang.String")) (class "sun.nio.cs.StreamDecoder")))) 
                                      (15 (putfield (fieldCP "sd" "java.io.InputStreamReader" (class "sun.nio.cs.StreamDecoder")))) 
                                      (18 (goto 31)) ;;to TAG_0;;at TAG_2
                                      (21 (astore_2)) ;;at TAG_3
                                      (22 (new (class "java.lang.Error"))) 
                                      (25 (dup)) 
                                      (26 (aload_2)) 
                                      (27 (invokespecial (methodCP "<init>" "java.lang.Error" ((class "java.lang.Throwable")) void))) 
                                      (30 (athrow)) 
                                      (31 (return)) ;;at TAG_0
                                      (endofcode 32))
                                   (Exceptions 
                                     (handler 5 18  21 (class "java.io.UnsupportedEncodingException")))
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.io.InputStream") (class "java.lang.String"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 3) (code_length . 30)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (aload_1)) 
                                      (2 (invokespecial (methodCP "<init>" "java.io.Reader" ((class "java.lang.Object")) void))) 
                                      (5 (aload_2)) 
                                      (6 (ifnonnull 19))  ;;to TAG_0
                                      (9 (new (class "java.lang.NullPointerException"))) 
                                      (12 (dup)) 
                                      (13 (ldc 0)) ;;STRING:: "charsetName"
                                      (15 (invokespecial (methodCP "<init>" "java.lang.NullPointerException" ((class "java.lang.String")) void))) 
                                      (18 (athrow)) 
                                      (19 (aload_0)) ;;at TAG_0
                                      (20 (aload_1)) 
                                      (21 (aload_0)) 
                                      (22 (aload_2)) 
                                      (23 (invokestatic (methodCP "forInputStreamReader" "sun.nio.cs.StreamDecoder" ((class "java.io.InputStream") (class "java.lang.Object") (class "java.lang.String")) (class "sun.nio.cs.StreamDecoder")))) 
                                      (26 (putfield (fieldCP "sd" "java.io.InputStreamReader" (class "sun.nio.cs.StreamDecoder")))) 
                                      (29 (return)) 
                                      (endofcode 30))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.io.InputStream") (class "java.nio.charset.Charset"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 3) (code_length . 30)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (aload_1)) 
                                      (2 (invokespecial (methodCP "<init>" "java.io.Reader" ((class "java.lang.Object")) void))) 
                                      (5 (aload_2)) 
                                      (6 (ifnonnull 19))  ;;to TAG_0
                                      (9 (new (class "java.lang.NullPointerException"))) 
                                      (12 (dup)) 
                                      (13 (ldc 1)) ;;STRING:: "charset"
                                      (15 (invokespecial (methodCP "<init>" "java.lang.NullPointerException" ((class "java.lang.String")) void))) 
                                      (18 (athrow)) 
                                      (19 (aload_0)) ;;at TAG_0
                                      (20 (aload_1)) 
                                      (21 (aload_0)) 
                                      (22 (aload_2)) 
                                      (23 (invokestatic (methodCP "forInputStreamReader" "sun.nio.cs.StreamDecoder" ((class "java.io.InputStream") (class "java.lang.Object") (class "java.nio.charset.Charset")) (class "sun.nio.cs.StreamDecoder")))) 
                                      (26 (putfield (fieldCP "sd" "java.io.InputStreamReader" (class "sun.nio.cs.StreamDecoder")))) 
                                      (29 (return)) 
                                      (endofcode 30))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<init>"
                              (parameters (class "java.io.InputStream") (class "java.nio.charset.CharsetDecoder"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 3) (code_length . 30)
                                   (parsedcode
                                      (0 (aload_0)) 
                                      (1 (aload_1)) 
                                      (2 (invokespecial (methodCP "<init>" "java.io.Reader" ((class "java.lang.Object")) void))) 
                                      (5 (aload_2)) 
                                      (6 (ifnonnull 19))  ;;to TAG_0
                                      (9 (new (class "java.lang.NullPointerException"))) 
                                      (12 (dup)) 
                                      (13 (ldc 2)) ;;STRING:: "charset decoder"
                                      (15 (invokespecial (methodCP "<init>" "java.lang.NullPointerException" ((class "java.lang.String")) void))) 
                                      (18 (athrow)) 
                                      (19 (aload_0)) ;;at TAG_0
                                      (20 (aload_1)) 
                                      (21 (aload_0)) 
                                      (22 (aload_2)) 
                                      (23 (invokestatic (methodCP "forInputStreamReader" "sun.nio.cs.StreamDecoder" ((class "java.io.InputStream") (class "java.lang.Object") (class "java.nio.charset.CharsetDecoder")) (class "sun.nio.cs.StreamDecoder")))) 
                                      (26 (putfield (fieldCP "sd" "java.io.InputStreamReader" (class "sun.nio.cs.StreamDecoder")))) 
                                      (29 (return)) 
                                      (endofcode 30))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getEncoding"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 8)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "sd" "java.io.InputStreamReader" (class "sun.nio.cs.StreamDecoder"))))
                                      (4 (invokevirtual
					(methodCP "getEncoding" "sun.nio.cs.StreamDecoder" () (class "java.lang.String"))))
                                      (7 (areturn))
                                      (endofcode 8))
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
                                      (1 (getfield (fieldCP "sd" "java.io.InputStreamReader" (class "sun.nio.cs.StreamDecoder"))))
                                      (4 (invokevirtual
					(methodCP "read" "sun.nio.cs.StreamDecoder" () int)))
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
                                      (1 (getfield (fieldCP "sd" "java.io.InputStreamReader" (class "sun.nio.cs.StreamDecoder"))))
                                      (4 (aload_1))
                                      (5 (iload_2))
                                      (6 (iload_3))
                                      (7 (invokevirtual
					(methodCP "read" "sun.nio.cs.StreamDecoder" ((array char) int int) int)))
                                      (10 (ireturn))
                                      (endofcode 11))
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
                                      (1 (getfield (fieldCP "sd" "java.io.InputStreamReader" (class "sun.nio.cs.StreamDecoder"))))
                                      (4 (invokevirtual
					(methodCP "ready" "sun.nio.cs.StreamDecoder" () boolean)))
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
                                      (1 (getfield (fieldCP "sd" "java.io.InputStreamReader" (class "sun.nio.cs.StreamDecoder"))))
                                      (4 (invokevirtual
					(methodCP "close" "sun.nio.cs.StreamDecoder" () void)))
                                      (7 (return))
                                      (endofcode 8))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *InputStreamReader-class-table*
  (make-static-class-decls 
   *java.io.InputStreamReader*))

(defconst *package-name-map* 
  ("java.io.InputStreamReader" . "java.io"))
