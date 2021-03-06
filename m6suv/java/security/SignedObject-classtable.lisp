; SignedObject-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:41 CDT 2014.
;

(defconst *java.security.SignedObject*
 (make-class-def
      '(class "java.security.SignedObject"
            "java.lang.Object"
            (constant_pool
                        (LONG 720502720485447167)
                        (STRING  "content")
                        (STRING  "signature")
                        (STRING  "thealgorithm"))
            (fields
                        (field "serialVersionUID" long (accessflags  *class*  *final*  *private*  *static* ) 0)
                        (field "content" (array byte) (accessflags  *class*  *private* ) -1)
                        (field "signature" (array byte) (accessflags  *class*  *private* ) -1)
                        (field "thealgorithm" (class "java.lang.String") (accessflags  *class*  *private* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.io.Serializable") (class "java.security.PrivateKey") (class "java.security.Signature"))
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 6) (code_length . 67)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (new (class "java.io.ByteArrayOutputStream")))
                                      (7 (dup))
                                      (8 (invokespecial
					(methodCP "<init>" "java.io.ByteArrayOutputStream" () void)))
                                      (11 (astore 4))
                                      (13 (new (class "java.io.ObjectOutputStream")))
                                      (16 (dup))
                                      (17 (aload 4))
                                      (19 (invokespecial
					(methodCP "<init>" "java.io.ObjectOutputStream" ((class "java.io.OutputStream")) void)))
                                      (22 (astore 5))
                                      (24 (aload 5))
                                      (26 (aload_1))
                                      (27 (invokeinterface
					(methodCP "writeObject" "java.io.ObjectOutput" ((class "java.lang.Object")) void) 2))
                                      (32 (aload 5))
                                      (34 (invokeinterface
					(methodCP "flush" "java.io.ObjectOutput" () void) 1))
                                      (39 (aload 5))
                                      (41 (invokeinterface
					(methodCP "close" "java.io.ObjectOutput" () void) 1))
                                      (46 (aload_0))
                                      (47 (aload 4))
                                      (49 (invokevirtual
					(methodCP "toByteArray" "java.io.ByteArrayOutputStream" () (array byte))))
                                      (52 (putfield (fieldCP "content" "java.security.SignedObject" (array byte))))
                                      (55 (aload 4))
                                      (57 (invokevirtual
					(methodCP "close" "java.io.ByteArrayOutputStream" () void)))
                                      (60 (aload_0))
                                      (61 (aload_2))
                                      (62 (aload_3))
                                      (63 (invokespecial
					(methodCP "sign" "java.security.SignedObject" ((class "java.security.PrivateKey") (class "java.security.Signature")) void)))
                                      (66 (return))
                                      (endofcode 67))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getObject"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 4) (code_length . 40)
                                   (parsedcode
                                      (0 (new (class "java.io.ByteArrayInputStream")))
                                      (3 (dup))
                                      (4 (aload_0))
                                      (5 (getfield (fieldCP "content" "java.security.SignedObject" (array byte))))
                                      (8 (invokespecial
					(methodCP "<init>" "java.io.ByteArrayInputStream" ((array byte)) void)))
                                      (11 (astore_1))
                                      (12 (new (class "java.io.ObjectInputStream")))
                                      (15 (dup))
                                      (16 (aload_1))
                                      (17 (invokespecial
					(methodCP "<init>" "java.io.ObjectInputStream" ((class "java.io.InputStream")) void)))
                                      (20 (astore_2))
                                      (21 (aload_2))
                                      (22 (invokeinterface
					(methodCP "readObject" "java.io.ObjectInput" () (class "java.lang.Object")) 1))
                                      (27 (astore_3))
                                      (28 (aload_1))
                                      (29 (invokevirtual
					(methodCP "close" "java.io.ByteArrayInputStream" () void)))
                                      (32 (aload_2))
                                      (33 (invokeinterface
					(methodCP "close" "java.io.ObjectInput" () void) 1))
                                      (38 (aload_3))
                                      (39 (areturn))
                                      (endofcode 40))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getSignature"
                              (parameters )
                              (returntype . (array byte))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 11)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "signature" "java.security.SignedObject" (array byte))))
                                      (4 (invokevirtual
					(methodCP "clone" "byte[]" () (class "java.lang.Object"))))
                                      (7 (checkcast (array byte)))
                                      (10 (areturn))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getAlgorithm"
                              (parameters )
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "thealgorithm" "java.security.SignedObject" (class "java.lang.String"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap )))
                        (method "verify"
                              (parameters (class "java.security.PublicKey") (class "java.security.Signature"))
                              (returntype . boolean)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 34)
                                   (parsedcode
                                      (0 (aload_2))
                                      (1 (aload_1))
                                      (2 (invokevirtual
					(methodCP "initVerify" "java.security.Signature" ((class "java.security.PublicKey")) void)))
                                      (5 (aload_2))
                                      (6 (aload_0))
                                      (7 (getfield (fieldCP "content" "java.security.SignedObject" (array byte))))
                                      (10 (invokevirtual
					(methodCP "clone" "byte[]" () (class "java.lang.Object"))))
                                      (13 (checkcast (array byte)))
                                      (16 (invokevirtual
					(methodCP "update" "java.security.Signature" ((array byte)) void)))
                                      (19 (aload_2))
                                      (20 (aload_0))
                                      (21 (getfield (fieldCP "signature" "java.security.SignedObject" (array byte))))
                                      (24 (invokevirtual
					(methodCP "clone" "byte[]" () (class "java.lang.Object"))))
                                      (27 (checkcast (array byte)))
                                      (30 (invokevirtual
					(methodCP "verify" "java.security.Signature" ((array byte)) boolean)))
                                      (33 (ireturn))
                                      (endofcode 34))
                                   (Exceptions )
                                   (StackMap )))
                        (method "sign"
                              (parameters (class "java.security.PrivateKey") (class "java.security.Signature"))
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 42)
                                   (parsedcode
                                      (0 (aload_2))
                                      (1 (aload_1))
                                      (2 (invokevirtual
					(methodCP "initSign" "java.security.Signature" ((class "java.security.PrivateKey")) void)))
                                      (5 (aload_2))
                                      (6 (aload_0))
                                      (7 (getfield (fieldCP "content" "java.security.SignedObject" (array byte))))
                                      (10 (invokevirtual
					(methodCP "clone" "byte[]" () (class "java.lang.Object"))))
                                      (13 (checkcast (array byte)))
                                      (16 (invokevirtual
					(methodCP "update" "java.security.Signature" ((array byte)) void)))
                                      (19 (aload_0))
                                      (20 (aload_2))
                                      (21 (invokevirtual
					(methodCP "sign" "java.security.Signature" () (array byte))))
                                      (24 (invokevirtual
					(methodCP "clone" "byte[]" () (class "java.lang.Object"))))
                                      (27 (checkcast (array byte)))
                                      (30 (putfield (fieldCP "signature" "java.security.SignedObject" (array byte))))
                                      (33 (aload_0))
                                      (34 (aload_2))
                                      (35 (invokevirtual
					(methodCP "getAlgorithm" "java.security.Signature" () (class "java.lang.String"))))
                                      (38 (putfield (fieldCP "thealgorithm" "java.security.SignedObject" (class "java.lang.String"))))
                                      (41 (return))
                                      (endofcode 42))
                                   (Exceptions )
                                   (StackMap )))
                        (method "readObject"
                              (parameters (class "java.io.ObjectInputStream"))
                              (returntype . void)
                              (accessflags  *class*  *private* )
                              (code
                                   (max_stack . 4) (max_locals . 3) (code_length . 66)
                                   (parsedcode
                                      (0 (aload_1))
                                      (1 (invokevirtual
					(methodCP "readFields" "java.io.ObjectInputStream" () (class "java.io.ObjectInputStream$GetField"))))
                                      (4 (astore_2))
                                      (5 (aload_0))
                                      (6 (aload_2))
                                      (7 (ldc 1))         ;;STRING:: "content"
                                      (9 (aconst_null))
                                      (10 (invokevirtual
					(methodCP "get" "java.io.ObjectInputStream$GetField" ((class "java.lang.String") (class "java.lang.Object")) (class "java.lang.Object"))))
                                      (13 (checkcast (array byte)))
                                      (16 (checkcast (array byte)))
                                      (19 (invokevirtual
					(methodCP "clone" "byte[]" () (class "java.lang.Object"))))
                                      (22 (checkcast (array byte)))
                                      (25 (putfield (fieldCP "content" "java.security.SignedObject" (array byte))))
                                      (28 (aload_0))
                                      (29 (aload_2))
                                      (30 (ldc 2))        ;;STRING:: "signature"
                                      (32 (aconst_null))
                                      (33 (invokevirtual
					(methodCP "get" "java.io.ObjectInputStream$GetField" ((class "java.lang.String") (class "java.lang.Object")) (class "java.lang.Object"))))
                                      (36 (checkcast (array byte)))
                                      (39 (checkcast (array byte)))
                                      (42 (invokevirtual
					(methodCP "clone" "byte[]" () (class "java.lang.Object"))))
                                      (45 (checkcast (array byte)))
                                      (48 (putfield (fieldCP "signature" "java.security.SignedObject" (array byte))))
                                      (51 (aload_0))
                                      (52 (aload_2))
                                      (53 (ldc 3))        ;;STRING:: "thealgorithm"
                                      (55 (aconst_null))
                                      (56 (invokevirtual
					(methodCP "get" "java.io.ObjectInputStream$GetField" ((class "java.lang.String") (class "java.lang.Object")) (class "java.lang.Object"))))
                                      (59 (checkcast (class "java.lang.String")))
                                      (62 (putfield (fieldCP "thealgorithm" "java.security.SignedObject" (class "java.lang.String"))))
                                      (65 (return))
                                      (endofcode 66))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.io.Serializable")
            (accessflags  *class*  *final*  *public*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *SignedObject-class-table*
  (make-static-class-decls 
   *java.security.SignedObject*))

(defconst *package-name-map* 
  ("java.security.SignedObject" . "java.security"))

