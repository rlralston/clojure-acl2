; ConcurrentHashMap$HashEntry-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:43 CDT 2014.
;

(defconst *java.util.concurrent.ConcurrentHashMap$HashEntry*
 (make-class-def
      '(class "java.util.concurrent.ConcurrentHashMap$HashEntry"
            "java.lang.Object"
            (constant_pool
                        (STRING  "next"))
            (fields
                        (field "hash" int (accessflags  *class*  *final* ) -1)
                        (field "key" (class "java.lang.Object") (accessflags  *class*  *final* ) -1)
                        (field "value" (class "java.lang.Object") (accessflags  *class*  *volatile* ) -1)
                        (field "next" (class "java.util.concurrent.ConcurrentHashMap$HashEntry") (accessflags  *class*  *volatile* ) -1)
                        (field "UNSAFE" (class "sun.misc.Unsafe") (accessflags  *class*  *final*  *static* ) -1)
                        (field "nextOffset" long (accessflags  *class*  *final*  *static* ) -1))
            (methods
                        (method "<init>"
                              (parameters int (class "java.lang.Object") (class "java.lang.Object") (class "java.util.concurrent.ConcurrentHashMap$HashEntry"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 5) (code_length . 26)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (aload_0))
                                      (5 (iload_1))
                                      (6 (putfield (fieldCP "hash" "java.util.concurrent.ConcurrentHashMap$HashEntry" int)))
                                      (9 (aload_0))
                                      (10 (aload_2))
                                      (11 (putfield (fieldCP "key" "java.util.concurrent.ConcurrentHashMap$HashEntry" (class "java.lang.Object"))))
                                      (14 (aload_0))
                                      (15 (aload_3))
                                      (16 (putfield (fieldCP "value" "java.util.concurrent.ConcurrentHashMap$HashEntry" (class "java.lang.Object"))))
                                      (19 (aload_0))
                                      (20 (aload 4))
                                      (22 (putfield (fieldCP "next" "java.util.concurrent.ConcurrentHashMap$HashEntry" (class "java.util.concurrent.ConcurrentHashMap$HashEntry"))))
                                      (25 (return))
                                      (endofcode 26))
                                   (Exceptions )
                                   (StackMap )))
                        (method "setNext"
                              (parameters (class "java.util.concurrent.ConcurrentHashMap$HashEntry"))
                              (returntype . void)
                              (accessflags  *class*  *final* )
                              (code
                                   (max_stack . 5) (max_locals . 2) (code_length . 12)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "UNSAFE" "java.util.concurrent.ConcurrentHashMap$HashEntry" (class "sun.misc.Unsafe"))))
                                      (3 (aload_0))
                                      (4 (getstatic (fieldCP "nextOffset" "java.util.concurrent.ConcurrentHashMap$HashEntry" long)))
                                      (7 (aload_1))
                                      (8 (invokevirtual
					(methodCP "putOrderedObject" "sun.misc.Unsafe" ((class "java.lang.Object") long (class "java.lang.Object")) void)))
                                      (11 (return))
                                      (endofcode 12))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 3) (max_locals . 1) (code_length . 39)
                                   (parsedcode
                                      (0 (invokestatic (methodCP "getUnsafe" "sun.misc.Unsafe" () (class "sun.misc.Unsafe")))) ;;at TAG_1
                                      (3 (putstatic (fieldCP "UNSAFE" "java.util.concurrent.ConcurrentHashMap$HashEntry" (class "sun.misc.Unsafe")))) 
                                      (6 (ldc_w )) 
                                      (9 (astore_0)) 
                                      (10 (getstatic (fieldCP "UNSAFE" "java.util.concurrent.ConcurrentHashMap$HashEntry" (class "sun.misc.Unsafe")))) 
                                      (13 (aload_0)) 
                                      (14 (ldc 0)) ;;STRING:: "next"
                                      (16 (invokevirtual (methodCP "getDeclaredField" "java.lang.Class" ((class "java.lang.String")) (class "java.lang.reflect.Field")))) 
                                      (19 (invokevirtual (methodCP "objectFieldOffset" "sun.misc.Unsafe" ((class "java.lang.reflect.Field")) long))) 
                                      (22 (putstatic (fieldCP "nextOffset" "java.util.concurrent.ConcurrentHashMap$HashEntry" long))) 
                                      (25 (goto 38)) ;;to TAG_0;;at TAG_2
                                      (28 (astore_0)) ;;at TAG_3
                                      (29 (new (class "java.lang.Error"))) 
                                      (32 (dup)) 
                                      (33 (aload_0)) 
                                      (34 (invokespecial (methodCP "<init>" "java.lang.Error" ((class "java.lang.Throwable")) void))) 
                                      (37 (athrow)) 
                                      (38 (return)) ;;at TAG_0
                                      (endofcode 39))
                                   (Exceptions 
                                     (handler 0 25  28 (class "java.lang.Exception")))
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *final*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "Signature")
              (attribute "InnerClasses")))))


(defconst *ConcurrentHashMap$HashEntry-class-table*
  (make-static-class-decls 
   *java.util.concurrent.ConcurrentHashMap$HashEntry*))

(defconst *package-name-map* 
  ("java.util.concurrent.ConcurrentHashMap$HashEntry" . "java.util.concurrent"))

