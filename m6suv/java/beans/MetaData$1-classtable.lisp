; MetaData$1-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:31 CDT 2014.
;

(defconst *java.beans.MetaData$1*
 (make-class-def
      '(class "java.beans.MetaData$1"
            "java.lang.Object"
            (constant_pool
                        (STRING  "Could not find class")
                        (STRING  "Could not find field"))
            (fields
                        (field "val$className" (class "java.lang.String") (accessflags  *class*  *final* ) -1)
                        (field "val$fieldName" (class "java.lang.String") (accessflags  *class*  *final* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.lang.String") (class "java.lang.String"))
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 3) (code_length . 15)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (aload_1))
                                      (2 (putfield (fieldCP "val$className" "java.beans.MetaData$1" (class "java.lang.String"))))
                                      (5 (aload_0))
                                      (6 (aload_2))
                                      (7 (putfield (fieldCP "val$fieldName" "java.beans.MetaData$1" (class "java.lang.String"))))
                                      (10 (aload_0))
                                      (11 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (14 (return))
                                      (endofcode 15))
                                   (Exceptions )
                                   (StackMap )))
                        (method "run"
                              (parameters )
                              (returntype . (class "java.lang.reflect.Field"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 4) (max_locals . 2) (code_length . 46)
                                   (parsedcode
                                      (0 (aload_0)) ;;at TAG_0
                                      (1 (getfield (fieldCP "val$className" "java.beans.MetaData$1" (class "java.lang.String")))) 
                                      (4 (invokestatic (methodCP "forName" "java.lang.Class" ((class "java.lang.String")) (class "java.lang.Class")))) 
                                      (7 (aload_0)) 
                                      (8 (getfield (fieldCP "val$fieldName" "java.beans.MetaData$1" (class "java.lang.String")))) 
                                      (11 (invokevirtual (methodCP "getDeclaredField" "java.lang.Class" ((class "java.lang.String")) (class "java.lang.reflect.Field")))) 
                                      (14 (astore_1)) 
                                      (15 (aload_1)) 
                                      (16 (iconst_1)) 
                                      (17 (invokevirtual (methodCP "setAccessible" "java.lang.reflect.Field" (boolean) void))) 
                                      (20 (aload_1)) 
                                      (21 (areturn)) ;;at TAG_1
                                      (22 (astore_1)) ;;at TAG_2
                                      (23 (new (class "java.lang.IllegalStateException"))) 
                                      (26 (dup)) 
                                      (27 (ldc 0)) ;;STRING:: "Could not find class"
                                      (29 (aload_1)) 
                                      (30 (invokespecial (methodCP "<init>" "java.lang.IllegalStateException" ((class "java.lang.String") (class "java.lang.Throwable")) void))) 
                                      (33 (athrow)) 
                                      (34 (astore_1)) ;;at TAG_3
                                      (35 (new (class "java.lang.IllegalStateException"))) 
                                      (38 (dup)) 
                                      (39 (ldc 1)) ;;STRING:: "Could not find field"
                                      (41 (aload_1)) 
                                      (42 (invokespecial (methodCP "<init>" "java.lang.IllegalStateException" ((class "java.lang.String") (class "java.lang.Throwable")) void))) 
                                      (45 (athrow)) 
                                      (endofcode 46))
                                   (Exceptions 
                                     (handler 0 21  22 (class "java.lang.ClassNotFoundException"))
                                     (handler 0 21  34 (class "java.lang.NoSuchFieldException")))
                                   (StackMap )))
                        (method "run"
                              (parameters )
                              (returntype . (class "java.lang.Object"))
                              (accessflags  *class*  *public*  *volatile* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokevirtual
					(methodCP "run" "java.beans.MetaData$1" () (class "java.lang.reflect.Field"))))
                                      (4 (areturn))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.security.PrivilegedAction")
            (accessflags  *class*  *final*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "EnclosingMethod")
              (attribute "Signature")
              (attribute "InnerClasses")))))


(defconst *MetaData$1-class-table*
  (make-static-class-decls 
   *java.beans.MetaData$1*))

(defconst *package-name-map* 
  ("java.beans.MetaData$1" . "java.beans"))

