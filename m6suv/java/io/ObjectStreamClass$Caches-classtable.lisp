; ObjectStreamClass$Caches-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:32 CDT 2014.
;

(defconst *java.io.ObjectStreamClass$Caches*
 (make-class-def
      '(class "java.io.ObjectStreamClass$Caches"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "localDescs" (class "java.util.concurrent.ConcurrentMap") (accessflags  *class*  *final*  *static* ) -1)
                        (field "reflectors" (class "java.util.concurrent.ConcurrentMap") (accessflags  *class*  *final*  *static* ) -1)
                        (field "localDescsQueue" (class "java.lang.ref.ReferenceQueue") (accessflags  *class*  *final*  *private*  *static* ) -1)
                        (field "reflectorsQueue" (class "java.lang.ref.ReferenceQueue") (accessflags  *class*  *final*  *private*  *static* ) -1))
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
                        (method "access$200"
                              (parameters )
                              (returntype . (class "java.lang.ref.ReferenceQueue"))
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 1) (max_locals . 0) (code_length . 4)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "localDescsQueue" "java.io.ObjectStreamClass$Caches" (class "java.lang.ref.ReferenceQueue"))))
                                      (3 (areturn))
                                      (endofcode 4))
                                   (Exceptions )
                                   (StackMap )))
                        (method "access$2600"
                              (parameters )
                              (returntype . (class "java.lang.ref.ReferenceQueue"))
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 1) (max_locals . 0) (code_length . 4)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "reflectorsQueue" "java.io.ObjectStreamClass$Caches" (class "java.lang.ref.ReferenceQueue"))))
                                      (3 (areturn))
                                      (endofcode 4))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 41)
                                   (parsedcode
                                      (0 (new (class "java.util.concurrent.ConcurrentHashMap")))
                                      (3 (dup))
                                      (4 (invokespecial
					(methodCP "<init>" "java.util.concurrent.ConcurrentHashMap" () void)))
                                      (7 (putstatic (fieldCP "localDescs" "java.io.ObjectStreamClass$Caches" (class "java.util.concurrent.ConcurrentMap"))))
                                      (10 (new (class "java.util.concurrent.ConcurrentHashMap")))
                                      (13 (dup))
                                      (14 (invokespecial
					(methodCP "<init>" "java.util.concurrent.ConcurrentHashMap" () void)))
                                      (17 (putstatic (fieldCP "reflectors" "java.io.ObjectStreamClass$Caches" (class "java.util.concurrent.ConcurrentMap"))))
                                      (20 (new (class "java.lang.ref.ReferenceQueue")))
                                      (23 (dup))
                                      (24 (invokespecial
					(methodCP "<init>" "java.lang.ref.ReferenceQueue" () void)))
                                      (27 (putstatic (fieldCP "localDescsQueue" "java.io.ObjectStreamClass$Caches" (class "java.lang.ref.ReferenceQueue"))))
                                      (30 (new (class "java.lang.ref.ReferenceQueue")))
                                      (33 (dup))
                                      (34 (invokespecial
					(methodCP "<init>" "java.lang.ref.ReferenceQueue" () void)))
                                      (37 (putstatic (fieldCP "reflectorsQueue" "java.io.ObjectStreamClass$Caches" (class "java.lang.ref.ReferenceQueue"))))
                                      (40 (return))
                                      (endofcode 41))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *ObjectStreamClass$Caches-class-table*
  (make-static-class-decls 
   *java.io.ObjectStreamClass$Caches*))

(defconst *package-name-map* 
  ("java.io.ObjectStreamClass$Caches" . "java.io"))

