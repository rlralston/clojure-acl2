; ObjectInputStream$Caches-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:32 CDT 2014.
;

(defconst *java.io.ObjectInputStream$Caches*
 (make-class-def
      '(class "java.io.ObjectInputStream$Caches"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "subclassAudits" (class "java.util.concurrent.ConcurrentMap") (accessflags  *class*  *final*  *static* ) -1)
                        (field "subclassAuditsQueue" (class "java.lang.ref.ReferenceQueue") (accessflags  *class*  *final*  *static* ) -1))
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
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 21)
                                   (parsedcode
                                      (0 (new (class "java.util.concurrent.ConcurrentHashMap")))
                                      (3 (dup))
                                      (4 (invokespecial
					(methodCP "<init>" "java.util.concurrent.ConcurrentHashMap" () void)))
                                      (7 (putstatic (fieldCP "subclassAudits" "java.io.ObjectInputStream$Caches" (class "java.util.concurrent.ConcurrentMap"))))
                                      (10 (new (class "java.lang.ref.ReferenceQueue")))
                                      (13 (dup))
                                      (14 (invokespecial
					(methodCP "<init>" "java.lang.ref.ReferenceQueue" () void)))
                                      (17 (putstatic (fieldCP "subclassAuditsQueue" "java.io.ObjectInputStream$Caches" (class "java.lang.ref.ReferenceQueue"))))
                                      (20 (return))
                                      (endofcode 21))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *ObjectInputStream$Caches-class-table*
  (make-static-class-decls 
   *java.io.ObjectInputStream$Caches*))

(defconst *package-name-map* 
  ("java.io.ObjectInputStream$Caches" . "java.io"))

