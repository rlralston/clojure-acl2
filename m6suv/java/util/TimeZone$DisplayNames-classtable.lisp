; TimeZone$DisplayNames-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:48 CDT 2014.
;

(defconst *java.util.TimeZone$DisplayNames*
 (make-class-def
      '(class "java.util.TimeZone$DisplayNames"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "CACHE" (class "java.util.Map") (accessflags  *class*  *final*  *private*  *static* ) -1))
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
                        (method "access$000"
                              (parameters )
                              (returntype . (class "java.util.Map"))
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 1) (max_locals . 0) (code_length . 4)
                                   (parsedcode
                                      (0 (getstatic (fieldCP "CACHE" "java.util.TimeZone$DisplayNames" (class "java.util.Map"))))
                                      (3 (areturn))
                                      (endofcode 4))
                                   (Exceptions )
                                   (StackMap )))
                        (method "<clinit>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class*  *static* )
                              (code
                                   (max_stack . 2) (max_locals . 0) (code_length . 11)
                                   (parsedcode
                                      (0 (new (class "java.util.concurrent.ConcurrentHashMap")))
                                      (3 (dup))
                                      (4 (invokespecial
					(methodCP "<init>" "java.util.concurrent.ConcurrentHashMap" () void)))
                                      (7 (putstatic (fieldCP "CACHE" "java.util.TimeZone$DisplayNames" (class "java.util.Map"))))
                                      (10 (return))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *TimeZone$DisplayNames-class-table*
  (make-static-class-decls 
   *java.util.TimeZone$DisplayNames*))

(defconst *package-name-map* 
  ("java.util.TimeZone$DisplayNames" . "java.util"))

