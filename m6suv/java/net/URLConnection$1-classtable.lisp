; URLConnection$1-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:38 CDT 2014.
;

(defconst *java.net.URLConnection$1*
 (make-class-def
      '(class "java.net.URLConnection$1"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "map" (class "java.net.FileNameMap") (accessflags  *class*  *private* ) -1))
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 2) (max_locals . 1) (code_length . 12)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (aload_0))
                                      (5 (invokestatic
					(methodCP "access$000" "java.net.URLConnection" () (class "java.net.FileNameMap"))))
                                      (8 (putfield (fieldCP "map" "java.net.URLConnection$1" (class "java.net.FileNameMap"))))
                                      (11 (return))
                                      (endofcode 12))
                                   (Exceptions )
                                   (StackMap )))
                        (method "getContentTypeFor"
                              (parameters (class "java.lang.String"))
                              (returntype . (class "java.lang.String"))
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 2) (max_locals . 2) (code_length . 11)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (getfield (fieldCP "map" "java.net.URLConnection$1" (class "java.net.FileNameMap"))))
                                      (4 (aload_1))
                                      (5 (invokeinterface
					(methodCP "getContentTypeFor" "java.net.FileNameMap" ((class "java.lang.String")) (class "java.lang.String")) 2))
                                      (10 (areturn))
                                      (endofcode 11))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces "java.net.FileNameMap")
            (accessflags  *class*  *final*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "EnclosingMethod")
              (attribute "InnerClasses")))))


(defconst *URLConnection$1-class-table*
  (make-static-class-decls 
   *java.net.URLConnection$1*))

(defconst *package-name-map* 
  ("java.net.URLConnection$1" . "java.net"))

