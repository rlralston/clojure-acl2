; InetAddressContainer-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:37 CDT 2014.
;

(defconst *java.net.InetAddressContainer*
 (make-class-def
      '(class "java.net.InetAddressContainer"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "addr" (class "java.net.InetAddress") (accessflags  *class* ) -1))
            (methods
                        (method "<init>"
                              (parameters )
                              (returntype . void)
                              (accessflags  *class* )
                              (code
                                   (max_stack . 1) (max_locals . 1) (code_length . 5)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (return))
                                      (endofcode 5))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *InetAddressContainer-class-table*
  (make-static-class-decls 
   *java.net.InetAddressContainer*))

(defconst *package-name-map* 
  ("java.net.InetAddressContainer" . "java.net"))

