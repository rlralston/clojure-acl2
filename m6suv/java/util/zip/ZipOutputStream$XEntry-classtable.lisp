; ZipOutputStream$XEntry-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:48 CDT 2014.
;

(defconst *java.util.zip.ZipOutputStream$XEntry*
 (make-class-def
      '(class "java.util.zip.ZipOutputStream$XEntry"
            "java.lang.Object"
            (constant_pool)
            (fields
                        (field "entry" (class "java.util.zip.ZipEntry") (accessflags  *class*  *final*  *public* ) -1)
                        (field "offset" long (accessflags  *class*  *final*  *public* ) -1))
            (methods
                        (method "<init>"
                              (parameters (class "java.util.zip.ZipEntry") long)
                              (returntype . void)
                              (accessflags  *class*  *public* )
                              (code
                                   (max_stack . 3) (max_locals . 4) (code_length . 15)
                                   (parsedcode
                                      (0 (aload_0))
                                      (1 (invokespecial
					(methodCP "<init>" "java.lang.Object" () void)))
                                      (4 (aload_0))
                                      (5 (aload_1))
                                      (6 (putfield (fieldCP "entry" "java.util.zip.ZipOutputStream$XEntry" (class "java.util.zip.ZipEntry"))))
                                      (9 (aload_0))
                                      (10 (lload_2))
                                      (11 (putfield (fieldCP "offset" "java.util.zip.ZipOutputStream$XEntry" long)))
                                      (14 (return))
                                      (endofcode 15))
                                   (Exceptions )
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *ZipOutputStream$XEntry-class-table*
  (make-static-class-decls 
   *java.util.zip.ZipOutputStream$XEntry*))

(defconst *package-name-map* 
  ("java.util.zip.ZipOutputStream$XEntry" . "java.util.zip"))

