; ZipConstants64-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:48 CDT 2014.
;

(defconst *java.util.zip.ZipConstants64*
 (make-class-def
      '(class "java.util.zip.ZipConstants64"
            "java.lang.Object"
            (constant_pool
                        (LONG 101075792)
                        (LONG 117853008)
                        (INT 56)
                        (INT 20)
                        (INT 24)
                        (INT 1)
                        (INT 65535)
                        (LONG 4294967295)
                        (INT 4)
                        (INT 12)
                        (INT 14)
                        (INT 16)
                        (INT 32)
                        (INT 40)
                        (INT 48)
                        (INT 8)
                        (INT 2048))
            (fields
                        (field "ZIP64_ENDSIG" long (accessflags  *class*  *final*  *static* ) 0)
                        (field "ZIP64_LOCSIG" long (accessflags  *class*  *final*  *static* ) 1)
                        (field "ZIP64_ENDHDR" int (accessflags  *class*  *final*  *static* ) 2)
                        (field "ZIP64_LOCHDR" int (accessflags  *class*  *final*  *static* ) 3)
                        (field "ZIP64_EXTHDR" int (accessflags  *class*  *final*  *static* ) 4)
                        (field "ZIP64_EXTID" int (accessflags  *class*  *final*  *static* ) 5)
                        (field "ZIP64_MAGICCOUNT" int (accessflags  *class*  *final*  *static* ) 6)
                        (field "ZIP64_MAGICVAL" long (accessflags  *class*  *final*  *static* ) 7)
                        (field "ZIP64_ENDLEN" int (accessflags  *class*  *final*  *static* ) 8)
                        (field "ZIP64_ENDVEM" int (accessflags  *class*  *final*  *static* ) 9)
                        (field "ZIP64_ENDVER" int (accessflags  *class*  *final*  *static* ) 10)
                        (field "ZIP64_ENDNMD" int (accessflags  *class*  *final*  *static* ) 11)
                        (field "ZIP64_ENDDSK" int (accessflags  *class*  *final*  *static* ) 3)
                        (field "ZIP64_ENDTOD" int (accessflags  *class*  *final*  *static* ) 4)
                        (field "ZIP64_ENDTOT" int (accessflags  *class*  *final*  *static* ) 12)
                        (field "ZIP64_ENDSIZ" int (accessflags  *class*  *final*  *static* ) 13)
                        (field "ZIP64_ENDOFF" int (accessflags  *class*  *final*  *static* ) 14)
                        (field "ZIP64_ENDEXT" int (accessflags  *class*  *final*  *static* ) 2)
                        (field "ZIP64_LOCDSK" int (accessflags  *class*  *final*  *static* ) 8)
                        (field "ZIP64_LOCOFF" int (accessflags  *class*  *final*  *static* ) 15)
                        (field "ZIP64_LOCTOT" int (accessflags  *class*  *final*  *static* ) 11)
                        (field "ZIP64_EXTCRC" int (accessflags  *class*  *final*  *static* ) 8)
                        (field "ZIP64_EXTSIZ" int (accessflags  *class*  *final*  *static* ) 15)
                        (field "ZIP64_EXTLEN" int (accessflags  *class*  *final*  *static* ) 11)
                        (field "EFS" int (accessflags  *class*  *final*  *static* ) 16))
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
                                   (StackMap ))))
            (interfaces)
            (accessflags  *class*  *super*  *synchronized* )
            (attributes
              (attribute "SourceFile")))))


(defconst *ZipConstants64-class-table*
  (make-static-class-decls 
   *java.util.zip.ZipConstants64*))

(defconst *package-name-map* 
  ("java.util.zip.ZipConstants64" . "java.util.zip"))

