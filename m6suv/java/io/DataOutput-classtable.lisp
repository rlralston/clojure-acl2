; DataOutput-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:32 CDT 2014.
;

(defconst *java.io.DataOutput*
 (make-class-def
      '(class "java.io.DataOutput"
            "java.lang.Object"
            (constant_pool)
            (fields)
            (methods
                        (method "write"
                              (parameters int)
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "write"
                              (parameters (array byte))
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "write"
                              (parameters (array byte) int int)
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "writeBoolean"
                              (parameters boolean)
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "writeByte"
                              (parameters int)
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "writeShort"
                              (parameters int)
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "writeChar"
                              (parameters int)
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "writeInt"
                              (parameters int)
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "writeLong"
                              (parameters long)
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "writeFloat"
                              (parameters float)
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "writeDouble"
                              (parameters double)
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "writeBytes"
                              (parameters (class "java.lang.String"))
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "writeChars"
                              (parameters (class "java.lang.String"))
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "writeUTF"
                              (parameters (class "java.lang.String"))
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code)))
            (interfaces)
            (accessflags  *abstract*  *interface*  *public* )
            (attributes
              (attribute "SourceFile")))))


(defconst *DataOutput-class-table*
  (make-static-class-decls 
   *java.io.DataOutput*))

(defconst *package-name-map* 
  ("java.io.DataOutput" . "java.io"))

