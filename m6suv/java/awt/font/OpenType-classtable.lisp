; OpenType-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:39:26 CDT 2014.
;

(defconst *java.awt.font.OpenType*
 (make-class-def
      '(class "java.awt.font.OpenType"
            "java.lang.Object"
            (constant_pool
                        (INT 1668112752)
                        (INT 1751474532)
                        (INT 1851878757)
                        (INT 1735162214)
                        (INT 1835104368)
                        (INT 1886545264)
                        (INT 1752003704)
                        (INT 1801810542)
                        (INT 1751412088)
                        (INT 1819239265)
                        (INT 1886352244)
                        (INT 1330851634)
                        (INT 1668707360)
                        (INT 1734439792)
                        (INT 1447316824)
                        (INT 1986884728)
                        (INT 1986553185)
                        (INT 1751672161)
                        (INT 1954115633)
                        (INT 1651731566)
                        (INT 1196643650)
                        (INT 1146308935)
                        (INT 1718642541)
                        (INT 1719034226)
                        (INT 1735811442)
                        (INT 1128678944)
                        (INT 1296913220)
                        (INT 1296909912)
                        (INT 1111577413)
                        (INT 1195656518)
                        (INT 1196445523)
                        (INT 1246975046)
                        (INT 1161970772)
                        (INT 1161972803)
                        (INT 1161974595)
                        (INT 1280594760)
                        (INT 1346587732)
                        (INT 1633906292)
                        (INT 1635148146)
                        (INT 1650745716)
                        (INT 1651273571)
                        (INT 1668702578)
                        (INT 1717920116)
                        (INT 1717859171)
                        (INT 1718449272)
                        (INT 1786082164)
                        (INT 1818452338)
                        (INT 1836020340)
                        (INT 1886547824)
                        (INT 1953653099))
            (fields
                        (field "TAG_CMAP" int (accessflags  *class*  *final*  *public*  *static* ) 0)
                        (field "TAG_HEAD" int (accessflags  *class*  *final*  *public*  *static* ) 1)
                        (field "TAG_NAME" int (accessflags  *class*  *final*  *public*  *static* ) 2)
                        (field "TAG_GLYF" int (accessflags  *class*  *final*  *public*  *static* ) 3)
                        (field "TAG_MAXP" int (accessflags  *class*  *final*  *public*  *static* ) 4)
                        (field "TAG_PREP" int (accessflags  *class*  *final*  *public*  *static* ) 5)
                        (field "TAG_HMTX" int (accessflags  *class*  *final*  *public*  *static* ) 6)
                        (field "TAG_KERN" int (accessflags  *class*  *final*  *public*  *static* ) 7)
                        (field "TAG_HDMX" int (accessflags  *class*  *final*  *public*  *static* ) 8)
                        (field "TAG_LOCA" int (accessflags  *class*  *final*  *public*  *static* ) 9)
                        (field "TAG_POST" int (accessflags  *class*  *final*  *public*  *static* ) 10)
                        (field "TAG_OS2" int (accessflags  *class*  *final*  *public*  *static* ) 11)
                        (field "TAG_CVT" int (accessflags  *class*  *final*  *public*  *static* ) 12)
                        (field "TAG_GASP" int (accessflags  *class*  *final*  *public*  *static* ) 13)
                        (field "TAG_VDMX" int (accessflags  *class*  *final*  *public*  *static* ) 14)
                        (field "TAG_VMTX" int (accessflags  *class*  *final*  *public*  *static* ) 15)
                        (field "TAG_VHEA" int (accessflags  *class*  *final*  *public*  *static* ) 16)
                        (field "TAG_HHEA" int (accessflags  *class*  *final*  *public*  *static* ) 17)
                        (field "TAG_TYP1" int (accessflags  *class*  *final*  *public*  *static* ) 18)
                        (field "TAG_BSLN" int (accessflags  *class*  *final*  *public*  *static* ) 19)
                        (field "TAG_GSUB" int (accessflags  *class*  *final*  *public*  *static* ) 20)
                        (field "TAG_DSIG" int (accessflags  *class*  *final*  *public*  *static* ) 21)
                        (field "TAG_FPGM" int (accessflags  *class*  *final*  *public*  *static* ) 22)
                        (field "TAG_FVAR" int (accessflags  *class*  *final*  *public*  *static* ) 23)
                        (field "TAG_GVAR" int (accessflags  *class*  *final*  *public*  *static* ) 24)
                        (field "TAG_CFF" int (accessflags  *class*  *final*  *public*  *static* ) 25)
                        (field "TAG_MMSD" int (accessflags  *class*  *final*  *public*  *static* ) 26)
                        (field "TAG_MMFX" int (accessflags  *class*  *final*  *public*  *static* ) 27)
                        (field "TAG_BASE" int (accessflags  *class*  *final*  *public*  *static* ) 28)
                        (field "TAG_GDEF" int (accessflags  *class*  *final*  *public*  *static* ) 29)
                        (field "TAG_GPOS" int (accessflags  *class*  *final*  *public*  *static* ) 30)
                        (field "TAG_JSTF" int (accessflags  *class*  *final*  *public*  *static* ) 31)
                        (field "TAG_EBDT" int (accessflags  *class*  *final*  *public*  *static* ) 32)
                        (field "TAG_EBLC" int (accessflags  *class*  *final*  *public*  *static* ) 33)
                        (field "TAG_EBSC" int (accessflags  *class*  *final*  *public*  *static* ) 34)
                        (field "TAG_LTSH" int (accessflags  *class*  *final*  *public*  *static* ) 35)
                        (field "TAG_PCLT" int (accessflags  *class*  *final*  *public*  *static* ) 36)
                        (field "TAG_ACNT" int (accessflags  *class*  *final*  *public*  *static* ) 37)
                        (field "TAG_AVAR" int (accessflags  *class*  *final*  *public*  *static* ) 38)
                        (field "TAG_BDAT" int (accessflags  *class*  *final*  *public*  *static* ) 39)
                        (field "TAG_BLOC" int (accessflags  *class*  *final*  *public*  *static* ) 40)
                        (field "TAG_CVAR" int (accessflags  *class*  *final*  *public*  *static* ) 41)
                        (field "TAG_FEAT" int (accessflags  *class*  *final*  *public*  *static* ) 42)
                        (field "TAG_FDSC" int (accessflags  *class*  *final*  *public*  *static* ) 43)
                        (field "TAG_FMTX" int (accessflags  *class*  *final*  *public*  *static* ) 44)
                        (field "TAG_JUST" int (accessflags  *class*  *final*  *public*  *static* ) 45)
                        (field "TAG_LCAR" int (accessflags  *class*  *final*  *public*  *static* ) 46)
                        (field "TAG_MORT" int (accessflags  *class*  *final*  *public*  *static* ) 47)
                        (field "TAG_OPBD" int (accessflags  *class*  *final*  *public*  *static* ) 47)
                        (field "TAG_PROP" int (accessflags  *class*  *final*  *public*  *static* ) 48)
                        (field "TAG_TRAK" int (accessflags  *class*  *final*  *public*  *static* ) 49))
            (methods
                        (method "getVersion"
                              (parameters )
                              (returntype . int)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getFontTable"
                              (parameters int)
                              (returntype . (array byte))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getFontTable"
                              (parameters (class "java.lang.String"))
                              (returntype . (array byte))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getFontTable"
                              (parameters int int int)
                              (returntype . (array byte))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getFontTable"
                              (parameters (class "java.lang.String") int int)
                              (returntype . (array byte))
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getFontTableSize"
                              (parameters int)
                              (returntype . int)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "getFontTableSize"
                              (parameters (class "java.lang.String"))
                              (returntype . int)
                              (accessflags  *abstract*  *class*  *public* )
                              (code)))
            (interfaces)
            (accessflags  *abstract*  *interface*  *public* )
            (attributes
              (attribute "SourceFile")))))


(defconst *OpenType-class-table*
  (make-static-class-decls 
   *java.awt.font.OpenType*))

(defconst *package-name-map* 
  ("java.awt.font.OpenType" . "java.awt.font"))
