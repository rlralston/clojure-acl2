; Compiler$MaybePrimitiveExpr-classtable.lisp
; Automatically generated by jvm2acl2 on Tue May 27 00:28:50 CDT 2014.
;

(defconst *clojure.lang.Compiler$MaybePrimitiveExpr*
 (make-class-def
      '(class "clojure.lang.Compiler$MaybePrimitiveExpr"
            "java.lang.Object"
            (constant_pool)
            (fields)
            (methods
                        (method "canEmitPrimitive"
                              (parameters )
                              (returntype . boolean)
                              (accessflags  *abstract*  *class*  *public* )
                              (code))
                        (method "emitUnboxed"
                              (parameters (class "clojure.lang.Compiler$C") (class "clojure.lang.Compiler$ObjExpr") (class "clojure.asm.commons.GeneratorAdapter"))
                              (returntype . void)
                              (accessflags  *abstract*  *class*  *public* )
                              (code)))
            (interfaces "clojure.lang.Compiler$Expr")
            (accessflags  *abstract*  *interface*  *public* )
            (attributes
              (attribute "SourceFile")
              (attribute "InnerClasses")))))


(defconst *Compiler$MaybePrimitiveExpr-class-table*
  (make-static-class-decls 
   *clojure.lang.Compiler$MaybePrimitiveExpr*))

(defconst *package-name-map* 
  ("clojure.lang.Compiler$MaybePrimitiveExpr" . "clojure.lang"))

