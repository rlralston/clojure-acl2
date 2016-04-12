#|$ACL2s-Preamble$;
(include-book "../../../mc/mc")
(include-book "../../../mc/utilities")
(include-book "../../big-step")

(acl2::begin-book t :ttags :all);$ACL2s-Preamble$|#


(in-package "MC")

(defconst *java.lang.Boolean*
  (make-class-decl
   "java.lang.Boolean"
   '("java.lang.Object")
   '("java.io.Serializable" "java.lang.Comparable")
   '("value")
   '("TRUE"
     "FALSE"
    )
   '()   
   '()
   '(REF -1)))

(defun |Boolean:TRUE|-get (heap class-table)
  (static-field-value "java.lang.Boolean"
                      "TRUE"
                      heap
                      class-table))

(defun |Boolean:FALSE|-get (heap class-table)
  (static-field-value "java.lang.Boolean"
                      "FALSE"
                      heap
                      class-table))

(defun |Boolean|-p (ref heap)
  (and (not (nullrefp ref))
       (j-type-p "java.lang.Boolean" ref heap))) 

(defun |Boolean|-loaded? (class-table heap)
  (let* ((TRUE (|Boolean:TRUE|-get heap class-table))
         (FALSE (|Boolean:FALSE|-get heap class-table)))
    (and 
     (|Object|-loaded? class-table)
     (loaded? class-table 
              "java.lang.Boolean" 
              *java.lang.Boolean*)
   (|Boolean|-p TRUE heap)
   (|Boolean|-p FALSE heap)       
   (not (equal TRUE FALSE)))))

(defthm |Boolean|-dep
  (implies (|Boolean|-loaded? (class-table s) (heap s))
           (|Object|-loaded? (class-table s)))
  :rule-classes :forward-chaining)

(defthm TRUE-is-not-FALSE
  (implies 
   (|Boolean|-loaded? (class-table s) (heap s))
   (not (equal (|Boolean:TRUE|-get (heap s) (class-table s))
               (|Boolean:FALSE|-get (heap s) (class-table s))))))

(defthm TRUE-and-FALSE-Boolean
  (implies 
   (|Boolean|-loaded? class-table heap)
   (and (|Boolean|-p (|Boolean:TRUE|-get heap class-table) heap)
        (|Boolean|-p (|Boolean:FALSE|-get heap class-table) heap)))
  :rule-classes :forward-chaining)

(in-theory (disable |Boolean|-loaded?))

(defun |Boolean_get-FALSE|-poised (s)
  (equal (next-inst s) 
         '(GETSTATIC "java.lang.Boolean" "FALSE" NIL)))

(defthm Boolean-FALSE=FALSE
  (implies 
   (and (|Boolean|-loaded? (class-table s) (heap s))
        (|Boolean_get-FALSE|-poised s))
   (-> s
       (modify s
               :pc (+ 3 (pc (top-frame s)))
               :stack (push (|Boolean:FALSE|-get (heap s)
                                                 (class-table s)) 
                            (stack (top-frame s))))))
  :hints (("Goal" :in-theory (enable ->-execute-GETSTATIC))))

(defun |Boolean_get-TRUE|-poised (s)
  (equal (next-inst s) 
         '(GETSTATIC "java.lang.Boolean" "TRUE" NIL)))

(defthm Boolean-TRUE=TRUE
  (implies 
   (and (|Boolean|-loaded? (class-table s) (heap s))
        (|Boolean_get-TRUE|-poised s))
   (-> s
       (modify 
        s
        :pc (+ 3 (pc (top-frame s)))
        :stack 
        (push 
         (|Boolean:TRUE|-get (heap s)
                             (class-table s)) 
         (stack (top-frame s))))))
  :hints (("Goal" :in-theory (enable ->-execute-GETSTATIC))))#|ACL2s-ToDo-Line|#
