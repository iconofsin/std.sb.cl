(in-package :std.sb.cl)

(defmacro define-constant (name value &optional doc)
  "defconstant workaround (per SBCL documentation)"
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))
