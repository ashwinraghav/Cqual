(require 'pam-copyright)

; faces for highlighting

(defmacro pam-add-face (name face)
  `(progn (if (not (facep ',name))
	      (custom-set-faces '(,name ,face t)))
	  (defvar ,name ',name)))

(pam-add-face pam-color-1 ((t (:foreground "Wheat" :underline t))))
(pam-add-face pam-color-2 ((t (:foreground "Turquoise" :underline t))))
(pam-add-face pam-color-3 ((t (:foreground "GreenYellow" :underline t))))
(pam-add-face pam-color-4 ((t (:foreground "LightBlue" :underline t))))
(pam-add-face pam-color-5 ((t (:foreground "MediumBlue" :underline t))))
(pam-add-face pam-color-6 ((t (:foreground "Red" :underline t))))
(pam-add-face pam-color-7 ((t (:foreground "Green" :underline t))))
(pam-add-face pam-color-8 ((t (:foreground "Brown" :underline t))))
(pam-add-face pam-color-mouse ((t (:foreground "White" :background "Grey" :underline t))))

; Y2K qualifier colors
(pam-add-face pam-color-yyyy ((t (:foreground "GreenYellow" :underline t))))
(pam-add-face pam-color-yy   ((t (:foreground "LightBlue" :underline t))))
(pam-add-face pam-color-nonyear ((t (:foreground "MediumBlue" :underline t))))
(pam-add-face pam-color-ww50 ((t (:foreground "Red" :underline t))))
(pam-add-face pam-color-rcsdate ((t (:foreground "Green" :underline t))))

(if (not (facep 'pam-highlight-face))
    (copy-face 'highlight 'pam-highlight-face))
(defvar pam-highlight-face 'pam-highlight-face)

(provide 'pam-faces)
