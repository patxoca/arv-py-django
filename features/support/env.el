(require 'f)

(defvar arv-py-django-support-path
  (f-dirname load-file-name))

(defvar arv-py-django-features-path
  (f-parent arv-py-django-support-path))

(defvar arv-py-django-root-path
  (f-parent arv-py-django-features-path))

(add-to-list 'load-path arv-py-django-root-path)

(require 'arv-py-django)
(require 'espuds)
(require 'ert)

(Setup
 ;; Before anything has run
 )

(Before
 ;; Before each scenario is run
 )

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )
