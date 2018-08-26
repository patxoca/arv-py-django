;;; arv-py-django.el --- utilitats per python-django

;; $Id: arv-py-django.el 763 2017-11-12 13:22:21Z alex $

;; Emacs List Archive Entry
;; Filename: arv-py-django.el
;; Version: $Revision:$
;; Keywords:
;; Author:  Alexis Roda <alexis.roda.villalonga@gmail.com>
;; Maintainer:  <alexis.roda.villalonga@gmail.com>
;; Created: 2018-03-28
;; Description: Minor mode per treballar amb django.
;; URL: https://github.com/patxoca/arv-py-django
;; Compatibility: Emacs24

;; COPYRIGHT NOTICE
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Install:

;; Put this file on your Emacs-Lisp load path and add the following
;; into your emacs startup file:
;;
;;     (require 'arv-py-django)

;;; Commentary:
;;
;; Aquest mòdul integra diferents eines per facilitar el treball amb
;; django:
;;
;; * djira-el: introspecció d'un projecte django
;;
;; * pony-tpl: la part de pony-mode encarregada de les plantilles
;;
;; Actualment la forma recomanada de treballar és:
;;
;; * cada projecte django utilitza un virtualenv independent.
;;
;; * la ruta de l'arrel del projecte i el nom del mòdul de settings
;;   arriben a emacs mitjançant les variables d'entorn
;;   `DJANGO_PROJECT' i `DJANGO_SETTINGS_MODULE' respectivament,
;;   definides al activar el virtualenv, per exemple.
;;
;; * en tot moment només pot haver un projecte obert. Canviar de
;;   projecte requereix tancar emacs, activar el nou virtualenv i
;;   obrir emacs.
;;
;; Si les necessitats canvien miraré si és possible canviar de
;; virtualenv en calent o com tindre varis projectes django obert
;; simultàniament.

;;; History:
;;


;;; Code:

(require 'dash)
(require 'djira)
(require 'f)
(require 's)
(require 'thingatpt)


(defun arv-get-string-at-point ()
  "Retorna la cadena en el punt, ni si el punt no està sobre una
cadena."
  (if (in-string-p)
      (let ((start (save-excursion (while (in-string-p) (forward-char -1))
                                   (1+ (point))))
            (end  (save-excursion (while (in-string-p) (forward-char 1))
                                  (1- (point)))))
        (buffer-substring-no-properties start end))))

(defun arv/django--get-template-candidates (filename current-app)
  "Return template candidates for `completing-read'.

FILENAME is the filename of a template, relative to the template
directory ('admin/login.html'). CURRENT-APP is the label of some
django app (usually the one to which the file we are editing
belongs).

This function returns a list of dotted pairs '(APP . FULL-PATH)'
than can be feed to `completing-read'. The list contains actual
templates matching FILENAME and, maybe, a fake one corresponding
to CURRENT-APP."
  (-non-nil
   (mapcar
    (lambda (app)
      (let ((filename-full (f-join (cdr app) "templates" filename)))
        (when (or (string= (car app) current-app)
                  (file-exists-p filename-full))
          (cons (car app) filename-full))))
    (djira-info-get-all-apps-paths))))

(defun arv/django--get-js-controller-candidates (filename current-app)
  (-non-nil
   (mapcar
    (lambda (app)
      (let ((filename-full (f-join (cdr app) "static" realname)))
        (when (or (equal (car app) current-app)
                  (file-exists-p filename-full))
          (cons (car app) filename-full))))
    (djira-info-get-all-apps-labels))))

(defun arv/django--js-controller-to-filename (name)
  (let ((parts (f-split name)))
    (concat
     (apply 'f-join (append (list (car parts) "js") (cdr parts)))
     ".js")))

;;;###autoload
(defun arv/django-jump-to-template ()
  "Visita la plantilla en el punt.

Cal que:

* el punt es trobi sobre una cadena. La ruta de la plantilla es
  determina pel valor de la cadena sobre la que es troba el punt.

* la ruta de la plantilla sigui relativa al directori 'templates'
  de l'aplicació (seguint el conveni django).

La funció opera contruint una llista amb les aplicacions que
contenen la plantilla. Per simplificar la creació de plantilles
aquesta llista sempre contindrà el nom de l'aplicació
actual (l'aplicació que conté l'arxiu des del que s'ha cridat a
la funció) independenment de que contingui la plantilla. Si
aquesta llista només conté una aplicació (l'actual) s'obre la
plantilla directament (creant-la si és necessari). Si conté més
d'una aplicació permet triar quina obrir."
  (interactive)
  (let ((filename (arv-get-string-at-point))
        (current-app (djira-get-app-for-buffer (current-buffer))))
    (if (null filename)
        (message "Point must be over an string.")
      (let ((candidates (arv/django--get-template-candidates filename current-app)))
        (find-file (cdr (assoc
                         (if (= (length candidates) 1)
                             (caar candidates)
                           (completing-read "Choose app: " candidates nil t nil))
                         candidates)))))))

;;;###autoload
(defun arv/django-jump-to-javascript-controller ()
  "Açò funciona amb el meu workflow.

El controller és un identificador AMD, en el meu cas
'app/controller'. Cal convertir-ho en 'app/js/controller.js'. per
obtindre l'arxiu."
  (interactive)
  (let ((amd-name (arv-get-string-at-point)))
    (if (null amd-name)
        (message "Point must be over an string.")
      (let* ((current-app (djira-get-app-for-buffer (current-buffer)))
             (filename (arv/django--js-controller-to-filename amd-name))
             (candidates (arv/django--get-js-controller-candidates filename current-app)))
        (find-file (cdr (assoc
                         (if (= (length candidates) 1)
                             (caar candidates)
                           (completing-read "Choose app: " candidates nil t nil))
                         candidates)))))))

;;;###autoload
(defun arv/django-insert-template-name ()
  "Insereix el nom de la plantilla.

El nom es calcula a partir del nom de la app actual i el nom del
buffer, sense extensió."
  (interactive)
  (let ((name (pyx/get-current-package-name)))
    (insert name
            "/"
            (file-name-sans-extension (file-name-base (buffer-file-name)))
            ".html")))

;;;###autoload
(defun arv/django-autopair-template-tag ()
  "Facilita introduir blocs '{% %}'."
  (interactive "")
  (let ((within-block (save-excursion
                        (backward-char)
                        (looking-at "{"))))
    (insert "%")
    (when within-block
      (insert "  %")
      (backward-char 2))))

(defun arv/django--ido-select-app ()
  (ido-completing-read "App: " (djira-info-get-all-apps-labels) nil t))

(defun arv/django--ido-select-model ()
  (ido-completing-read "Model: " (djira-info-get-all-apps-models) nil t))

(defun arv/django-hera-notes ()
  "Executa `hera_notes'."
  (interactive)
  (compilation-start "hera_manage tasks --emacs"
                     t
                     (lambda (mode) "*notes*")))

(defun arv/django--visit-file (dir-rel-path at-app-root)
  (let* ((app-name (arv/django--ido-select-app))
         (app-root (djira-info-get-app-root app-name)))
    (if at-app-root
        (setq app-root (file-name-directory app-root)))
    (setq app-root (concat app-root "/" dir-rel-path))
    (if (f-directory-p app-root)
        (ido-file-internal ido-default-file-method nil app-root)
      (find-file (concat app-root ".py")))))

(defun arv/django-visit-app ()
  "Permet selecionar app i obrir un arxiu dins l'arrel de la app."
  (interactive)
  (arv/django--visit-file "." nil))

(defun arv/django-visit-app-test-module ()
  "Permet selecionar app i obrir un arxiu de test."
  (interactive)
  (arv/django--visit-file "tests" nil))

(defun arv/django-visit-app-view-module ()
  "Permet selecionar app i obrir un arxiu de views."
  (interactive)
  (arv/django--visit-file "views" nil))

(defun arv/django-visit-app-template-file ()
  "Permet selecionar app i obrir un arxiu de template."
  (interactive)
  (arv/django--visit-file "templates" nil))

(defun arv/django-visit-app-model-module ()
  "Permet selecionar app i obrir un arxiu de models."
  (interactive)
  (arv/django--visit-file "models" nil))

(defun arv/django-visit-app-static-dir ()
  "Permet selecionar app i obrir un arxiu de static."
  (interactive)
  (arv/django--visit-file "static" nil))

(defun arv/django-visit-project ()
  ""
  (interactive)
  (ido-file-internal ido-default-file-method nil (djira-info-get-project-root)))

;; TODO: es pot navegar a la documentacions dels models en
;; http://localhost:8000/admin/docs/models
;;
;; Seria bonic accedir a la docu d'un model concret, utilitzant
;; completació http://localhost:8000/admin/docs/models/app.nommodelminuscules
;;
;; Hi ha documentació per template tags, template filters, models i
;; vistes. Només els models i vistes semblen interessants.

(defun arv/django-admindocs-browse ()
  ""
  (interactive)
  (eww "http://localhost:8000/admin/docs"))

(defun arv/django-admindocs-browse-model-docs ()
  ""
  (interactive)
  (let ((model-name (downcase (arv/django--ido-select-model))))
    (if model-name
        (eww (concat "http://localhost:8000/admin/docs/models/" model-name)))))

;;; TODO: quan treballo en un projecte django molta de la
;;; funcionalitat del mode resulta útil en tots els buffers, no sols
;;; des de buffers python-mode. Mirar con definir un minor-mode
;;; global.

(defvar arv/django-mode-map (make-sparse-keymap "arv/django-mode") "arv/django-mode keymap")

(defun arv/django-mode-setup-keymap ()
  "Setup a default keymap."
  ;; documentations
  (define-key arv/django-mode-map (kbd "C-c d d a") 'arv/django-admindocs-browse)
  (define-key arv/django-mode-map (kbd "C-c d d m") 'arv/django-admindocs-browse-model-docs)
  ;; insert something
  (define-key arv/django-mode-map (kbd "C-c d i t") 'arv/django-insert-template-name)
  ;; file navigation
  (define-key arv/django-mode-map (kbd "C-c d v a") 'arv/django-visit-app)
  (define-key arv/django-mode-map (kbd "C-c d v m") 'arv/django-visit-app-model-module)
  (define-key arv/django-mode-map (kbd "C-c d v p") 'arv/django-visit-project)
  (define-key arv/django-mode-map (kbd "C-c d v s") 'arv/django-visit-app-static-dir)
  (define-key arv/django-mode-map (kbd "C-c d v t") 'arv/django-visit-app-test-module)
  (define-key arv/django-mode-map (kbd "C-c d v T") 'arv/django-visit-app-template-file)
  (define-key arv/django-mode-map (kbd "C-c d v v") 'arv/django-visit-app-view-module)
)

(define-minor-mode arv/django-mode
  "Minor mode for working with django." nil " django" arv/django-mode-map
  (arv/django-mode-setup-keymap))


(provide 'arv-py-django)

;;; 'arv-py-django.el ends here
