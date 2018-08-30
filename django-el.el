;;; django-el.el --- the django mode from Krypton       -*- lexical-binding: t; -*-

;; Emacs List Archive Entry
;; Filename:django-el.el
;; Version: 0.1.0
;; Keywords: convenience
;; Author:  Alexis Roda <alexis.roda.villalonga@gmail.com>
;; Maintainer:  <alexis.roda.villalonga@gmail.com>
;; Created: 2018-03-28
;; Description: Minor mode per treballar amb django.
;; URL: https://github.com/patxoca/django-el
;; Compatibility: Emacs24
;; Package-Requires: ((dash "2.12.0") (djira "0.1.0") (emacs "24.3") (f "0.20.0") (s "1.12.0"))

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
;;     (require 'django-el)

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
;;   arriben a Emacs mitjançant les variables d'entorn
;;   `DJANGO_PROJECT' i `DJANGO_SETTINGS_MODULE' respectivament,
;;   definides al activar el virtualenv, per exemple.
;;
;; * en tot moment només pot haver un projecte obert. Canviar de
;;   projecte requereix tancar Emacs, activar el nou virtualenv i
;;   obrir Emacs.
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
(require 'ido)
(require 's)
(require 'thingatpt)

(defsubst django-el--in-string-p ()
  "Check if point is in a string.

The function `is-string-p' is deprecated but using a name instead
of (nth 3 (syntax-ppss) makes code clearer."
  (nth 3 (syntax-ppss)))

(defun django-el--get-current-package-name ()
  "Return the current package name.

The package name is the name of the directory that contains the
'setup.py'. If no 'setup.py' is found nil is returned."
  (let ((parent (locate-dominating-file (buffer-file-name) "setup.py")))
    (if (null parent)
        nil
      (file-name-base (directory-file-name parent)))))

(defun django-el--search-string-boundary (advance)
  "Return point at string boundary.

ADVANCE is the direction of the search, -1 to the left, 1 to the
right. If point is not on a string return nil."
  (when (django-el--in-string-p)
    (save-excursion
      (while (django-el--in-string-p)
        (forward-char advance))
      (- (point) advance))))

(defun django-el--get-string-at-point ()
  "Return string at point.

Return nil if string is not over an string."
  (let ((start (django-el--search-string-boundary -1))
        (end  (django-el--search-string-boundary 1)))
    (when (and start end)
      (buffer-substring-no-properties start end))))

(defun django-el--collect-candidate-files (subdir filename current-app)
  "Collect candidates matching FILENAME.

For each app, find files matching FILENAME in the app's SUBDIR
directory. The value returned is an alist '(app-label .
full-path)' that includes all matches plus an entry for
CURRENT-APP, so that we can create the file in the current app.

SUBDIR is a directory name, like 'templates', 'static' etc.

FILENAME is a filename relative to SUBDIR ('js/something.js').

CURRENT-APP is the label of some django app (usually the one to
which the file we are editing belongs)."
  (-non-nil
   (mapcar
    (lambda (app)
      (let ((filename-full (f-join (cdr app) subdir filename)))
        (when (or (string= (car app) current-app)
                  (file-exists-p filename-full))
          (cons (car app) filename-full))))
    (djira-info-get-all-apps-paths))))

(defun django-el--js-controller-to-filename (name)
  "Convert an AMD controller NAME to a path."
  (let ((parts (f-split name)))
    (concat
     (apply 'f-join (append (list (car parts) "js") (cdr parts)))
     ".js")))

(defun django-el--filename-from-app-and-module (extension)
  "Make a file name based on current app and python module.

The file name contains the name of the current app as directory
and the name of the current python module, replacing '.py' with
EXTENSION, as file name. This is useful for naming templates
etc."
  (f-join (django-el--get-current-package-name)
          (concat (file-name-sans-extension (file-name-base (buffer-file-name)))
                  extension)))

(defun django-el--ido-select-app ()
  "Select an app using IDO."
  (ido-completing-read "App: " (djira-info-get-all-apps-labels) nil t))

(defun django-el--ido-select-model ()
  "Select a model using IDO."
  (ido-completing-read "Model: " (djira-info-get-all-apps-models) nil t))

(defun django-el--ido-select-url-by-name ()
  "Select an URL using IDO."
  (ido-completing-read "View: " (djira-info-get-url-names)))

(defun django-el--visit-file (dir-rel-path at-app-root)
  "Visit a directory within an app.

Select an app and visit the subdirectory DIR-REL-PATH, relative
to the app root. If AT-APP-ROOT is not nil visit the root of the
python package."
  (let* ((app-name (django-el--ido-select-app))
         (app-root (djira-info-get-app-root app-name)))
    (if at-app-root
        (setq app-root (file-name-directory app-root)))
    (setq app-root (concat app-root "/" dir-rel-path))
    (if (f-directory-p app-root)
        (ido-file-internal ido-default-file-method nil app-root)
      (find-file (concat app-root ".py")))))


;;;                    _             _   _
;;;  _ __   __ ___   _(_) __ _  __ _| |_(_) ___  _ __
;;; | '_ \ / _` \ \ / / |/ _` |/ _` | __| |/ _ \| '_ \
;;; | | | | (_| |\ V /| | (_| | (_| | |_| | (_) | | | |
;;; |_| |_|\__,_| \_/ |_|\__, |\__,_|\__|_|\___/|_| |_|
;;;                      |___/

;; jumping

(defun django-el-jump-to-template ()
  "Jump to the template at point.

Point must be on an string containing a filename path relative to
the 'templates' directory (as per django conventions).

Django allows overriding templates from other apps, so this
function collects all candidates. If there's only one candidate
it jumps straight to it otherwise allow choosing the app.

The list will always contain an entry for the current app as a
mean of creating the template if it doesn't exist."
  (interactive)
  (let ((filename (django-el--get-string-at-point))
        (current-app (djira-get-app-for-buffer (current-buffer))))
    (if (null filename)
        (message "Point must be over an string.")
      (let ((candidates (django-el--collect-candidate-files "templates" filename current-app)))
        (find-file (cdr (assoc
                         (if (= (length candidates) 1)
                             (caar candidates)
                           (completing-read "Choose app: " candidates nil t nil))
                         candidates)))))))

(defun django-el-jump-to-javascript-controller ()
  "Jump to javascript controller at point.

Point must be on an string containing an AMD module identifier in
the form 'app/module'. The function opens the file
'app/js/module.js'."
  (interactive)
  (let ((amd-name (django-el--get-string-at-point)))
    (if (null amd-name)
        (message "Point must be over an string.")
      (let* ((current-app (djira-get-app-for-buffer (current-buffer)))
             (filename (django-el--js-controller-to-filename amd-name))
             (candidates (django-el--collect-candidate-files "static" filename current-app)))
        (find-file (cdr (assoc
                         (if (= (length candidates) 1)
                             (caar candidates)
                           (completing-read "Choose app: " candidates nil t nil))
                         candidates)))))))

(defun django-el-jump-to-app-class ()
  "Jump to the app class, if any."
  (interactive)
  (let* ((data (djira-info-get-app-class-source (django-el--ido-select-app)))
         (path (car data))
         (lineno (cadr data)))
    (if path
        (progn
          (find-file path)
          (goto-char (point-min))
          (forward-line (1- lineno)))
      (message "The app don't define an AppConfig class."))))

(defun django-el-jump-to-settings-module ()
  "Jump to the project's 'settings.py' module."
  (interactive)
  (let ((path (djira-info-get-settings-path)))
    (if path
        (find-file path)
      (message "Can't find 'settings.py'."))))

(defun django-el-jump-to-view ()
  "Select view by url name and jump to source code."
  (interactive)
  (let* ((url-name (django-el--ido-select-url-by-name))
         (view-info (djira-info-get-view-source url-name)))
    (if view-info
        (progn
          (find-file (car view-info))
          (goto-char (point-min))
          (forward-line (1- (cadr view-info))))
      (message "Can't find view."))))


;; visiting:

(defun django-el-visit-app ()
  "Select app and visit app's root."
  (interactive)
  (django-el--visit-file "." nil))

(defun django-el-visit-app-test-module ()
  "Select app and visit 'tests' module."
  (interactive)
  (django-el--visit-file "tests" nil))

(defun django-el-visit-app-view-module ()
  "Select app and visit 'views' module."
  (interactive)
  (django-el--visit-file "views" nil))

(defun django-el-visit-app-template-file ()
  "Select app and visit 'templates' directory."
  (interactive)
  (django-el--visit-file "templates" nil))

(defun django-el-visit-app-model-module ()
  "Select app and visit 'models' module."
  (interactive)
  (django-el--visit-file "models" nil))

(defun django-el-visit-app-static-dir ()
  "Select app and visit 'static' directory."
  (interactive)
  (django-el--visit-file "static" nil))

(defun django-el-visit-project ()
  "Visit the 'project' directory."
  (interactive)
  (ido-file-internal ido-default-file-method nil (djira-info-get-project-root)))


;;;  _                     _   _
;;; (_)_ __  ___  ___ _ __| |_(_) ___  _ __
;;; | | '_ \/ __|/ _ \ '__| __| |/ _ \| '_ \
;;; | | | | \__ \  __/ |  | |_| | (_) | | | |
;;; |_|_| |_|___/\___|_|   \__|_|\___/|_| |_|


(defun django-el-insert-template-name ()
  "Generate and insert template name.

Convenience function that inserts a template name made from the
current app and python module. Useful if you keep each view in a
separated module."
  (interactive)
  (insert "\"" (django-el--filename-from-app-and-module ".html") "\""))

(defun django-el-insert-url-name ()
  "Insert url name."
  (interactive)
  (insert "\"" (django-el--ido-select-url-by-name) "\""))

(defun django-el-insert-amd-js-controller-name ()
  "Generate and insert amd module name.

Convenience function that inserts a javascript amd module name
made from the current app and python module. Useful if you
associate just a js module per view (a controller)."
  (interactive)
  (insert "\"" (django-el--filename-from-app-and-module "") "\""))


;;;  _                       _       _
;;; | |_ ___ _ __ ___  _ __ | | __ _| |_ ___  ___
;;; | __/ _ \ '_ ` _ \| '_ \| |/ _` | __/ _ \/ __|
;;; | ||  __/ | | | | | |_) | | (_| | ||  __/\__ \
;;;  \__\___|_| |_| |_| .__/|_|\__,_|\__\___||___/
;;;                   |_|

(defun django-el-autopair-template-tag ()
  "Helps inserting '{% %}' in templates.

Pressing '%' after a '{' inserts a second '%'."
  (interactive "")
  (let ((within-block (save-excursion
                        (backward-char)
                        (looking-at "{"))))
    (insert "%")
    (when within-block
      (insert "  %")
      (backward-char 2))))


;;;  _ __ ___   __ _ _ __   __ _  __ _  ___   _ __  _   _
;;; | '_ ` _ \ / _` | '_ \ / _` |/ _` |/ _ \ | '_ \| | | |
;;; | | | | | | (_| | | | | (_| | (_| |  __/_| |_) | |_| |
;;; |_| |_| |_|\__,_|_| |_|\__,_|\__, |\___(_) .__/ \__, |
;;;                              |___/       |_|    |___/

(defun django-el-hera-notes ()
  "Execute management command 'tasks'."
  (interactive)
  (compilation-start "hera_manage tasks --emacs"
                     t
                     (lambda (mode) "*notes*")))


;;;            _           _           _
;;;   __ _  __| |_ __ ___ (_)_ __   __| | ___   ___ ___
;;;  / _` |/ _` | '_ ` _ \| | '_ \ / _` |/ _ \ / __/ __|
;;; | (_| | (_| | | | | | | | | | | (_| | (_) | (__\__ \
;;;  \__,_|\__,_|_| |_| |_|_|_| |_|\__,_|\___/ \___|___/

;; TODO: es pot navegar a la documentacions dels models en
;; http://localhost:8000/admin/docs/models
;;
;; Seria bonic accedir a la docu d'un model concret, utilitzant
;; completació http://localhost:8000/admin/docs/models/app.nommodelminuscules
;;
;; Hi ha documentació per template tags, template filters, models i
;; vistes. Només els models i vistes semblen interessants.

(defun django-el-admindocs-browse ()
  "Browse the admindocs."
  (interactive)
  (eww "http://localhost:8000/admin/docs"))

(defun django-el-admindocs-browse-model-docs ()
  "Browse the model's admindocs."
  (interactive)
  (let ((model-name (downcase (django-el--ido-select-model))))
    (if model-name
        (eww (concat "http://localhost:8000/admin/docs/models/" model-name)))))

;;; TODO: quan treballo en un projecte django molta de la
;;; funcionalitat del mode resulta útil en tots els buffers, no sols
;;; des de buffers python-mode. Mirar con definir un minor-mode
;;; global.

(defun django-el-refresh-cache ()
  "Refresh the cache."
  (interactive)
  (djira--cache-invalidate))

(defvar django-el-prefix-key "C-c C-d" "Prefix key for the django-el keymap.")

(defvar django-el-mode-map (make-sparse-keymap "django-el-mode") "django-el-mode keymap.")

(defun django-el-mode-setup-keymap ()
  "Setup a default keymap."
  (define-prefix-command 'django-el-mode-map)

  ;; documentations
  (define-key django-el-mode-map (kbd "d a") 'django-el-admindocs-browse)
  (define-key django-el-mode-map (kbd "d m") 'django-el-admindocs-browse-model-docs)
  ;; insert something
  (define-key django-el-mode-map (kbd "i j") 'django-el-insert-amd-js-controller-name)
  (define-key django-el-mode-map (kbd "i t") 'django-el-insert-template-name)
  (define-key django-el-mode-map (kbd "i u") 'django-el-insert-url-name)
  ;; file navigation
  (define-key django-el-mode-map (kbd "v a") 'django-el-visit-app)
  (define-key django-el-mode-map (kbd "v m") 'django-el-visit-app-model-module)
  (define-key django-el-mode-map (kbd "v p") 'django-el-visit-project)
  (define-key django-el-mode-map (kbd "v s") 'django-el-visit-app-static-dir)
  (define-key django-el-mode-map (kbd "v t") 'django-el-visit-app-test-module)
  (define-key django-el-mode-map (kbd "v T") 'django-el-visit-app-template-file)
  (define-key django-el-mode-map (kbd "v v") 'django-el-visit-app-view-module)
  ;; jump to something
  (define-key django-el-mode-map (kbd "j a") 'django-el-jump-to-app-class)
  (define-key django-el-mode-map (kbd "j j") 'django-el-jump-to-javascript-controller)
  (define-key django-el-mode-map (kbd "j s") 'django-el-jump-to-settings-module)
  (define-key django-el-mode-map (kbd "j t") 'django-el-jump-to-template)
  (define-key django-el-mode-map (kbd "j v") 'django-el-jump-to-view)
  ;; assorted
  (define-key django-el-mode-map (kbd "C-l") 'django-el-refresh-cache)

  (global-set-key (kbd django-el-prefix-key) django-el-mode-map)
)

;;;###autoload
(define-minor-mode django-el-mode
  "Minor mode for working with django." nil " django" django-el-mode-map
  (django-el-mode-setup-keymap))


(provide 'django-el)

;;; django-el.el ends here
