;;; django-el-tests.el --- tests for django-el    -*- lexical-binding: t; -*-

(require 'ert)
(require 'django-el)


(defmacro with-buffer (text point-mark &rest body)
  `(with-temp-buffer
     (insert ,text)
     (goto-char (point-min))
     (when (search-forward ,point-mark nil t)
       (delete-char -1))
     ,@body))


(ert-deftest test-django-el--search-string-boundary ()
  "asdf"

  (with-buffer
   "some te@xt" "@"
   (should (= (point) 8))
   (should (= (progn  ; ensure that point is preserved
                (django-el--search-string-boundary -1)
                (point))
              8))
   (should (null (django-el--search-string-boundary -1)))
   (should (null (django-el--search-string-boundary 1))))

  (with-buffer
   "some \"te@xt\" foo" "@"
   (should (= (point) 9))
   (should (= (progn  ; ensure that point is preserved
                (django-el--search-string-boundary -1)
                (point))
              9))
   (should (string= (buffer-substring-no-properties 7 11) "text"))
   (should (= (django-el--search-string-boundary -1) 7))
   (should (= (django-el--search-string-boundary 1) 11))))


(ert-deftest test-django-el--get-string-at-point ()
  "asdf"

  (with-buffer
   "some te@xt" "@"
   (should (null (django-el--get-string-at-point))))

  (with-buffer
   "some \"te@xt\" foo" "@"
   (should (string= (django-el--get-string-at-point) "text"))))


;;;  django-el-tests.el ends here
