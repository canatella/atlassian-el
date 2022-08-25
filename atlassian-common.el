;;; at-common.el --- Atlassian integration  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Damien Merenne <dam@cosinux.org>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'auth-source)
(require 'subr-x)
(require 'request)

(defgroup atlassian nil "Atlassian integration customization." :group 'development)

(defun atlassian-credentials (url)
  "Return username and password for jira."
  (let ((found
         (nth 0
              (auth-source-search :max 1
                                  :host (url-host (url-generic-parse-url url))
                                  :port (number-to-string (url-port (url-generic-parse-url url)))
                                  :require '(:user :secret)
                                  :create t)))
        user secret)
    (when found
      (setq user
            (plist-get found :user)
            secret
            (let ((sec (plist-get found :secret)))
              (if (functionp sec) (funcall sec) sec)))
      (list user secret))))

(defun atlassian-authorization (url)
  "Return Atlassian authorization header value for URL."
  (cl-destructuring-bind
      (username password)
      (atlassian-credentials url)
    (format "Basic %s" (base64-encode-string (concat username ":" password) t))))

(defun atlassian-param-key (inflection sym)
  "Convert SYM symbol to an Atlassian api parameter using INFLECTION."
  (unless (symbolp sym) (signal 'wrong-type-argument (list 'symbolp sym)))
  (cl-destructuring-bind
      (first &rest rest)
      (split-string (symbol-name sym) "-")
    (cond
     ((eq inflection 'camelize)
      (concat first (mapconcat #'capitalize rest "")))
     ((eq inflaction 'underscorize)
      (string-join (cons first rest) "_"))
     (t (signal 'wrong-type-argument "'camelize or 'underscorize")))))

(defun atlassian-param-key-value (inflection pair)
  "Convert cons cell from symbol value PAIR to Atlassian key value parameter using INFLECTION."
  (when (cdr pair)
    (cons (atlassian-param-key inflection (car pair)) (cdr pair))))

(defun atlassian-params (inflection params)
  "Convert Emacs keywords PARAMS to atlassian api params using INFLECTION."
  (seq-map #'identity (seq-map (apply-partially #'atlassian-param-key-value inflection) params)))

(defun atlassian-data-key (key)
  "Convert an Atlassian API KEY to an Emacs symbol."
  (intern
   (downcase
    (let ((case-fold-search nil))
      (replace-regexp-in-string "\\([A-Z][a-zA-Z0-9]+\\)" "-\\1" (symbol-name key) t)))))

(defun atlassian-data-key-value (pair)
  "Convert cons cell from symbol value PAIR to Atlassian API key value parameter."
  (cons (atlassian-data-key (car pair)) (cdr pair)))

(defun atlassian-data (data)
  "Convert Atlassian DATA to Emacs."
  (cond
   ((not (sequencep data))
    data)
   ((stringp data)
    data)
   ((consp (elt data 0))
    (seq-map #'atlassian-data-key-value data))
   (t (seq-map #'atlassian-data data))))

(defun atlassian-parser () "Parse an Atlassian API response." (atlassian-data (json-read)))

(defun atlassian-request-callback-dispatch (settings data args)
  "Dispatch request to SETTINGS using DATA and ARGS."
  (let* ((success (plist-get settings :success))
         (error (plist-get settings :error))
         (complete (plist-get settings :complete))
         (status-code (plist-get settings :status-code))
         (args (plist-put args :data data))
         (symbol-status (plist-get args :symbol-status))
         (response (plist-get args :response)))
    (setf (request-response-data response) data)
    (let* ((success-p (eq symbol-status 'success))
           (cb (if success-p success error)))
      (when cb (apply cb args)))
    (let ((cb (cdr (assq (request-response-status-code response) status-code))))
      (when cb (apply cb args)))

    (when complete (apply complete args))))

(cl-defun atlassian-request (url &rest settings &key (params nil) (headers nil) (data nil) inflection &allow-other-keys)
  "Request URL with SETTINGS, PARAMS, HEADERS and DATA.

See `request' for parameter documentation."
  (let ((headers
         (append
          `(("accept" . "application/json") ("authorization" . ,(atlassian-authorization url)))
          headers))
        (params (atlassian-params inflection params)))
    (when data
      (setq data (json-serialize data))
      (setq headers (cons '("content-type" . "application/json") headers)))
    (apply #'request url :params params :headers headers :data data :parser #'atlassian-parser settings)))

(defun atlassian-resource (resource)
  "Convert RESOURCE to a path.

If `(stringp resource)', it will be returned as is.

If `(keywordp resource)', it will be returned as a string with
the colon stripped.

If `(symbolp resource)', it will be returned as a string.

If `(sequencep resource)', each element of the sequence will be converted and the result will be joined using slashes."
  (cond
   ((stringp resource)
    resource)
   ((keywordp resource)
    (replace-regexp-in-string "^:" "" (symbol-name resource)))
   ((symbolp resource)
    (symbol-name resource))
   ((sequencep resource)
    (string-join (seq-map #'atlassian-resource resource) "/"))
   (signal 'wrong-type-argument "resource must be a string, a symbol or a list of those")))

(defun atlassian-date-time (time)
  "Convert TIME to an iso8601 string.

TIME can either be a Lisp timestamp, an iso8601 string or nil.
When nil or an iso8601 string, it is returned as is.  Otherwise
the timestamp is converted to an iso8601 string."
  (when time
    (if (stringp)
        (progn
          (when (not (iso8601-valid-p time)) (signal 'wrong-type-argument 'iso8601-valid-p))
          time)
      (format-time-string "%FT%TZ"))))



(provide 'atlassian-common)
;;; atlassian-common.el ends here
