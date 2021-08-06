;;; atlassian-confluence.el -- Elisp bindings for Atlassian Confluence REST API.  -*- lexical-binding: t; -*-

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

(require 'atlassian-common)

(cl-defun atlassian-confluence-success (url params headers settings previous-values &rest args &key data &allow-other-keys)
  "Handle pagination in Atlassian API responses."
  (if (eq 'success (plist-get args :symbol-status))
      (map-let
          (size start limit results)
          data
        (if start
            ;; When there is pagination
            (let ((all-values (seq-concatenate 'vector previous-values results)))
              (if (eq size limit)
                  ;; And there are pages left
                  (let ((params (cons `(start . ,(seq-length all-values)) params)))
                    (apply #'atlassian-request url :params params :headers headers :pagination-values all-values settings))
                ;; If it is the last page
                (atlassian-request-callback-dispatch settings all-values args)))
          ;; When there is no pagination
          (atlassian-request-callback-dispatch settings data args)))
    (atlassian-request-callback-dispatch settings data args)))

(cl-defun atlassian-confluence-request (url &rest settings &key (params nil) (headers nil) (pagination-values nil) &allow-other-keys)
  "Request URL with SETTINGS, PARAMS and HEADERS.

See `request' for parameter documentation.

PAGINATION-VALUES is a private argument for handling pagination."
  (let ((complete
         (apply-partially #'atlassian-confluence-success url params headers settings pagination-values)))
    (atlassian-request url :params params :headers headers :pagination-values pagination-values :complete complete)))

(atlassian-confluence-request "https://bloomlife.atlassian.net/wiki/rest/api/content"
                              :success (cl-function
                                        (lambda (&key data &key error-thrown &allow-other-keys)
                                          (message "content: %s" data))))

(provide 'atlassian-confluence)
;;; atlassian-jira.el ends here
