;;; at-jira.el --- Elisp bindings for Atlassian Jira REST API.  -*- lexical-binding: t; -*-

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

(defconst atlassian-jira-api-version "3" "Jira API version to use.")

(cl-defun atlassian-jira-success (resource url params headers settings previous-values &rest args &key data &allow-other-keys)
  "Handle pagination in Atlassian API responses."
  (if (eq 'success (plist-get args :symbol-status))
      (map-let
          (is-last start-at (resource values))
          data
        (if start-at
            ;; When there is pagination
            (let ((all-values (seq-concatenate 'vector previous-values values)))
              (if (eq is-last :json-false)
                  ;; And there are pages left
                  (let ((params (cons `(start-at . ,(seq-length all-values)) params)))
                    (apply #'atlassian-request url :params params :headers headers :pagination-values all-values :inflection 'camelize settings))
                ;; If it is the last page
                (atlassian-request-callback-dispatch settings all-values args)))
          ;; When there is no pagination
          (atlassian-request-callback-dispatch settings data args)))
    (atlassian-request-callback-dispatch settings data args)))

(cl-defun atlassian-jira-request (url &rest settings &key
                                      (params nil)
                                      (headers nil)
                                      (data nil)
                                      (pagination-resource nil)
                                      (pagination-values nil)
                                      (sync nil)
                                      &allow-other-keys)
  "Request URL with SETTINGS, PARAMS, HEADERS and DATA.

See `request' for parameter documentation.

PAGINATION-RESOURCE is the name of the field holding the resource
list in case of pagination.

PAGINATION-VALUES is a private argument for handling pagination."
  (if pagination-resource
      (let ((complete
             (apply-partially #'atlassian-jira-success pagination-resource url params headers settings pagination-values)))
        (atlassian-request url :params params :headers headers :data data :pagination-values pagination-values :inflection 'camelize :complete complete :sync sync))
    (apply #'atlassian-request url :params params :headers headers :data data :pagination-values pagination-values :inflection 'camelize settings)))

(defun atlassian-jira-url (domain api-name api-version &rest resource)
  "Return url for DOMAIN RESOURCE, API-NAME and API-VERSION.

RESOURCES can be a string or a sequence.  As a string, it will be
appended to the api endpoint.  As a sequence, it will be
concatenated using / and appended to the api endpoint."
  (format "https://%s.atlassian.net/rest/%s/%s/%s" domain api-name api-version
          (atlassian-resource resource)))

(defun atlassian-jira-api (domain resource &rest settings)
  "Execute a REST request for DOMAIN and RESOURCE with request SETTINGS."
  (apply #'atlassian-jira-request
         (atlassian-jira-url domain "api" atlassian-jira-api-version resource)
         settings))

(defmacro atlassian-jira-params (&rest names)
  "Return a parameter object based on parameter NAMES."
  (let ((forms
         (seq-map
          (lambda (name)
            `(when ,name (setq value (cons (cons (quote ,name) ,name) value))))
          names)))
    `(let ((value)) ,@forms value)))

(cl-defun atlassian-jira-search (domain jql &rest settings &key
                                               (expand nil)
                                               (fields nil)
                                               (validate-query nil)
                                               &allow-other-keys)
  "Search DOMAIN for issues using JQL query and request SETTINGS.

EXPAND is a list of fields to expand.

FIELDS is a list of fields to fetch.

Non-nil VALIDATE-QUERY to non-nil will trigger query validation."
  (apply #'atlassian-jira-api domain :search :type "POST" :data
         (atlassian-jira-params jql expand fields validate-query)
         :pagination-resource 'issues
         settings))

(cl-defun atlassian-jira-issue (domain id-or-key &rest settings &key (expand nil) (fields nil) &allow-other-keys)
  "Retrieve DOMAIN issue ID-OR-KEY using request SETTINGS.

EXPAND is a list of fields to expand.

FIELDS is a list of fields to fetch."
  (apply #'atlassian-jira-api domain
         `(issue ,id-or-key)
         :params (atlassian-jira-params expand fields)
         settings))

(defun atlassian-jira-read-issue (prompt domain jql &optional require-match)
  "Fetch issues matching JQL and prompt the user to choose one with PROMPT."
  (let* ((issues (request-response-data (atlassian-jira-search domain jql :sync t)))
         (titles (seq-map (lambda (issue)
                              (format "%s: %s"
                                      (alist-get 'key issue)
                                      (alist-get 'summary (alist-get 'fields issue))))
                            issues))
         (key (car (split-string (completing-read prompt titles nil require-match) ":"))))
    (if-let ((issue (seq-find (lambda (req) (string= (alist-get 'key req) key)) issues)))
        issue
      (request-response-data (atlassian-jira-issue domain key :sync t)))))

(provide 'atlassian-jira)
;;; atlassian-jira.el ends here
