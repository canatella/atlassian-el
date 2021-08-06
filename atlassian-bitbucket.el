;;; atlassian-bitbucket.el -- API for bitbucket access -*- lexical-binding: t; -*-

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

(defconst atlassian-bitbucket-api-version "2.0" "Bitbucket API version to use.")

(cl-defun atlassian-bitbucket-success (each settings previous-values &rest args &key data &allow-other-keys)
  "Handle pagination in Atlassian API responses.

EACH if not nil, it must be a lambda that will be call for each
resource retrieved.

SETTINGS are passed to `requests'.

PREVIOUS-VALUES is an accumulator for collecting all paginated
resources.

DATA is the request callback data.

ARGS are the request callbacks rest arguments."
  (if (eq 'success (plist-get args :symbol-status))
      (map-let
          (size pagelen next values)
          data
        (if (or size pagelen)
            ;; If an iterator was given yield the resource
            (progn
              (if each
                  (seq-do each values)
                ;; Otherwise collect all value
                (setq previous-values (seq-concatenate 'vector previous-values values)))
              ;; Maybe fetch the rest
              (if next
                  ;; If there are results left
                  (apply #'atlassian-request next :complete
                         (apply-partially #'atlassian-bitbucket-success each settings previous-values)
                         settings)
                ;; If it is the last page
                (atlassian-request-callback-dispatch settings previous-values args)))
          ;; When there is no pagination
          (when each (apply each data))
          (atlassian-request-callback-dispatch settings data args)))
    (atlassian-request-callback-dispatch settings data args)))

(cl-defun atlassian-bitbucket-request (url &rest settings &key each &allow-other-keys)
  "Request URL with request SETTINGS.

EACH is a callback function that will be called for each
Bitbucket resource in the response.

This method handles Bitbucket pagination.

See `request' for parameter documentation."
  (let ((complete (apply-partially #'atlassian-bitbucket-success each settings nil)))
    (atlassian-request url :inflection 'underscorize :complete complete)))

(defun atlassian-bitbucket-url (api-version resource)
  "Return url API-VERSION and RESOURCE."
  (format "https://api.bitbucket.org/%s/%s" api-version (atlassian-resource resource)))

(defun atlassian-bitbucket-api (resource &rest settings)
  "Execute a REST request for RESOURCE with request SETTINGS."
  (apply #'atlassian-bitbucket-request
         (atlassian-bitbucket-url atlassian-bitbucket-api-version resource)
         settings))

(defun atlassian-bitbucket-workspaces (&rest settings)
  "Return the list of availlable workspaces for the authenticated user with request SETTINGS."
  (apply #'atlassian-bitbucket-api :workspaces settings))

(cl-defun atlassian-bitbucket-repositories (&rest settings &key workspace after role query sort &allow-other-keys)
  "Return the list of repositories for the current user with request SETTINGS.

WORKSPACE: limit query to workspace.

AFTER: Filter the results to include only repositories created on
  or after this Lisp timestamp or ISO-8601 string.

ROLE: Filters the result based on the authenticated user's role
on each repository.

QUERY: Query string to narrow down the response as per filtering
and sorting.  Role parameter must also be specified.

SORT: Field by which the results should be sorted as per
filtering and sorting."
  (apply #'atlassian-bitbucket-api :repositories :params
         `((workspace . ,workspace)
           (after . ,(atlassian-date-time after))
           (role . ,role)
           (q . ,query)
           (sort . ,sort))
         settings))

(cl-defun atlassian-bitbucket-repositories-pullrequests (workspace repo-slug &rest settings &key state &allow-other-keys)
  "Return pull requests on the repository specified by WORKSPACE and REPO-SLUG.

By default only open pull requests are returned.  This can be
controlled using the STATE query parameter.  To retrieve pull
requests that are in one of multiple states, repeat the state
parameter for each individual state.

This endpoint also supports filtering and sorting of the
  results.  See filtering and sorting for more details.

SETTINGS: `request' settings."
  (apply #'atlassian-bitbucket-api
         `(:repositories ,workspace ,repo-slug :pullrequests)
         :params `((state . ,state))
         settings))

(cl-defun atlassian-bitbucket-pullrequests (user &rest settings &key state &allow-other-keys)
  "Return all pull requests authored by USER with request SETTINGS.

By default only open pull requests are returned.  This can be
controlled using the STATE query parameter.  To retrieve pull
requests that are in one of multiple states, repeat the state
parameter for each individual state.

This endpoint also supports filtering and sorting of the results.
See filtering and sorting for more details."
  (apply #'atlassian-bitbucket-api `(:pullrequests ,user) :params `((state . ,state)) settings))

;;(atlassian-repositories :role "owner" :each (lambda (repo) (message "repo: %s" (map-elt repo 'name))))
;;(atlassian-bitbucket-workspaces :each
;;                                (lambda (workspace)
;;                                  (message "workspace: %s" (map-elt workspace 'name))))

;;(atlassian-bitbucket-pullrequests-create username repository title )
;;(atlassian-bitbucket-url atlassian-bitbucket-api-version '(:repositories :bloomlife))
;;
;;(atlassian-bitbucket-request "https://api.bitbucket.org/2.0/repositories/bloomlife"

(provide 'atlassian-bitbucket)

;;; atlassian-bitbucket.el ends here
