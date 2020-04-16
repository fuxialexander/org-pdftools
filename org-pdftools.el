;;; org-pdftools.el --- Support for links to documents in pdfview mode  -*- lexical-binding: t; -*-
;; Copyright (C) 2020 Alexander Fu Xi

;; Author: Alexander Fu Xi <fuxialexander@gmail.com>
;; Maintainer: Alexander Fu Xi <fuxialexnader@gmail.com>
;; Homepage: https://github.com/fuxialexander/org-pdftools
;; Version: 1.0
;; Keywords: convenience
;; Package-Requires: ((emacs "26.1") (org "9.3.6") (pdf-tools "0.8") (org-noter "1.4.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; Add support for org links from pdftools buffers with more precise location control.
;;
;; https://github.com/fuxialexander/org-pdftools/


;;; Code:
(require 'subr-x)
(require 'cl-lib)
(require 'org)
(require 'org-refile nil t)
(require 'org-noter)
(require 'pdf-tools)
(require 'pdf-view)
(require 'pdf-annot)
(require 'pdf-occur)

(defgroup org-pdftools nil
  "Tools for adding pdftools link support in Org mode."
  :group 'tools)

(defcustom org-pdftools-root-dir org-directory
  "Root directory for document."
  :group 'org-pdftools
  :type 'directory)

(defcustom org-pdftools-open-custom-open nil
  "Custom function to open linked pdf files."
  :group 'org-pdftools
  :type '(choice function nil))
(defcustom org-pdftools-export-style 'pdftools
  "Export style of org-pdftools links."
  :group 'org-pdftools
  :type 'symbol)
(defcustom org-pdftools-markup-pointer-function 'pdf-annot-add-underline-markup-annotation
  "Color for markup pointer annotations.
Can be one of highlight/underline/strikeout/squiggly."
  :group 'org-pdftools
  :type 'function)
(defcustom org-pdftools-markup-pointer-color "#A9A9A9"
  "Color for markup pointer annotations."
  :group 'org-pdftools
  :type 'string)
(defcustom org-pdftools-markup-pointer-opacity 1.0
  "Opacity for markup pointer annotations."
  :group 'org-pdftools
  :type 'float)
(defcustom org-pdftools-free-pointer-icon "Circle"
  "Color for free pointer annotations. Refer to `pdf-annot-standard-text-icons`."
  :group 'org-pdftools
  :type 'string)
(defcustom org-pdftools-search-string-separator "$$"
  "Separator of search-string."
  :group 'org-pdftools
  :type 'string)
(defcustom org-pdftools-free-pointer-color "#FFFFFF"
  "Color for free pointer annotations."
  :group 'org-pdftools
  :type 'string)
(defcustom org-pdftools-free-pointer-opacity 1.0
  "Opacity for free pointer annotations."
  :group 'org-pdftools
  :type 'float)

(defcustom org-pdftools-protocol "pdftools"
  "The link and link type to use for pdftools links."
  :group 'org-pdftools
  :type 'string)


;; pdftools://path::page++height_percent;;annot_id$$isearch_string or @@occur_search_string
(defun org-pdftools-open-pdftools (link)
  "Internal function to open org-pdftools LINK."
  (let ((link-regexp
         (concat "\\(.*\\)::\\([0-9]*\\)\\(\\+\\+\\)?\\([[0-9]\\.*[0-9]*\\)?\\(;;\\|"
                 (regexp-quote org-pdftools-search-string-separator)
                 "\\)?\\(.*\\)")))
    (cond ((string-match link-regexp link)
           (let ((path (match-string 1 link))
                 (page (match-string 2 link))
                 (height (match-string 4 link))
                 annot-id
                 search-string)
             (cond ((string-equal
                     (match-string 5 link)
                     ";;")
                    (setq annot-id
                          (match-string 6 link)))
                   ((string-equal
                     (match-string 5 link)
                     org-pdftools-search-string-separator)
                    (setq search-string
                          (replace-regexp-in-string
                           "%20"
                           " "
                           (match-string 6 link)))))
             (when (and path
                        (not (string-empty-p path)))
               (if (bound-and-true-p org-noter--session)
                   (org-noter--with-valid-session
                    (let ((doc (with-selected-window
                                   (org-noter--get-doc-window)
                                 (buffer-file-name)))
                          (fullpath (expand-file-name
                                     path
                                     org-pdftools-root-dir)))
                      (if (string-equal doc fullpath)
                          (select-window
                           (org-noter--get-doc-window))
                        (let ((org-link-frame-setup
                               (cl-acons 'file 'find-file-other-frame org-link-frame-setup)))
                          (org-open-file path 1)))))
                 (org-open-file path 1)))
             (if (and page
                      (not (string-empty-p page)))
                 (progn
                   (setq page (string-to-number page))
                   (if (bound-and-true-p org-noter--session)
                       (org-noter--with-valid-session
                        (with-selected-window
                            (org-noter--get-doc-window)
                          (pdf-view-goto-page page)))
                     (pdf-view-goto-page page)))
               (setq page nil))
             (when (and height
                        (not (string-empty-p height)))
               (if (bound-and-true-p org-noter--session)
                   (org-noter--with-valid-session
                    (with-selected-window
                        (org-noter--get-doc-window)
                      (image-set-window-vscroll
                       (round
                        (/
                         (*
                          (string-to-number height)
                          (cdr (pdf-view-image-size)))
                         (frame-char-height))))))
                 (image-set-window-vscroll
                  (round
                   (/
                    (*
                     (string-to-number height)
                     (cdr (pdf-view-image-size)))
                    (frame-char-height))))))
             (when (and annot-id
                        (not (string-empty-p annot-id)))
               (if (bound-and-true-p org-noter--session)
                   (org-noter--with-valid-session
                    (with-selected-window
                        (org-noter--get-doc-window)
                      (pdf-annot-show-annotation
                       (pdf-info-getannot annot-id)
                       t)))
                 (pdf-annot-show-annotation
                  (pdf-info-getannot annot-id)
                  t)))
             (when (and search-string
                        (not (string-empty-p search-string)))
               (if (bound-and-true-p org-noter--session)
                   (org-noter--with-valid-session
                    (with-selected-window
                        (org-noter--get-doc-window)
                      (isearch-mode t)
                      (isearch-yank-string search-string)))
                 (isearch-mode t)
                 (isearch-yank-string search-string)))))
          ((string-match
            "\\(.*\\)@@\\(.*\\)"
            link)
           (let* ((paths (match-string 1 link))
                  (occur-search-string (match-string 2 link))
                  (pathlist (split-string paths "%&%")))
             (pdf-occur-search
              pathlist
              occur-search-string))))))

;;;###autoload
(defun org-pdftools-open (link)
  "Function to open org-pdftools LINK."
  (if (and (display-graphic-p)
           (featurep 'pdf-tools))
      (org-pdftools-open-pdftools
       link)
    (if (bound-and-true-p org-pdftools-open-custom-open)
        (funcall
         org-pdftools-open-custom-open
         link)
      (let* ((path (when (string-match
                          "\\(.+\\)::.+"
                          link)
                     (match-string 1 link))))
        (org-open-file path)))))

(defun org-pdftools-get-link (&optional from-org-noter)
  "Get link from the active pdf buffer.
Integrate with `org-noter' when FROM-ORG-NOTER."
  (let* ((path (org-pdftools-get-path
                (file-relative-name
                 buffer-file-name
                 org-pdftools-root-dir)))
         (page (pdf-view-current-page))
         (annot-id (if (pdf-view-active-region-p)
                       (pdf-annot-get-id
                        (funcall
                         org-pdftools-markup-pointer-function
                         (pdf-view-active-region t)
                         org-pdftools-markup-pointer-color
                         `((opacity . ,org-pdftools-markup-pointer-opacity))))
                     (if (and (not from-org-noter)
                              (pdf-annot-getannots page))
                         (condition-case nil
                             (pdf-annot-get-id
                              (pdf-annot-read-annotation
                               "Click the annotation that you want to link to."))
                           (error
                            (if (y-or-n-p
                                 "You can click anywhere on the page to add a link to. Do you want to do that? ")
                                (pdf-annot-get-id
                                 (funcall-interactively
                                  #'pdf-annot-add-text-annotation
                                  (pdf-util-read-image-position
                                   "Click where a new text annotation should be added ...")
                                  org-pdftools-free-pointer-icon
                                  `((color . ,org-pdftools-free-pointer-color)
                                    (opacity . ,org-pdftools-free-pointer-opacity))))
                              nil)))
                       (if (y-or-n-p
                            "You can click anywhere on the page to add a link to. Do you want to do that? ")
                           (pdf-annot-get-id
                            (funcall-interactively
                             #'pdf-annot-add-text-annotation
                             (pdf-util-read-image-position
                              "Click where a new text annotation should be added ...")
                             org-pdftools-free-pointer-icon
                             `((color . ,org-pdftools-free-pointer-color)
                               (opacity . ,org-pdftools-free-pointer-opacity))))
                         nil))))
         (height (cond ((bound-and-true-p annot-id)
                        (nth 1 (pdf-annot-get
                                (pdf-info-getannot
                                 annot-id
                                 path)
                                'edges)))
                       (t
                        (/
                         (*
                          (or (image-mode-window-get
                               'vscroll)
                              0)
                          (frame-char-height))
                         (float
                          (cdr (pdf-view-image-size)))))))
         ;; pdftools://path::page++height_percent;;annot_id\\|??search-string
         (search-string (if (and (not annot-id)
                                 (y-or-n-p
                                  "Do you want to add a isearch link? "))
                            isearch-string
                          ""))
         (link (concat
                org-pdftools-protocol ":"
                path
                "::"
                (number-to-string page)
                "++"
                (format "%.2f" height)
                (if annot-id
                    (concat
                     ";;"
                     (symbol-name annot-id))
                  (if (not (string-empty-p search-string))
                      (concat
                       org-pdftools-search-string-separator
                       (replace-regexp-in-string
                        " "
                        "%20"
                        search-string))
                    (message
                     "   Reminder: You haven't performed a isearch!") "")))))
    link))

;;;###autoload
(defun org-pdftools-store-link ()
  "Store a link to a pdfview/pdfoccur buffer."
  (cond ((eq major-mode 'pdf-view-mode)
         ;; This buffer is in pdf-view-mode
         (let ((desc (if (pdf-view-active-region-p)
                         (replace-regexp-in-string
                          "\n"
                          " "
                          (mapconcat
                           'identity
                           (pdf-view-active-region-text)
                           ? )))))
         (org-link-store-props
                :type org-pdftools-protocol
                :link (org-pdftools-get-link)
                :description desc)))
        ((eq major-mode
             'pdf-occur-buffer-mode)
         (let* ((paths (mapconcat
                        #'identity
                        (mapcar
                         #'car
                         pdf-occur-search-documents)
                        "%&%"))
                (occur-search-string pdf-occur-search-string)
                (link (concat
                       org-pdftools-protocol ":"
                       paths
                       "@@"
                       occur-search-string)))
           (org-link-store-props
                :type org-pdftools-protocol
                :link link
                :description (concat "Search: " occur-search-string))))))

(defun org-pdftools-export (link description format)
  "Export the pdfview LINK with DESCRIPTION for FORMAT from Org files."
  (let* ((path (when (string-match
                      "\\(.+\\)::.+"
                      link)
                 (match-string 1 link)))
         (desc (or description link)))
    (when (stringp path)
      (setq path
            (org-link-escape
             (expand-file-name path)))
      (cond ((eq format 'html)
             (format
              "<a href=\"%s\">%s</a>"
              path
              desc))
            ((eq format 'latex)
             (format
              "\\href{%s}{%s}"
              path
              desc))
            ((eq format 'ascii)
             (format "%s (%s)" desc path))
            (t path)))))

;;;###autoload
(defun org-pdftools-complete-link (&optional arg)
  "Use the existing file name completion for file.
Links to get the file name, then ask the user for the page number
and append it. ARG is passed to `org-link-complete-file'."
  (concat
   (replace-regexp-in-string
    "^file:"
    (concat org-pdftools-protocol ":")
    (org-link-complete-file arg))
   "::"
   (read-from-minibuffer
    "Page:"
    "1")))


(defun org-pdftools-get-path (rel-path)
  "Get full path from REL-PATH."
  (let* ((fullpath (expand-file-name rel-path org-pdftools-root-dir))
         (rel-home-path (file-relative-name fullpath (getenv "HOME"))))
    (if (string-suffix-p ".." rel-home-path)
        fullpath
      (concat "~/" rel-home-path))))

(provide 'org-pdftools)
;;; org-pdftools.el ends here
