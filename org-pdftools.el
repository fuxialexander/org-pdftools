;;; org-pdfview.el --- Support for links to documents in pdfview mode
;; Copyright (C) 2019 Alexander Fu Xi

;; Author: Alexander Fu Xi <fuxialexander@gmail.com>
;; Maintainer: Alexander Fu Xi <fuxialexnader@gmail.com>
;; Keywords: org, pdf-tools
;; Version: 0.9
;; Package-Requires: ((org "9.2") (pdf-tools "1.0"))

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
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Add support for org links from pdftools buffers
;;
;; To enable this automatically, use:
;;     (eval-after-load 'org '(require 'org-pdfview))

;; If you want, you can also configure the org-mode default open PDF file function.
;; (add-to-list 'org-file-apps '("\\.pdf\\'" . (lambda (file link) (org-pdfview-open link))))

;;; Code:
(require 'org)
(require 'pdf-tools)
(require 'pdf-view)
(require 'pdf-annot)
(require 'pdf-occur)

(defcustom org-pdftools-root-dir org-directory
  "Root directory for document."
  :group 'org-pdftools
  :type 'directory)

(defcustom org-pdftools-open-custom-open nil
  "Custom function to open linked pdf files."
  :group 'org-pdftools
  :type 'function)
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
  "Color for markup pointer annotations"
  :group 'org-pdftools
  :type 'string)
(defcustom org-pdftools-markup-pointer-opacity 1.0
  "Color for markup pointer annotations"
  :group 'org-pdftools
  :type 'float)
(defcustom org-pdftools-free-pointer-icon "Circle"
  "Color for free pointer annotations. Refer to `pdf-annot-standard-text-icons`."
  :group 'org-pdftools
  :type 'string)
(defcustom org-pdftools-free-pointer-color "#FFFFFF"
  "Color for free pointer annotations"
  :group 'org-pdftools
  :type 'string)
(defcustom org-pdftools-free-pointer-opacity 1.0
  "Color for free pointer annotations"
  :group 'org-pdftools
  :type 'float)

(if (fboundp 'org-link-set-parameters)
    (org-link-set-parameters
     "pdftools"
     :follow #'org-pdftools-open
     :complete #'org-pdftools-complete-link
     :store #'org-pdftools-store-link)
  (org-add-link-type
   "pdftools"
   'org-pdftools-open)
  (add-hook
   'org-store-link-functions
   'org-pdftools-store-link))

;; pdftools://path::page++height_percent;;annot_id@@search_string
(defun org-pdftools-open-pdftools (link)
  (cond ((string-match
          "\\(.*\\)::\\([0-9]*\\)\\+\\+\\([[0-9]\\.*[0-9]*\\);;\\(.*\\)"
          link)
         (let* ((path (match-string 1 link))
                (page (string-to-number
                       (match-string 2 link)))
                (height (string-to-number
                         (match-string 3 link)))
                (annot-id (match-string 4 link)))
           (org-open-file path 1)
                                        ;TODO Integrate org-noter?
           (pdf-view-goto-page page)
           (image-set-window-vscroll
            (round
             (/
              (*
               height
               (cdr (pdf-view-image-size)))
              (frame-char-height))))
           (pdf-annot-show-annotation
            (pdf-info-getannot annot-id)
            t)))
        ((string-match
          "\\(.*\\)@@\\(.*\\)"
          link)
         (let* ((paths (match-string 1 link))
                (search-string (match-string 2 link))
                (pathlist (split-string paths "%&%")))
           (pdf-occur-search
            pathlist
            search-string)))
        ((string-match
          "\\(.*\\)::\\([0-9]*\\)\\+\\+\\([[0-9]\\.*[0-9]*\\)"
          link)
         (let* ((path (match-string 1 link))
                (page (string-to-number
                       (match-string 2 link)))
                (height (string-to-number
                         (match-string 3 link))))
           (org-open-file path 1)
           (pdf-view-goto-page page)
           (image-set-window-vscroll
            (round
             (/
              (*
               height
               (cdr (pdf-view-image-size)))
              (frame-char-height))))))
        ;; vscroll * frame char height / pdf view imge y size = height
        ((string-match
          "\\(.*\\)::\\([0-9]*\\)\\)"
          link)
         (let* ((path (match-string 1 link))
                (page (string-to-number
                       (match-string 2 link))))
           (org-open-file path 1)
           (pdf-view-goto-page page)))
        (t (org-open-file link 1))))

(defun org-pdftools-open (link)
  (if (and (display-graphic-p)
           (featurep 'pdf-tools))
      (org-pdftools-open-pdftools
       link)
    (if (bound-and-true-p org-pdftools-open-custom-open)
        (funcall
         #'org-pdftools-open-custom-open
         link)
      (let* ((path (when (string-match
                          "\\(.+\\)::.+"
                          link)
                     (match-string 1 link))))
        (org-open-file path)))))

(add-hook 'org-store-link-functions 'org-pdftools-store-link)

(defun org-pdftools-store-link ()
  "Store a link to a pdfview/pdfoccur buffer."
  (cond ((eq major-mode 'pdf-view-mode)
         ;; This buffer is in pdf-view-mode
         (let* ((path (concat
                       org-pdftools-root-dir
                       (file-relative-name
                        buffer-file-name
                        org-pdftools-root-dir)))
                (page (pdf-view-current-page))
                (annot-id
                 (if pdf-view-active-region
                     (pdf-annot-get-id
                      (funcall
                       org-pdftools-markup-pointer-function
                       (pdf-view-active-region t)
                       org-pdftools-markup-pointer-color
                       `((opacity . ,org-pdftools-markup-pointer-opacity))))
                   (if (pdf-annot-getannots page)
                       (condition-case-unless-debug
                           nil
                           (pdf-annot-get-id
                            (pdf-annot-read-annotation
                             "Click the annotation that you want to link to."))
                         (error nil)))
                   (if (y-or-n-p
                        "Do you want to create a free pointer annotation for the link?")
                       (pdf-annot-get-id
                        (funcall-interactively
                         #'pdf-annot-add-text-annotation
                         (pdf-util-read-image-position
                          "Click where a new text annotation should be added ...")
                         org-pdftools-free-pointer-icon
                         `((color . ,org-pdftools-free-pointer-color)
                           (opacity . ,org-pdftools-free-pointer-opacity))))
                     nil)))
                (height (cond ((boundp 'annot-id)
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
                ;; pdftools://path::page++height_percent;;annot_id
                (link (concat
                       "pdftools:"
                       path
                       "::"
                       (number-to-string page)
                       "++"
                       (format "%.2f" height)
                       (if annot-id
                           (concat
                            ";;"
                            (symbol-name annot-id))
                         nil))))
           (org-link-store-props
            :type "pdftools"
            :link link)))
        ((eq major-mode
             'pdf-occur-buffer-mode)
         (let* ((paths (mapconcat
                        #'identity
                        (mapcar
                         #'car
                         pdf-occur-search-documents)
                        "%&%"))
                (search-string pdf-occur-search-string)
                (link (concat
                       "pdftools:"
                       paths
                       "@@"
                       search-string)))
           (org-link-store-props
            :type "pdftools"
            :link link
            :description (concat
                          "Search PDF for: "
                          search-string))))))

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

(defun org-pdftools-complete-link (&optional arg)
  "Use the existing file name completion for file.
Links to get the file name, then ask the user for the page number
and append it."
  (concat
   (replace-regexp-in-string
    "^file:"
    "pdfview:"
    (org-file-complete-link arg))
   "::"
   (read-from-minibuffer
    "Page:"
    "1")))


(provide 'org-pdftools)
;;; org-pdfview.el ends here
