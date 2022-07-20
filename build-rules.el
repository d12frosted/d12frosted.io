;;; build-rules.el --- Build rules for d12frosted.io -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2022, Boris Buliga <boris@d12frosted.io>
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; Maintainer: Boris Buliga <boris@d12frosted.io>
;; Version: 0.1
;; Package-Requires: ((emacs "29") (publicatorg "0.1"))
;;
;; Created: 11 Jul 2022
;;
;; URL: https://github.com/d12frosted/d12frosted.io
;;
;; License: GPLv3
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see
;; <http://www.gnu.org/licenses/>.
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;; Code:

(require 'init)

(require 'dash)
(require 'vulpea)
(require 'publicatorg)



(defconst d12-supported-images '("jpeg" "png" "jpg" "heic" "webp"))

(defun d12-supported-image-p (file)
  "Return non-nil if FILE is a supported image."
  (file-name-has-ext-p file d12-supported-images))

(defun d12-supported-attachment-p (file)
  "Return non-nil if FILE is a supported attachment."
  (file-name-has-ext-p file (-concat d12-supported-images '("gif" "svg" "mp4"))))



(cl-defmethod porg-describe ((item porg-item))
  "Describe ITEM."
  (pcase (porg-item-type item)
    ("note" (porg-describe (porg-item-item item)))
    ("attachment" (concat "(image) " (file-name-nondirectory (porg-item-target-abs item))))
    (_ (concat "(" (porg-item-type item) ") " (porg-item-id item)))))

(cl-defmethod porg-describe ((item porg-rule-output))
  "Describe ITEM."
  (pcase (porg-rule-output-type item)
    ("note" (porg-describe (porg-rule-output-item item)))
    ("attachment" (concat "(image) " (file-name-nondirectory (porg-rule-output-file item))))
    (_ (concat "(" (porg-rule-output-type item) ") " (porg-rule-output-id item)))))

(cl-defmethod porg-describe ((note vulpea-note))
  "Describe NOTE."
  (let ((tags (vulpea-note-tags note)))
    (format "(%s) %s"
            (cond
             ((seq-contains-p tags "post") "post")
             ((seq-contains-p tags "page") "page")
             (t "???"))
            (vulpea-note-title note))))



(cl-defun d12-make-outputs (&key file
                                 attach-dir
                                 attach-filter
                                 soft-deps
                                 hard-deps
                                 outputs-extra)
  "Make outputs function for note.

Just a wrapper around `porg-note-output' and
`porg-attachments-output'.

FILE is a function that takes a `vulpea-note' and returns
relative output file.

ATTACH-DIR is a function that takes a `porg-rule-output' of note
and returns relative directory for attachments. Optional. I am
too lazy to explain default implementation.

ATTACH-FILTER is a predicate on attachment file. Controls which
attachments should be part of the output. Defaults to
`d12-supported-attachment-p'.

OUTPUTS-EXTRA is a function that takes a `porg-rule-output' of
note and returns list of additional outputs.

See `porg-note-output' for documentation for SOFT-DEPS and
HARD-DEPS. But in this case these are functions on
`vulpea-note'."
  (lambda (note)
    (let ((note-output
           (porg-note-output note
                             :file (funcall file note)
                             :soft-deps (when soft-deps (funcall soft-deps note))
                             :hard-deps (when hard-deps (funcall hard-deps note)))))
      (-concat (list note-output)
               (porg-attachments-output
                note
                :dir (if attach-dir
                         (funcall attach-dir note-output)
                       (let ((name (directory-from-uuid
                                    (file-name-base (porg-rule-output-file note-output)))))
                         (concat "images/" name)))
                :file-mod #'file-name-fix-attachment
                :filter (or attach-filter #'d12-supported-attachment-p))
               (when outputs-extra
                 (funcall outputs-extra note-output))))))

(cl-defun d12-make-publish (&key copy-fn metadata)
  "Create public function with COPY-FN and METADATA."
  (lambda (item items _cache)
    (let* ((target (porg-item-target-abs item))
           (hash-a (when (file-exists-p target)
                     (porg-sha1sum (porg-item-target-abs item)))))
      ;; 1. copy file
      (mkdir (file-name-directory target) 'parents)
      (funcall copy-fn item items)

      ;; 2. remove private parts
      (porg-clean-noexport-headings target)

      ;; 3. cleanup and transform links
      (with-current-buffer (find-file-noselect target)
        (porg-clean-links-in-buffer
         :sanitize-id-fn (-rpartial #'d12-sanitize-id-link items)
         :sanitize-attachment-fn
         (lambda (link)
           (let* ((path (org-ml-get-property :path link))
                  (path (file-name-fix-attachment path))
                  (dir (directory-from-uuid (file-name-base target))))
             (->> link
                  (org-ml-set-property :path (format "/images/%s/%s" dir path))
                  (org-ml-set-property :type "file")
                  (org-ml-set-children nil)))))
        (save-buffer))

      ;; 4. generate metadata
      (let* ((meta-file (concat (porg-item-target-abs item) ".metadata"))
             (note (porg-item-item item))

             (hash-b (porg-sha1sum (porg-item-target-abs item)))

             (update (when (file-exists-p meta-file)
                       (with-current-buffer (find-file-noselect meta-file)
                         (goto-char (point-min))
                         (when (re-search-forward "update: \"\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)\"" nil t)
                           (match-string 1)))))
             (update (or (and (string-equal hash-a hash-b) update)
                         (format-time-string "%F")))

             (publish (vulpea-utils-with-note note
                        (or (vulpea-buffer-prop-get "publish") "true")))

             (meta (if (functionp metadata) (funcall metadata item) metadata))
             (meta (-concat (list "publish" publish
                                  "title" (vulpea-note-title note))
                            (when update (list "update" update))
                            meta)))
        (with-current-buffer (find-file-noselect meta-file)
          (delete-region (point-min) (point-max))
          (cl-loop for (key value) on meta by 'cddr
                   do (insert key ": " "\"" (string-from value) "\"" "\n"))
          (save-buffer))))))



(cl-defun d12-sanitize-id-link (link items)
  "Sanitize ID LINK according to ITEMS."
  (if-let* ((id (org-ml-get-property :path link))
            (note (vulpea-db-get-by-id id))
            (item (gethash id items))
            (file (porg-item-target-rel item))
            (path (concat "/" (s-chop-suffix ".org" file))))
      (->> link
           (org-ml-set-property :type "d12frosted")
           (org-ml-set-property :path path))
    (org-ml-from-string
     'plain-text
     (concat (nth 2 link) (s-repeat (or (org-ml-get-property :post-blank link) 0) " ")))))



(cl-defun d12-build-post (item _items)
  "Build post ITEM."
  (with-current-buffer (find-file-noselect (porg-item-target-abs item))
    (delete-region (point-min) (point-max))
    (insert (vulpea-utils-with-note (porg-item-item item)
              (let* ((meta (vulpea-buffer-meta))
                     (pl (plist-get meta :pl)))
                (buffer-substring-no-properties
                 (if pl
                     (org-element-property :end pl)
                   (save-excursion
                     (goto-char (point-min))
                     (while (looking-at org-property-re)
                       (forward-line 1))
                     (while (looking-at "^#\\+.+$")
                       (forward-line 1))
                     (while (looking-at "^ *$")
                       (forward-line 1))
                     (point)))
                 (point-max)))))
    (save-buffer)))

(cl-defun d12-delete (cached-item root)
  "Delete CACHED-ITEM from ROOT."
  (let* ((path (porg-cache-item-output cached-item))
         (file (expand-file-name path root))
         (meta (expand-file-name (concat path ".metadata") root)))
    (delete-file file)
    (delete-file meta)))



(defun file-name-has-ext-p (file ext)
  "Return non-nil if FILE has EXT."
  (let ((e (s-downcase (file-name-extension file))))
    (if (listp ext)
        (seq-contains-p ext e)
      (string-equal ext e))))

(defun file-name-fix-attachment (file-name)
  "Fix attachment FILE-NAME."
  (let* ((ext-old (file-name-extension file-name))
         (ext-new (if (file-name-has-ext-p file-name d12-supported-images)
                      "webp"
                    ext-old)))
    (concat (s-replace "_" "-" (s-chop-suffix ext-old file-name)) ext-new)))

(defun directory-from-uuid (uuid)
  "Adapt UUID to directory name."
  (if (string-match string-uuid-regexp uuid)
      (concat (s-left 2 uuid) "/" (s-chop-prefix (s-left 2 uuid) uuid))
    uuid))



(org-link-set-parameters "d12frosted" :follow #'org-roam-link-follow-link)

(setf porg-log-level 'info)

(porg-define
 :name "d12frosted.io"
 :root (when load-file-name (file-name-directory load-file-name))
 :cache-file "build-cache"

 :input
 (lambda ()
   (--filter
    (and (null (vulpea-note-primary-title it))
         (= 0 (vulpea-note-level it)))
    (vulpea-db-query-by-tags-every '("d12frosted/public"))))

 :rules
 (list
  (porg-rule
   :name "posts"
   :match (-rpartial #'vulpea-note-tagged-all-p "post")
   :outputs
   (d12-make-outputs
    :file (lambda (note)
            (concat
             "posts/"
             (vulpea-utils-with-note note
               (let ((date (vulpea-buffer-prop-get "date"))
                     (slug (or (vulpea-buffer-prop-get "slug")
                               (porg-slug (vulpea-note-title note)))))
                 (unless date (user-error "Post '%s' is missing date" (vulpea-note-title note)))
                 (concat (org-read-date nil nil date) "-" slug)))
             ".org")))))

 :compilers
 (list
  (porg-compiler
   :name "post"
   :match (-rpartial #'porg-rule-output-that :type "note"
                     :predicate (-rpartial #'vulpea-note-tagged-all-p "post"))
   :hash #'porg-sha1sum
   :build
   (d12-make-publish
    :copy-fn #'d12-build-post
    :metadata
    (lambda (item)
      (let ((note (porg-item-item item)))
        (vulpea-utils-with-note note
          (let ((date (vulpea-buffer-prop-get "date"))
                (image (vulpea-buffer-prop-get "image"))
                (description (vulpea-buffer-prop-get "description"))
                (tags (vulpea-buffer-prop-get-list "tags")))
            (unless date (user-error "Post '%s' is missing date" (vulpea-note-title note)))
            (-concat
             (list "date" (org-read-date nil nil date))
             (when image
               (list "image"
                     (concat
                      (file-name-as-directory
                       (concat "images/" (file-name-base (porg-item-target-rel item))))
                      (file-name-fix-attachment image))))
             (when description
               (list "description" description))
             (when tags
               (list "tags" (string-join tags ", ")))))))))
   :clean #'d12-delete)

  (porg-compiler
   :name "images"
   :match (-rpartial #'porg-rule-output-that :type "attachment" :predicate #'d12-supported-image-p)
   :build
   (lambda (item _items _cache)
     (make-directory (file-name-directory (porg-item-target-abs item)) 'parents)
     (let ((max-width 960)
           (width (string-to-number
                   (shell-command-to-string
                    (format "identify -format %%W '%s'" (porg-item-item item))))))
       (porg-debug "input:  %s" (porg-item-item item))
       (porg-debug "output: %s" (porg-item-target-abs item))
       (if (> width max-width)
           (shell-command-to-string
            (format "convert '%s' -strip -auto-orient -resize %sx100^ '%s'"
                    (porg-item-item item) max-width (porg-item-target-abs item)))
         (shell-command-to-string
          (format "convert '%s' -strip -auto-orient '%s'"
                  (porg-item-item item) (porg-item-target-abs item))))))
   :clean #'d12-delete)

  (porg-compiler
   :name "gifs"
   :match (-rpartial #'porg-rule-output-that :type "attachment" :predicate (-rpartial #'file-name-has-ext-p "gif"))
   :build
   (lambda (item _items _cache)
     (make-directory (file-name-directory (porg-item-target-abs item)) 'parents)
     (copy-file (porg-item-item item) (porg-item-target-abs item) t))
   :clean #'d12-delete)

  (porg-compiler
   :name "svg"
   :match (-rpartial #'porg-rule-output-that :type "attachment" :predicate (-rpartial #'file-name-has-ext-p "svg"))
   :build
   (lambda (item _items _cache)
     (make-directory (file-name-directory (porg-item-target-abs item)) 'parents)
     (copy-file (porg-item-item item) (porg-item-target-abs item) t))
   :clean #'d12-delete)

  (porg-compiler
   :name "videos"
   :match (-rpartial #'porg-rule-output-that :type "attachment" :predicate (-rpartial #'file-name-has-ext-p "mp4"))
   :build
   (lambda (item _items _cache)
     (make-directory (file-name-directory (porg-item-target-abs item)) 'parents)
     (copy-file (porg-item-item item) (porg-item-target-abs item) t))
   :clean #'d12-delete)))



(provide 'build-rules)
;;; build-rules.el ends here
