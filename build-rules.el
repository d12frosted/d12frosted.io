;;; build-rules.el --- Build rules for d12frosted.io -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2022-2026, Boris Buliga <boris@d12frosted.io>
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

(add-to-list 'load-path (expand-file-name ".config/emacs/" (getenv "HOME")))
(add-to-list 'load-path (expand-file-name ".config/emacs/lisp" (getenv "HOME")))

(require 'init)

;; Force reload vulpea and publicatorg from local development versions
;; This overrides elpaca-loaded versions with local development code
(let ((projects-dir (expand-file-name "Developer/" (getenv "HOME"))))
  (push (expand-file-name "vulpea" projects-dir) load-path)
  (push (expand-file-name "publicatorg" projects-dir) load-path)
  ;; Force reload to override elpaca versions
  (load (expand-file-name "vulpea/vulpea-db-query.el" projects-dir) nil t)
  (load (expand-file-name "publicatorg/publicatorg.el" projects-dir) nil t))

(require 'dash)
(require 'vulpea)
(require 'publicatorg)



;; Media predicates now in publicatorg:
;; - porg-supported-media-p
;; - porg-supported-video-p
;; - porg-supported-image-p
;; - porg-convertible-image-p



(defvar blog-static-images
  '(("static_d12frosted" . "d12frosted.png")
    ("static_emacs" . "emacs.png")
    ("static_flyspell_correct" . "flyspell-correct.png")
    ("static_haskell" . "haskell.png")
    ("static_org_mode" . "org-mode.png")
    ("static_vino" . "vino.png")
    ("static_vulpea" . "vulpea.png")
    ("static_yabai" . "yabai.png")
    ("static_fish" . "fish.png")
    ("static_vui" . "vui.png")))



;; porg-describe for porg-item and porg-rule-output now in publicatorg.
;; Only the vulpea-note method is project-specific.

(cl-defmethod porg-describe ((note vulpea-note))
  "Describe NOTE."
  (let ((tags (vulpea-note-tags note)))
    (format "(%s) %s"
            (cond
             ((seq-contains-p tags "post") "post")
             ((seq-contains-p tags "page") "page")
             (t "???"))
            (vulpea-note-title note))))



;; File name utilities now in publicatorg:
;; - porg-file-name-sanitize (replaces porg-file-name-sanitize)
;; - porg-file-name-for-web (replaces porg-file-name-for-web)
;; - porg-porg-file-name-replace-ext (replaces porg-file-name-replace-ext)

(defun directory-from-uuid (uuid)
  "Adapt UUID to directory name."
  (if (string-match string-uuid-regexp uuid)
      (concat (s-left 2 uuid) "/" (s-chop-prefix (s-left 2 uuid) uuid))
    uuid))

(defun file-name-has-ext-p (file ext)
  "Return non-nil if FILE has EXT."
  (let ((e (s-downcase (file-name-extension file))))
    (if (listp ext)
        (seq-contains-p ext e)
      (string-equal ext e))))

(defun string-to-bool (str)
  "Convert STR to bool."
  (pcase str
    ("true" t)
    ("t" t)
    (_ nil)))



;; blog-sanitize-id-link moved to publicatorg as porg-sanitize-id-link



;; blog-sha1sum-attachment moved to publicatorg as porg-sha1sum-attachment



;; Delete utility now in publicatorg:
;; - porg-delete-with-metadata (replaces porg-delete-with-metadata)






;; Using porg-make-publish from publicatorg for org->markdown conversion



(cl-defun blog-build-post (temp-target item _items)
  "Build post ITEM to TEMP-TARGET."
  ;; (vulpea-utils-with-note (porg-item-item item)
  ;;   (--each (seq-reverse
  ;;            (org-element-map
  ;;                (org-element-parse-buffer 'element)
  ;;                'src-block
  ;;              (lambda (h)
  ;;                (org-element-property :begin h))))
  ;;     (goto-char it)
  ;;     (let ((org-confirm-babel-evaluate nil))
  ;;       (save-excursion
  ;;         (silenzio
  ;;          (funcall-interactively #'org-babel-execute-src-block)))))
  ;;   (save-buffer))
  (with-current-buffer (find-file-noselect temp-target)
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
                     (while (and (looking-at "^ *$")
                                 (not (equal (point) (point-max))))
                       (forward-line 1))
                     (point)))
                 (point-max)))))
    (save-excursion
      (replace-string-in-region
       "—" " - "
       (point-min)
       (point-max)))
    (save-buffer)))



(defun blog-uniq-porg-items (items)
  "Return uniq ITEMS."
  (let ((-compare-fn #'(lambda (a b)
                         (string-equal
                          (porg-item-target-rel a)
                          (porg-item-target-rel b)))))
    (-uniq items)))

(cl-defun blog-publish-nextjs/images (target items _items-all _cache)
  "Generate nextjs/images file from ITEMS in TARGET file.

_ITEMS-ALL is input table as returned by `porg-build-input'."
  (let ((auto-mode-alist '("\\.tsx\\'" . text-mode))
        (tbl (make-hash-table :test 'equal :size (hash-table-size items))))
    (with-current-buffer (find-file-noselect target)
      (delete-region (point-min) (point-max))
      (insert
       "// THIS FILE IS AUTOMATICALLY GENERATED BY RUNNING\n"
       "//\n"
       "//   $ yarn notes\n"
       "//\n"
       "// DO NOT MODIFY IT MANUALLY\n"
       "\n"
       "import { StaticImageData } from \"next/image\";\n"
       "\n")
      (--each (blog-uniq-porg-items (hash-table-values items))
        (let ((var (concat "pic_"
                           (->> (porg-item-id it)
                                (s-chop-suffix ".webp")
                                (s-replace-regexp "[^a-zA-Z0-9]" "_")))))
          (puthash (s-chop-prefix "src/public/content/" (porg-item-target-rel it)) var tbl)
          (insert "import " var " from \"../../" (s-chop-prefix "src/" (porg-item-target-rel it)) "\";\n")))
      (insert "\n")
      (--each blog-static-images
        (insert "import " (car it) " from \"../../public/content/images/" (cdr it) "\";\n"))
      (insert "\n")
      (insert "const lookupMap = new Map<string, StaticImageData>([\n")
      (--each (hash-table-keys tbl)
        (insert "  [\"/" it "\", " (gethash it tbl) "],\n"))
      (--each blog-static-images
        (insert   "  [\"/images/" (cdr it) "\", " (car it) "],\n"))
      (insert "]);\n\n")
      (insert
       "export function getImage(path: string): StaticImageData {\n"
       "  const res = lookupMap.get(path);\n"
       "  if (res) return res;\n"
       "  throw new Error(`Image ${path} not found`);\n"
       "}\n")
      (save-buffer))))



(cl-defun blog-rule-void (&key name tags)
  "Create a rule with a NAME that voids notes with TAGS."
  (porg-rule
   :name name
   :match (lambda (note) (apply (-partial #'vulpea-note-tagged-all-p note) tags))
   :outputs (lambda (note) (list (porg-void-output note)))))



(org-link-set-parameters "d12frosted" :follow #'org-roam-link-follow-link)
(setf porg-log-level 'info)
(setf porg-cache-backend 'sqlite)



(porg-define
 :name "d12frosted.io"
 :root (if load-file-name
           (file-name-directory load-file-name)
         (expand-file-name "d12frosted.io/" path-projects-dir))
 :cache-file "notes-cache"

 :input
 (lambda ()
   (--filter
    (and (null (vulpea-note-primary-title it))
         (= 0 (vulpea-note-level it)))
    (vulpea-db-query-by-tags-every '("d12frosted/public"))))

 :rules
 (list
  ;; void rules, just to ignore some of the notes, which are used as soft deps only
  (blog-rule-void :name "grapes" :tags '("wine" "grape"))
  (blog-rule-void :name "country" :tags '("wine" "country"))
  (blog-rule-void :name "regions" :tags '("wine" "region"))
  (blog-rule-void :name "appellations" :tags '("wine" "appellation"))
  (blog-rule-void :name "places" :tags '("places"))

  ;; real rules
  (porg-rule
   :name "posts"
   :match (-rpartial #'vulpea-note-tagged-all-p "post")
   :outputs
   (porg-make-outputs
    :file
    (lambda (note)
      (concat
       "src/public/content/posts/"
       (vulpea-utils-with-note note
         (let ((date (vulpea-buffer-prop-get "date"))
               (slug (or (vulpea-buffer-prop-get "slug")
                         (porg-slug (vulpea-note-title note)))))
           (unless date (user-error "Post '%s' (%s) is missing date" (vulpea-note-title note)
                                    (vulpea-note-path note)))
           (concat (org-read-date nil nil date) "-" slug)))
       ".md"))
    :attach-dir
    (lambda (note attachment)
      (let* ((date (vulpea-utils-with-note note (vulpea-buffer-prop-get "date")))
             (slug (vulpea-utils-with-note note
                     (or (vulpea-buffer-prop-get "slug")
                         (porg-slug (vulpea-note-title note)))))
             (name (concat (org-read-date nil nil date) "-" slug)))
        (if (porg-supported-video-p attachment)
            (concat "src/public/content/" name)
          (concat "src/public/content/images/" name))))
    :outputs-extra
    (lambda (output)
      (let* ((note (porg-rule-output-item output)))
        (list
         (porg-rule-output
          :id (concat (vulpea-note-id note) ".json")
          :type "json"
          :item note
          :file (porg-file-name-replace-ext (porg-rule-output-file output) "json")
          :hard-deps (-distinct
                      (-concat
                       (list (porg-rule-output-id output))
                       (porg-rule-output-hard-deps output)))
          :soft-deps (porg-rule-output-soft-deps output)))))))

  (porg-batch-rule
   :name "nextjs/images"
   :filter (-rpartial #'porg-item-that :type "attachment" :predicate #'porg-supported-image-p)
   :target "src/components/content/images.tsx"
   :publish #'blog-publish-nextjs/images))

 :compilers
 (list
  (porg-compiler
   :name "post"
   :match (-rpartial #'porg-rule-output-that :type "note"
                     :predicate (-rpartial #'vulpea-note-tagged-all-p "post"))
   :hash #'porg-sha1sum
   :build (porg-make-publish
           :copy-fn #'blog-build-post
           :image-dir-fn (lambda (item)
                           (directory-from-uuid
                            (file-name-base (porg-item-target-abs item))))
           :sanitize-id-fn (lambda (items)
                             (lambda (link)
                               (porg-sanitize-id-link
                                link items
                                :content-prefix "src/public/content"))))
   :clean #'porg-delete-with-metadata)

  (porg-compiler
   :name "post/metadata"
   :match (-rpartial #'porg-rule-output-that :type "json"
                     :predicate (-rpartial #'vulpea-note-tagged-all-p "post"))
   :hash #'porg-sha1sum
   :build (lambda (item items _cache)
            (let* ((note (porg-item-item item))
                   (metadata (vulpea-utils-with-note note
                               (let ((date (vulpea-buffer-prop-get "date"))
                                     (author (let ((x (vulpea-buffer-prop-get "author")))
                                               (if (and x (string-equal x "Boris")) "Boris Buliga" x)))
                                     (image (vulpea-buffer-prop-get "image"))
                                     (description (vulpea-buffer-prop-get "description"))
                                     (tags (vulpea-buffer-prop-get-list "tags")))
                                 (unless date (user-error "Post '%s' is missing date" (vulpea-note-title note)))
                                 (unless author (user-error "Post '%s' is missing author" (vulpea-note-title note)))
                                 (-concat
                                  `(("date" . ,(org-read-date nil nil date))
                                    ("authors" . ,(list author)))
                                  (when image
                                    (let ((image-item (gethash (concat (vulpea-note-id note)
                                                                       ":"
                                                                       (porg-file-name-sanitize
                                                                        (s-chop-prefix "attachment:" image)
                                                                        "webp"))
                                                               items)))
                                      (unless image-item
                                        (user-error "%s is using %s as an image, but it does not exist"
                                                    (funcall #'porg-describe item) image))
                                      (unless (file-exists-p (porg-item-target-abs image-item))
                                        (user-error "Image %s does not exist at %S"
                                                    (funcall #'porg-describe image-item)
                                                    (porg-item-target-abs image-item)))
                                      `(("image" . ,(porg-item-target-rel image-item))
                                        ("image-width" . ,(shell-command-to-string
                                                           (format "identify -format '%%w' '%s'"
                                                            (porg-item-target-abs image-item))))
                                        ("image-height" . ,(shell-command-to-string
                                                            (format "identify -format '%%h' '%s'"
                                                             (porg-item-target-abs image-item)))))))
                                  (when description
                                    `(("description" . ,description)))
                                  (when tags
                                    `(("tags" . ,tags)))))))
                   (hash-a (when (file-exists-p (porg-item-target-abs item))
                             (porg-sha1sum (porg-item-target-abs item))))
                   (meta-file (porg-file-name-replace-ext (porg-item-target-abs item) "json"))

                   (hash-b (porg-sha1sum (porg-item-target-abs item)))

                   (meta (if (functionp metadata) (funcall metadata item items) metadata))

                   (update (when (file-exists-p meta-file)
                             (with-current-buffer (find-file-noselect meta-file)
                               (goto-char (point-min))
                               (when (re-search-forward
                                      "\"update\": \"\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)\""
                                      nil t)
                                 (match-string 1)))))
                   (update (when (string-equal hash-a hash-b) update))
                   (update (or update (format-time-string "%F")))

                   (date (plist-get meta "date"))
                   (update (if (and date (org-time< update date)) date update))

                   (pinned (string-to-bool (or (vulpea-note-meta-get note "pinned") "false")))
                   (publish (string-to-bool (or (vulpea-note-meta-get note "publish") "false")))
                   (hide (string-to-bool (or (vulpea-note-meta-get note "hide") "false")))

                   (related (vulpea-note-meta-get-list note "related" 'link))

                   (event (when (vulpea-note-tagged-all-p note "event")
                            (let ((summary (blog-event-summary note)))
                              (setf (alist-get 'wines summary)
                                    (-map (lambda (data)
                                            ;; replace convives inside the scores with id, name and public name
                                            (-map (lambda (score)
                                                    (setf
                                                     (alist-get 'participant score)
                                                     (let ((c (alist-get 'participant score)))
                                                       (if (vulpea-note-p c)
                                                           `((id . ,(vulpea-note-id c))
                                                             (name . ,(vulpea-note-title c))
                                                             (publicName . ,(vulpea-note-meta-get c "public name")))
                                                         c)))
                                                    score)
                                                  (alist-get 'scores data))
                                            ;; replace note with id
                                            (setf (alist-get 'wine data)
                                                  (vulpea-note-id (or (alist-get 'wine data)
                                                                      (user-error "Seems like scores table has more wines than your events"))))
                                            data)
                                          (alist-get 'wines summary)))
                              summary)))

                   (meta (-concat `(("publish" . ,publish)
                                    ("hide" . ,hide)
                                    ("pinned" . ,pinned)
                                    ("title" . ,(vulpea-note-title note))
                                    ("id" . ,(vulpea-note-id note))
                                    ("related" . ,related))
                                  (when update `(("update" . ,update)))
                                  meta
                                  (when event `(("event" . ,event))))))
              (with-current-buffer (find-file-noselect meta-file)
                (delete-region (point-min) (point-max))
                (let ((json-encoding-pretty-print t))
                  (insert (json-encode meta)))
                (save-buffer))))
   :clean #'porg-delete-with-metadata)

  (porg-images-compiler :hash #'porg-sha1sum-attachment)

  (porg-videos-compiler :hash #'porg-sha1sum-attachment)))



(provide 'build-rules)
;;; build-rules.el ends here
