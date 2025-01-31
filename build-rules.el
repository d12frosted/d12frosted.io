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



(defconst d12-supported-video-media '("mp4"))
(defconst d12-supported-image-media '("jpeg" "png" "jpg" "heic" "webp" "gif" "svg"))
(defconst d12-convertible-images '("jpeg" "png" "jpg" "heic" "webp"))

(defun d12-supported-media-p (file)
  "Return non-nil if FILE is a supported media."
  (seq-contains-p (-concat d12-supported-image-media d12-supported-video-media)
                  (s-downcase (file-name-extension file))))

(defun d12-supported-video-media-p (file)
  "Return non-nil if FILE is a supported video."
  (seq-contains-p d12-supported-video-media
                  (s-downcase (file-name-extension file))))

(defun d12-supported-image-media-p (file)
  "Return non-nil if FILE is a supported image."
  (seq-contains-p d12-supported-image-media
                  (s-downcase (file-name-extension file))))

(defun d12-convertible-image-p (file)
  "Return non-nil if FILE is a convertible image."
  (seq-contains-p d12-convertible-images
                  (s-downcase (file-name-extension file))))



(cl-defmethod porg-describe ((item porg-item))
  "Describe ITEM."
  (pcase (porg-item-type item)
    ("note" (porg-describe (porg-item-item item)))
    ("attachment"
     (concat "("
             (cond
              ((d12-supported-image-media-p (porg-item-target-abs item)) "image")
              ((d12-supported-video-media-p (porg-item-target-abs item)) "video")
              (t "???"))
             ") "
             (file-name-nondirectory (porg-item-target-abs item))))
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



(defun file-name-fix-attachment (file-name ext-new)
  "Fix attachment FILE-NAME with EXT-NEW."
  (let ((ext-old (file-name-extension file-name)))
    (concat (s-replace-regexp "[^a-zA-Z0-9/\\.]" "-" (s-chop-suffix ext-old file-name)) ext-new)))

(defun file-name-fix-media-attachment (file-name)
  "Fix attachment FILE-NAME with EXT-NEW."
  (if (d12-convertible-image-p file-name)
      (file-name-fix-attachment file-name "webp")
    file-name))

(defun file-name-replace-ext (file-name ext-new)
  "Replace FILE-NAME extension with EXT-NEW."
  (let ((ext-old (file-name-extension file-name)))
    (concat (s-chop-suffix ext-old file-name) ext-new)))

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



(cl-defun d12-sanitize-id-link (link items)
  "Sanitize ID LINK according to ITEMS."
  (if-let* ((id (org-ml-get-property :path link))
            (note (vulpea-db-get-by-id id))
            (item (gethash id items))
            (file (porg-item-target-rel item))
            (path (->> file
                       (s-chop-suffix ".org")
                       (s-chop-suffix ".md")
                       (s-chop-prefix "src/public/content"))))
      (->> link
           (org-ml-set-property :type "file")
           (org-ml-set-property :path path))
    (org-ml-from-string
     'plain-text
     (concat (nth 2 link) (s-repeat (or (org-ml-get-property :post-blank link) 0) " ")))))



(defun d12-sha1sum-attachment (obj)
  "Calculate sha1sum of attachment OBJ.

The attachments table is defined by custom configurations in the
init file."
  (cond
   ((porg-rule-output-p obj)
    (let ((id (s-split ":" (porg-rule-output-id obj)))
          (file (file-name-nondirectory (porg-rule-output-item obj))))
      (org-roam-db-query
       [:select hash
                :from attachments
                :where (and (= node-id $s1)
                            (= file $s2))]
       id file)))
   (t (user-error "Unknown type of attachments %s" obj))))



(cl-defun d12-delete (file)
  "Delete FILE."
  (message "delete %s" file)
  (delete-file file)
  (let ((meta (concat file ".metadata")))
    (delete-file meta)))



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
`d12-supported-media-p'.

OUTPUTS-EXTRA is a function that takes a `porg-rule-output' of
note and returns list of additional outputs.

See `porg-note-output' for documentation for SOFT-DEPS and
HARD-DEPS. But in this case these are functions on
`vulpea-note'."
  (lambda (note)
    (let* ((note-output (porg-note-output note :file (funcall file note)))
           (attachments-output
            (porg-attachments-output
             note
             :dir (lambda (attachment)
                    (if attach-dir
                        (funcall attach-dir note-output)
                      (let ((name (directory-from-uuid
                                   (file-name-base (porg-rule-output-file note-output)))))
                        (if (d12-supported-video-media-p attachment)
                            (concat "public/content/" name)
                          (concat "src/public/content/images/" name)))))
             :file-mod (list #'file-name-fix-media-attachment)
             :filter (or attach-filter #'d12-supported-media-p)))
           (outputs-extra (when outputs-extra (funcall outputs-extra note-output))))
      (-concat attachments-output
               outputs-extra
               (list
                (porg-note-output
                 note
                 :file (funcall file note)
                 :soft-deps (when soft-deps (funcall soft-deps note))
                 :hard-deps (-concat
                             (when hard-deps (funcall hard-deps note))
                             (-map #'porg-rule-output-id attachments-output)
                             (-map #'porg-rule-output-id
                                   (--filter (string-equal (porg-rule-output-type it) "attachment")
                                             outputs-extra)))))))))



(cl-defun d12-make-publish (&key copy-fn)
  "Create public function with COPY-FN."
  (lambda (item items _cache)
    (let* ((temp-file (make-temp-file "d12frosted-io" nil ".org")))
      (porg-debug "writing to temp file")
      (porg-debug "%s" temp-file)

      ;; 1. copy file
      (mkdir (file-name-directory (porg-item-target-abs item)) 'parents)
      (funcall copy-fn temp-file item items)

      ;; 2. remove private parts
      (porg-clean-noexport-headings temp-file)

      ;; 3. cleanup and transform links
      (with-current-buffer (find-file-noselect temp-file)
        (porg-clean-links-in-buffer
         :sanitize-id-fn (-rpartial #'d12-sanitize-id-link items)
         :sanitize-attachment-fn
         (lambda (link)
           (let* ((path (org-ml-get-property :path link))
                  (path (file-name-fix-media-attachment path))
                  (dir (directory-from-uuid (file-name-base (porg-item-target-abs item)))))
             (->> link
                  (org-ml-set-property :path (if (d12-supported-video-media-p path)
                                                 (format "/content/%s/%s" dir path)
                                               (format "/images/%s/%s" dir path)))
                  (org-ml-set-property :type "file")
                  (org-ml-set-children nil)))))
        (save-buffer))

      ;; 4. cleanup unsupported things
      (with-current-buffer (find-file-noselect temp-file)
        ;; table captions are not supported
        (save-excursion
          (goto-char (point-min))
          (while (search-forward-regexp "^#\\+caption:" nil t)
            (when (save-excursion
                    (forward-line)
                    (looking-at "^#\\+results:"))
              (beginning-of-line)
              (kill-line 1))))

        ;; pandoc fails to properly handle caption when attr_html is missing
        (save-excursion
          (goto-char (point-min))
          (while (search-forward-regexp "^#\\+caption:" nil t)
            (forward-line)
            (when (looking-at "\\[\\[file.*\\]\\]")
              (insert "#+attr_html: :class image\n"))))

        ;; verses are not supported by pandoc
        (save-excursion
          (goto-char (point-min))
          (while (search-forward-regexp "^#\\+begin_verse" nil t)
            (replace-match "#+begin_export html")
            (forward-line)
            (insert "<blockquote>\n")
            (while (not (looking-at "^#\\+end_verse"))
              ;; replace ---
              (when (looking-at "---")
                (replace-match "–")
                (beginning-of-line))

              ;; add line break
              (end-of-line)
              (insert "<br/>")
              (forward-line))
            (kill-line)

            ;; done
            (insert "</blockquote>\n")
            (insert "#+end_export")))

        ;; save
        (save-buffer))

      ;; 5. convert to md
      (shell-command-to-string
       (format "pandoc --from=org --to=gfm+hard_line_breaks+tex_math_dollars --wrap=none '%s' > '%s'"
               temp-file
               (porg-item-target-abs item)))
      (let ((auto-mode-alist '("\\.md\\'" . text-mode)))
        (with-current-buffer (find-file-noselect (porg-item-target-abs item))
          ;; fix attachment links
          (goto-char (point-min))
          (while (search-forward "file://" nil t)
            (replace-match ""))

          ;; fix video links
          (goto-char (point-min))
          (while (search-forward-regexp "\\[file:/content/.+\\](\\(/content/.+\\))" nil t)
            (let ((p (match-string 1)))
              (when (d12-supported-video-media-p p)
                (replace-match (format "![](%s)" p)))))
          (save-buffer))))))



(cl-defun d12-build-post (temp-target item _items)
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
    (save-buffer)))



(defun -uniq-porg-items (items)
  "Return uniq ITEMS."
  (let ((-compare-fn #'(lambda (a b)
                         (string-equal
                          (porg-item-target-rel a)
                          (porg-item-target-rel b)))))
    (-uniq items)))

(cl-defun d12-publish-nextjs/images (target items _items-all _cache)
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
      (--each (-uniq-porg-items (hash-table-values items))
        (let ((var (concat "pic_"
                           (->> (porg-item-id it)
                                (s-chop-suffix ".webp")
                                (s-replace-regexp "[^a-zA-Z0-9]" "_")))))
          (puthash (s-chop-prefix "src/public/content/" (porg-item-target-rel it)) var tbl)
          (insert "import " var " from \"../../" (s-chop-prefix "src/" (porg-item-target-rel it)) "\";\n")))
      (insert "\n")
      (insert "import static_d12frosted from \"../../public/content/images/d12frosted.png\";\n")
      (insert "import static_emacs from \"../../public/content/images/emacs.png\";\n")
      (insert "import static_flyspell_correct from \"../../public/content/images/flyspell-correct.png\";\n")
      (insert "import static_haskell from \"../../public/content/images/haskell.png\";\n")
      (insert "import static_org_mode from \"../../public/content/images/org-mode.png\";\n")
      (insert "import static_vino from \"../../public/content/images/vino.png\";\n")
      (insert "import static_vulpea from \"../../public/content/images/vulpea.png\";\n")
      (insert "import static_yabai from \"../../public/content/images/yabai.png\";\n")
      (insert "import static_fish from \"../../public/content/images/fish.png\";\n")
      (insert "\n")
      (insert "const lookupMap = new Map<string, StaticImageData>([\n")
      (--each (hash-table-keys tbl)
        (insert "  [\"/" it "\", " (gethash it tbl) "],\n"))
      (insert   "  [\"/images/d12frosted.png\", static_d12frosted],\n")
      (insert   "  [\"/images/emacs.png\", static_emacs],\n")
      (insert   "  [\"/images/flyspell-correct.png\", static_flyspell_correct],\n")
      (insert   "  [\"/images/haskell.png\", static_haskell],\n")
      (insert   "  [\"/images/org-mode.png\", static_org_mode],\n")
      (insert   "  [\"/images/vino.png\", static_vino],\n")
      (insert   "  [\"/images/vulpea.png\", static_vulpea],\n")
      (insert   "  [\"/images/yabai.png\", static_yabai],\n")
      (insert   "  [\"/images/fish.png\", static_fish],\n")
      (insert "]);\n\n")
      (insert
       "export function getImage(path: string): StaticImageData {\n"
       "  const res = lookupMap.get(path);\n"
       "  if (res) return res;\n"
       "  throw new Error(`Image ${path} not found`);\n"
       "}\n")
      (save-buffer))))



(cl-defun d12-rule-void (&key name tags)
  "Create a rule with a NAME that voids notes with TAGS."
  (porg-rule
   :name name
   :match (lambda (note) (apply (-partial #'vulpea-note-tagged-all-p note) tags))
   :outputs (lambda (note) (list (porg-void-output note)))))



(org-link-set-parameters "d12frosted" :follow #'org-roam-link-follow-link)
(setf porg-log-level 'info)
(setf porg-cache-method 'prin1)



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
  (d12-rule-void :name "grapes" :tags '("wine" "grape"))
  (d12-rule-void :name "country" :tags '("wine" "country"))
  (d12-rule-void :name "regions" :tags '("wine" "region"))
  (d12-rule-void :name "appellations" :tags '("wine" "appellation"))
  (d12-rule-void :name "places" :tags '("places"))

  ;; real rules
  (porg-rule
   :name "posts"
   :match (-rpartial #'vulpea-note-tagged-all-p "post")
   :outputs
   (d12-make-outputs
    :file
    (lambda (note)
      (concat
       "src/public/content/posts/"
       (vulpea-utils-with-note note
         (let ((date (vulpea-buffer-prop-get "date"))
               (slug (or (vulpea-buffer-prop-get "slug")
                         (porg-slug (vulpea-note-title note)))))
           (unless date (user-error "Post '%s' is missing date" (vulpea-note-title note)))
           (concat (org-read-date nil nil date) "-" slug)))
       ".md"))
    :outputs-extra
    (lambda (output)
      (s-chop-prefix "" "")
      (let* ((note (porg-rule-output-item output)))
        (-concat
         (list
          (porg-rule-output
           :id (concat (vulpea-note-id note) ".json")
           :type "json"
           :item note
           :file (file-name-replace-ext (porg-rule-output-file output) "json")
           :hard-deps (list (porg-rule-output-id output)))))))))

  (porg-batch-rule
   :name "nextjs/images"
   :filter (-rpartial #'porg-item-that :type "attachment" :predicate #'d12-supported-image-media-p)
   :target "src/components/content/images.tsx"
   :publish #'d12-publish-nextjs/images))

 :compilers
 (list
  (porg-compiler
   :name "post"
   :match (-rpartial #'porg-rule-output-that :type "note"
                     :predicate (-rpartial #'vulpea-note-tagged-all-p "post"))
   :hash #'porg-sha1sum
   :build (d12-make-publish :copy-fn #'d12-build-post)
   :clean #'d12-delete)

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
                                                                       (file-name-fix-attachment
                                                                        (s-chop-prefix "attachment:" image)
                                                                        "webp"))
                                                               items)))
                                      (unless image-item
                                        (user-error "%s is using %s as an image, but it does not exist"
                                                    (funcall #'porg-describe item) image))
                                      (unless (file-exists-p (porg-item-target-abs image-item))
                                        (user-error "Image %s does not exist" (funcall #'porg-describe image-item)))
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
                   (meta-file (file-name-replace-ext (porg-item-target-abs item) "json"))

                   (hash-b (porg-sha1sum (porg-item-target-abs item)))

                   (meta (if (functionp metadata) (funcall metadata item items) metadata))

                   (update (when (file-exists-p meta-file)
                             (with-current-buffer (find-file-noselect meta-file)
                               (goto-char (point-min))
                               (when (re-search-forward "update: \"\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)\"" nil t)
                                 (match-string 1)))))
                   (update (or (and (string-equal hash-a hash-b) update)
                               (format-time-string "%F")))

                   (date (plist-get meta "date"))
                   (update (or (and date (org-time< update date) date)
                               update))

                   (pinned (string-to-bool (or (vulpea-note-meta-get note "pinned") "false")))
                   (publish (string-to-bool (or (vulpea-note-meta-get note "publish") "false")))
                   (hide (string-to-bool (or (vulpea-note-meta-get note "hide") "false")))

                   (related (vulpea-note-meta-get-list note "related" 'link))

                   (event (when (vulpea-note-tagged-all-p note "event")
                            (let ((summary (d12-event-summary note)))
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
   :clean #'d12-delete)

  (porg-compiler
   :name "images"
   :match (-rpartial #'porg-rule-output-that :type "attachment" :predicate #'d12-supported-image-media-p)
   :hash #'d12-sha1sum-attachment
   :build
   (lambda (item _items _cache)
     (make-directory (file-name-directory (porg-item-target-abs item)) 'parents)
     (if (d12-convertible-image-p (porg-item-item item))
         (let ((max-width (or (plist-get (porg-item-extra-args item) :variant) 1600))
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
                      (porg-item-item item) (porg-item-target-abs item)))))
       (copy-file (porg-item-item item) (porg-item-target-abs item) t)))
   :clean #'d12-delete)

  (porg-compiler
   :name "videos"
   :match (-rpartial #'porg-rule-output-that :type "attachment" :predicate #'d12-supported-video-media-p)
   :hash #'d12-sha1sum-attachment
   :build
   (lambda (item _items _cache)
     (make-directory (file-name-directory (porg-item-target-abs item)) 'parents)
     (copy-file (porg-item-item item) (porg-item-target-abs item) t))
   :clean #'d12-delete)))



(provide 'build-rules)
;;; build-rules.el ends here
