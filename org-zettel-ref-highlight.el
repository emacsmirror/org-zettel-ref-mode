;;; org-zettel-ref-highlight-simple.el --- Simple highlighting with target links -*- lexical-binding: t; -*-

(require 'easymenu)

;;; Commentary:
;; Usage format:
;; <<hl-1>> §q{Highlighted text}  <- In the source file
;; ❓ Highlighted text <- Format displayed in the overview file

;;----------------------------------------------------------------------------  
;; Variables
;;----------------------------------------------------------------------------

(defun org-zettel-ref-highlight-type-to-char (type)
  "Convert highlight type to its single character identifier."
  (let ((config (cdr (assoc type org-zettel-ref-highlight-types))))
    (message "DEBUG: Converting type '%s' to char" type)
    (message "DEBUG: Config found: %S" config)
    (if config
        (let ((char (plist-get config :char)))
          (message "DEBUG: Found char: %s" char)
          char)
      (user-error "Unknown highlight type: %s" type))))

(defun org-zettel-ref-highlight-char-to-type (char)
  "Convert single character identifier to highlight type."
  (let ((found nil))
    (catch 'found
      (dolist (type-def org-zettel-ref-highlight-types)
        (when (string= (plist-get (cdr type-def) :char) char)
          (throw 'found (car type-def))))
      (user-error "Unknown highlight char: %s" char))))

(defcustom org-zettel-ref-highlight-types
  '(("question" . (:char "q"
                  :face (:background "#FFE0B2" :foreground "#000000" :extend t)
                  :name "question"
                  :prefix "❓"))
    ("fact" . (:char "f"
               :face (:background "#B2DFDB" :foreground "#000000" :extend t)
               :name "fact"
               :prefix "📝"))
    ("method" . (:char "m"
                :face (:background "#BBDEFB" :foreground "#000000" :extend t)
                :name "method"
                :prefix "🔧"))
    ("process" . (:char "p"
                 :face (:background "#E1BEE7" :foreground "#000000" :extend t)
                 :name "process"
                 :prefix "⛓️"))
    ("definition" . (:char "d"
                    :face (:background "#F8BBD0" :foreground "#000000" :extend t)
                    :name "definition"
                    :prefix "📖"))
    ("note" . (:char "n"
               :face (:background "#E8EAF6" :foreground "#000000" :extend t)
               :name "note"
               :prefix "✍️"))
    ("debate" . (:char "b"
                :face (:background "#FF8A80" :foreground "#000000" :extend t)
                :name "debate"
                :prefix "🙃"))
    ("future" . (:char "u"
                :face (:background "#FFB74D" :foreground "#000000" :extend t)
                :name "future"
                :prefix "🔮"))
    ("quote" . (:char "t"
                :face (:background "#C5CAE9" :foreground "#000000" :extend t)
                :name "quote"
                :prefix "💭"))
    ("image" . (:char "i"
                :face (:background "#FFECB3" :foreground "#000000" :extend t)
                :name "image"
                :prefix "🖼️")))
  "Configuration for highlight types.
Each type should have:
- :char    Single character identifier for the type
- :face    Face properties for highlighting
- :name    Display name of the type
- :prefix  Symbol to show in overview"
  :type '(alist :key-type string
                :value-type (plist :key-type symbol :value-type sexp))
  :group 'org-zettel-ref)


(defvar-local org-zettel-ref-highlight-counter 0
  "Global counter for highlight marks.")

(defvar-local org-zettel-ref-highlight--context-region nil
  "Cached (BEG END POINT) of the most recent active region for mouse menus.")

(defcustom org-zettel-ref-highlight-regexp
  "<<hl-\\([0-9]+\\)>> §\\([a-z]\\){\\([^}]+\\)}"
  "Regexp for matching highlight marks.
Group 1: Reference ID
Group 2: Type (single character identifier)
Group 3: Content (for images including path and description)"
  :type 'string
  :group 'org-zettel-ref)

(defcustom org-zettel-ref-highlight-hide-markers t
  "If non-nil, hide inline highlight markers visually while keeping overlays."
  :type 'boolean
  :group 'org-zettel-ref)

(defun org-zettel-ref--highlight-candidates ()
  "Return list of (DISPLAY . TYPE) with inline preview for completion."
  (let ((minibuffer-allow-text-properties t))
    (mapcar
     (lambda (type-def)
       (let* ((type (car type-def))
              (cfg (cdr type-def))
              (prefix (or (plist-get cfg :prefix) ""))
              (face (plist-get cfg :face))
              (display (format "%-12s %s"
                               (propertize type 'face face)
                               (propertize prefix 'face face))))
         (cons display type)))
     org-zettel-ref-highlight-types)))

(defun org-zettel-ref--highlight-id-at-point ()
  "Return highlight ID at point based on `org-zettel-ref-highlight-regexp`."
  (save-excursion
    (let ((pos (point)))
      (or
       (when (re-search-backward org-zettel-ref-highlight-regexp nil t)
         (when (and (>= pos (match-beginning 0))
                    (<= pos (match-end 0)))
           (match-string-no-properties 1)))
       (when (re-search-forward org-zettel-ref-highlight-regexp nil t)
         (when (and (>= pos (match-beginning 0))
                    (<= pos (match-end 0)))
           (match-string-no-properties 1)))))))

(defun org-zettel-ref--goto-overview-highlight (hl-id)
  "Open overview file and jump to highlight HL-ID. Return non-nil if handled."
  (let* ((overview-file (or (and (boundp 'org-zettel-ref-overview-file)
                                 org-zettel-ref-overview-file)
                            (when org-zettel-ref-use-single-overview-file
                              (expand-file-name org-zettel-ref-single-overview-file-path))))
         (msg-prefix (format "Highlight %s" hl-id)))
    (unless overview-file
      (message "%s: no overview file associated; run org-zettel-ref-init first." msg-prefix)
      (cl-return-from org-zettel-ref--goto-overview-highlight nil))
    (let* ((buf (find-file-noselect overview-file))
           (_ (pop-to-buffer buf)))
      (goto-char (point-min))
      (cond
       ((re-search-forward (format "\\[\\[hl:%s\\]" (regexp-quote hl-id)) nil t)
        (goto-char (match-beginning 0))
        (org-reveal)
        (message "%s opened in overview." msg-prefix)
        t)
       ((re-search-forward (format "hl-%s" (regexp-quote hl-id)) nil t)
        (goto-char (match-beginning 0))
        (org-reveal)
        (message "%s opened in overview." msg-prefix)
        t)
       (t
        (message "%s not found in overview %s" msg-prefix overview-file)
        nil)))))

(defun org-zettel-ref-open-highlight-at-point ()
  "Open the overview entry corresponding to the highlight at point.
Hook this into `org-open-at-point-functions` so `C-c C-o` works on hidden highlights."
  (interactive)
  (when-let ((hl-id (org-zettel-ref--highlight-id-at-point)))
    (org-zettel-ref--goto-overview-highlight hl-id)))

;;----------------------------------------------------------------------------
;; Highlight ID
;;----------------------------------------------------------------------------

(defun org-zettel-ref-highlight-ensure-counter ()
  "Ensure the highlight counter is properly initialized."
  (unless (and (boundp 'org-zettel-ref-highlight-counter)
               (numberp org-zettel-ref-highlight-counter))
    (make-local-variable 'org-zettel-ref-highlight-counter)
    (setq-local org-zettel-ref-highlight-counter 0)
    (org-zettel-ref-highlight-initialize-counter)))


(defun org-zettel-ref-highlight-generate-id ()
  "Generate the next highlight ID."
  (org-zettel-ref-highlight-ensure-counter)  
  (setq-local org-zettel-ref-highlight-counter 
              (1+ org-zettel-ref-highlight-counter))
  (number-to-string org-zettel-ref-highlight-counter))


;;----------------------------------------------------------------------------
;; Highlight Display
;;----------------------------------------------------------------------------

(defun org-zettel-ref-highlight-region (type)
  "Highlight the current region with the specified type TYPE."
  (interactive
   (let* ((cands (org-zettel-ref--highlight-candidates))
          (minibuffer-allow-text-properties t)
          (choice (completing-read "Highlight type: "
                                   (mapcar #'car cands)
                                   nil t)))
     (list (cdr (assoc choice cands)))))
  (unless (use-region-p)
    (user-error "Highlight requires an active region"))
  (message "Selected type: %s" type)
  (org-zettel-ref-highlight--apply-bounds
   type (region-beginning) (region-end) (point)))

(defun org-zettel-ref-highlight--apply-bounds (type beg end point-pos)
  "Core helper to highlight range between BEG and END using TYPE.
POINT-POS captures where `point' was when the region was selected so we
preserve the original inclusive semantics used by the interactive command."
  (let* ((text (buffer-substring-no-properties beg end))
         (highlight-id (org-zettel-ref-highlight-generate-id))
         (type-char (org-zettel-ref-highlight-type-to-char type)))
    (message "DEBUG: Using char '%s' for type '%s'" type-char type)
    (delete-region beg end)
    (goto-char beg)
    (let ((insert-text (format "<<hl-%s>> §%s{%s}"
                              highlight-id
                              type-char
                              text)))
      (message "DEBUG: Inserting: %s" insert-text)
      (insert insert-text))
    (run-with-idle-timer 0 nil #'org-zettel-ref-highlight-refresh)))

;;----------------------------------------------------------------------------
;; Context Menu Support
;;----------------------------------------------------------------------------

(defun org-zettel-ref-highlight-context-apply (type &optional _click)
  "Apply highlight TYPE on the current or most recent region.
CLICK is ignored aside from satisfying the context-menu calling
convention. When invoked interactively fallback to the regular prompt."
  (interactive
   (let* ((cands (org-zettel-ref--highlight-candidates))
          (choice (completing-read "Highlight type: "
                                   (mapcar #'car cands)
                                   nil t)))
     (list (cdr (assoc choice cands)))))
  (let ((bounds (cond
                 ((use-region-p)
                  (list (region-beginning) (region-end) (point)))
                 (org-zettel-ref-highlight--context-region
                  org-zettel-ref-highlight--context-region)
                 (t nil))))
    (unless bounds
      (user-error "Highlight requires an active region"))
    (org-zettel-ref-highlight--apply-bounds
     type (nth 0 bounds) (nth 1 bounds) (nth 2 bounds))))

(defun org-zettel-ref-highlight--context-menu-command-symbol (type)
  "Return a unique command symbol for TYPE.
Create the command on the fly if it is not defined yet."
  (let* ((type-str (if (symbolp type) (symbol-name type) type))
         (sanitized (replace-regexp-in-string "[^[:alnum:]-]+" "-" type-str))
         (sym-name (format "org-zettel-ref-highlight-context-%s" sanitized))
         (sym (intern sym-name)))
    (unless (fboundp sym)
      (fset sym
            (lambda (event)
              (interactive "e")
              (org-zettel-ref-highlight-context-apply type event))))
    sym))

(defun org-zettel-ref-highlight--context-menu-items (click)
  "Build menu entries to highlight the current region at CLICK."
  (mapcar
   (lambda (type-def)
     (let* ((type (car type-def))
            (config (cdr type-def))
            (name (or (plist-get config :name) type))
            (prefix (plist-get config :prefix))
            (label (if (and prefix (> (length prefix) 0))
                       (format "%s %s" prefix name)
                     name))
            (command (org-zettel-ref-highlight--context-menu-command-symbol type)))
       (vector label command :enable '(or (use-region-p)
                                          org-zettel-ref-highlight--context-region))))
   org-zettel-ref-highlight-types))

(defun org-zettel-ref-highlight-context-menu (_menu click)
  "Return a context menu for highlight commands triggered by CLICK."
  (when (derived-mode-p 'org-mode)
    (easy-menu-create-menu
     "Org Zettel Ref Highlight"
     (org-zettel-ref-highlight--context-menu-items click))))

(defun org-zettel-ref-highlight--record-context-region ()
  "Keep track of the most recent region for context menu workflows."
  (when (use-region-p)
    (setq org-zettel-ref-highlight--context-region
          (list (region-beginning) (region-end) (point)))))

(defun org-zettel-ref-highlight-refresh ()
  "Refresh the display of all highlights in the current buffer."
  (interactive)
  (remove-overlays (point-min) (point-max) 'org-zettel-ref-highlight t)
  
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward org-zettel-ref-highlight-regexp nil t)
      (let* ((type-char (match-string 2))
             (type (org-zettel-ref-highlight-char-to-type type-char))
             (text (match-string 3))
             (text-len (length text))
             (marker-start (match-beginning 0))
             (marker-end (match-end 0))
             (pre-start (max (point-min) (- marker-start text-len)))
             (pre-end (+ pre-start text-len))
             (text-start (match-beginning 3))
             (text-end (match-end 3))
             (use-pre (and (<= pre-start pre-end)
                           (<= pre-end marker-start)
                           (string= (buffer-substring-no-properties pre-start pre-end) text)))
             (visible-range (if use-pre
                                (cons pre-start pre-end)
                              (cons text-start text-end))))
        (let ((config (cdr (assoc type org-zettel-ref-highlight-types))))
          (when (and config (car visible-range) (cdr visible-range))
            ;; Overlay for the visible text (original region) with face only.
            (let ((text-ov (make-overlay (car visible-range) (cdr visible-range))))
              (overlay-put text-ov 'org-zettel-ref-highlight t)
              (overlay-put text-ov 'face (plist-get config :face)))
            ;; Overlay to hide marker/metadata while keeping the jump anchor.
            (when org-zettel-ref-highlight-hide-markers
              ;; Hide prefix: everything before the text payload.
              (when (< marker-start text-start)
                (let ((prefix-ov (make-overlay marker-start text-start)))
                  (overlay-put prefix-ov 'org-zettel-ref-highlight t)
                  (overlay-put prefix-ov 'org-zettel-ref-marker t)
                  (overlay-put prefix-ov 'display "")))
              ;; Hide suffix: everything after the text payload.
              (when (< text-end marker-end)
                (let ((suffix-ov (make-overlay text-end marker-end)))
                  (overlay-put suffix-ov 'org-zettel-ref-highlight t)
                  (overlay-put suffix-ov 'org-zettel-ref-marker t)
                  (overlay-put suffix-ov 'display ""))))))))))

(defun org-zettel-ref-toggle-target-display ()
  "Toggle whether to display target marks."
  (interactive)
  (setq org-zettel-ref-highlight-hide-markers
        (not org-zettel-ref-highlight-hide-markers))
  (org-zettel-ref-highlight-refresh)
  (message "Highlight markers are now %s"
           (if org-zettel-ref-highlight-hide-markers "hidden" "visible")))

;;----------------------------------------------------------------------------
;; Synchronization
;;----------------------------------------------------------------------------  

(defun org-zettel-ref-get-source-from-overview ()
  "Get the corresponding source file path from the current overview buffer."
  (let* ((db (org-zettel-ref-ensure-db))
         (overview-file (buffer-file-name))
         (overview-id (gethash overview-file (org-zettel-ref-db-overview-paths db))))
    (when overview-id
      (let* ((overview-entry (gethash overview-id (org-zettel-ref-db-overviews db)))
             (ref-id (org-zettel-ref-overview-entry-ref-id overview-entry))
             (ref-entry (org-zettel-ref-db-get-ref-entry db ref-id)))
        (org-zettel-ref-ref-entry-file-path ref-entry)))))

;;----------------------------------------------------------------------------
;; Image Handling
;;----------------------------------------------------------------------------  

(defun org-zettel-ref-highlight--check-init ()
  "Check if initialization is complete."
  (unless (and org-zettel-ref-overview-file
               (stringp org-zettel-ref-overview-file)
               (file-exists-p org-zettel-ref-overview-file))
    (user-error "Please run M-x org-zettel-ref-init to initialize the system")))

(defun org-zettel-ref-highlight--ensure-image-dir ()
  "Ensure the Images directory exists in the overview file's directory."
  (org-zettel-ref-highlight--check-init)  ; First check initialization
  (let* ((overview-dir (file-name-directory 
                       (expand-file-name org-zettel-ref-overview-file)))
         (image-dir (expand-file-name "Images" overview-dir)))
    (unless (file-exists-p image-dir)
      (make-directory image-dir t))
    image-dir))

(defun org-zettel-ref-highlight--copy-image (source-path)
  "Copy an image to the Images directory and return the new relative path."
  (let* ((image-dir (org-zettel-ref-highlight--ensure-image-dir))
         (file-name (file-name-nondirectory source-path))
         ;; Generate a unique filename (using timestamp)
         (new-name (format "%s-%s" 
                          (format-time-string "%Y%m%d-%H%M%S")
                          file-name))
         (dest-path (expand-file-name new-name image-dir)))
    (copy-file source-path dest-path t)
    ;; Return the path relative to the overview file
    (concat "Images/" new-name)))

(defun org-zettel-ref-add-image ()
  "Add a highlight mark to the image at the current position and copy it to the Images directory."
  (interactive)
  (org-zettel-ref-highlight--check-init)
  (save-excursion
    (let ((context (org-element-context)))
      (when (and (eq (org-element-type context) 'link)
                 (string= (org-element-property :type context) "file"))
        (let* ((path (org-element-property :path context))
               (abs-path (expand-file-name path (file-name-directory (buffer-file-name))))
               (link-end (org-element-property :end context))
               (description (read-string "Image description (optional): ")))
          (when (and (string-match-p "\\.\\(jpg\\|jpeg\\|png\\|gif\\|svg\\|webp\\)$" path)
                    (file-exists-p abs-path))
            ;; Copy the image to the Images directory
            (let ((new-path (org-zettel-ref-highlight--copy-image abs-path)))
              ;; Move to the end of the line containing the link and insert a newline
              (goto-char link-end)
              (end-of-line)
              (insert "\n")
              ;; Add the highlight mark on the new line
              (let ((highlight-id (org-zettel-ref-highlight-generate-id)))
                (insert (format "<<hl-%s>> §i{%s|%s}"
                              highlight-id
                              new-path
                              (or description "")))
                (org-zettel-ref-highlight-refresh)))))))))

;;----------------------------------------------------------------------------
;; Highlight Editing
;;----------------------------------------------------------------------------

;; Constants
(defconst org-zettel-ref-highlight-threshold 100
  "Threshold for number of highlights to consider a file as large.")

(defun org-zettel-ref-count-highlights ()
  "Count total number of highlights in current buffer."
  (save-excursion
    (save-match-data
      (let ((count 0))
        (goto-char (point-min))
        (while (re-search-forward org-zettel-ref-highlight-regexp nil t)
          (setq count (1+ count)))
        count))))

(defun org-zettel-ref-renumber-highlights-after-point (start-number)
  "Renumber all highlights after START-NUMBER."
  (save-excursion
    (save-match-data
      (let* ((total-highlights (org-zettel-ref-count-highlights))
             (is-large-file (> total-highlights org-zettel-ref-highlight-threshold))
             (processed 0)
             (new-number start-number))
        
        (message "Buffer size: %d" (buffer-size)) ;; Debug info
        ;; Move to the beginning of the buffer
        (goto-char (point-min))
        ;; Find and renumber all highlights
        (while (re-search-forward org-zettel-ref-highlight-regexp nil t)
          (let* ((current-number (string-to-number (match-string 1)))  ; Get the number using group 1
                 (type-char (match-string 2))                          ; Get the type using group 2
                 (text (match-string 3)))                              ; Get the text using group 3
            
            (when (>= current-number start-number)
              ;; Replace only the number part, keep the format unchanged
              (goto-char (match-beginning 1))
              (delete-region (match-beginning 1) (match-end 1))
              (insert (number-to-string new-number))
              (setq new-number (1+ new-number)))))
        
        ;; Update the counter
        (setq-local org-zettel-ref-highlight-counter (1- new-number))))))

(defun org-zettel-ref-remove-marked ()
  "Remove the highlight mark at the cursor and renumber subsequent highlights."
  (interactive)
  (let ((pos (point))
        (found nil))
    (save-excursion
      ;; Find the highlight mark on the current line
      (beginning-of-line)
      (when (re-search-forward org-zettel-ref-highlight-regexp (line-end-position) t)
        (let* ((target-start (match-beginning 0))
               (target-end (match-end 0))
               (highlight-id (match-string 1))    ; Get the number using group 1
               (type-char (match-string 2))       ; Get the type using group 2
               (text (match-string 3))            ; Get the text using group 3
               (current-number (string-to-number highlight-id)))
          (setq found t)
          ;; Confirm deletion
          (when (y-or-n-p "Remove highlight mark? ")
            ;; Delete the mark and insert original text
            (delete-region target-start target-end)
            (goto-char target-start)
            (insert (propertize text 'face 'org-zettel-ref-highlight-face))
            ;; Renumber subsequent highlights
            (org-zettel-ref-renumber-highlights-after-point current-number)
            ;; Synchronize the overview file
            (org-zettel-ref-sync-highlights)))))
    ;; Message outside save-excursion
    (unless found
      (message "No highlight mark found at point"))))

;; Edit highlighted text
(defun org-zettel-ref-edit-highlight ()
  "Edit the highlighted text under the cursor."
  (interactive)
  (save-excursion
    (when (org-zettel-ref-highlight-at-point)
      (let* ((bounds (org-zettel-ref-highlight-get-bounds))
             (old-text (org-zettel-ref-highlight-get-text bounds))
             (type (org-zettel-ref-highlight-get-type bounds))
             (ref (org-zettel-ref-highlight-get-ref bounds))
             (new-text (read-string "Edit highlighted text: " old-text)))
        (unless (string= old-text new-text)
          (save-excursion
            (goto-char (car bounds))
            (delete-region (car bounds) (cdr bounds))
            (insert (format "<<hl-%s>> §%s{%s}"
                          ref type new-text))
            (org-zettel-ref-highlight-refresh)
            (org-zettel-ref-sync-highlights)))))))

(defun org-zettel-ref-edit-note ()
  "Edit the content of the current note."
  (interactive)
  (when (org-zettel-ref-highlight-at-point)
    (let* ((bounds (org-zettel-ref-highlight-get-bounds))
           (ref (org-zettel-ref-highlight-get-ref bounds))
           (type (org-zettel-ref-highlight-get-type bounds))
           (old-text (org-zettel-ref-highlight-get-text bounds)))
      (when (string= type "n")  ; Ensure it's a note type
        (let ((new-text (read-string "Edit note: " old-text)))
          (unless (string= old-text new-text)
            (save-excursion
              (goto-char (car bounds))
              (delete-region (car bounds) (cdr bounds))
              (insert (format "<<hl-%s>> §n{%s}"
                            ref new-text))
              (org-zettel-ref-highlight-refresh)
              (org-zettel-ref-sync-highlights))))))))

;;----------------------------------------------------------------------------
;; Helper Functions
;;----------------------------------------------------------------------------

(defun org-zettel-ref-highlight-at-point ()
  "Check if the cursor is within a highlight region."
  (save-excursion
    (let ((pos (point)))
      (beginning-of-line)
      (when (re-search-forward org-zettel-ref-highlight-regexp (line-end-position) t)
        (and (>= pos (match-beginning 0))
             (<= pos (match-end 0)))))))

(defun org-zettel-ref-highlight-get-bounds ()
  "Get the start and end positions of the current highlight."
  (save-excursion
    (beginning-of-line)
    (when (re-search-forward org-zettel-ref-highlight-regexp (line-end-position) t)
      (cons (match-beginning 0) (match-end 0)))))

(defun org-zettel-ref-highlight-get-text (bounds)
  "Get the highlighted text within the specified range."
  (save-excursion
    (goto-char (car bounds))
    (when (re-search-forward org-zettel-ref-highlight-regexp (cdr bounds) t)
      (match-string 3))))

(defun org-zettel-ref-highlight-get-type (bounds)
  "Get the highlighted type within the specified range."
  (save-excursion
    (goto-char (car bounds))
    (when (re-search-forward org-zettel-ref-highlight-regexp (cdr bounds) t)
      (match-string 2))))

(defun org-zettel-ref-highlight-get-ref (bounds)
  "Get the highlighted reference number within the specified range."
  (save-excursion
    (goto-char (car bounds))
    (when (re-search-forward org-zettel-ref-highlight-regexp (cdr bounds) t)
      (match-string 1))))

(defun org-zettel-ref-highlight-initialize-counter ()
  "Scan all highlight marks in the current buffer and initialize the counter to the maximum value."
  (save-excursion
    (goto-char (point-min))
    (let ((max-id 0))
      ;; Scan all highlight marks
      (while (re-search-forward org-zettel-ref-highlight-regexp nil t)
        (when-let* ((id-str (match-string 1))
                    (id-num (string-to-number id-str)))
          (setq max-id (max max-id id-num))))
      ;; Set the counter to the maximum value found
      (setq-local org-zettel-ref-highlight-counter max-id))))

(defun org-zettel-ref-follow-link-and-highlight ()
  "Jump to the link target and highlight it."
  (let* ((link-prop (org-element-context))
         (target-file (org-element-property :path link-prop))
         (target-id (org-element-property :search-option link-prop)))
    (when (and target-file target-id)
      (find-file target-file)
      (goto-char (point-min))
      (when (re-search-forward (concat "<<" target-id ">>") nil t)
        (org-show-context)
        (recenter)))))

(defun org-zettel-ref-highlight-enable ()
  "Enable highlight mode and initialize the counter."
  ;; Ensure the buffer-local variable is set
  (make-local-variable 'org-zettel-ref-highlight-counter)
  ;; Initialize the counter
  (org-zettel-ref-highlight-initialize-counter)
  ;; Refresh display
  (org-zettel-ref-highlight-refresh))


(defun org-zettel-ref-highlight-debug-counter ()
  "Display the highlight counter status of the current buffer."
  (interactive)
  (let ((current-counter org-zettel-ref-highlight-counter)
        (max-found (org-zettel-ref-highlight-initialize-counter)))
    (org-zettel-ref-debug-message-category 'highlight
      "Current counter: %d, Maximum found in buffer: %d"
      current-counter max-found)))

(defun org-zettel-ref-highlight-debug-info ()
  "Display the highlight debugging information of the current buffer."
  (interactive)
  (org-zettel-ref-debug-message-category 'highlight
    "Current counter value: %s" org-zettel-ref-highlight-counter)
  (save-excursion
    (goto-char (point-min))
    (let ((count 0))
      (while (re-search-forward org-zettel-ref-highlight-regexp nil t)
        (cl-incf count)
        (org-zettel-ref-debug-message-category 'highlight
          "Found highlight #%d: %s" count (match-string 0)))
      (org-zettel-ref-debug-message-category 'highlight
        "Total found %d highlight marks" count))))

(defun org-zettel-ref-highlight-add-note ()
  "Add a standalone note, using the highlight system's ID counter."
  (interactive)
  (let* ((note-text (read-string "Insert note: "))
         (highlight-id (org-zettel-ref-highlight-generate-id)))
    (insert (format "<<hl-%s>> §n{%s}"
                   highlight-id
                   note-text))
    (org-zettel-ref-highlight-refresh)))

;; Modify after-change processing function
(defun org-zettel-ref-highlight-after-change (beg end _len)
  "Handle highlight updates after text changes."
  (save-excursion
    (goto-char beg)
    (let ((line-beg (line-beginning-position))
          (line-end (line-end-position)))
      (when (and (>= end line-beg)
                 (<= beg line-end)
                 (string-match org-zettel-ref-highlight-regexp
                             (buffer-substring-no-properties line-beg line-end)))
        ;; Refresh display
        (org-zettel-ref-highlight-refresh)
        ;; Synchronize to overview
        (when (and (boundp 'org-zettel-ref-overview-file)
                  org-zettel-ref-overview-file)
          (org-zettel-ref-sync-highlights))))))

(defun org-zettel-ref-highlight-debug-config ()
  "Display current highlight type configurations."
  (interactive)
  (message "Current highlight types:")
  (dolist (type-def org-zettel-ref-highlight-types)
    (let* ((type (car type-def))
           (config (cdr type-def))
           (char (plist-get config :char))
           (face (plist-get config :face))
           (name (plist-get config :name))
           (prefix (plist-get config :prefix)))
      (message "Type: %s\n  char: %s\n  face: %s\n  name: %s\n  prefix: %s"
               type char face name prefix))))


(defun org-zettel-ref-highlight-setup ()
  "Setup highlight system."
  (interactive)
  ;; 确保变量是 buffer-local
  (make-local-variable 'org-zettel-ref-highlight-counter)
  ;; 验证配置
  (unless (org-zettel-ref-highlight-validate-types)
    (org-zettel-ref-debug-message-category 'highlight 
      "Warning: Invalid highlight types configuration"))
  ;; 初始化计数器
  (org-zettel-ref-highlight-initialize-counter)
  ;; 刷新显示
  (org-zettel-ref-highlight-refresh)
  ;; Enable opening hidden highlights via org-open-at-point
  (add-hook 'org-open-at-point-functions
            #'org-zettel-ref-open-highlight-at-point
            nil t)
  ;; Provide right-click menu entries when available
  (when (boundp 'context-menu-functions)
    (add-hook 'context-menu-functions
              #'org-zettel-ref-highlight-context-menu
              nil t))
  (add-hook 'post-command-hook
            #'org-zettel-ref-highlight--record-context-region
            nil t)
  ;; 显示当前配置状态
  (org-zettel-ref-debug-message-category 'highlight 
    "Highlight system setup complete. Use M-x org-zettel-ref-highlight-debug-config to check configuration."))

;; 在初始化时设置高亮
(defun org-zettel-ref--setup-highlight (buffer)
  "Setup highlight for BUFFER."
  (with-current-buffer buffer
    (org-zettel-ref-highlight-setup)))

(defun org-zettel-ref-highlight-validate-types ()
  "Validate highlight types configuration."
  (let ((chars (make-hash-table :test 'equal))
        (valid t))
    (dolist (type-def org-zettel-ref-highlight-types)
      (let* ((type (car type-def))
             (config (cdr type-def))
             (char (plist-get config :char)))
        ;; Check required properties
        (unless (and (plist-get config :char)
                    (plist-get config :face)
                    (plist-get config :name)
                    (plist-get config :prefix))
          (message "Warning: Type %s missing required properties" type)
          (setq valid nil))
        ;; Check for duplicate chars
        (when (gethash char chars)
          (message "Warning: Duplicate character identifier %s" char)
          (setq valid nil))
        (puthash char type chars)))
    valid))

;; When highlight system is initialized, validate configuration.
(defun org-zettel-ref-highlight-initialize ()
  "Initialize highlight system and validate configuration."
  (unless (org-zettel-ref-highlight-validate-types)
    (message "Warning: Invalid highlight types configuration")))

(add-hook 'after-init-hook #'org-zettel-ref-highlight-initialize)

(defun org-zettel-ref-reset-org-element-cache ()
  "Reset the org-element cache for the current buffer."
  (interactive)
  (when (fboundp 'org-element-cache-reset)
    (org-element-cache-reset)
    (message "Org element cache has been reset for current buffer.")))

(defun org-zettel-ref-ensure-org-element-cache ()
  "Ensure the org-element cache is in a good state."
  (condition-case err
      (progn
        (when (and (boundp 'org-element-use-cache)
                   org-element-use-cache)
          (org-element-cache-reset)))
    (error
     (message "Error resetting org-element cache: %s" (error-message-string err))
     (when (boundp 'org-element-use-cache)
       (setq-local org-element-use-cache nil)))))

(defun org-zettel-ref-jump-to-source-highlight-from-overview (path &optional data)
  "Jump from a highlight entry in the overview file to the corresponding highlight in the source file.
PATH is the highlight ID (e.g., \"1\", \"2\").
DATA is the link data (optional, provided by org-mode).
This command should be called when the point is on a highlight entry's headline or list item in the overview file."
  (if (and org-zettel-ref-use-single-overview-file
           (string= (buffer-file-name (current-buffer)) (expand-file-name org-zettel-ref-single-overview-file-path)))
      ;; --- Single-File Mode ---
      (save-excursion
        (let (source-ref-id)
          ;; The 'path' argument is our original-hl-id
          (let ((original-hl-id path))
            ;; Find the parent heading to get the SOURCE_REF_ID
            ;; This logic is still needed as source-ref-id is not part of the 'path' argument
            (org-back-to-heading t)
            (let ((parent-props (org-entry-properties)))
              (setq source-ref-id (cdr (assoc "REF_ID" parent-props))))

            (message "DEBUG: Jump command - Source_Ref_ID: %s, Original_HL_ID: %s" source-ref-id original-hl-id)

            (unless source-ref-id
              (user-error "Could not find source reference ID for this entry."))
            (unless original-hl-id
              (user-error "Could not find highlight ID for this entry."))

            (let* ((db (org-zettel-ref-ensure-db))
                   (ref-entry (org-zettel-ref-db-get-ref-entry db source-ref-id))
                   (source-file-path (when ref-entry (org-zettel-ref-ref-entry-file-path ref-entry))))
              (if source-file-path
                  (progn
                    (message
                     "DEBUG: Jumping to source file: %s" source-file-path)
                    (let ((source-buffer (find-file-other-window source-file-path)))
                      (with-selected-window (get-buffer-window source-buffer)
                        (with-current-buffer source-buffer
                          (widen)
                          (goto-char (point-min))
                          (let ((target-mark (concat "<<hl-" original-hl-id ">>")))
                            (if (re-search-forward (regexp-quote target-mark) nil t)
                                (progn
                                  (goto-char (match-beginning 0))
                                  (recenter-top-bottom)
                                  (message "Jumped to %s in %s" target-mark (file-name-nondirectory source-file-path)))
                              (user-error "Target mark %s not found in %s" target-mark source-file-path)))))))
                (user-error "Could not find source file for REF_ID: %s" source-ref-id))))))
    ;; --- Multi-File Mode ---
    ;; When not in single-file mode, find the source file from the current overview file.
    (let* ((db (org-zettel-ref-ensure-db))
           (overview-file-path (buffer-file-name (current-buffer)))
           (overview-id (when overview-file-path
                         (gethash overview-file-path (org-zettel-ref-db-overview-paths db))))
           (overview-entry (when overview-id
                           (gethash overview-id (org-zettel-ref-db-overviews db))))
           (ref-id (when overview-entry
                    (org-zettel-ref-overview-entry-ref-id overview-entry)))
           (ref-entry (when ref-id
                       (org-zettel-ref-db-get-ref-entry db ref-id))))
      (if ref-entry
          (let ((source-file-path (org-zettel-ref-ref-entry-file-path ref-entry)))
            (if (file-exists-p source-file-path)
                (progn
                  (find-file source-file-path)
                  (with-current-buffer (find-file-noselect source-file-path)
                    (goto-char (point-min))
                    (if (re-search-forward (format "<<hl-%s>>" path) nil t)
                        (goto-char (match-beginning 0))
                      (message "Highlight %s not found in %s" path (file-name-nondirectory source-file-path)))))
              (message "Source file does not exist: %s" source-file-path)))
        (message "Could not find a source file linked to this overview file. DB path: %s"
                 (org-zettel-ref-db-get-db-file-path db))))))

(provide 'org-zettel-ref-highlight)
