;;; context-navigator-add-paths.el --- Add files by names/paths from text/minibuffer -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Minimal functional implementation of the spec:
;; - Extract path-like tokens from arbitrary text
;; - Resolve to project files (with index/TTL cache) or absolute/relative existing paths
;; - Apply filters (size, non-regular, symlink), TRAMP confirmation, count limit
;; - Abort on ambiguities, report unresolved; add resulting files to model and optionally push to gptel
;;
;; Small pure helpers, isolated side effects.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'rx)
(require 'project) ;; project-current / project-files
(require 'context-navigator-core)
(require 'context-navigator-model)
(require 'context-navigator-events)
(require 'context-navigator-i18n)
(require 'context-navigator-ui)
(require 'context-navigator-project)
(require 'context-navigator-util)

(defgroup context-navigator-path-add nil
  "Settings for adding files from names/paths."
  :group 'context-navigator)

(defcustom context-navigator-path-add-limit 70
  "Maximum number of files to add in a single operation."
  :type 'integer :group 'context-navigator-path-add)

(defcustom context-navigator-path-add-index-cache-ttl 30.0
  "TTL (seconds) for project file index cache."
  :type 'number :group 'context-navigator-path-add)

(defcustom context-navigator-path-add-case-sensitive 'on
  "Case sensitivity policy for basename matching: auto|on|off."
  :type '(choice (const auto) (const on) (const off))
  :group 'context-navigator-path-add)

(defcustom context-navigator-path-add-ignore-gitignored t
  "Prefer sources that respect .gitignore (project.el/git)."
  :type 'boolean :group 'context-navigator-path-add)

(defcustom context-navigator-path-add-exclude-dotdirs t
  "Exclude dot-directories (.*) from fallback recursion."
  :type 'boolean :group 'context-navigator-path-add)

(defcustom context-navigator-path-add-fallback-exclude
  '("node_modules" "dist" "build" "target")
  "Directory names to exclude in fallback recursion."
  :type '(repeat string) :group 'context-navigator-path-add)

(defcustom context-navigator-path-add-fallback-enabled nil
  "When non-nil, allow fallback recursive scan if project.el and git index are unavailable.
Set to nil to completely disable fallback scanning (default)."
  :type 'boolean :group 'context-navigator-path-add)

(defcustom context-navigator-path-add-fallback-max-files 50000
  "Hard cap on number of files collected by fallback recursion before aborting."
  :type 'integer :group 'context-navigator-path-add)

(defcustom context-navigator-path-add-fallback-max-depth 12
  "Maximum directory recursion depth for fallback traversal."
  :type 'integer :group 'context-navigator-path-add)

(defcustom context-navigator-path-add-fallback-top-level-max 8000
  "Abort fallback immediately when the top-level directory has more than this many entries."
  :type 'integer :group 'context-navigator-path-add)

(defcustom context-navigator-path-add-fallback-time-budget 2.5
  "Soft time budget in seconds for fallback traversal; abort when exceeded."
  :type 'number :group 'context-navigator-path-add)

;; Masks/globs controls (v1)
(defcustom context-navigator-mask-include-dotfiles nil
  "When non-nil, include dotfiles even if pattern components do not start with a dot.
Default behavior (nil) hides dotfiles unless the component explicitly begins with '.'."
  :type 'boolean :group 'context-navigator-path-add)

(defcustom context-navigator-mask-enable-remote nil
  "Allow TRAMP mask expansion when non-nil (disabled by default due to performance)."
  :type 'boolean :group 'context-navigator-path-add)

(defcustom context-navigator-mask-globstar t
  "Enable ** globstar matching (0+ intermediate directories)."
  :type 'boolean :group 'context-navigator-path-add)

;; Reuse max file size if defined by transient module; declare to avoid require.
(defvar context-navigator-max-file-size (* 1 1024 1024)
  "Maximum file size (bytes) to include when adding files (fallback default).")

;; -----------------------------------------------------------------------------
;; Small helpers (pure where possible)

(defun context-navigator-path-add--now () (float-time))

(defun context-navigator-path-add--string-trim-quotes (s)
  "Strip surrounding quotes or angle brackets from S."
  (let* ((len (length s)))
    (cond
     ((and (>= len 2)
           (or (and (eq (aref s 0) ?\") (eq (aref s (1- len)) ?\"))
               (and (eq (aref s 0) ?\') (eq (aref s (1- len)) ?\'))
               (and (eq (aref s 0) ?<)  (eq (aref s (1- len)) ?>))))
      (substring s 1 (1- len)))
     (t s))))

(defun context-navigator-path-add--looks-like-url-p (s)
  "Return non-nil when S looks like a URL with scheme://."
  (string-match-p "^[[:alnum:]+.-]+://." s))

(defun context-navigator-path-add--strip-position-suffix (s)
  "Strip trailing :N or :A-B chains (e.g., :492:64) from S when appropriate.
Does not strip Windows drive letters like C:."
  (let ((q s)
        (continue t))
    ;; Repeatedly strip a trailing :N or :A-B (optionally followed by a colon),
    ;; but stop if doing so would leave just a drive like \"C:\".
    (while (and continue
                (string-match "\\(:[0-9]+\\(?:-[0-9]+\\)?\\)\\(?::?\\)\\'" q))
      (let ((prefix (substring q 0 (match-beginning 0))))
        (if (string-match-p "\\=[A-Za-z]:\\'" prefix)
            (setq continue nil) ;; keep as-is (drive letter), stop stripping
          (setq q prefix))))
    q))


(defun context-navigator-path-add--normalize-token (raw)
  "Normalize RAW path-like token or return nil when not acceptable.

Steps:
- trim whitespace
- strip surrounding quotes/angles/brackets/parentheses
- strip trailing position suffix :N or :A-B (but not drive C:)
- strip trailing annotation blocks in parentheses at the end
- strip trailing punctuation that cannot be part of filename (,.;:)]}\"»)"
  (let* ((s (and (stringp raw) (string-trim raw))))
    (when (and (stringp s) (not (string-empty-p s)))
      (let* ((strip1 (context-navigator-path-add--string-trim-quotes s))
             ;; Also strip () [] {} and «» if wrapped
             (strip2 (let ((q strip1))
                       (cond
                        ((and (>= (length q) 2)
                              (or (and (eq (aref q 0) ?\() (eq (aref q (1- (length q))) ?\)))
                                  (and (eq (aref q 0) ?\[) (eq (aref q (1- (length q))) ?\]))
                                  (and (eq (aref q 0) ?\{) (eq (aref q (1- (length q))) ?\}))
                                  (and (eq (aref q 0) ?«)  (eq (aref q (1- (length q))) ?»))))
                         (substring q 1 (1- (length q))))
                        (t q)))))
        ;; URLs are rejected early without using cl-return-from (avoid no-catch in batch)
        (if (context-navigator-path-add--looks-like-url-p strip2)
            nil
          (let* (;; Remove :N or :A-B position suffixes (keep C: drive)
                 (no-pos (context-navigator-path-add--strip-position-suffix strip2))
                 ;; Strip trailing annotation blocks in parentheses e.g. " (…)" or "（…）"
                 (no-parens
                  (let ((q no-pos))
                    ;; Remove one or more trailing parenthesized annotation blocks with optional spaces
                    (while (string-match "[ \t]*\\(([^)]*)\\|（[^）]*）\\)\\s-*\\'" q)
                      (setq q (replace-match "" t t q)))
                    ;; If a stray opening bracket remains at the end, drop it too
                    (setq q (replace-regexp-in-string "[ \t]*[({（]\\s-*\\'" "" q))
                    q))
                 ;; Collapse runs of backslashes inside token, but keep leading UNC (\\) as exactly two
                 (no-esc
                  (let ((q no-parens))
                    (let ((m (string-match "\\`\\\\+" q)))
                      (if (and m (>= (- (match-end 0) (match-beginning 0)) 2))
                          (let* ((run-end (match-end 0))
                                 (rest (substring q run-end))
                                 (rest1 (replace-regexp-in-string "\\\\\\\\+" "\\\\" rest)))
                            (concat "\\\\" rest1))
                        (replace-regexp-in-string "\\\\\\\\+" "\\\\" q)))))
                 ;; Strip trailing punctuation like ",.;:)]}\"»" if present, repeatedly
                 ;; Guard: keep bare Windows drive like "C:" intact.
                 (trim-tails
                  (let ((q no-esc)
                        (tails '("," "." ";" ":" ")" "]" "}" "”" "»")))
                    (let ((drive-only (string-match-p "\\`[A-Za-z]:\\'" q)))
                      (while (and (stringp q)
                                  (> (length q) 0)
                                  (let* ((last (substring q (1- (length q)))))
                                    (and (member last tails)
                                         (not (and drive-only (string= last ":"))))))
                        (setq q (substring q 0 (1- (length q))))))
                    q)))
            (string-trim trim-tails)))))))

(defun context-navigator-path-add--case-fold-p ()
  "Return non-nil when basename match should be case-insensitive."
  (pcase context-navigator-path-add-case-sensitive
    ('on nil)
    ('off t)
    (_ ;; auto
     (memq system-type '(windows-nt ms-dos cygwin)))))

(defun context-navigator-path-add--basename= (a b)
  "Compare basenames A and B according to case policy."
  (if (context-navigator-path-add--case-fold-p)
      (string-equal (downcase a) (downcase b))
    (string-equal a b)))



(defun context-navigator-path-add--has-dirsep-p (s)
  "Return non-nil when S contains a directory separator (/ or \\)."
  (string-match-p "[/\\]" s))

(defun context-navigator-path-add--has-extension-p (s)
  "Return non-nil when S has a filename extension (basename has a dot suffix)."
  (let ((ext (file-name-extension (file-name-nondirectory s))))
    (and (stringp ext) (not (string-empty-p ext)))))

(defun context-navigator-path-add--has-double-sep-p (s)
  "Return non-nil when S contains a double directory separator (// or \\\\)."
  (and (stringp s)
       (string-match-p "\\(//\\|\\\\\\\\\\)" s)))

(defun context-navigator-path-add--regular-file-p (p)
  (and (stringp p) (file-exists-p p) (file-regular-p p)))

(defun context-navigator-path-add--symlink-p (p)
  (condition-case _ (file-symlink-p p) (error nil)))

(defun context-navigator-path-add--file-size (p)
  (when (and (stringp p) (file-exists-p p))
    (let ((a (file-attributes p 'string)))
      (and a (file-attribute-size a)))))

(defun context-navigator-path-add--project-root ()
  "Resolve project root using core state first, then project module fallbacks, else default-directory."
  (let* ((st (ignore-errors (context-navigator--state-get)))
         (last-root (and st (context-navigator-state-last-project-root st)))
         (root (or last-root
                   ;; Prefer project module (handles org/gptel/dired via default-directory),
                   ;; and fallback to any file-visiting window on current frame.
                   (ignore-errors
                     (or (context-navigator-project-root (current-buffer))
                         (context-navigator-project--frame-file-project-root)))
                   ;; Finally, naive project.el detection
                   (ignore-errors
                     (let ((proj (project-current nil)))
                       (when proj (expand-file-name (project-root proj))))))))
    (or root default-directory)))

;; -----------------------------------------------------------------------------
;; Index cache

(defvar context-navigator-path-add--index-cache (make-hash-table :test 'equal)
  "Hash: root -> (timestamp . files) where files is list of absolute paths.")

(defun context-navigator-path-add--cache-get (root)
  (let ((cell (gethash root context-navigator-path-add--index-cache)))
    (when (and cell
               (numberp (car cell))
               (< (- (context-navigator-path-add--now) (car cell))
                  (max 0.0 context-navigator-path-add-index-cache-ttl)))
      (cdr cell))))

(defun context-navigator-path-add--cache-put (root files)
  (puthash root (cons (context-navigator-path-add--now) files)
           context-navigator-path-add--index-cache)
  files)

(defun context-navigator-path-add--cache-reset (&rest _)
  "Clear the whole index cache."
  (clrhash context-navigator-path-add--index-cache))

;; Reset cache on project/group switches
(context-navigator-events-subscribe :project-switch #'context-navigator-path-add--cache-reset)
(context-navigator-events-subscribe :group-switch-start #'context-navigator-path-add--cache-reset)

;; -----------------------------------------------------------------------------
;; Project file index (pure wrt returned list)

(defun context-navigator-path-add--project-files-project-el (root)
  "Use project.el to list project files, return absolute paths or nil."
  (when (and (fboundp 'project-current) (fboundp 'project-files))
    (let* ((default-directory root)
           (proj (ignore-errors (project-current nil))))
      (when proj
        (let ((lst (ignore-errors (project-files proj))))
          (when (listp lst)
            (mapcar (lambda (p) (expand-file-name p root)) lst)))))))

(defun context-navigator-path-add--project-files-git (root)
  "Use git ls-files to list repository files, return absolute paths or nil."
  (when (and (executable-find "git")
             (file-directory-p (expand-file-name ".git" root)))
    (let* ((default-directory root)
           (buf (generate-new-buffer " *cn-git-ls*"))
           (code (call-process "git" nil buf nil
                               "ls-files" "-co" "--exclude-standard")))
      (unwind-protect
          (when (and (integerp code) (= code 0))
            (with-current-buffer buf
              (goto-char (point-min))
              (let (acc)
                (while (not (eobp))
                  (let ((line (string-trim (buffer-substring-no-properties
                                            (line-beginning-position)
                                            (line-end-position)))))
                    (when (not (string-empty-p line))
                      (push (expand-file-name line root) acc)))
                  (forward-line 1))
                (nreverse acc))))
        (kill-buffer buf)))))

(defun context-navigator-path-add--fallback-dir-p-allowed (dir)
  "Return non-nil when DIR (basename) is allowed in fallback traversal."
  (let* ((bn (file-name-nondirectory (directory-file-name dir))))
    (and
     (or (not context-navigator-path-add-exclude-dotdirs)
         (not (string-prefix-p "." bn)))
     (not (member bn context-navigator-path-add-fallback-exclude)))))

(defun context-navigator-path-add--unsafe-root-p (root)
  "Return non-nil when ROOT is an unsafe filesystem root for fallback scanning."
  (let* ((r (file-name-as-directory (expand-file-name root))))
    (or (string= r "/")
        (string-match-p "\\=[A-Za-z]:/\\'" r))))

(defun context-navigator-path-add--project-files-fallback (root)
  "Recursive directory listing (filtered) with safety limits. Return absolute files.

Respects:
- =context-navigator-path-add-fallback-enabled'
- =context-navigator-path-add-fallback-top-level-max'
- =context-navigator-path-add-fallback-max-files'
- =context-navigator-path-add-fallback-max-depth'
- =context-navigator-path-add-fallback-time-budget'

Skips .git, dot-dirs when configured, and symlinked directories."
  (let* ((root (directory-file-name (expand-file-name root))))
    (if (or (not context-navigator-path-add-fallback-enabled)
            (context-navigator-path-add--unsafe-root-p root)
            (let* ((top (ignore-errors (directory-files root nil nil t)))
                   (n (and (listp top) (length top))))
              (and (integerp n)
                   (> n (or context-navigator-path-add-fallback-top-level-max 8000)))))
        '()
      (let* ((max-files (or context-navigator-path-add-fallback-max-files 50000))
             (max-depth (or context-navigator-path-add-fallback-max-depth 12))
             (budget   (or context-navigator-path-add-fallback-time-budget 0))
             (start    (float-time))
             (res '())
             (count 0))
        (cl-labels
            ((recurse (dir depth)
               (when (<= depth max-depth)
                 (dolist (entry (directory-files dir t nil t))
                   (let ((bn (file-name-nondirectory entry)))
                     (cond
                      ((member bn '("." ".." ".git"))) ;; skip
                      ((file-directory-p entry)
                       (when (and (context-navigator-path-add--fallback-dir-p-allowed entry)
                                  (not (file-symlink-p entry)))
                         (recurse entry (1+ depth))))
                      ((file-regular-p entry)
                       (push entry res)
                       (setq count (1+ count))
                       (when (>= count max-files)
                         (throw 'cn-fallback-limit t)))))
                   (when (and (numberp budget) (> budget 0)
                              (> (- (float-time) start) budget))
                     (throw 'cn-fallback-limit t))))))
          (catch 'cn-fallback-limit
            (recurse root 0))
          res)))))

(defun context-navigator-project-file-index (root)
  "Return list of absolute file paths for ROOT, with TTL cache."
  (let* ((abs-root (directory-file-name (expand-file-name root)))
         (remote (file-remote-p abs-root)))
    (or (context-navigator-path-add--cache-get abs-root)
        (context-navigator-path-add--cache-put
         abs-root
         (cond
          ;; avoid heavy scan on TRAMP roots
          (remote '())
          ((context-navigator-path-add--project-files-project-el abs-root))
          ((context-navigator-path-add--project-files-git abs-root))
          (t
           (if (and context-navigator-path-add-fallback-enabled
                    (not (context-navigator-path-add--unsafe-root-p abs-root))
                    (let* ((top (ignore-errors (directory-files abs-root nil nil t)))
                           (n (and (listp top) (length top))))
                      (and (integerp n)
                           (<= n (or context-navigator-path-add-fallback-top-level-max 8000)))))
               (context-navigator-path-add--project-files-fallback abs-root)
             '())))))))

;; -----------------------------------------------------------------------------
;; Token extraction

(defun context-navigator-path-add--token-acceptable-p (s)
  "Heuristic: accept S only if it really looks like a file path.
Rules:
- reject URLs
- accept absolute paths (POSIX/Windows/UNC)
- accept tokens with a directory separator when:
  - basename is non-empty and not . or ..
  - string does not contain double separators (// or \\\\)
- accept tokens without a directory separator only when they have an extension
Everything else (e.g. trailing slash directories like 'app/', plain words, or
well-known extensionless names like Makefile) is rejected."
  (and (stringp s)
       (not (string-empty-p s))
       (not (context-navigator-path-add--looks-like-url-p s))
       (let ((bn (file-name-nondirectory s)))
         (or
          ;; Absolute paths (POSIX/Windows/UNC)
          (context-navigator-path-add--absolute-p s)
          ;; With dir separators: must not have double separators and basename must be valid
          (and (context-navigator-path-add--has-dirsep-p s)
               (not (context-navigator-path-add--has-double-sep-p s))
               (stringp bn)
               (not (string-empty-p bn))
               (not (member bn '("." ".."))))
          ;; Bare basenames: require extension
          (and (not (context-navigator-path-add--has-dirsep-p s))
               (context-navigator-path-add--has-extension-p s))))))

(defun context-navigator-extract-pathlike-tokens (text)
  "Extract path-like tokens from TEXT. Return list of normalized strings.

Primary pass:
- regex-based extraction from quotes/angles and path-like char runs

Fallback (when primary found nothing):
- dired-like lines: take the last whitespace-separated field on each line,
  normalize and validate as a candidate (helps with org blocks containing dired listings)."
  (let* ((re "\\(?:\"[^\"\n]+\"\\|'[^'\n]+'\\|<[^>\n]+>\\|[~[:alnum:]_.:/\\\\-]+\\)")
         (pos 0) (acc '()))
    ;; Primary pass
    (while (and (< pos (length text))
                (string-match re text pos))
      (let* ((m0 (match-string 0 text)))
        (setq pos (match-end 0))
        (when (and (stringp m0) (not (string-empty-p (string-trim m0))))
          (let ((norm (context-navigator-path-add--normalize-token m0)))
            (when (and (stringp norm)
                       (not (string-empty-p norm))
                       (context-navigator-path-add--token-acceptable-p norm))
              (push norm acc))))))
    (setq acc (nreverse (delete-dups acc)))
    (if (> (length acc) 0)
        acc
      ;; Fallback: scan line-by-line, pick last field
      (let ((lines (split-string text "\n" t)))
        (dolist (ln lines)
          (let* ((trim (string-trim ln)))
            (when (and (stringp trim) (not (string-empty-p trim)))
              ;; split by whitespace; take last non-empty chunk
              (let* ((parts (cl-remove-if (lambda (s) (or (null s) (string-empty-p s)))
                                          (split-string trim "[ \t]+" t)))
                     (last (car (last parts))))
                (when last
                  (let* ((norm (context-navigator-path-add--normalize-token last)))
                    (when (and (stringp norm)
                               (not (string-empty-p norm))
                               (context-navigator-path-add--token-acceptable-p norm))
                      (push norm acc))))))))
        (nreverse (delete-dups acc))))))

;; -----------------------------------------------------------------------------
;; Resolution algorithm

(defun context-navigator-path-add--absolute-p (s)
  (or (file-name-absolute-p s)
      ;; Windows drive or UNC
      (string-match-p "\\`[A-Za-z]:[\\/]" s)
      (string-match-p "\\`\\\\\\\\[^\\/]+" s)))

(defun context-navigator-path-add--resolve-relative (root s)
  (expand-file-name s (or root default-directory)))

(defun context-navigator-path-add--resolve-candidates (tokens root index)
  "Return plist with resolution results before filtering by size/type.
:accepted list<abs-file>  — direct unique resolutions (abs/rel or unique index matches)
:ambiguous alist (token . matches)
:unresolved list<token>"
  (let* ((casefold (context-navigator-path-add--case-fold-p))
         (basename-map
          ;; basename (with ext) -> list of files
          (let ((ht (make-hash-table :test (if casefold 'equal 'equal))))
            (dolist (p index)
              (let* ((bn (file-name-nondirectory p))
                     (k  (if casefold (downcase bn) bn)))
                (puthash k (cons p (gethash k ht)) ht)))
            ht))
         (noext-map
          ;; basename sans extension -> list of files
          (let ((ht (make-hash-table :test (if casefold 'equal 'equal))))
            (dolist (p index)
              (let* ((bn (file-name-nondirectory p))
                     (nx (file-name-sans-extension bn))
                     (k  (if casefold (downcase nx) nx)))
                (puthash k (cons p (gethash k ht)) ht)))
            ht))
         (acc-accepted '())
         (acc-amb '())
         (acc-unres '()))
    (dolist (tok tokens)
      (cond
       ;; Absolute path
       ((context-navigator-path-add--absolute-p tok)
        (if (context-navigator-path-add--regular-file-p tok)
            (push (expand-file-name tok) acc-accepted)
          (push tok acc-unres)))
       ;; Relative path that exists
       ((file-exists-p (context-navigator-path-add--resolve-relative root tok))
        (let ((p (context-navigator-path-add--resolve-relative root tok)))
          (if (context-navigator-path-add--regular-file-p p)
              (push (expand-file-name p) acc-accepted)
            (push tok acc-unres))))
       (t
        ;; Search through project index
        (let* ((bn (file-name-nondirectory tok)))
          (cond
           ;; Bare basename without extension → search by sans-extension map
           ((and (not (context-navigator-path-add--has-dirsep-p tok))
                 (not (context-navigator-path-add--has-extension-p tok)))
            (let* ((key (if casefold (downcase (file-name-sans-extension bn))
                          (file-name-sans-extension bn)))
                   (hits (copy-sequence (or (gethash key noext-map) '()))))
              (cond
               ((= (length hits) 1) (push (car hits) acc-accepted))
               ((> (length hits) 1)
                (push (cons tok (cl-subseq hits 0 (min 10 (length hits)))) acc-amb))
               (t (push tok acc-unres)))))
           ;; Basename with extension or with dirs → search by full basename
           (t
            (let* ((key (if casefold (downcase bn) bn))
                   (hits (copy-sequence (or (gethash key basename-map) '()))))
              (cond
               ((= (length hits) 1) (push (car hits) acc-accepted))
               ((> (length hits) 1)
                (push (cons tok (cl-subseq hits 0 (min 10 (length hits)))) acc-amb))
               (t (push tok acc-unres))))))))))
    (list :accepted (nreverse (delete-dups acc-accepted))
          :ambiguous (nreverse acc-amb)
          :unresolved (nreverse acc-unres))))

(defun context-navigator-path-add--apply-filters (files)
  "Apply size/type/remote filters. Return plist:
:files L :skipped-too-big N :skipped-nonregular N :remote N"
  (let* ((limit (or context-navigator-max-file-size most-positive-fixnum))
         (kept '())
         (big 0) (nonreg 0) (remote 0))
    (dolist (f files)
      (cond
       ((or (not (stringp f))
            (not (file-exists-p f))
            (not (file-regular-p f))
            (context-navigator-path-add--symlink-p f))
        (setq nonreg (1+ nonreg)))
       (t
        (when (file-remote-p f) (setq remote (1+ remote)))
        (let ((sz (context-navigator-path-add--file-size f)))
          (if (and sz (> sz limit))
              (setq big (1+ big))
            (push f kept))))))
    (list :files (nreverse (delete-dups kept))
          :skipped-too-big big
          :skipped-nonregular nonreg
          :remote remote)))

(defun context-navigator-resolve-names->files (tokens root &rest _opts)
  "High-level resolution pipeline. Return plist:
:files L :ambiguous A :unresolved L :skipped-too-big N :skipped-nonregular N :remote N"
  (let* ((remote-root (and (stringp root) (file-remote-p root)))
         (index (if remote-root '() (context-navigator-project-file-index root)))
         (res (context-navigator-path-add--resolve-candidates tokens root index))
         (flt (context-navigator-path-add--apply-filters (plist-get res :accepted))))
    (list :files (plist-get flt :files)
          :ambiguous (plist-get res :ambiguous)
          :unresolved (plist-get res :unresolved)
          :skipped-too-big (plist-get flt :skipped-too-big)
          :skipped-nonregular (plist-get flt :skipped-nonregular)
          :remote (plist-get flt :remote))))

;; -----------------------------------------------------------------------------
;; Add to model + UX

(defun context-navigator-path-add--maybe-apply-to-gptel (&optional only-items)
  "Apply to gptel when push is ON.
If ONLY-ITEMS is provided (list of items), add them in background batches.
Otherwise, apply a full diff for the current model.

Change: do not require a visible gptel window during add-from-text/minibuffer flows,
so auto-push works even when gptel buffer is hidden."
  (when (and (boundp 'context-navigator--push-to-gptel)
             context-navigator--push-to-gptel)
    (let* ((st (context-navigator--state-get))
           (token (and st (context-navigator-state-load-token st))))
      (let ((context-navigator-gptel-require-visible-window nil))
        (if (and only-items (listp only-items))
            (ignore-errors (context-navigator--gptel-defer-or-start only-items token))
          (let ((items (and st (context-navigator-state-items st))))
            (ignore-errors (context-navigator-gptel-apply (or items '())))))))
    ;; Post-apply: не затираем предиктивные лампы устаревшим снапшотом.
    ;; Вместо этого — отложенный refresh и "подсказка" событием для View.
    (ignore-errors
      (when (fboundp 'context-navigator-add--refresh-gptel-keys-snapshot)
        (run-at-time 0.12 nil #'context-navigator-add--refresh-gptel-keys-snapshot)))
    (ignore-errors
      (let* ((st (and (fboundp 'context-navigator--state-get) (context-navigator--state-get)))
             (items (and st (fboundp 'context-navigator-state-items) (context-navigator-state-items st)))
             (n (length (or items '()))))
        ;; Для "полного" apply (не батч) View не получает :gptel-change; подадим "done",
        ;; чтобы индикаторы обновились (View на :batch-done обновит keys и перерисует).
        (when (fboundp 'context-navigator-events-publish)
          (context-navigator-events-publish :gptel-change :batch-done n))))
    ;; Сохраняем отзывчивость UI: мягкий ререндер, чтобы остались предиктивные лампы.
    (when (fboundp 'context-navigator-view--schedule-render-soft)
      (context-navigator-view--schedule-render-soft))
    (when (fboundp 'context-navigator-view--render-if-visible)
      (context-navigator-view--render-if-visible))))

(defun context-navigator-path-add--append-files-as-items (files)
  "Append FILES as enabled file items to the model in one batch.
If a file item already exists, re-enable it instead of duplicating.
Return count of items that became present/enabled (new + re-enabled)."
  (let* ((abs (delq nil (mapcar (lambda (p) (and (stringp p) (expand-file-name p))) files)))
         ;; Track paths for quick path-based filtering
         (abs-set (let ((h (make-hash-table :test 'equal)))
                    (dolist (p abs) (puthash p t h)) h))
         (st  (ignore-errors (context-navigator--state-get)))
         (idx (and st (context-navigator-state-index st)))
         (old (and (context-navigator-state-p st) (context-navigator-state-items st)))
         (keys-to-replace (make-hash-table :test 'equal))
         (replacements '())
         (apply-items '()))
    ;; Build replacements (new or re-enabled) for each requested file
    (dolist (p abs)
      (when (and (stringp p) (file-exists-p p) (file-regular-p p))
        (let* ((tmp (context-navigator-item-create :type 'file
                                                   :name (file-name-nondirectory p)
                                                   :path p :enabled t))
               (key (context-navigator-model-item-key tmp))
               (existing (and (hash-table-p idx) (gethash key idx))))
          (if (context-navigator-item-p existing)
              (let* ((en (context-navigator-item-enabled existing))
                     (upd (if en existing
                            (context-navigator-item-create
                             :type (context-navigator-item-type existing)
                             :name (context-navigator-item-name existing)
                             :path (context-navigator-item-path existing)
                             :buffer (context-navigator-item-buffer existing)
                             :beg (context-navigator-item-beg existing)
                             :end (context-navigator-item-end existing)
                             :size (context-navigator-item-size existing)
                             :enabled t
                             :meta (context-navigator-item-meta existing)))))
                (puthash key t keys-to-replace)
                (when (context-navigator-item-p upd)
                  (push upd replacements))
                (when (and (context-navigator-item-p upd) (not en))
                  (push upd apply-items)))
            (let ((it (context-navigator-item-create
                       :type 'file
                       :name (file-name-nondirectory p)
                       :path (expand-file-name p)
                       :enabled t)))
              (when it
                (push it replacements)
                (push it apply-items)))))))
    ;; Keep old items except ones replaced by key or referencing these paths
    (let* ((keep
            (cl-remove-if
             (lambda (it)
               (let* ((p (and (stringp (context-navigator-item-path it))
                              (expand-file-name (context-navigator-item-path it))))
                      (key (context-navigator-model-item-key it)))
                 (or (and p (gethash p abs-set))
                     (and key (gethash key keys-to-replace)))))
             (or old '())))
           (merged (append keep (nreverse replacements)))
           (added (length apply-items)))
      (context-navigator-set-items merged)
      added)))

(cl-defun context-navigator-add-files-from-names (tokens &optional _interactive)
  "Resolve TOKENS relative to current root and add resulting files to model.
Handles ambiguities/unresolved/limits/remote confirmation. Returns plist result."
  (let* ((root (context-navigator-path-add--project-root)))
    (context-navigator-ui-info :resolve-start)
    (let* ((res (context-navigator-resolve-names->files tokens root))
           (files (plist-get res :files))
           (amb (plist-get res :ambiguous))
           (unr (plist-get res :unresolved))
           (too-many (> (length files) (or context-navigator-path-add-limit 50)))
           (aborted nil)
           (result nil))
      (when (and (consp amb) (> (length amb) 0))
        (let* ((sample (mapconcat
                        (lambda (cell)
                          (format "%s → %d" (car cell) (length (cdr cell))))
                        (cl-subseq amb 0 (min 10 (length amb)))
                        ", ")))
          (context-navigator-ui-info :ambiguous-found sample))
        (setq result (plist-put (copy-sequence res) :aborted :ambiguous))
        (setq aborted t))
      (when (and (not aborted) (consp unr) (> (length unr) 0))
        (let ((sample (string-join (cl-subseq unr 0 (min 10 (length unr))) ", ")))
          (context-navigator-ui-info :unresolved-found sample)))
      (when (and (not aborted) too-many)
        (if (context-navigator-path-add--preview-and-confirm files res)
            (setq aborted nil)
          (context-navigator-ui-info :aborted)
          (setq result (plist-put (copy-sequence res) :aborted :too-many))
          (setq aborted t)))
      (when (and (not aborted)
                 (> (plist-get res :remote) 0)
                 (not (context-navigator-ui-ask :remote-warning (plist-get res :remote))))
        (context-navigator-ui-info :aborted)
        (setq result (plist-put (copy-sequence res) :aborted :remote))
        (setq aborted t))
      (if aborted
          result
        (let* ((st-before (context-navigator--state-get))
               (items-before (and st-before (context-navigator-state-items st-before)))
               (added (context-navigator-path-add--append-files-as-items files))
               (st-after (context-navigator--state-get))
               (items-after (and st-after (context-navigator-state-items st-after)))
               (diff (context-navigator-model-diff (or items-before '()) (or items-after '())))
               (adds (plist-get diff :add)))
          (when (fboundp 'context-navigator-add--ui-refresh-now)
            (context-navigator-add--ui-refresh-now t))
          (context-navigator-path-add--maybe-apply-to-gptel adds)
          (context-navigator-ui-info :added-files added)
          (plist-put (copy-sequence res) :added added))))))

;; -----------------------------------------------------------------------------
;; Interactive commands

;;;###autoload
(cl-defun context-navigator-add-from-text ()
  "Extract path-like tokens from region or buffer, preview, and add resolved files to the active group."
  (interactive)
  (let* ((src (if (use-region-p)
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (buffer-substring-no-properties (point-min) (point-max))))
         (tokens (context-navigator-extract-pathlike-tokens (or src ""))))
    (if (or (null tokens) (= (length tokens) 0))
        (context-navigator-ui-info :text-no-tokens)
      (let* ((root (context-navigator-path-add--project-root))
             (res (context-navigator-resolve-names->files tokens root))
             (files (plist-get res :files))
             (amb (plist-get res :ambiguous))
             (unr (plist-get res :unresolved))
             ;; Если есть неоднозначности — включаем все их совпадения и пересчитываем фильтры/статистику
             (amb-files (and (consp amb)
                             (cl-mapcan (lambda (cell) (copy-sequence (cdr cell))) amb)))
             (combined-files (if amb-files (nreverse (delete-dups (append files amb-files))) files))
             (flt (and amb-files (context-navigator-path-add--apply-filters combined-files)))
             (files (or (and flt (plist-get flt :files)) combined-files))
             (stats (or flt res))
             (too-many (> (length files) (or context-navigator-path-add-limit 50))))
        ;; Неоднозначности: уведомляем, но продолжаем и предлагаем добавить все подходящие файлы
        (when (and (consp amb) (> (length amb) 0))
          (let ((sample (mapconcat
                         (lambda (cell)
                           (format "%s → %d" (car cell) (length (cdr cell))))
                         (cl-subseq amb 0 (min 10 (length amb)))
                         ", ")))
            (context-navigator-ui-info :ambiguous-found sample)))
        ;; Нерешённые: просто сообщаем (не фатально)
        (when (and (consp unr) (> (length unr) 0))
          (let ((sample (string-join (cl-subseq unr 0 (min 10 (length unr))) ", ")))
            (context-navigator-ui-info :unresolved-found sample)))
        ;; Слишком много
        (when too-many
          (context-navigator-ui-warn :too-many (length files) (or context-navigator-path-add-limit 50))
          (cl-return-from context-navigator-add-from-text nil))
        ;; Подтверждение для remote
        (when (and (> (plist-get stats :remote) 0)
                   (not (context-navigator-ui-ask :remote-warning (plist-get stats :remote))))
          (context-navigator-ui-info :aborted)
          (cl-return-from context-navigator-add-from-text nil))
        ;; Нечего добавлять
        (if (not (and (listp files) (> (length files) 0)))
            (context-navigator-ui-info :unresolved-found)
          ;; Подтверждение: большие наборы → превью, иначе только минибуфер
          (let ((proceed (if too-many
                             (context-navigator-path-add--preview-and-confirm files stats)
                           (context-navigator-ui-ask :confirm-add (length files)))))
            (if (not proceed)
                (context-navigator-ui-info :aborted)
              ;; Добавление и пуш (батч)
              (let* ((st-before (context-navigator--state-get))
                     (items-before (and st-before (context-navigator-state-items st-before)))
                     (added (context-navigator-path-add--append-files-as-items files))
                     (st-after (context-navigator--state-get))
                     (items-after (and st-after (context-navigator-state-items st-after)))
                     (diff (context-navigator-model-diff (or items-before '()) (or items-after '())))
                     (adds (plist-get diff :add)))
                (when (fboundp 'context-navigator-add--ui-refresh-now)
                  (context-navigator-add--ui-refresh-now t))
                (context-navigator-path-add--maybe-apply-to-gptel adds)
                (context-navigator-ui-info :added-files added)))))))))

;;;###autoload
(defun context-navigator-add-from-minibuffer ()
  "Read names/paths or a single mask (glob) from minibuffer and add files.

Rules:
- If input contains glob metacharacters (* ? [ ]) → mask mode
- Only one mask is supported at a time
- Mixed input: masks are ignored; only explicit names are added"
  (interactive)
  (let* ((prompt (format "%s: " (context-navigator-i18n :mask-minibuf-prompt)))
         (input (read-from-minibuffer prompt nil nil nil nil nil t))
         (has-mask-in-input (context-navigator--input-has-mask-p (or input ""))))
    ;; First pass: decide using raw segments so explicit names always win.
    (let* ((parts (cl-remove-if (lambda (s) (or (null s) (string-empty-p s)))
                                (split-string (or input "") "[ \t]+" t)))
           (norm (delq nil (mapcar (lambda (s)
                                     (let ((n (context-navigator-path-add--normalize-token s)))
                                       (and (stringp n) (not (string-empty-p n)) n)))
                                   parts)))
           (name-raw (cl-remove-if #'context-navigator--input-has-mask-p norm))
           (mask-raw (cl-remove-if-not #'context-navigator--input-has-mask-p norm)))
      (cond
       ;; Explicit names present → ignore any masks entirely.
       ((> (length name-raw) 0)
        (when (> (length mask-raw) 0)
          (context-navigator-ui-info :mask-mixed-input))
        (let ((names (cl-remove-if-not #'context-navigator-path-add--token-acceptable-p name-raw)))
          (if (> (length names) 0)
              (context-navigator-add-files-from-names names t)
            (context-navigator-ui-info :unresolved-found))))
       ;; Too many masks
       ((> (length mask-raw) 1)
        (context-navigator-ui-warn :mask-only-one))
       ;; Single mask
       ((= (length mask-raw) 1)
        (context-navigator-add-files-from-mask (car mask-raw) t))
       ;; Fallback to tokenizer-based logic for tricky inputs (quotes/dired-like lines)
       (t
        (let* ((tokens (context-navigator-extract-pathlike-tokens (or input ""))))
          (cond
           ;; Raw input clearly has mask meta but tokenizer produced no tokens
           ((and has-mask-in-input (null tokens))
            (context-navigator-add-files-from-mask (string-trim (or input "")) t))
           ((null tokens)
            (context-navigator-ui-info :unresolved-found))
           (t
            (let* ((mask-tokens (cl-remove-if-not #'context-navigator--input-has-mask-p tokens))
                   (name-tokens (cl-remove-if #'context-navigator--input-has-mask-p tokens)))
              (cond
               ((> (length name-tokens) 0)
                (when (> (length mask-tokens) 0)
                  (context-navigator-ui-info :mask-mixed-input))
                (context-navigator-add-files-from-names name-tokens t))
               ((> (length mask-tokens) 1)
                (context-navigator-ui-warn :mask-only-one))
               ((= (length mask-tokens) 1)
                (context-navigator-add-files-from-mask (car mask-tokens) t))
               ((and has-mask-in-input (= (length mask-tokens) 0))
                (context-navigator-add-files-from-mask (string-trim (or input "")) t))
               (t
                (context-navigator-add-files-from-names tokens t))))))))))))

;; -----------------------------------------------------------------------------
;; Mask/glob helpers (v1)

(defun context-navigator--input-has-mask-p (s)
  "Return non-nil when S contains glob mask metacharacters (* ? [ ])."
  (and (stringp s) (string-match-p "[*?\\[]"
                                   s)))

(defun context-navigator--normalize-separators (s)
  "Normalize Windows path separators to POSIX style."
  (when (stringp s)
    (setq s (replace-regexp-in-string "\\\\" "/" s))
    s))

(defun context-navigator--glob-static-prefix (pattern)
  "Return directory prefix of PATTERN before first meta segment and the rest.
Result is cons (DIR . REST) where DIR has no trailing slash (\"\" for none)."
  (let* ((p (or pattern ""))
         (p (string-remove-prefix "./" p))
         (segs (split-string p "/" t))
         (acc '())
         (rest "")
         (n (length segs))
         (i 0))
    (while (< i n)
      (let ((seg (nth i segs)))
        (if (or (string-match-p "[*?\\[]"
                                seg)
                (string= seg "**"))
            (progn
              (setq rest (string-join (cl-subseq segs i) "/"))
              (setq i n)) ;; break
          (push seg acc)
          (setq i (1+ i)))))
    (cons (string-join (nreverse acc) "/") rest)))

(defun context-navigator--mask-base (pattern)
  "Return plist describing mask base and scan-root.

Keys:
 :base       one of 'project | 'cwd | 'abs
 :base-root  absolute path to project root / cwd / absolute root
 :scan-root  absolute dir to start enumeration (static prefix)
 :rel-pattern pattern relative to scan-root
 :full-pattern absolute pattern string
 :remote     non-nil when scan-root is TRAMP"
  (let* ((raw (context-navigator--normalize-separators (or pattern "")))
         (is-abs (or (file-name-absolute-p raw)
                     (string-match-p "\\`[A-Za-z]:[\\/]" raw)
                     (string-prefix-p "~" raw)))
         (starts-dot (or (string-prefix-p "./" raw)
                         (string-prefix-p "../" raw)))
         (base-root
          (cond
           (starts-dot (expand-file-name default-directory))
           (is-abs (expand-file-name raw))
           (t (or (context-navigator-path-add--project-root)
                  default-directory))))
         ;; For abs starting with ~, normalize full pattern using expand-file-name
         (full-pattern
          (cond
           (starts-dot (expand-file-name raw default-directory))
           (is-abs (expand-file-name raw))
           (t (expand-file-name raw (or (context-navigator-path-add--project-root)
                                        default-directory)))))
         ;; Determine base symbol
         (base (cond
                (starts-dot 'cwd)
                (is-abs 'abs)
                (t 'project)))
         ;; Compute static prefix and scan-root
         (rel-for-scan
          (cond
           (is-abs
            ;; make it relative to absolute root dir (without drive specifics)
            (let* ((dir (file-name-directory full-pattern))
                   (file (file-name-nondirectory full-pattern))
                   (_ dir) ;; keep quiet byte-compiler
                   (abs-pattern (file-relative-name full-pattern "/")))
              abs-pattern))
           (t raw)))
         (prefix+rest (context-navigator--glob-static-prefix rel-for-scan))
         (prefix (car prefix+rest))
         (rel-rest (cdr prefix+rest))
         (scan-root
          (cond
           (is-abs
            (if (string-empty-p prefix)
                (file-name-directory full-pattern)
              (expand-file-name prefix "/")))
           (t
            (let ((root (cond
                         (starts-dot (expand-file-name default-directory))
                         ((eq base 'project) (or (context-navigator-path-add--project-root)
                                                 default-directory))
                         (t base-root))))
              (if (string-empty-p prefix)
                  root
                (expand-file-name prefix root))))))
         (remote (file-remote-p scan-root)))
    (list :base base
          :base-root (directory-file-name (expand-file-name
                                           (cond
                                            ((eq base 'abs) (file-name-directory full-pattern))
                                            (t base-root))))
          :scan-root (directory-file-name (expand-file-name scan-root))
          :rel-pattern (or rel-rest (file-name-nondirectory full-pattern))
          :full-pattern full-pattern
          :remote remote)))

(defun context-navigator--glob-to-regexp (pattern &optional globstar)
  "Translate glob PATTERN to an Emacs regexp matching full relative paths.

Supported:
- *  → any chars except /
- ?  → exactly one char except /
- [] → character classes including [:alnum:], [:space:], ranges, [!…] negation
- ** when GLOBSTAR non-nil → 0+ directory components"
  (let* ((globstar (if (null globstar) context-navigator-mask-globstar globstar))
         (p (or (context-navigator--normalize-separators pattern) ""))
         (segs (split-string p "/" t))
         (rx-segs '())
         (escape
          (lambda (ch)
            (if (string-match-p "[.^$+()|{}]" ch)
                (concat "\\" ch)
              ch))))
    (cl-labels
        ((tr-seg (seg)
           (if (and globstar (string= seg "**"))
               ;; 0+ directories (each at least one char not '/'), includes trailing slash
               "\\(?:[^/]+/\\)*"
             (let* ((i 0) (n (length seg)) (out ""))
               (while (< i n)
                 (let ((c (substring seg i (1+ i))))
                   (cond
                    ((string= c "*") (setq out (concat out "[^/]*")))
                    ((string= c "?") (setq out (concat out "[^/]")))
                    ((string= c "[")
                     ;; copy until closing ], convert [! …] to [^ …]
                     (let ((j (1+ i)) (buf "[") (neg nil))
                       (when (and (< j n) (string= (substring seg j (1+ j)) "!"))
                         (setq neg t) (setq j (1+ j)))
                       (while (and (< j n) (not (string= (substring seg j (1+ j)) "]")))
                         (setq buf (concat buf (substring seg j (1+ j))))
                         (setq j (1+ j)))
                       (when (< j n) (setq buf (concat buf "]")) (setq i j))
                       (setq out (concat out (if neg (replace-regexp-in-string "^\\[" "[^" buf) buf)))))
                    (t
                     (setq out (concat out (funcall escape c))))))
                 (setq i (1+ i)))
               out))))
      (let ((prev-trailing-slash nil))
        (dolist (seg segs)
          (let* ((tr (tr-seg seg))
                 (trailing (and globstar (string= seg "**"))))
            (when (and (not prev-trailing-slash) (> (length rx-segs) 0))
              (push "/" rx-segs))
            (push tr rx-segs)
            (setq prev-trailing-slash trailing))))
      (concat "\\`" (apply #'concat (nreverse rx-segs)) "\\'"))))

(defun context-navigator--path-has-dot-component-p (rel)
  "Return non-nil if REL has any component starting with a dot."
  (or (string-prefix-p "." rel)
      (string-match-p "/\\." rel)))

(defun context-navigator--pattern-allows-dotfiles-p (pattern)
  "Return non-nil if PATTERN explicitly allows matching dotfiles somewhere.

Heuristic v1:
- allow if any component begins with '.' (detect via \"^.\" or \"/.\")
- or when global override `context-navigator-mask-include-dotfiles' is non-nil"
  (or context-navigator-mask-include-dotfiles
      (string-prefix-p "." pattern)
      (string-match-p "/\\." pattern)))

(defun context-navigator--collect-candidates (base-pl)
  "Collect absolute file candidates for BASE-PL scan-root quickly.

Strategy:
- Prefer project index when available (fast), filtered to scan-root prefix.
- Otherwise, perform a safe recursive traversal from SCAN-ROOT (dotdirs/custom
  exclusions, symlinked dirs skipped), regardless of
  `context-navigator-path-add-fallback-enabled' (mask expansion needs real FS).
- On TRAMP roots, return empty (remote expansion is gated elsewhere by
  `context-navigator-mask-enable-remote')."
  (let* ((scan-root (plist-get base-pl :scan-root))
         (base-root (plist-get base-pl :base-root))
         (base (plist-get base-pl :base))
         (remote (plist-get base-pl :remote))
         (proj-root (and (eq base 'project) base-root))
         (idx (and proj-root (not remote)
                   (context-navigator-project-file-index proj-root)))
         (in-scan-p
          (lambda (p) (string-prefix-p
                       (file-name-as-directory (expand-file-name scan-root))
                       (expand-file-name p)))))
    (cond
     ;; Prefer project index (fast), then filter by scan-root prefix
     ((and (listp idx) (> (length idx) 0))
      (cl-remove-if-not in-scan-p idx))
     ;; Remote roots are not expanded here (caller handles warning/deny)
     (remote
      '())
     ;; Fallback: recursive scan from scan-root with exclusions (force-enabled)
     (t
      (let ((context-navigator-path-add-fallback-enabled t))
        (ignore-errors (context-navigator-path-add--project-files-fallback scan-root)))))))

(defun context-navigator--filter-matches (files rx base-pl)
  "Return subset of FILES whose relative path to SCAN-ROOT matches RX.
Respects case sensitivity and dotfiles rule."
  (let* ((scan-root (plist-get base-pl :scan-root))
         (pattern (plist-get base-pl :rel-pattern))
         (allow-dots (context-navigator--pattern-allows-dotfiles-p pattern))
         (case-fold-search (context-navigator-path-add--case-fold-p)))
    (cl-remove-if-not
     (lambda (f)
       (let* ((rel (file-relative-name (expand-file-name f)
                                       (file-name-as-directory scan-root))))
         (and (string-match-p rx rel)
              (or allow-dots (not (context-navigator--path-has-dot-component-p rel))))))
     files)))

(defun context-navigator-path-add--preview-and-confirm (files stats)
  "Show preview buffer for FILES and STATS, return non-nil to proceed."
  (let* ((buf (get-buffer-create "*Context Navigator Add Preview*"))
         (total (length files))
         (too-big (plist-get stats :skipped-too-big))
         (nonreg (plist-get stats :skipped-nonregular))
         (remote (plist-get stats :remote))
         (sum-bytes (cl-loop for f in files
                             for s = (context-navigator-path-add--file-size f)
                             when s sum s)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format (context-navigator-i18n :preview-title)
                        total
                        (context-navigator-human-size sum-bytes)))
        (insert "\n")
        (when (> too-big 0)
          (insert (format (context-navigator-i18n :preview-skipped-too-big)
                          too-big
                          (context-navigator-human-size context-navigator-max-file-size)))
          (insert "\n"))
        (when (> nonreg 0)
          (insert (format (context-navigator-i18n :preview-skipped-nonregular) nonreg))
          (insert "\n"))
        (when (> remote 0)
          (insert (format (context-navigator-i18n :preview-remote) remote))
          (insert "\n"))
        (insert "\n")
        (insert (context-navigator-i18n :preview-files))
        (insert "\n")
        (dolist (f files)
          (insert (format "  %s\n" (abbreviate-file-name f))))
        (goto-char (point-min))
        (view-mode 1))
      (save-window-excursion
        (display-buffer
         buf
         '((display-buffer-in-side-window)
           (side . bottom)
           (window-height . fit-window-to-buffer)
           (window-parameters . ((no-other-window . t)
                                 (no-delete-other-windows . t)))))
        (unwind-protect
            (context-navigator-ui-ask :confirm-add (length files))
          (when (buffer-live-p buf)
            (kill-buffer buf)))))))


(cl-defun context-navigator-add-files-from-mask (pattern &optional _interactive)
  "Resolve PATTERN (glob) to files and add them to the model with preview when needed."
  (let* ((base (context-navigator--mask-base pattern)))
    (when (and (plist-get base :remote)
               (not context-navigator-mask-enable-remote))
      (context-navigator-ui-warn :mask-remote-unsupported pattern)
      (cl-return-from context-navigator-add-files-from-mask nil))
    (condition-case err
        (let* ((rx (context-navigator--glob-to-regexp (plist-get base :rel-pattern)
                                                      context-navigator-mask-globstar))
               (cands (context-navigator--collect-candidates base))
               (matches (context-navigator--filter-matches cands rx base))
               (flt (context-navigator-path-add--apply-filters matches))
               (files (plist-get flt :files)))
          (cond
           ((null files)
            (context-navigator-ui-info :mask-nothing-found pattern)
            nil)
           (t
            (let* ((too-many (> (length files) (or context-navigator-path-add-limit 50)))
                   (go (if too-many
                           (context-navigator-path-add--preview-and-confirm files flt)
                         t)))
              (if (not go)
                  (context-navigator-ui-info :aborted)
                (let* ((st-before (context-navigator--state-get))
                       (items-before (and st-before (context-navigator-state-items st-before)))
                       (added (context-navigator-path-add--append-files-as-items files))
                       (st-after (context-navigator--state-get))
                       (items-after (and st-after (context-navigator-state-items st-after)))
                       (diff (context-navigator-model-diff (or items-before '()) (or items-after '())))
                       (adds (plist-get diff :add)))
                  (when (fboundp 'context-navigator-add--ui-refresh-now)
                    (context-navigator-add--ui-refresh-now t))
                  (context-navigator-path-add--maybe-apply-to-gptel adds)
                  (context-navigator-ui-info :added-files added)
                  (list :files files :added added)))))))
      (error
       (context-navigator-ui-error :mask-parse-error (or (nth 1 err) pattern))
       nil))))


(provide 'context-navigator-add-paths)
;;; context-navigator-add-paths.el ends here
