(require 'rcirc)
(require 'cl-lib)

(defmacro with-style-sandbox (string &rest body)
  "Evaluate BODY in a temp buffer containing only STRING."
  `(with-temp-buffer
     (insert ,string)
     ,@body))

(defvar rcirc-styles-tests/face-name
  (if (version< emacs-version "24.4.0")
      'face
      'font-lock-face)
  "Which symbol identifies the face property of a region of
  propertized text. This changed in Emacs 24.4, so we need to
  check the version and change our property of interest
  accordingly.")

(defvar rcirc-styles-tests/fixtures
  '(:green-face (((foreground-color . "green")))
    :blue-face (((background-color . "blue")))
    :bold-face ((bold))
    :italic-face ((italic))
    :underline-face ((underline))
    :inverse-face ((inverse))
    :bold-inverse-face ((bold) (bold inverse))
    :green-bold-face ((bold)
                      ((foreground-color . "green")))
    :green-red-face (((background-color . "red")
                      (foreground-color . "green")))
    :blue-red-face (((background-color . "blue")
                     (foreground-color . "red")))
    :green-blue-face (((background-color . "blue")
                       (foreground-color . "green"))))
  "Fixtures for use in the rcirc-styles tests.")

;; Basic color propertization cases.

(ert-deftest rcirc-styles-tests/propertize-foreground-color nil
  "Should propertize a foreground color specification correctly."
  (with-style-sandbox "3colorful"
    (rcirc-styles-markup-colors)
    (let ((expected (plist-get rcirc-styles-tests/fixtures :green-face))
          (result (get-text-property (point-min) 
                                     rcirc-styles-tests/face-name)))
      (should (cl-equalp result expected)))))

(ert-deftest rcirc-styles-tests/propertize-background-color nil
  "Should propertize a background color specification correctly."
  (with-style-sandbox ",2colorful"
    (rcirc-styles-markup-colors)
    (let ((expected (plist-get rcirc-styles-tests/fixtures :blue-face))
          (result (get-text-property (point-min) 
                                     rcirc-styles-tests/face-name)))
      (should (cl-equalp result expected)))))

(ert-deftest rcirc-styles-tests/propertize-both-colors nil
  "Should propertize a foreground & background color specification correctly."
  (with-style-sandbox "3,2colorful"
    (rcirc-styles-markup-colors)
    (let ((expected (plist-get rcirc-styles-tests/fixtures :green-blue-face))
          (result (get-text-property (point-min) 
                                     rcirc-styles-tests/face-name)))
      (should (cl-equalp result expected)))))

(ert-deftest rcirc-styles-tests/propertize-new-color-fg nil
  "Should preserve existing background when new foreground color given."
  (with-style-sandbox "3,2color4ful"
    (rcirc-styles-markup-colors)
    (let ((expected (plist-get rcirc-styles-tests/fixtures :blue-red-face))
          (result (get-text-property (+ (point-min) 6)
                                     rcirc-styles-tests/face-name)))
      (should (cl-equalp result expected)))))

(ert-deftest rcirc-styles-tests/propertize-new-color-bg nil
  "Should preserve existing foreground when new background color given."
  (with-style-sandbox "3,2color,4ful"
    (rcirc-styles-markup-colors)
    (let ((expected (plist-get rcirc-styles-tests/fixtures :green-red-face))
          (result (get-text-property (+ (point-min) 6)
                                     rcirc-styles-tests/face-name)))
      (should (cl-equalp result expected)))))

(ert-deftest rcirc-styles-tests/propertize-color-until-eol nil
  "Should terminate a foreground color specification on EOL correctly."
  (with-style-sandbox "3colorful"
    (rcirc-styles-markup-colors)
    (let ((expected nil)
          (result (get-text-property (point-max) 
                                     rcirc-styles-tests/face-name)))
      (should (cl-equalp result expected)))))

(ert-deftest rcirc-styles-tests/propertize-color-until-0x0f nil
  "Should terminate color specification property at ^O."
  (with-style-sandbox "3colorless"
    (rcirc-styles-markup-colors)
    (let ((expected nil)
          (result (get-text-property (+ (point-min) 6) 
                                     rcirc-styles-tests/face-name)))
      (should (cl-equalp result expected)))))

(ert-deftest rcirc-styles-tests/propertize-color-until-0x03 nil
  "Should terminate color specification on bare ^C correctly."
  (with-style-sandbox "3colorful"
    (rcirc-styles-markup-colors)
    (let ((expected nil)
          (result (get-text-property (point-max) 
                                     rcirc-styles-tests/face-name)))
      (should (cl-equalp result expected)))))

;; Basic attribute propertization cases.

(ert-deftest rcirc-styles-tests/propertize-bold nil
  "Should propertize a bold specification correctly."
  (with-style-sandbox "emboldened"
    (rcirc-styles-markup-styles)
    (let ((expected (plist-get rcirc-styles-tests/fixtures :bold-face))
          (result (get-text-property (point-min) 
                                     rcirc-styles-tests/face-name)))
      (should (cl-equalp result expected)))))

(ert-deftest rcirc-styles-tests/propertize-attr-until-eol nil
  "Should terminate attribute specification on EOL."
  (with-style-sandbox "emboldened"
    (rcirc-styles-markup-colors)
    (let ((expected nil)
          (result (get-text-property (point-max) 
                                     rcirc-styles-tests/face-name)))
      (should (cl-equalp result expected)))))

(ert-deftest rcirc-styles-tests/propertize-attr-until-0x0f nil
  "Should terminate attribute specification at ^O."
  (with-style-sandbox "emboldened"
    (rcirc-styles-markup-styles)
    (let ((expected nil)
          (result (get-text-property (+ (point-min) 7) 
                                     rcirc-styles-tests/face-name)))
      (should (cl-equalp result expected)))))

(ert-deftest rcirc-styles-tests/propertize-italic nil
  "Should propertize a italic specification correctly."
  (with-style-sandbox "italicized"
    (rcirc-styles-markup-styles)
    (let ((expected (plist-get rcirc-styles-tests/fixtures :italic-face))
          (result (get-text-property (point-min) 
                                     rcirc-styles-tests/face-name)))
      (should (cl-equalp result expected)))))

(ert-deftest rcirc-styles-tests/propertize-underline nil
  "Should propertize a underline specification correctly."
  (with-style-sandbox "lined-under"
    (rcirc-styles-markup-styles)
    (let ((expected (plist-get rcirc-styles-tests/fixtures :underline-face))
          (result (get-text-property (point-min) 
                                     rcirc-styles-tests/face-name)))
      (should (cl-equalp result expected)))))

(ert-deftest rcirc-styles-tests/propertize-inverse nil
  "Should propertize a inverse specification correctly."
  (with-style-sandbox "inverted"
    (rcirc-styles-markup-styles)
    (let ((expected (plist-get rcirc-styles-tests/fixtures :inverse-face))
          (result (get-text-property (point-min) 
                                     rcirc-styles-tests/face-name)))
      (should (cl-equalp result expected)))))

;; Combined color and attribute propertization cases.

(ert-deftest rcirc-styles-tests/propertize-color-and-attr nil
  "Should propertize an overlapping color and attribute spec correctly."
  (with-style-sandbox "3ab"
    (rcirc-styles-markup-styles)
    (should (cl-equalp (get-text-property (point-min) 
                                          rcirc-styles-tests/face-name)
                       (plist-get rcirc-styles-tests/fixtures :green-face)))
    (should (cl-equalp (get-text-property (+ (point-min) 1) 
                                          rcirc-styles-tests/face-name)
                       (plist-get rcirc-styles-tests/fixtures :green-bold-face)))))

;; Adjacent control character cases.

(ert-deftest rcirc-styles-tests/propertize-adjacent-same-attrs nil
  "Should propertize adjacent same attribute specifications correctly."
  (with-style-sandbox "disemboldened"
    (rcirc-styles-markup-styles)
    (let ((expected "disemboldened")
          (result (buffer-substring (point-min) (point-max))))
      (should (string= result expected)))))

(ert-deftest rcirc-styles-tests/propertize-adjacent-diff-attrs nil
  "Should propertize adjacent differing attribute specifications correctly."
  (with-style-sandbox "emboldened"
    (rcirc-styles-markup-styles)
    (let ((expected (plist-get rcirc-styles-tests/fixtures :bold-inverse-face))
          (result (get-text-property (+ (point-min) 4) 
                                     rcirc-styles-tests/face-name)))
      (should (cl-equalp result expected)))))

(ert-deftest rcirc-styles-tests/propertize-adjacent-attr-color nil
  "Should propertize adjacent attribute and color specifications correctly."
  (with-style-sandbox "3emboldened"
    (rcirc-styles-markup-styles)
    (let ((expected (plist-get rcirc-styles-tests/fixtures :green-face))
          (result (get-text-property (+ (point-min) 1) 
                                     rcirc-styles-tests/face-name)))
      (should (cl-equalp result expected)))))

;; Color and attribute insertion functions.

(ert-deftest rcirc-styles-tests/rcirc-styles-insert-color-noargs nil
  "Should insert a correct color code given no arguments."
  (let ((expected "")
        results)
    (cl-letf (((symbol-function #'insert)
               #'(lambda (&rest vals)
                   (push (mapconcat #'identity vals "") results))))
      (rcirc-styles-insert-color))
    (should (cl-equalp (mapconcat #'identity (reverse results) "") expected))))

(ert-deftest rcirc-styles-tests/rcirc-styles-insert-color-return nil
  "Should insert a correct color code given two valid args."
  (let ((expected "3,1")
        results)
    (cl-letf (((symbol-function #'insert)
               #'(lambda (&rest vals)
                   (push (mapconcat #'identity vals "") results))))
      (rcirc-styles-insert-color "green" "black"))
    (setq results (mapconcat #'identity (reverse results) "")) ;; because `push' is actually shift
    (should (cl-equalp results expected))))

(ert-deftest rcirc-styles-tests/rcirc-styles-insert-color-bg nil
  "Should insert a correct color code given only a background arg."
    (let ((expected ",3")
        results)
    (cl-letf (((symbol-function #'insert)
               #'(lambda (&rest vals)
                   (push (mapconcat #'identity vals "") results))))
      (rcirc-styles-insert-color nil "green"))
    (should (cl-equalp (mapconcat #'identity (reverse results) "") expected))))

(ert-deftest rcirc-styles-tests/rcirc-styles-insert-color-fg-and-nil nil
  "Should insert a correct color code given valid foreground arg (and no background)."
    (let ((expected "3")
        results)
    (cl-letf (((symbol-function #'insert)
               #'(lambda (&rest vals)
                   (push (mapconcat #'identity vals "") results))))
      (rcirc-styles-insert-color "green"))
    (should (cl-equalp (mapconcat #'identity (reverse results) "") expected))))

(ert-deftest rcirc-styles-tests/rcirc-styles-insert-color-fg-and-str nil
  "Should insert a correct color code given valid foreground arg (and an empty string as the background)."
    (let ((expected "3")
        results)
    (cl-letf (((symbol-function #'insert)
               #'(lambda (&rest vals)
                   (push (mapconcat #'identity vals "") results))))
      (rcirc-styles-insert-color "green" nil))
    (should (cl-equalp (mapconcat #'identity (reverse results) "") expected))))

(ert-deftest rcirc-styles-tests/rcirc-styles-insert-attribute-return nil
  "Should return a correct attribute code given a valid arg."
  (let ((expected '(""))
        results)
    (cl-letf (((symbol-function #'insert)
               #'(lambda (&rest vals)
                   (push (mapconcat #'identity vals "") results))))
      (rcirc-styles-insert-attribute "bold"))
    (should (equal results expected))))

(ert-deftest rcirc-styles-tests/rcirc-styles--read-attribute-require-value nil
  "Should require a valid arg."
  (let ((args '("bogus" "bold"))
        (expected "bold")
        result)
    (cl-letf (((symbol-function #'completing-read)
               #'(lambda (&rest ignore) (pop args))))
      (setq result (rcirc-styles--read-attribute)))
    (should (equal result expected))))

(ert-deftest rcirc-styles-tests/rcirc-styles--read-color-require-valid nil
  "Should not accept an invalid color name."
  (let ((args '("bogus" "green"))
        (expected "green")
        result)
    (cl-letf (((symbol-function #'completing-read)
               #'(lambda (&rest ignore) (pop args))))
      (setq result (rcirc-styles--read-color "foo")))
    (should (string= result expected))))

(ert-deftest rcirc-styles-tests/rcirc-styles--read-color-require-value nil
  "Should not accept an empty color name without allow-empty."
  (let ((args '("" "green"))
        (expected "green")
        result)
    (cl-letf (((symbol-function #'completing-read)
               #'(lambda (&rest ignore) (pop args))))
      (setq result (rcirc-styles--read-color "foo")))
    (should (string= result expected))))

(ert-deftest rcirc-styles-tests/rcirc-styles--read-color-allow-empty nil
  "Should accept an empty color name with allow-empty."
  (let ((args '("" "green"))
        (expected nil)
        result)
    (cl-letf (((symbol-function #'completing-read)
               #'(lambda (&rest ignore) (pop args))))
      (setq result (rcirc-styles--read-color "foo" t)))
    (should (string= result expected))))

;; rcirc-styles-toggle-preview.

(ert-deftest rcirc-styles-tests/rcirc-styles-preview-only-in-rcirc nil
  "Should do nothing when invoked and current-buffer is not an rcirc-mode buffer."
  (let ((not-called t))
    (cl-flet ((rcirc-styles--show-preview nil (setq not-called nil))
              (rcirc-styles--hide-preview nil (setq not-called nil)))
      (rcirc-styles-toggle-preview)
      (should (eq not-called t)))))

(ert-deftest rcirc-styles-tests/rcirc-styles-toggle-preview-on nil
  "Should invoke `rcirc-styles--show-preview' in a buffer not already prevewing."
  (with-temp-buffer
    (setq major-mode 'rcirc-mode)
    (let (right-call wrong-call)
      (cl-letf (((symbol-function #'rcirc-styles--show-preview)
                 #'(lambda nil (setq right-call t)))
                ((symbol-function #'rcirc-styles--hide-preview)
                 #'(lambda nil (setq wrong-call t))))
        (rcirc-styles-toggle-preview)
        (should (eq right-call t))
        (should (eq wrong-call nil))))))

(ert-deftest rcirc-styles-tests/rcirc-styles-toggle-preview-off nil
  "Should invoke `rcirc-styles--hide-preview' in a buffer already prevewing."
  (with-temp-buffer
    (setq major-mode 'rcirc-mode)
    (setq rcirc-styles-previewing t)
    (let (right-call wrong-call)
      (cl-letf (((symbol-function #'rcirc-styles--show-preview)
                 #'(lambda nil (setq wrong-call t)))
                ((symbol-function #'rcirc-styles--hide-preview)
                 #'(lambda nil (setq right-call t))))
        (rcirc-styles-toggle-preview)
        (should (eq right-call t))
        (should (eq wrong-call nil))))))

;; rcirc-styles--show-preview.

(ert-deftest rcirc-styles-tests/rcirc-styles--show-preview-works nil
  "Should correctly replace literal text with style codes, with styled preview text."
  (with-style-sandbox "3foo"
    (setq rcirc-prompt-end-marker (point-min))
    ;; (cl-flet ((message (&rest ignore) nil))
    (cl-letf (((symbol-function #'message)
               #'(lambda (&rest ignore) nil)))
      (rcirc-styles--show-preview))
    (let ((expected (plist-get rcirc-styles-tests/fixtures :green-face))
          (result (get-text-property (point-min)
                                     rcirc-styles-tests/face-name)))
      (should (cl-equalp result expected)))))

(ert-deftest rcirc-styles-tests/rcirc-styles--show-preview-read-only nil
  "Should correctly propertize styled preview text read-only."
  (with-style-sandbox "3foo"
    (setq rcirc-prompt-end-marker (point-min))
    (cl-letf (((symbol-function #'message)
               #'(lambda (&rest ignore) nil)))
      (rcirc-styles--show-preview))
    (let ((expected t)
          (result (stringp (get-text-property (point-min) 'read-only))))
      (should (cl-equalp result expected)))))

(ert-deftest rcirc-styles-tests/rcirc-styles--show-preview-message nil
  "Should emit a message about entering preview mode."
  (with-style-sandbox "3foo"
    (let ((not-called t))
      (setq rcirc-prompt-end-marker (point-min))
    (cl-letf (((symbol-function #'message)
               #'(lambda (&rest ignore) (setq not-called nil))))
        (rcirc-styles--show-preview))
      (should (eq not-called nil)))))

;; rcirc-styles--hide-preview.

(ert-deftest rcirc-styles-tests/rcirc-styles--hide-preview-works nil
  "Should correctly replace styled preview text with previously cached input."
  (with-style-sandbox "lolwut"
    (setq rcirc-styles-previewing t)
    (setq rcirc-styles-previewed-input "foobar")
    (setq rcirc-prompt-end-marker (point-min))
    (cl-letf (((symbol-function #'message)
               #'(lambda (&rest ignore) nil)))
      (rcirc-styles--hide-preview))
    (let ((expected "foobar")
          (result (buffer-substring (point-min) (point-max))))
      (should (string= result expected)))))

(ert-deftest rcirc-styles-tests/rcirc-styles--hide-preview-message nil
  "Should emit a message about leaving preview mode."
  (with-style-sandbox "lolwut"
    (setq rcirc-styles-previewing t)
    (setq rcirc-styles-previewed-input "foobar")
    (setq rcirc-prompt-end-marker (point-min))
    (let ((not-called t))
    (cl-letf (((symbol-function #'message)
               #'(lambda (&rest ignore) (setq not-called nil))))
      (rcirc-styles--hide-preview)
      (should (eq not-called nil))))))

;; Administrative details and suchlike.

(ert-deftest rcirc-styles-tests/rcirc-styles-map-preview nil
  "Should correctly define `rcirc-styles-map' bindings for styled text preview."
  (let ((expected #'rcirc-styles-toggle-preview)
        (result (lookup-key rcirc-styles-map (kbd "C-p"))))
    (should (cl-equalp result expected))))

(ert-deftest rcirc-styles-tests/rcirc-styles-map-insert-attr nil
  "Should correctly define `rcirc-styles-map' binding for attribute insertion."
  (let ((expected #'rcirc-styles-insert-attribute)
        (result (lookup-key rcirc-styles-map (kbd "C-a"))))
    (should (cl-equalp result expected))))

(ert-deftest rcirc-styles-tests/rcirc-styles-map-insert-color nil
  "Should correctly define `rcirc-styles-map' binding for color insertion."
  (let ((expected #'rcirc-styles-insert-color)
        (result (lookup-key rcirc-styles-map (kbd "C-c"))))
    (should (cl-equalp result expected))))

(ert-deftest rcirc-styles-tests/rcirc-styles-disable-rcirc-controls nil
  "Should remove rcirc-controls' hooks, if they're defined."
  (let (removed-hooks)
    (cl-letf (((symbol-function #'functionp)
               #'(lambda (fun) t))
              ((symbol-function #'remove-hook)
               #'(lambda (var fun) (setq removed-hooks (push fun removed-hooks)))))
      (rcirc-styles-disable-rcirc-controls))
      (should (not (eq nil (member 'rcirc-markup-controls removed-hooks))))
      (should (not (eq nil (member 'rcirc-markup-colors removed-hooks))))))

(ert-deftest rcirc-styles-tests/rcirc-styles-activate-warning nil
  "Should warn about disabling rcirc-controls, if it does so."
  (provide 'rcirc-controls)
  (let ((not-called t))
    (cl-letf (((symbol-function #'message)
               #'(lambda (&rest ignore) (setq not-called nil)))
              ((symbol-function #'add-hook)
               #'(lambda (&rest ignore) nil))
              ((symbol-function #'remove-hook)
               #'(lambda (&rest ignore) nil)))
      (rcirc-styles-activate))
    (should (eq not-called nil))))

(ert-deftest rcirc-styles-tests/rcirc-styles-activate-hooks nil
  "Should remove bogus rcirc-markup-attributes hook and add our own."
  (provide 'rcirc-controls)
  (let (removed-hooks added-hooks)
    (cl-letf (((symbol-function #'message)
               #'(lambda (&rest ignore) nil))
              ((symbol-function #'add-hook)
               #'(lambda (var fun) (setq added-hooks (push fun added-hooks))))
              ((symbol-function #'remove-hook)
               #'(lambda (var fun) (setq removed-hooks (push fun removed-hooks)))))
      (rcirc-styles-activate))
    (should (equal added-hooks '(rcirc-styles-markup-styles)))
    (should (equal removed-hooks '(rcirc-markup-attributes)))))

