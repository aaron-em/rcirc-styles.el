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

(ert-deftest rcirc-styles-tests/propertize-both-colors nil
  "Should propertize a foreground & background color specification correctly."
  (with-style-sandbox "3,2colorful"
    (rcirc-styles-markup-colors)
    (let ((expected (plist-get rcirc-styles-tests/fixtures :green-blue-face))
          (result (get-text-property (point-min) 
                                     rcirc-styles-tests/face-name)))
      (should (cl-equalp result expected)))))

(ert-deftest rcirc-styles-tests/propertize-new-color nil
  "Should propertize implicit color specification correctly."
  (with-style-sandbox "3,2color4ful"
    (rcirc-styles-markup-colors)
    (let ((expected (plist-get rcirc-styles-tests/fixtures :blue-red-face))
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
    (let ((result (get-text-property (+ (point-min) 6) 
                                     rcirc-styles-tests/face-name))
          (expected nil))
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
    (let ((expected (get-text-property (+ (point-min) 4) 
                                       rcirc-styles-tests/face-name))
          (result (plist-get rcirc-styles-tests/fixtures :bold-inverse-face)))
      (should (cl-equalp result expected)))))

(ert-deftest rcirc-styles-tests/propertize-adjacent-attr-color nil
  "Should propertize adjacent attribute and color specifications correctly."
  (with-style-sandbox "3emboldened"
    (rcirc-styles-markup-styles)
    (let ((expected (get-text-property (+ (point-min) 1) 
                                       rcirc-styles-tests/face-name))
          (result (plist-get rcirc-styles-tests/fixtures :green-face)))
      (should (cl-equalp result expected)))))
