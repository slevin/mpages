;;; mpages.el --- A mode for quickly writing your Morning Pages

;; Copyright (C) 2014 Sean Levin

;; Author: Sean Levin
;; Created: 20 Aug 2014
;; Version: 20140824

;;; Commentary:
;; This is the commentary

;;; Change Log:
;; 20140824 First Version


;;; Code:

(defgroup mpages nil
  "Mpages helps with writing a daily Morning Pages style diary."
  :prefix "mpages-"
  :group 'wp)

(defcustom mpages-word-threshold 750
  "This threshold is the number of words required before daily diary is complete."
  :type 'integer
  :group 'mpages)

(defcustom mpages-update-frequency 1
  "How many seconds before recounting your words.
Increasing this number may improve performance."
  :type 'integer
  :group 'mpages)

(defcustom mpages-content-directory nil
  "This is the directory to store your daily Morning Pages documents."
  :type 'directory
  :group 'mpages)

(defvar mpages-start-time)
(defvar mpages-count-timer)

(defun mpages-formatted-count (num threshold)
  "Colorize the NUM based on being above/below THRESHOLD."
  (let ((numstr (number-to-string num)))
    (if (< num threshold)
        (propertize numstr 'face '(:foreground "red"))
      (propertize numstr 'face '(:foreground "green")))))

(defun mpages-word-count-string (time-elapsed word-count)
  "Generate header line format string for TIME-ELAPSED and WORD-COUNT."
  (concat "Words: "
          (mpages-formatted-count word-count mpages-word-threshold)
          "   "
          "Time Elapsed: "
          (mpages-tfmt time-elapsed)))

(defun mpages-timer-tick ()
  "Run update on header with latest values."
  ;; this check makes sure it only runs update if the timer has been
  (if mpages-count-timer
      (let ((word-count (count-words 1 (length (buffer-string))))
            (time-elapsed (time-subtract (current-time) mpages-start-time)))
        (mpages-update-word-count time-elapsed word-count))))

(defun mpages-update-word-count (time-elapsed word-count)
  "Set the generated header line with TIME-ELAPSED and WORD-COUNT."
  (setq header-line-format (mpages-word-count-string time-elapsed word-count)))

(defun mpages-end-timer-stuff ()
  "Remove all runtime stuff related to this mode."
  (cancel-timer mpages-count-timer)
  (setq header-line-format nil)
  (makunbound 'mpages-count-timer)
  (makunbound 'mpages-start-time))

(defun mpages-tfmt (time)
  "Format the TIME for the header."
  (format-time-string "%M:%S" time))

(defun mpages-setup-time ()
  "Capture the start time of the mode."
  (setq mpages-start-time (current-time)))

(defun mpages-setup-timer ()
  "Start periodic timer to update the header with word count and time."
  (setq-local mpages-count-timer (run-at-time nil mpages-update-frequency 'mpages-timer-tick))
  (add-hook 'kill-buffer-hook 'mpages-end-timer-stuff nil t))

(defun open-today ()
  "Open a Morning Pages file for today."
  (find-file (concat (file-name-as-directory mpages-content-directory) (format-time-string "%Y%m%d") ".txt"))
  (auto-fill-mode)
  (set-fill-column 80))

;; open todays file
(defun mpages-today ()
  "Entry point to starting mpages."
  (interactive)
  (if (not mpages-content-directory)
      (customize-save-variable 'mpages-content-directory (file-name-as-directory (read-directory-name "Directory to put your Morning Pages: "))))
  (make-directory mpages-content-directory t) ;; ensure it exists
  (open-today)
  (mpages-setup-time)
  (mpages-setup-timer))

;; (defun testy ()
;;   "Throwaway function for testing."
;;   (interactive)
;;   (makunbound 'mpages-content-directory))

(provide 'mpages)
;;; mpages.el ends here
