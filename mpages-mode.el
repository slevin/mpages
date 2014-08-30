;;; mpages-mode.el --- A mode for quickly writing your Morning Pages

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
  "Settings for Morning Pages editing"
  :group 'text)

(defcustom mpages-word-threshold 750
  "This threshold is the number of words required before complete."
  :type 'integer)

(defcustom mpages-update-frequency 1
  "How many seconds before recounting your words.
Increasing this number may improve performance."
  :type 'integer)

(defcustom mpages-content-directory ""
  "This is the directory to store Morning Pages documents."
  :type 'directory)

(defvar mpages-mode-start-time)
(defvar mpages-mode-count-timer)

(defun formatted-count (num threshold)
  "Colorize the NUM based on being above/below THRESHOLD."
  (let ((numstr (number-to-string num)))
    (if (< num threshold)
        (propertize numstr 'face '(:foreground "red"))
      (propertize numstr 'face '(:foreground "green")))))

(defun word-count-string (time-elapsed word-count)
  "Generate header line format string for TIME-ELAPSED and WORD-COUNT."
  (concat "Words: "
          (formatted-count word-count mpages-word-threshold)
          "   "
          "Time Elapsed: "
          (tfmt time-elapsed)))

(defun timer-tick ()
  "Run update on header with latest values."
  (let ((word-count (count-words 1 (length (buffer-string))))
        (time-elapsed (time-subtract (current-time) mpages-mode-start-time)))
    (update-word-count time-elapsed word-count)))

;; (defun update-word-count-maybe (start-time)
;;   (if (boundp 'mpages-mode-count-timer)
;;       (update-word-count start-time)))

(defun update-word-count (time-elapsed word-count)
  "Set the generated header line with TIME-ELAPSED and WORD-COUNT."
  (setq header-line-format (word-count-string time-elapsed word-count)))

(defun end-timer-stuff ()
  "Remove all runtime stuff related to this mode."
  (cancel-timer mpages-mode-count-timer)
  (setq header-line-format nil)
  (makunbound 'mpages-mode-count-timer)
  (makunbound 'mpages-mode-start-time))

(defun tfmt (time)
  "Format the TIME for the header."
  (format-time-string "%M:%S" time))

(defun setup-time ()
  "Capture the start time of the mode."
  (setq mpages-mode-start-time (current-time)))

(defun setup-timer ()
  "Start periodic timer to update the header with word count and time."
  (setq mpages-mode-count-timer (run-at-time nil mpages-update-frequency 'timer-tick))
  (add-hook 'kill-buffer-hook 'end-timer-stuff nil t))

(defun open-today ()
  "Open a Morning Pages file for today."
  (find-file (concat "~/wrk/words/" (format-time-string "%Y%m%d") ".txt"))
  (auto-fill-mode)
  (set-fill-column 80))

;; open todays file
(defun mp-today ()
  "Entry point to starting mpages-mode."
  (interactive)
  (open-today)
  (setup-time)
  (setup-timer))

;; (defun testy ()
;;   "Throwaway function for testing."
;;   (interactive)
;;   (update-word-count))

;; maybe function names should be mpages prefix

;; turn it into a mode

;; bigger text
;; disappearing text (make it feel fading away)
;; sorter word wrap to make it feel more like more is coming out

;; make the timer check happen only if the timer has been defined
;; so check for local variable and then do the header format
;; that way switching away won't make anything happen

;; should customizable morning pages directory
;; should ask the first time its run to set that

;;; mpages-mode.el ends here
