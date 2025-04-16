;; -*- lexical-binding: t; -*-

(when (require 'undercover nil t)
  (undercover "*.el"
              (:send-report nil)
              (:report-format 'simplecov)
              (:report-file "coverage/coverage.json")))

(require 'kibela)
