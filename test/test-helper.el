(when (require 'undercover nil t)
  (undercover "*.el"
              (:send-report nil)
              (:report-format 'simplecov)
              (:report-file "tmp/rspec_results.json")))

(require 'kibela)
