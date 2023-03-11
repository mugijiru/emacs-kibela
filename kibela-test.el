(require 'kibela)
(require 'ert)

(ert-deftest test-kibela--store-default-group-success ()
  (setq-local kibela-default-group nil)
  (let ((expect '((id . "TestId") (name . "Test group")))
        (response '((data (defaultGroup (id . "TestId") (name . "Test group"))))))
    (kibela--store-default-group-success :data response)
    (should (equal expect kibela-default-group))))

(ert-deftest test-kibela-store-default-group/success ()
  (let ((kibela-default-group nil)
        (response '((data (defaultGroup (id . "TestId") (name . "Test group"))))))
    (noflet ((kibela--request (query variables success)
                              (apply success :data `(,response))))
      (kibela-store-default-group)
      (should (equal (symbol-value 'kibela-default-group)
                     '((id . "TestId") (name . "Test group")))))))

(ert-deftest test-kibela-build-header-line/from-a-group ()
  (let* ((group '((id . "g1ID") (name . "g1")))
         (groups `(,group))
         (expect "g1")
         (actual (kibela--build-header-line groups)))
    (should (string-equal expect actual))))

(ert-deftest test-kibela-build-header-line/from-groups ()
  (let* ((group1 '((id . "g1ID") (name . "g1")))
         (group2 '((id . "g2ID") (name . "g2")))
         (groups `(,group1, group2))
         (expect "g1 | g2")
         (actual (kibela--build-header-line groups)))
    (should (string-equal expect actual))))

(ert-deftest test-kibela-build-header-line/from-groups-and-folders ()
  (let* ((group1 '((id . "g1ID") (name . "g1")))
         (group2 '((id . "g2ID") (name . "g2")))
         (group3 '((id . "g3ID") (name . "g3")))
         (groups `(,group1, group2, group3))
         (folder1 `((id . "f1-1ID") (folderName . "f1/f1-1") (group . ,group1)))
         (folder2 `((id . "f2-1ID") (folderName . "f2/f2-1") (group . ,group2)))
         (folders `(,folder1 ,folder2))
         (expect "g3 | g1 > f1 > f1-1 | g2 > f2 > f2-1")
         (actual (kibela--build-header-line groups folders)))
    (should (string-equal expect actual))))

(ert-deftest test-kibela-note-new/when-saved-default-group ()
  (let ((kibela-default-group '((id . "TestId") (name . "Saved Test group")))
        (note-title "Test note"))
    (noflet ((kibela--request (query variables success)
                              (error "Unexpected request call")))
      (with-temp-buffer
        (kibela-note-new note-title)
        (should (string-equal (buffer-name) "*Kibela* newnote"))
        (should (string-equal header-line-format "Saved Test group"))
        (should (string-equal (buffer-substring-no-properties (point-min) (point-max)) "# Test note\n\n"))
        (kill-buffer)))))

(ert-deftest test-kibela-note-new/fetch-default-group ()
  (let* ((kibela-default-group nil)
         (group '((id . "TestId") (name . "Fetched Test group")))
         (note-title "Test note"))
    (noflet ((kibela--request (query variables success)
                              (setq kibela-default-group group)))
      (with-temp-buffer
        (kibela-note-new note-title)
        (should (string-equal (buffer-name) "*Kibela* newnote"))
        (should (string-equal header-line-format "Fetched Test group"))
        (should (string-equal (buffer-substring-no-properties (point-min) (point-max)) "# Test note\n\n"))
        (kill-buffer)))))
