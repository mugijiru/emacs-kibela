(require 'kibela)
(require 'ert)
(require 'noflet)

(defmacro kibela-test--use-response-stub (response &rest body)
  (declare (indent defun))
  `(noflet ((request () (error "Unexpected request call")) ;; Don't send request
            (kibela--request (query variables success)
                             (apply success :data `(,response))))
     ,@body))

(ert-deftest test-kibela--store-default-group-success ()
  (setq-local kibela-default-group nil)
  (let ((expect '((id . "TestId") (name . "Test group")))
        (response '((data (defaultGroup (id . "TestId") (name . "Test group"))))))
    (kibela--store-default-group-success :data response)
    (should (equal expect kibela-default-group))))

(ert-deftest test-kibela-store-default-group/success ()
  (let ((kibela-default-group nil)
        (response '((data (defaultGroup (id . "TestId") (name . "Test group"))))))
    (kibela-test--use-response-stub response
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
    (kibela-test--use-response-stub nil
      (with-temp-buffer
        (kibela-note-new note-title)
        (should (string-equal (buffer-name) "*Kibela* newnote"))
        (should (string-equal header-line-format "Saved Test group"))
        (should (string-equal (buffer-substring-no-properties (point-min) (point-max)) "# Test note\n\n"))
        (kill-buffer)))))

(ert-deftest test-kibela-note-new/fetch-default-group ()
  (let* ((kibela-default-group nil)
         (group '((id . "TestId") (name . "Fetched Test group")))
         (default-group (append '(defaultGroup) group))
         (response `((data ,default-group)))
         (note-title "Test note"))
    (kibela-test--use-response-stub response
      (with-temp-buffer
        (kibela-note-new note-title)
        (should (string-equal (buffer-name) "*Kibela* newnote"))
        (should (string-equal header-line-format "Fetched Test group"))
        (should (string-equal (buffer-substring-no-properties (point-min) (point-max)) "# Test note\n\n"))
        (kill-buffer)))))

;;; template tests

(ert-deftest test-kibela-select-note-template-action ()
  "選択した文字列から template property を取得して
kibela--new-note-from-template に渡すことを確認する."
  (let* ((selected-template '(:title "日報 2000/01/01"
                                     :content "# DONE\n\n- [x] \n\n# DOING\n\n- [ ] \n\n# TODO\n\n- [ ] \n\n"
                                     :group-ids '("TestID1" "TestID2")
                                     :groups '(((id "TestID1") (name "Home"))
                                               ((id "TestID2") (name "Private")))
                                     :folders '(((id "FolderID1") (folderName "Folder 1") (groups ((id "TestID1") (name "Home"))) (groupId "Home"))
                                                ((id "FolderID2") (folderName "Folder 2") (groups ((id "TestID2") (name "Home"))) (groupId "Private")))))
         (selected (propertize "日報" 'template selected-template)))
    (noflet ((kibela--new-note-from-template (template)
                                             (should (string-equal "日報 2000/01/01"
                                                                   (plist-get template :title)))))
      (kibela-select-note-template-action selected))))
