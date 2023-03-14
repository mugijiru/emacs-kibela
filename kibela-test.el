(require 'kibela)
(require 'ert)
(require 'noflet)
(require 'with-simulated-input)

(defmacro kibela-test--use-response-stub (res &rest body)
  (declare (indent defun))
  `(noflet ((request () (error "Unexpected request call")) ;; Don't send request
            (kibela--request (query variables success)
                             (apply success :data `(((data (,@ (,@ res))))))))
     ,@body))

;; store default group

(ert-deftest test-kibela--store-default-group-success ()
  (setq-local kibela-default-group nil)
  (let ((expect '((id . "TestId") (name . "Test group")))
        (response '((data (defaultGroup (id . "TestId") (name . "Test group"))))))
    (kibela--store-default-group-success :data response)
    (should (equal expect kibela-default-group))))

(ert-deftest test-kibela-store-default-group/success ()
  (let ((kibela-default-group nil)
        (response '(defaultGroup (id . "TestId") (name . "Test group"))))
    (kibela-test--use-response-stub response
      (kibela-store-default-group)
      (should (equal (symbol-value 'kibela-default-group)
                     '((id . "TestId") (name . "Test group")))))))

;; header line

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

;; list

;; (ert-deftest test-kibela-group-notes-refresh/per-page ()
;;   (let ((kibela-per-page 100))
;;     (kibela-test--inspect-request-arguments
;;       (let* ((result (kibela-group-notes-refresh))
;;              (query (first result))
;;              (variables (second result))
;;              (per-page (assoc-default 'per-page variables)))
;;         (should (equal per-page 100)))))

;;   (let ((kibela-per-page 20))
;;     (kibela-test--inspect-request-arguments
;;       (let* ((result (kibela-group-notes-refresh))
;;              (query (first result))
;;              (variables (second result))
;;              (per-page (assoc-default 'per-page variables)))
;;         (should (equal per-page 20))))))

(ert-deftest test-kibela-group-notes ()
  (let* ((default-group '((id . "GroupId") (name . "Test group")))
         (kibela-default-group default-group)
         (response '(group
                     (id . "GroupId")
                     (name . "Test group")
                     (notes
                      (edges . [((node
                                  (id . "NoteId1")
                                  (title . "Test note1")
                                  (contentUpdatedAt . "2000-01-01T00:00:00.000+09:00")))
                                ((node
                                  (id . "NoteId2")
                                  (title . "Test note2")
                                  (contentUpdatedAt . "2000-01-02T00:00:00.000+09:00")))]
                             ))))
         (expected-buffer-name "*Kibela* notes in Test group"))
    (kibela-test--use-response-stub response
      (kibela-group-notes)
      (should (string-equal expected-buffer-name (buffer-name)))
      (should (string-equal major-mode "kibela-list-mode"))
      (let ((content (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match "Test note1" content))
        (should (string-match "2000-01-01" content))
        (should (string-match "Test note2" content))
        (should (string-match "2000-01-02" content))))

    ;; Show note
    (let ((response '(note (id . "NoteID2")
                           (title . "Test note2")
                           (content . "posted content2")
                           (coediting . t)
                           (groups . (((id . "GroupId")
                                       (name . "Test group"))))
                           (folders . ()))))
      (kibela-test--use-response-stub response
        (goto-char (point-min))
        (execute-kbd-macro (read-kbd-macro "RET")) ;; type RET
        (should (string-equal "*Kibela* NoteId2" (buffer-name)))
        (should (string-equal "kibela-markdown-view-mode" major-mode))
        (should (string-equal "# Test note2\n\nposted content2"
                              (buffer-substring-no-properties (point-min) (point-max))))))

    ;; Tear down
    (kill-buffer "*Kibela* NoteId2")
    (kill-buffer expected-buffer-name)))

;; note-new

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
         (note-title "Test note"))
    (kibela-test--use-response-stub default-group
      (with-temp-buffer
        (kibela-note-new note-title)
        (should (string-equal (buffer-name) "*Kibela* newnote"))
        (should (string-equal header-line-format "Fetched Test group"))
        (should (string-equal (buffer-substring-no-properties (point-min) (point-max)) "# Test note\n\n"))
        (kill-buffer)))))

;;; template tests

(ert-deftest test-kibela-build-collection-from-note-templates ()
  (let* ((expected `(,(propertize
                       "日報"
                       'template
                       '(:title "日報 2000/01/01"
                               :content "# DONE\n\n- [x] \n\n# DOING\n\n- [ ] \n\n# TODO\n\n- [ ] \n\n"
                               :group-ids ("TestID1")
                               :groups (((id "TestID1") (name "Home")))
                               :folders ()))
                     ,(propertize
                       "定例 MTG"
                       'template
                       '(:title "定例 MTG 2000/01/01"
                               :content "# 参加者\n\n - (参加者名) # アジェンダ\n\n- \n\n# \n\n- [ ] \n\n# 前回の宿題\n\n- [ ] \n\n# 議題\n\n# 宿題\n\n"
                               :group-ids ("TestID1")
                               :groups (((id "TestID1") (name "Home")))
                               :folders (((id "FolderID1") (folderName "議事録/定例 MTG") (groups ((id "TestID1") (name "Home"))) (groupId "Home")))))))
         (template1 '((name . "日報")
                      (evaluatedTitle . "日報 2000/01/01")
                      (content . "# DONE\n\n- [x] \n\n# DOING\n\n- [ ] \n\n# TODO\n\n- [ ] \n\n")
                      (group-ids . ("TestID1"))
                      (groups . (((id "TestID1") (name "Home"))))
                      (folders . ())))
         (template2 '((name . "定例 MTG")
                      (evaluatedTitle . "定例 MTG 2000/01/01")
                      (content . "# 参加者\n\n - (参加者名) # アジェンダ\n\n- \n\n# \n\n- [ ] \n\n# 前回の宿題\n\n- [ ] \n\n# 議題\n\n# 宿題\n\n")
                      (group-ids . ("TestID1"))
                      (groups . (((id "TestID1") (name "Home"))))
                      (folders . (((id "FolderID1")
                                  (evaluatedFullName "議事録/定例 MTG")
                                  (groups ((id "TestID1")
                                           (name "Home"))))))))
         (templates `(,template1 ,template2))
         (actual (kibela-build-collection-from-note-templates templates)))
    (should (seq-set-equal-p expected actual
                             (lambda (elt1 elt2)
                               (let* ((template1 (get-text-property 0 'template elt1))
                                      (title1 (plist-get template1 :title))
                                      (template2 (get-text-property 0 'template elt2))
                                      (title2 (plist-get template2 :title)))
                                 (and (string-equal elt1 elt2)
                                      (string-equal title1 title2))))))))

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

(ert-deftest test-kibela--new-note-from-template ()
  (let* ((template '(:title "日報 2000/01/01"
                            :content "# DONE\n\n- [x] \n\n# DOING\n\n- [ ] \n\n# TODO\n\n- [ ] \n\n"
                            :group-ids '("TestID1" "TestID2")
                            :groups (((id . "TestID1") (name . "Home"))
                                     ((id . "TestID2") (name . "Private")))
                            :folders (((id . "FolderID1")
                                       (folderName . "Folder 1")
                                       (group . ((id . "TestID1")
                                                  (name . "Home")))
                                       (groupId . "Home"))
                                      ((id . "FolderID2")
                                       (folderName . "Folder 2")
                                       (group . ((id . "TestID2")
                                                  (name . "Private")))
                                       (groupId . "Private"))))))
      (with-temp-buffer
        (kibela--new-note-from-template template)
        (should (string-equal (buffer-name) "*Kibela* newnote"))
        (should (string-equal header-line-format "Home > Folder 1 | Private > Folder 2"))
        (should (string-equal (buffer-substring-no-properties (point-min) (point-max))
                              "# 日報 2000/01/01\n\n# DONE\n\n- [x] \n\n# DOING\n\n- [ ] \n\n# TODO\n\n- [ ] \n\n"))
        (kill-buffer) ;; FIXME: expect always executed but its only execute on success
        )))

(ert-deftest test-kibela-note-new-from-template ()
  (let* ((response '(noteTemplates (edges . [((node
                                               (id . "TestId1")
                                               (name . "foo")
                                               (title . "Foo title")
                                               (evaluatedTitle . "Foo title")
                                               (content . "")
                                               (groups . [((id . "GroupId")
                                                           (name . "Home"))])
                                               (folders . [])))
                                             ((node
                                               (id . "TestId2")
                                               (name . "bar")
                                               (title . "Bar title")
                                               (evaluatedTitle . "Bar title")
                                               (content . "")
                                               (groups . [((id . "GroupId")
                                                           (name . "Home"))])
                                               (folders . [])))])))
         (completing-read-function #'completing-read-default))
    (kibela-test--use-response-stub response
      (with-temp-buffer
        (with-simulated-input "ba TAB RET"
          (kibela-note-new-from-template)
          (should (string-equal (buffer-name) "*Kibela* newnote"))
          (should (string-equal header-line-format "Home"))
          (should (string-equal (buffer-substring-no-properties (point-min) (point-max))
                                "# Bar title\n\n")))
        (kill-buffer) ;; FIXME: expect always executed but its only execute on success
        ))))

;; show

(ert-deftest test-kibela-note-show ()
  (let* ((response '(note (id . "NoteID")
                          (title . "posted note")
                          (content . "posted content")
                          (coediting . t)
                          (groups . (((id . "GroupID1")
                                      (name . "Home"))))
                          (folders . (((id . "FolderID1")
                                       (fullName . "foo/bar")
                                       (group . ((id . "GroupID1")
                                                 (name . "Home")))))))))
    (kibela-test--use-response-stub response
      (with-temp-buffer
        (kibela-note-show "NoteID")
        (should (string-equal major-mode "kibela-markdown-view-mode"))
        (should (string-equal (buffer-name) "*Kibela* NoteID"))
        (should (string-equal (buffer-substring-no-properties (point-min) (point-max))
                              "# posted note\n\nposted content"))
        (should (string-equal header-line-format "Home > foo > bar"))

        (kill-buffer)) ;; FIXME: expect always executed but its only execute on success
      )))
