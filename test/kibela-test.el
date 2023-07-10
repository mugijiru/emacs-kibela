(require 'kibela)
(require 'ert)
(require 'ert-x)
(require 'noflet)
(require 'with-simulated-input)

(defmacro kibela-test--use-response-stub (res &rest body)
  (declare (indent defun))
  `(noflet ((request () (error "Unexpected request call")) ;; Don't send request
            (kibela--request (query variables success)
                             (apply success :data `(((data (,@ (,@ res))))))))
     ,@body))

(defmacro kibela-test--inspect-request-arguments (&rest body)
  (declare (indent defun))
  `(noflet ((request () (error "Unexpected request call")) ;; Don't send request
            (kibela--request (query variables success)
                             `(,query ,variables))) ;; FIXME: response argument is unused
     ,@body))

;; switch team

(ert-deftest test-kibela-switch-team/select-team ()
  (let* ((kibela-default-group '((id . "TestId") (name . "Test group")))
         (kibela-auth-list '(("Foo" "foo" "secret/TokenA")
                              ("Personal" "my-team" "secret/TokenB")))
         (kibela-team nil)
         (kibela-access-token nil))
    (with-simulated-input "Per TAB RET"
      (kibela-switch-team)
      (should (string-equal "my-team" kibela-team))
      (should (string-equal "secret/TokenB" kibela-access-token))
      (should (equal nil kibela-default-group)))))

(ert-deftest test-kibela-switch-team/no-changes-when-no-match ()
  (let* ((captured-message nil)
         (kibela-default-group '((id . "TestId") (name . "Test group")))
         (kibela-auth-list '(("Foo" "foo" "secret/TokenA")
                              ("Personal" "my-team" "secret/TokenB")))
         (kibela-team "Foo")
         (kibela-access-token "secret/TokenA"))
    (with-simulated-input "zzzzz RET"
      (ert-with-message-capture captured-message
        (kibela-switch-team)
        (should (string-equal "No match team.\n" captured-message))))
    (should (string-equal "Foo" kibela-team))
    (should (string-equal "secret/TokenA" kibela-access-token))
    (should (equal '((id . "TestId") (name . "Test group"))
                   kibela-default-group))))

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

(ert-deftest test-kibela-graphql-query-group-notes-prev ()
  (let ((expected (graphql-query
                   (:arguments (($id . ID!) ($perPage . Int!) ($cursor . String))
                               (group
                                :arguments ((id . ($ id)))
                                (notes
                                 :arguments((last . ($ perPage))
                                            (before . ($ cursor))
                                            (orderBy . ((field . CONTENT_UPDATED_AT) (direction . DESC))))
                                 (pageInfo
                                  hasNextPage
                                  hasPreviousPage)
                                 (edges
                                  cursor
                                  (node
                                   id
                                   title
                                   content
                                   contentUpdatedAt
                                   coediting
                                   canBeUpdated
                                   url))))))))
    (should (equal expected kibela-graphql-query-group-notes-prev))))

(ert-deftest test-kibela-graphql-query-group-notes-next ()
  (let ((expected (graphql-query
                   (:arguments (($id . ID!) ($perPage . Int!) ($cursor . String))
                               (group
                                :arguments ((id . ($ id)))
                                (notes
                                 :arguments((first . ($ perPage))
                                            (after . ($ cursor))
                                            (orderBy . ((field . CONTENT_UPDATED_AT) (direction . DESC))))
                                 (pageInfo
                                  hasNextPage
                                  hasPreviousPage)
                                 (edges
                                  cursor
                                  (node
                                   id
                                   title
                                   content
                                   contentUpdatedAt
                                   coediting
                                   canBeUpdated
                                   url))))))))
    (should (equal expected kibela-graphql-query-group-notes-next))))

(ert-deftest test-kibela-group-notes-refresh/per-page ()
  (let ((kibela-per-page 100))
    (kibela-test--inspect-request-arguments
      (let* ((result (kibela-group-notes-refresh))
             (query (cl-first result))
             (variables (cl-second result))
             (per-page (assoc-default 'perPage variables)))
        (should (equal per-page 100)))))

  (let ((kibela-per-page 20))
    (kibela-test--inspect-request-arguments
      (let* ((result (kibela-group-notes-refresh))
             (query (cl-first result))
             (variables (cl-second result))
             (per-page (assoc-default 'perPage variables)))
        (should (equal per-page 20))))))

(ert-deftest test-kibela-group-notes ()
  (let* ((default-group '((id . "GroupId") (name . "Test group")))
         (kibela-default-group default-group)
         (kibela-team "dummy")
         (kibela-access-token "dummy")
         (response '(group
                     (id . "GroupId")
                     (name . "Test group")
                     (notes
                      (edges . [((cursor . "A")
                                 (node
                                  (id . "NoteId1")
                                  (title . "Test note1")
                                  (contentUpdatedAt . "2000-01-01T00:00:00.000+09:00")))
                                ((cursor . "B")
                                 (node
                                  (id . "NoteId2")
                                  (title . "Test note2")
                                  (contentUpdatedAt . "2000-01-02T00:00:00.000+09:00")))])
                      (pageInfo
                       (hasPreviousPage . :json-false)
                       (hasNextPage . :json-false)))))
         (expected-buffer-name "*Kibela* notes in Test group"))
    (kibela-test--use-response-stub response
      (kibela-group-notes)
      (should (string-equal expected-buffer-name (buffer-name)))
      (should (string-equal major-mode "kibela-list-mode"))
      (let ((content (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "Test note1" content))
        (should (string-match-p "2000-01-01" content))
        (should (string-match-p "Test note2" content))
        (should (string-match-p "2000-01-02" content))))

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

(ert-deftest test-kibela-recent-browsing-notes ()
  (let* ((kibela-team "dummy")
         (kibela-access-token "dummy")
         (response '(noteBrowsingHistories
                     (edges . [((cursor . "A")
                                (node
                                 (note
                                  (id . "NoteId1")
                                  (title . "Test note1")
                                  (contentUpdatedAt . "2000-01-01T00:00:00.000+09:00"))))
                               ((cursor . "B")
                                (node
                                 (note
                                  (id . "NoteId2")
                                  (title . "Test note2")
                                  (contentUpdatedAt . "2000-01-02T00:00:00.000+09:00"))))])
                      (pageInfo
                       (hasPreviousPage . :json-false)
                       (hasNextPage . :json-false))))
         (expected-buffer-name "*Kibela* recent browsing notes"))
    (kibela-test--use-response-stub response
      (kibela-recent-browsing-notes)
      (should (string-equal expected-buffer-name (buffer-name)))
      (should (string-equal major-mode "kibela-recent-browsing-notes-mode"))
      (let ((content (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "Test note1" content))
        (should (string-match-p "2000-01-01" content))
        (should (string-match-p "Test note2" content))
        (should (string-match-p "2000-01-02" content))))))

;; note-new

(ert-deftest test-kibela-note-new/when-saved-default-group ()
  (let ((kibela-default-group '((id . "TestId") (name . "Saved Test group")))
        (kibela-team "dummy")
        (kibela-access-token "dummy")
        (note-title "Test note"))
    (kibela-test--use-response-stub nil
      (with-temp-buffer
        (kibela-note-new note-title)
        (should (string-equal (buffer-name) "*Kibela* newnote"))
        (should (string-equal header-line-format "Saved Test group"))
        (should (string-equal (buffer-substring-no-properties (point-min) (point-max)) "# Test note\n\n"))
        (kill-buffer)))))

(ert-deftest test-kibela-note-new/fetch-default-group ()
  (let* ((kibela-team "dummy")
         (kibela-access-token "dummy")
         (kibela-default-group nil)
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
  "kibela-build-collection-from-note-templates が
取得したテンプレート一覧からテンプレート名を抽出し
その他の属性を property としてそこに詰めていることをテストしている"
  (let* ((expected `(,(propertize
                       "日報"
                       'template
                       '(:title "日報 2000/01/01"
                                :content "# DONE\n\n- [x] \n\n# DOING\n\n- [ ] \n\n# TODO\n\n- [ ] \n\n"
                                :coeding nil
                                :group-ids ("TestID1")
                                :groups (((id "TestID1") (name "Home")))
                                :folders ()))
                     ,(propertize
                       "定例 MTG"
                       'template
                       '(:title "定例 MTG 2000/01/01"
                                :content "# 参加者\n\n - (参加者名) # アジェンダ\n\n- \n\n# \n\n- [ ] \n\n# 前回の宿題\n\n- [ ] \n\n# 議題\n\n# 宿題\n\n"
                                :coeding t
                                :group-ids ("TestID1")
                                :groups (((id "TestID1") (name "Home")))
                                :folders (((id "FolderID1") (folderName "議事録/定例 MTG") (groups ((id "TestID1") (name "Home"))) (groupId "Home")))))))
         (template1 '((name . "日報")
                      (evaluatedTitle . "日報 2000/01/01")
                      (content . "# DONE\n\n- [x] \n\n# DOING\n\n- [ ] \n\n# TODO\n\n- [ ] \n\n")
                      (coeding . nil)
                      (group-ids . ("TestID1"))
                      (groups . (((id "TestID1") (name "Home"))))
                      (folders . ())))
         (template2 '((name . "定例 MTG")
                      (evaluatedTitle . "定例 MTG 2000/01/01")
                      (content . "# 参加者\n\n - (参加者名) # アジェンダ\n\n- \n\n# \n\n- [ ] \n\n# 前回の宿題\n\n- [ ] \n\n# 議題\n\n# 宿題\n\n")
                      (coeding . t)
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
                                      (string-equal title1 title2)
                                      (string-equal (plist-get template1 :content)
                                                    (plist-get template2 :content))
                                      ;; (equal (plist-get template1 :group-ids)
                                      ;;        (plist-get template2 :group-ids))
                                      ;; (equal (plist-get template1 :groups)
                                      ;;        (plist-get template2 :groups))
                                      ;; (equal (plist-get template1 :folders)
                                      ;;        (plist-get template2 :folders))
                                      (equal (plist-get template1 :coediting)
                                             (plist-get template2 :coediting))
                                      )))))))

(ert-deftest test-kibela-select-note-template-action ()
  "選択した文字列から template property を取得して
kibela--new-note-from-template に渡すことを確認する."
  (let* ((selected-template '(:title "日報 2000/01/01"
                                     :content "# DONE\n\n- [x] \n\n# DOING\n\n- [ ] \n\n# TODO\n\n- [ ] \n\n"
                                     :coediting t
                                     :group-ids '("TestID1" "TestID2")
                                     :groups '(((id "TestID1") (name "Home"))
                                               ((id "TestID2") (name "Private")))
                                     :folders '(((id "FolderID1") (folderName "Folder 1") (groups ((id "TestID1") (name "Home"))) (groupId "Home"))
                                                ((id "FolderID2") (folderName "Folder 2") (groups ((id "TestID2") (name "Home"))) (groupId "Private")))))
         (selected (propertize "日報" 'template selected-template)))
    (noflet ((kibela--new-note-from-template (template)
                                             (should (string-equal "日報 2000/01/01"
                                                                   (plist-get template :title)))
                                             (should (equal t (plist-get template :coediting)))))
      (kibela-select-note-template-action selected))))

(ert-deftest test-kibela--new-note-from-template ()
  "kibela-note-new-from-template の内部で実行される kibela--new-note-from-template のテスト.
この関数は渡された template の内容を元に新規ノート用のバッファを作成し
kibela-note-template に渡された template の情報を格納する."
  (let* ((template '(:title "日報 2000/01/01"
                            :content "# DONE\n\n- [x] \n\n# DOING\n\n- [ ] \n\n# TODO\n\n- [ ] \n\n"
                            :group-ids '("TestID1" "TestID2")
                            :coediting nil
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

        (should (not (null (plist-member kibela-note-template :coediting))))
        (should (equal (plist-get kibela-note-template :coediting) nil))
        (kill-matching-buffers "^\\*Kibela\\*" nil t) ;; FIXME: expect always executed but its only execute on success
        )))

(ert-deftest test-kibela-note-new-from-template ()
  "kibela-note-new-from-template を実行した際に
kibela からのレスポンスを completing-read で絞り込んで
そのテンプレートの情報を buffer にセットすることをテストしている"
  (let* ((kibela-team "dummy")
         (kibela-access-token "dummy")
         (response '(noteTemplates (edges . [((node
                                               (id . "TestId1")
                                               (name . "foo")
                                               (title . "Foo title")
                                               (evaluatedTitle . "Foo title")
                                               (content . "")
                                               (coediting . json-false)
                                               (groups . [((id . "GroupId")
                                                           (name . "Home"))])
                                               (folders . [])))
                                             ((node
                                               (id . "TestId2")
                                               (name . "bar")
                                               (title . "Bar title")
                                               (evaluatedTitle . "Bar title")
                                               (content . "")
                                               (coediting . t)
                                               (groups . [((id . "GroupId")
                                                           (name . "Home"))])
                                               (folders . [])))])))
         (completing-read-function #'completing-read-default))
    (kibela-test--use-response-stub response
      (with-temp-buffer
        (with-simulated-input "ba TAB RET"
          (kibela-note-new-from-template)
          (should (string-equal header-line-format "Home"))
          (should (string-equal (buffer-name) "*Kibela* newnote"))
          (should (string-equal header-line-format "Home"))
          (should (string-equal (buffer-substring-no-properties (point-min) (point-max))
                                "# Bar title\n\n")))
        (kill-matching-buffers "^\\*Kibela\\*" nil t) ;; FIXME: expect always executed but its only execute on success
        ))))

;; show

(ert-deftest test-kibela-note-show ()
  (let* ((kibela-team "dummy")
         (kibela-access-token "dummy")
         (response '(note (id . "NoteID")
                          (title . "posted note")
                          (content . "posted content")
                          (coediting . t)
                          (isLikedByCurrentUser . t)
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
        (should (string-equal header-line-format "♥ | Home > foo > bar"))

        (kill-buffer)) ;; FIXME: expect always executed but its only execute on success
      )))

(ert-deftest test-kibea-note-show--unliked-note ()
  (let* ((kibela-team "dummy")
         (kibela-access-token "dummy")
         (response '(note (id . "NoteID")
                          (title . "posted note")
                          (content . "posted content")
                          (coediting . t)
                          (isLikedByCurrentUser . nil)
                          (groups . (((id . "GroupID1")
                                      (name . "Home"))))
                          (folders . (((id . "FolderID1")
                                       (fullName . "foo/bar")
                                       (group . ((id . "GroupID1")
                                                 (name . "Home")))))))))
    (kibela-test--use-response-stub response
      (kibela-note-show "NoteID")))
  (should (string-equal major-mode "kibela-markdown-view-mode"))
  (should (string-equal (buffer-name) "*Kibela* NoteID"))
  (should (string-equal (buffer-substring-no-properties (point-min) (point-max))
                        "# posted note\n\nposted content"))
  (should (string-equal header-line-format "♡ | Home > foo > bar"))
  (kill-matching-buffers "^\\*Kibela\\*" nil t)) ;; FIXME: expect always executed but its only execute on success

;; create

(ert-deftest test-kibela-note-create-with-not-coediting-template ()
  "テンプレート経由で共同編集無効の記事を書く場合のテスト."

  (let* ((kibela-team "dummy")
         (kibela-access-token "dummy")
         (template '(:title "posted title"
                            :content "posted content"
                            :coediting nil
                            :group-ids ("GroupID1")
                            :groups (((id . "GroupID1") (name . "Home")))
                            :folders ())))

    (with-temp-buffer
      (kibela--new-note-from-template template)
      (kibela-test--inspect-request-arguments
       (let* ((result (kibela-note-create))
              (query (cl-first result))
              (variables (cl-second result))
              (input (assoc-default 'input variables)))
         (should (string-equal (assoc-default 'title input) "posted title"))
         (should (string-equal (assoc-default 'content input) "posted content"))
         (should (equal (assoc-default 'coediting input) json-false))
         (should (equal (assoc-default 'groupIds input) '("GroupID1")))
         (should (equal (assoc-default 'folders input) '()))
         ))))
  (kill-matching-buffers "^\\*Kibela\\*" nil t))

(ert-deftest test-kibela-note-create-with-coediting-template ()
  "テンプレート経由で記事を書く場合の挙動をテストする"

  (let* ((kibela-team "dummy")
         (kibela-access-token "dummy")
         (template '(:title "posted title"
                            :content "posted content"
                            :coediting t
                            :group-ids ("GroupID1")
                            :groups (((id . "GroupID1") (name . "Home")))
                            :folders ())))

    (with-temp-buffer
      (kibela--new-note-from-template template)
      (kibela-test--inspect-request-arguments
       (let* ((result (kibela-note-create))
              (query (cl-first result))
              (variables (cl-second result))
              (input (assoc-default 'input variables)))
         (should (string-equal (assoc-default 'title input) "posted title"))
         (should (string-equal (assoc-default 'content input) "posted content"))
         (should (equal (assoc-default 'coediting input) t))
         (should (equal (assoc-default 'groupIds input) '("GroupID1")))
         (should (equal (assoc-default 'folders input) '()))))))
  (kill-matching-buffers "^\\*Kibela\\*" nil t))
