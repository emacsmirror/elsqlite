;;; elsqlite-test.el --- Tests for ELSQLite -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dusan Popovic

;;; Commentary:

;; Test suite for ELSQLite using ERT (Emacs Lisp Regression Testing).

;;; Code:

(require 'ert)
(require 'elsqlite)
(require 'elsqlite-db)
(require 'elsqlite-sql)
(require 'elsqlite-table)

(defvar elsqlite-test-db-path
  (expand-file-name "test.db"
                    (file-name-directory
                     (or load-file-name buffer-file-name)))
  "Path to test database.")

;;; Database Operations Tests

(ert-deftest elsqlite-test-db-open ()
  "Test opening a SQLite database."
  (skip-unless (sqlite-available-p))
  (let ((db (sqlite-open elsqlite-test-db-path)))
    (should db)
    (should (sqlite-available-p))
    (sqlite-close db)))

(ert-deftest elsqlite-test-schema-cache ()
  "Test schema caching functionality."
  (skip-unless (sqlite-available-p))
  (let ((db (sqlite-open elsqlite-test-db-path)))
    (unwind-protect
        (progn
          (elsqlite-db-cache-schema db)
          (let ((schema elsqlite-db--schema-cache))
            (should schema)
            (should (alist-get 'tables schema))
            (should (alist-get 'views schema))
            (should (alist-get 'indexes schema))

            ;; Check that tables are present
            (let ((tables (alist-get 'tables schema)))
              (should (assoc "users" tables))
              (should (assoc "posts" tables)))

            ;; Check that view is present
            (let ((views (alist-get 'views schema)))
              (should (member "active_users" views)))))
      (sqlite-close db))))

(ert-deftest elsqlite-test-get-tables ()
  "Test retrieving table names."
  (skip-unless (sqlite-available-p))
  (let ((db (sqlite-open elsqlite-test-db-path)))
    (unwind-protect
        (progn
          (let ((tables (elsqlite-db-get-tables db)))
            (should (member "users" tables))
            (should (member "posts" tables))
            (should-not (member "sqlite_master" tables))))
      (sqlite-close db))))

(ert-deftest elsqlite-test-get-table-columns ()
  "Test retrieving table column information."
  (skip-unless (sqlite-available-p))
  (let ((db (sqlite-open elsqlite-test-db-path)))
    (unwind-protect
        (progn
          (let ((columns (elsqlite-db-get-table-columns db "users")))
            (should (assoc "id" columns))
            (should (assoc "name" columns))
            (should (assoc "email" columns))
            (should (assoc "age" columns))
            ;; Check column types
            (should (equal (cdr (assoc "id" columns)) "INTEGER"))
            (should (equal (cdr (assoc "name" columns)) "TEXT"))))
      (sqlite-close db))))

(ert-deftest elsqlite-test-count-rows ()
  "Test counting rows in a table."
  (skip-unless (sqlite-available-p))
  (let ((db (sqlite-open elsqlite-test-db-path)))
    (unwind-protect
        (progn
          (should (= (elsqlite-db-count-rows db "users") 4))
          (should (= (elsqlite-db-count-rows db "posts") 4)))
      (sqlite-close db))))

(ert-deftest elsqlite-test-select ()
  "Test selecting data from database."
  (skip-unless (sqlite-available-p))
  (let ((db (sqlite-open elsqlite-test-db-path)))
    (unwind-protect
        (progn
          (let ((rows (elsqlite-db-select db "SELECT name FROM users ORDER BY name")))
            (should (= (length rows) 4))
            (should (equal (car (car rows)) "Alice Smith")))

          ;; Test with parameters
          (let ((rows (elsqlite-db-select db "SELECT name FROM users WHERE age > ?" '(25))))
            (should (= (length rows) 2))))
      (sqlite-close db))))

(ert-deftest elsqlite-test-select-full ()
  "Test selecting data with column names."
  (skip-unless (sqlite-available-p))
  (let ((db (sqlite-open elsqlite-test-db-path)))
    (unwind-protect
        (progn
          (let* ((result (elsqlite-db-select-full db "SELECT id, name FROM users LIMIT 1"))
                 (columns (car result))
                 (rows (cdr result)))
            (should (equal columns '("id" "name")))
            (should (= (length rows) 1))))
      (sqlite-close db))))

;;; Query Analysis Tests

(ert-deftest elsqlite-test-query-is-editable ()
  "Test query editability detection."
  ;; Simple SELECT - editable
  (should (elsqlite-db-query-is-editable-p "SELECT * FROM users"))
  (should (elsqlite-db-query-is-editable-p "SELECT id, name FROM users WHERE age > 21"))
  (should (elsqlite-db-query-is-editable-p "  select * from users  "))

  ;; Complex queries - not editable
  (should-not (elsqlite-db-query-is-editable-p "SELECT DISTINCT name FROM users"))
  (should-not (elsqlite-db-query-is-editable-p "SELECT COUNT(*) FROM users"))
  (should-not (elsqlite-db-query-is-editable-p "SELECT * FROM users GROUP BY age"))
  (should-not (elsqlite-db-query-is-editable-p "SELECT * FROM users JOIN posts ON users.id = posts.user_id"))
  (should-not (elsqlite-db-query-is-editable-p "SELECT * FROM (SELECT * FROM users)")))

(ert-deftest elsqlite-test-extract-table-name ()
  "Test extracting table name from simple queries."
  (should (equal (elsqlite-db-extract-table-name "SELECT * FROM users")
                 "users"))
  (should (equal (elsqlite-db-extract-table-name "SELECT id FROM posts WHERE id > 1")
                 "posts"))

  ;; Complex queries should return nil
  (should-not (elsqlite-db-extract-table-name "SELECT * FROM users JOIN posts"))
  (should-not (elsqlite-db-extract-table-name "SELECT COUNT(*) FROM users")))

;;; SQL Completion Context Tests

(ert-deftest elsqlite-test-completion-context ()
  "Test SQL completion context detection."
  (with-temp-buffer
    (elsqlite-sql-mode)

    ;; Test statement context
    (insert "SEL")
    (should (eq (elsqlite-sql--get-completion-context) 'statement))
    (erase-buffer)

    ;; Test table context (after FROM)
    (insert "SELECT * FROM us")
    (should (eq (elsqlite-sql--get-completion-context) 'table))
    (erase-buffer)

    ;; Test column context (after SELECT)
    (insert "SELECT na")
    (should (eq (elsqlite-sql--get-completion-context) 'select-column))
    (erase-buffer)

    ;; Test WHERE context
    (insert "SELECT * FROM users WHERE ag")
    (should (eq (elsqlite-sql--get-completion-context) 'where-column))
    (erase-buffer)

    ;; Test ORDER BY context
    (insert "SELECT * FROM users ORDER BY na")
    (should (eq (elsqlite-sql--get-completion-context) 'order-column))))

;;; Value Formatting Tests

(ert-deftest elsqlite-test-format-value ()
  "Test value formatting for display."
  (should (equal (elsqlite-table--format-value nil) "NULL"))
  (should (equal (elsqlite-table--format-value "text") "text"))
  (should (equal (elsqlite-table--format-value 42) "42"))
  (should (equal (elsqlite-table--format-value 3.14) "3.14"))

  ;; Test BLOB formatting
  (let ((blob (make-vector 500 0)))
    (should (string-match-p "^<BLOB:500 bytes>$" (elsqlite-table--format-value blob))))

  ;; Test long string truncation
  (let ((long-string (make-string 200 ?x))
        (elsqlite-max-column-width 100))
    (let ((result (elsqlite-table--format-value long-string)))
      (should (= (length result) 103))  ; 100 chars + "..."
      (should (string-suffix-p "..." result)))))

;;; Integration Tests

(ert-deftest elsqlite-test-full-workflow ()
  "Test complete workflow: open DB, browse schema, view table."
  (skip-unless (sqlite-available-p))
  (let ((db (sqlite-open elsqlite-test-db-path)))
    (unwind-protect
        (with-temp-buffer
          (elsqlite-table-mode)
          (setq elsqlite--db db
                elsqlite--db-file elsqlite-test-db-path)

          ;; Test schema view
          (elsqlite-table-show-schema)
          (should (eq elsqlite-table--view-type 'schema))
          (should-not elsqlite-table--editable-p)
          (should tabulated-list-entries)

          ;; Test table view
          (elsqlite-table-show-table "users")
          (should (eq elsqlite-table--view-type 'table))
          (should (equal elsqlite-table--current-table "users"))
          (should elsqlite-table--editable-p)
          (should (= elsqlite-table--rows-loaded 4))
          (should tabulated-list-entries))
      (sqlite-close db))))

(ert-deftest elsqlite-test-query-execution ()
  "Test executing custom queries."
  (skip-unless (sqlite-available-p))
  (let ((db (sqlite-open elsqlite-test-db-path)))
    (unwind-protect
        (with-temp-buffer
          (elsqlite-table-mode)
          (setq elsqlite--db db
                elsqlite--db-file elsqlite-test-db-path)

          ;; Execute simple query
          (elsqlite-table-execute-query "SELECT name, age FROM users WHERE age > 25")
          (should (eq elsqlite-table--view-type 'table))
          (should elsqlite-table--editable-p)

          ;; Execute complex query
          (elsqlite-table-execute-query "SELECT COUNT(*) as count FROM users")
          (should (eq elsqlite-table--view-type 'query))
          (should-not elsqlite-table--editable-p))
      (sqlite-close db))))

(ert-deftest elsqlite-test-blob-image-detection ()
  "Test BLOB image type detection for various formats."
  (require 'elsqlite-table)
  ;; PNG signature (needs > 4 bytes)
  (should (equal "png" (elsqlite-table--detect-image-type "\x89PNG\r\n\x1a\n")))
  ;; JPEG signature (needs > 4 bytes, so add one more byte)
  (should (equal "jpeg" (elsqlite-table--detect-image-type "\xFF\xD8\xFF\xE0\x00")))
  ;; GIF87a signature (needs > 4 bytes)
  (should (equal "gif" (elsqlite-table--detect-image-type "GIF87a")))
  ;; GIF89a signature (needs > 4 bytes)
  (should (equal "gif" (elsqlite-table--detect-image-type "GIF89a")))
  ;; BMP signature (needs > 4 bytes)
  (should (equal "bmp" (elsqlite-table--detect-image-type "BM\x00\x00\x00")))
  ;; WEBP signature (RIFF....WEBP, needs >= 12 bytes)
  (should (equal "webp" (elsqlite-table--detect-image-type "RIFF\x00\x00\x00\x00WEBP")))
  ;; Not an image
  (should (null (elsqlite-table--detect-image-type "plain text")))
  (should (null (elsqlite-table--detect-image-type "PDF-1.4")))
  ;; Too short (4 bytes or less)
  (should (null (elsqlite-table--detect-image-type "BM")))
  (should (null (elsqlite-table--detect-image-type "1234")))
  (should (null (elsqlite-table--detect-image-type "")))
  (should (null (elsqlite-table--detect-image-type nil))))

(ert-deftest elsqlite-test-multiline-query-recognition ()
  "Test that multiline SELECT queries are recognized as read-only.
This is a regression test for the bug where multiline queries
weren't recognized due to regex not matching newlines."
  (require 'elsqlite-sql)
  ;; Single-line SELECT
  (should (elsqlite-sql--query-is-read-only-p "SELECT * FROM users"))
  ;; Multiline SELECT with newlines
  (should (elsqlite-sql--query-is-read-only-p "SELECT\n  *\nFROM users"))
  ;; Multiline with extra whitespace
  (should (elsqlite-sql--query-is-read-only-p "  SELECT\n\n  id\n  FROM  users  "))
  ;; Complex multiline query
  (should (elsqlite-sql--query-is-read-only-p
           "SELECT\n  requests.viewer as client,\n  replies.image as image\nfrom messages"))
  ;; WITH query (CTE)
  (should (elsqlite-sql--query-is-read-only-p "WITH cte AS (SELECT 1)\nSELECT * FROM cte"))
  ;; EXPLAIN
  (should (elsqlite-sql--query-is-read-only-p "EXPLAIN SELECT * FROM users"))
  ;; PRAGMA
  (should (elsqlite-sql--query-is-read-only-p "PRAGMA table_info(users)"))
  ;; Empty string (triggers schema view)
  (should (elsqlite-sql--query-is-read-only-p ""))
  ;; Modification queries should NOT be read-only
  (should-not (elsqlite-sql--query-is-read-only-p "INSERT INTO users VALUES (1)"))
  (should-not (elsqlite-sql--query-is-read-only-p "UPDATE users SET name = 'foo'"))
  (should-not (elsqlite-sql--query-is-read-only-p "DELETE FROM users")))

(ert-deftest elsqlite-test-field-value-trimming ()
  "Test that field values are trimmed when copied/saved.
Ensures display padding doesn't leak into copied data."
  ;; Test string-trim behavior (used in copy/save functions)
  (should (equal "test" (string-trim "  test  ")))
  (should (equal "test" (string-trim "test  ")))
  (should (equal "test" (string-trim "  test")))
  (should (equal "test" (string-trim "\ttest\n")))
  (should (equal "" (string-trim "   ")))
  (should (equal "" (string-trim "")))
  ;; Test with realistic tabulated-list padding
  (should (equal "value" (string-trim "value                    ")))
  (should (equal "long value here" (string-trim "  long value here        "))))

(provide 'elsqlite-test)
;;; elsqlite-test.el ends here
