# Changelog

All notable changes to ELSQLite will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.4.0] - 2026-02-06

### Added
- **Advanced SQL completion engine** (`elsqlite-completion.el`)
  - Context-aware completion knows which SQL clause you're in (SELECT, FROM, WHERE, UPDATE SET, etc.)
  - Schema caching for fast performance (no polling or TTL)
  - Automatic cache invalidation after modification queries (INSERT/UPDATE/DELETE/CREATE/ALTER/DROP)
  - Manual cache refresh available via `elsqlite-completion-invalidate-cache` for external schema changes
  - Rich annotations showing column types and primary key markers (e.g., `id INTEGER PK`, `name TEXT`)
  - Table and column name completion from live database schema
  - SQL keyword completion (SELECT, INSERT, UPDATE, DELETE, WHERE, JOIN, etc.)
  - SQL function completion (COUNT, SUM, AVG, MIN, MAX, DATE, etc.)
  - Table alias resolution (e.g., `FROM users u WHERE u.column` completes to users columns)
  - Statement flow awareness:
    - `SELECT * FROM` → offers table names
    - `SELECT * FROM users` → offers WHERE, JOIN, GROUP BY, ORDER BY, LIMIT
    - `UPDATE users` → offers SET
    - `UPDATE users SET name = 'x'` → offers WHERE (not more columns without comma)
    - `UPDATE users SET name = 'x',` → offers columns (for additional assignments)
    - `INSERT` → offers INTO
    - `INSERT INTO tablename` → partial support (column lists not yet implemented)
  - Completion triggers on TAB press only (not every keystroke) for optimal performance
  - Integrated into `elsqlite-sql-mode` via `completion-at-point-functions`
- Workspace integration for Doom Emacs
  - Child frames properly hide when switching or creating workspaces
  - Automatic cleanup of image preview frames across workspace operations
  - Full cleanup when deleting workspaces (closes all session buffers across all workspaces)
  - Compatible with Doom's `+workspace/switch-to`, `+workspace/new`, and `+workspace/kill`
- Bidirectional buffer cleanup
  - Closing SQL editor now closes its view buffer, image frame, and database
  - Closing view buffer now closes its SQL editor, image frame, and database
  - Ensures complete cleanup regardless of which buffer is closed
- Session identifier in mode line
  - Every database open gets a unique, ever-incrementing session number (`[1]`, `[2]`, `[3]`, etc.)
  - Session ID appears at the start of mode line in both SQL and View buffers (in bold)
  - Makes it easy to quickly identify and jump between window pairs, regardless of database name
  - Numbers persist across the Emacs session (counter is global, not per-database)
- Query execution feedback
  - Shows "SQL running..." message when query starts (useful for slow queries)
  - Shows execution time in completion messages (e.g., "Query returned 200 rows in 0.05 seconds")
  - Applies to all query types: schema viewer, table browsing, and custom queries

### Changed
- SQL completion engine completely rewritten for context-awareness (replaced basic keyword-only completion)
- Completion now uses schema caching instead of querying database on every completion

### Removed
- Refresh functionality (`g` keybinding and `elsqlite-refresh`/`elsqlite-table-refresh` functions)
  - Redundant with streaming - users can re-execute queries via SQL panel (C-c C-c)
  - Refresh would lose scroll position, causing confusion with streaming
- Old basic SQL completion functions (replaced by advanced completion engine)

### Fixed
- Child frames no longer bleed across Doom Emacs workspaces
- Image preview frames properly clean up when switching workspaces
- Mode line format error when row ID is not a number (added numberp check)
- Memory warning threshold now properly stops loading on user decline (fixed cl-return-from error)
- Opening the same database multiple times now creates unique buffer pairs with independent sessions
  - Each open creates new buffers with unique names (e.g., `<2>`, `<3>`)
  - Each session has its own database connection and paired SQL/View buffers

### Known Limitations
- INSERT statement completion is partial: supports `INSERT` → `INTO` → table names, but column lists `(col1, col2)` and VALUES clause are not yet implemented

### Planned
- Complete INSERT statement support (column lists and VALUES clause)
- Multi-statement support (completion in semicolon-separated queries)
- Subquery completion (completion inside nested SELECT statements)
- Comment-aware parsing (ignore SQL keywords in comments)
- CTE (WITH clause) support
- Cell editing with dirty state tracking
- Row insertion and deletion
- Transaction-based save/revert
- Record detail view
- Export functionality (CSV, org-table, JSON)
- Foreign key navigation

## [0.3.0] - 2026-02-03

### Added
- Incremental result streaming for large datasets
  - Load results in batches (~200 rows at a time) instead of all at once
  - Auto-load more rows as user scrolls near end of buffer
  - Memory safety warning at 10,000 loaded rows with option to continue
  - Handles tables with millions of rows without freezing or running out of memory
  - Configuration: `elsqlite-streaming-batch-size`, `elsqlite-streaming-load-threshold`, `elsqlite-streaming-warning-threshold`
  - Mode line shows dynamic row position: `[row/total/more to load]` or `[row/total]`
- Open BLOB images in external viewer (C-c C-o)
  - Saves image to temporary file and opens with system default viewer
  - Works with PNG, JPEG, GIF, BMP, and WEBP formats
- Field copy and save functionality (C-c C-w to copy, C-c C-s to save)
  - Copy field values to clipboard with automatic trimming
  - Save fields to files with type detection (PNG, JPEG, GIF, BMP, WEBP, txt)
  - Smart prompts showing file type hints
- Test coverage for new features
  - BLOB image type detection test
  - Multiline SELECT query recognition test (regression test)
  - Field value trimming test

### Changed
- Table browsing now uses streaming exclusively (removed pagination-based LIMIT/OFFSET approach)
- Mode line format updated to show loaded row counts instead of table/query names
- Schema view now has tighter formatting without blank lines between CREATE statements
- Keybindings changed to follow Emacs conventions: C-c w/s → C-c C-w/C-c C-s

### Removed
- Pagination-based table browsing (replaced by streaming)
- Unused pagination variables: `elsqlite-table--page-size`, `elsqlite-table--current-offset`, `elsqlite-table--total-rows`
- Unused pagination functions (elsqlite-table-next-page, elsqlite-table-previous-page)
- Unused navigation functions (elsqlite-table-drill-down, elsqlite-table-up)
- Unused elsqlite-default-page-size defcustom (was never used in code)
- Code cleanup: removed ~82 lines of dead code

### Fixed
- Multiline SELECT queries now properly recognized as read-only
- Console mode (-nw) no longer attempts to show image previews
- All MELPA compliance issues resolved (package-lint, checkdoc)

## [0.2.0] - 2026-02-01

### Added
- MELPA compliance improvements
- CI workflows for testing across Emacs versions (29.1, 29.4, snapshot)

## [0.1.0] - 2026-01-30

### Added
- Initial release
- Two-panel layout (SQL editor + results browser)
- Bidirectional sync between SQL and UI
- Schema browser with outline-based folding
- Table browser with pagination (default 50 rows per page)
- Column navigation (TAB/Shift-TAB to move between columns)
- Automatic edit vs browse mode detection
- BLOB image preview (PNG, JPEG, GIF, BMP, WEBP)
- Context-aware SQL completion
  - Table names after FROM/JOIN
  - Column names with table.column syntax
  - SQL keywords and functions
- Query history navigation (M-p/M-n)
- Smart query complexity analysis
- Query restoration after modification queries
- Comprehensive test suite with 14 passing tests

### Technical
- Requires Emacs 29.1+ with SQLite support
- Pure Elisp implementation, no external dependencies
- Uses `tabulated-list-mode` for results display
- Schema caching for fast completion
- Modeline shows edit/browse mode and pagination info

[Unreleased]: https://github.com/dusanx/elsqlite/compare/v0.4.0...HEAD
[0.4.0]: https://github.com/dusanx/elsqlite/compare/v0.3.0...v0.4.0
[0.3.0]: https://github.com/dusanx/elsqlite/compare/v0.2.0...v0.3.0
[0.2.0]: https://github.com/dusanx/elsqlite/releases/tag/v0.2.0
[0.1.0]: https://github.com/dusanx/elsqlite/releases/tag/v0.1.0
