# Changelog

All notable changes to ELSQLite will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Planned
- Cell editing with dirty state tracking
- Row insertion and deletion
- Transaction-based save/revert
- Enhanced column name completion
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

[Unreleased]: https://github.com/dusanx/elsqlite/compare/v0.3.0...HEAD
[0.3.0]: https://github.com/dusanx/elsqlite/compare/v0.2.0...v0.3.0
[0.2.0]: https://github.com/dusanx/elsqlite/releases/tag/v0.2.0
[0.1.0]: https://github.com/dusanx/elsqlite/releases/tag/v0.1.0
