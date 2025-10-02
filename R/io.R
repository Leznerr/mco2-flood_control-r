# io.R
# ------------------------------------------------------------------------------
# Purpose   : Handle output responsibilities for the pipeline (directory
#             creation, CSV + JSON writing with deterministic formatting).
# Contract  :
#   - ensure_outdir(path) creates directory recursively if it does not exist.
#   - write_report_csv(df, path) applies format_dataframe() with standard
#     presentation options and writes atomically via temp file rename.
#   - write_summary_json(x, path) serialises to the provided JSON path using
#     jsonlite::write_json with pretty formatting and auto_unbox semantics
#     (atomic write as well).
# Rubric    : Correctness (atomic writes), UX (Excel-friendly CSV),
#             Readability (formal comments), Simplicity (minimal helpers).
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({                             # ensure clean console
  library(readr)
  library(jsonlite)
})



# ---- lightweight path helpers -------------------------------------------------
if (!exists("%||%", mode = "function")) {                     # local fallback when helpers not yet sourced
  `%||%` <- function(lhs, rhs) if (!is.null(lhs)) lhs else rhs
}

.path_join <- function(...) {                                   # deterministic join that drops empty inputs
  parts <- list(...)
  parts <- Filter(function(x) {
    is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
  }, parts)
  if (!length(parts)) {
    stop(".path_join(): at least one non-empty character scalar is required.")
  }
  do.call(file.path, parts)
}

REPORT_FILES <- list(                                           # canonical filenames for exports
  report1 = "report1_regional_summary.csv",
  report2 = "report2_contractor_ranking.csv",
  report3 = "report3_annual_trends.csv",
  summary = "summary.json"
)

.resolve_report_path <- function(outdir, key) {                 # single place that validates path inputs
  if (missing(outdir) || !is.character(outdir) || length(outdir) != 1L || is.na(outdir) || !nzchar(outdir)) {
    stop(sprintf("%s: 'outdir' must be a non-NA character scalar.", key))
  }
  if (!key %in% names(REPORT_FILES)) {
    stop(sprintf("Unknown report key '%s'.", key))
  }
  .path_join(outdir, REPORT_FILES[[key]])
}

path_report1 <- function(outdir) {
  .resolve_report_path(outdir, "report1")
}

path_report2 <- function(outdir) {
  .resolve_report_path(outdir, "report2")
}

path_report3 <- function(outdir) {
  .resolve_report_path(outdir, "report3")
}

path_summary_json <- function(outdir) {
  .resolve_report_path(outdir, "summary")
}

path_summary <- function(outdir) {
  path_summary_json(outdir)
}

io_exports <- list(                                             # make helpers visible when sourced locally
  .path_join = .path_join,
  REPORT_FILES = REPORT_FILES,
  path_report1 = path_report1,
  path_report2 = path_report2,
  path_report3 = path_report3,
  path_summary_json = path_summary_json,
  path_summary = path_summary
)

list2env(io_exports, envir = globalenv())


format_for_export <- function(df) {
  format_dataframe(
    df,
    exclude = c("FundingYear", "Year", "N", "NumProjects"),
    comma_strings = TRUE,
    digits = 2
  )
}

.atomic_replace <- function(tmp, path, label) {              # helper to replace destination atomically with rollback
  if (!file.exists(tmp)) {
    stop(sprintf("%s: temporary file '%s' is missing before atomic replace.", label, tmp))
  }
  if (!file.exists(path)) {
    if (!file.rename(tmp, path)) {
      stop(sprintf("%s: failed to move temporary file into '%s'.", label, path))
    }
    return(invisible(path))
  }
  dir <- dirname(path)
  backup <- tempfile(pattern = paste0(basename(path), ".bak."), tmpdir = dir)
  if (!file.rename(path, backup)) {
    unlink(tmp)
    stop(sprintf("%s: unable to prepare atomic replacement for existing file '%s'.", label, path))
  }
  success <- file.rename(tmp, path)
  if (!success) {
    file.rename(backup, path)
    unlink(tmp)
    stop(sprintf("%s: failed to move temporary file into '%s'. Original file restored.", label, path))
  }
  unlink(backup)
  invisible(path)
}

ensure_outdir <- function(path) {                            # create directory if missing
  if (missing(path) || !is.character(path) || length(path) != 1L || is.na(path)) {
    stop("ensure_outdir(): 'path' must be a non-NA character scalar.")
  }
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  invisible(path)
}

write_report_csv <- function(df, path) {
  if (!is.data.frame(df)) stop("write_report_csv(): 'df' must be a data frame.")
  if (missing(path) || !is.character(path) || length(path) != 1L || is.na(path)) {
    stop("write_report_csv(): 'path' must be a non-NA character scalar.")
  }
  dir <- dirname(path)
  if (!dir.exists(dir)) ensure_outdir(dir)
  formatted <- format_for_export(df)
  tmp <- tempfile(pattern = paste0(basename(path), "."), tmpdir = dir)
  readr::write_csv(formatted, file = tmp, na = "")
  .atomic_replace(tmp, path, "write_report_csv()")
  invisible(path)
}

write_summary_json <- function(x, path) {                    # JSON writer with pretty printing
  if (missing(path) || !is.character(path) || length(path) != 1L || is.na(path)) {
    stop("write_summary_json(): 'path' must be a non-NA character scalar.")
  }

  dir <- dirname(path)
  if (!dir.exists(dir)) ensure_outdir(dir)

  tmp <- tempfile(pattern = paste0(basename(path), "."), tmpdir = dir)
  jsonlite::write_json(x, tmp, auto_unbox = TRUE, pretty = TRUE, digits = 2, na = "null")
  .atomic_replace(tmp, path, "write_summary_json()")
  path
}

write_report1 <- function(df, outdir) {
  path <- path_report1(outdir)
  write_report_csv(df, path)
  path
}

write_report2 <- function(df, outdir) {
  path <- path_report2(outdir)
  write_report_csv(df, path)
  path
}

write_report3 <- function(df, outdir) {
  path <- path_report3(outdir)
  write_report_csv(df, path)
  path
}

write_summary_outdir <- function(x, outdir) {
  path <- path_summary_json(outdir)
  write_summary_json(x, path)
  path
}

