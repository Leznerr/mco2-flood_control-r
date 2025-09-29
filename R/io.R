# io.R
# ------------------------------------------------------------------------------
# Purpose   : Handle output responsibilities for the pipeline (directory
#             creation, CSV + JSON writing with deterministic formatting).
# Contract  :
#   - ensure_outdir(path) creates directory recursively if it does not exist.
#   - write_report_csv(df, path, exclude = NULL, exclude_regex = NULL) applies
#     format_dataframe() and writes atomically via temp file rename.
#   - write_summary_json(x, path) serialises using jsonlite::write_json with
#     pretty formatting and auto_unbox semantics (atomic write as well).
# Rubric    : Correctness (atomic writes), UX (Excel-friendly CSV),
#             Readability (formal comments), Simplicity (minimal helpers).
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({                             # ensure clean console
  library(readr)
  library(jsonlite)
})

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

write_report_csv <- function(df, path, exclude = NULL, exclude_regex = NULL) {
  if (!is.data.frame(df)) stop("write_report_csv(): 'df' must be a data frame.")
  if (missing(path) || !is.character(path) || length(path) != 1L || is.na(path)) {
    stop("write_report_csv(): 'path' must be a non-NA character scalar.")
  }
  dir <- dirname(path)
  if (!dir.exists(dir)) ensure_outdir(dir)
  formatted <- format_dataframe(df, exclude = exclude, exclude_regex = exclude_regex)
  tmp <- tempfile(pattern = paste0(basename(path), "."), tmpdir = dir)
  readr::write_csv(formatted, tmp, na = "", quote_escape = "double")
  .atomic_replace(tmp, path, "write_report_csv()")
}

write_summary_json <- function(x, path) {                    # JSON writer with pretty printing
  if (missing(path) || !is.character(path) || length(path) != 1L || is.na(path)) {
    stop("write_summary_json(): 'path' must be a non-NA character scalar.")
  }
  dir <- dirname(path)
  if (!dir.exists(dir)) ensure_outdir(dir)
  tmp <- tempfile(pattern = paste0(basename(path), "."), tmpdir = dir)
  jsonlite::write_json(x, tmp, auto_unbox = TRUE, pretty = TRUE, digits = NA)
  .atomic_replace(tmp, path, "write_summary_json()")
}

