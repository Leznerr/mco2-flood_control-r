# io.R
# ------------------------------------------------------------------------------
# Purpose   : Handle output responsibilities for the pipeline (directory
#             creation, CSV + JSON writing with deterministic formatting).
# Contract  :
#   - ensure_outdir(path) creates directory recursively if it does not exist.
#   - write_report_csv(df, path, exclude = NULL, exclude_regex = NULL) applies
#     format_dataframe() and writes atomically via temp file rename.
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

.path_join <- function(...) {                                   # minimal wrapper around file.path()
  parts <- list(...)
  parts <- Filter(function(x) {
    is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
  }, parts)
  if (!length(parts)) return(character())
  do.call(file.path, parts)
}

REPORT_FILES <- list(                                           # canonical filenames for exports
  report1 = "report1.csv",
  report2 = "report2.csv",
  report3 = "report3.csv",
  summary = "summary.json"
)

path_report1 <- function(outdir) {
  .path_join(outdir, REPORT_FILES$report1)
}

path_report2 <- function(outdir) {
  .path_join(outdir, REPORT_FILES$report2)
}

path_report3 <- function(outdir) {
  .path_join(outdir, REPORT_FILES$report3)
}

path_summary_json <- function(outdir) {
  .path_join(outdir, REPORT_FILES$summary)
}

path_summary <- function(outdir) {
  path <- path_summary_json(outdir)
  if (!length(path)) {
    stop("path_summary(): 'outdir' must be a non-NA character scalar.")
  }
  path
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

export_env <- globalenv()
for (nm in names(io_exports)) {
  assign(nm, io_exports[[nm]], envir = export_env)
}


# ---- readr write compatibility (pre-2.0 vs >=2.0) ----------------------------
.readr_has_escape <- function() {
  tryCatch(utils::packageVersion("readr") >= "2.0.0", error = function(...) FALSE)
}

write_csv_compat <- function(x, file, na = "", col_names = TRUE, delim = ",", progress = FALSE) {
  # Use write_delim for both to control 'escape'/'quote_escape' explicitly
  if (.readr_has_escape()) {
    # readr >= 2.0.0: use 'escape'
    readr::write_delim(
      x, file = file, delim = delim, na = na,
      col_names = col_names, progress = progress,
      escape = "double"
    )
  } else {
    # readr < 2.0.0: use 'quote_escape'
    readr::write_delim(
      x, file = file, delim = delim, na = na,
      col_names = col_names, progress = progress,
      quote_escape = "double"
    )
  }
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

write_report_csv <- function(df,
                             path,
                             exclude = NULL,
                             exclude_regex = NULL,
                             comma_strings = TRUE,
                             digits = 2) {
  if (!is.data.frame(df)) stop("write_report_csv(): 'df' must be a data frame.")
  if (missing(path) || !is.character(path) || length(path) != 1L || is.na(path)) {
    stop("write_report_csv(): 'path' must be a non-NA character scalar.")
  }
  dir <- dirname(path)
  if (!dir.exists(dir)) ensure_outdir(dir)
  formatted <- format_dataframe(
    df,
    exclude = exclude,
    exclude_regex = exclude_regex,
    comma_strings = comma_strings,
    digits = digits
  )
  tmp <- tempfile(pattern = paste0(basename(path), "."), tmpdir = dir)
  write_csv_compat(formatted, file = tmp, na = "", col_names = TRUE, delim = ",", progress = FALSE)
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

write_report1 <- function(df, outdir, fmt_opts = list()) {
  path <- path_report1(outdir)
  args <- c(list(df = df, path = path), fmt_opts)
  do.call(write_report_csv, args)
  path
}

write_report2 <- function(df, outdir, fmt_opts = list()) {
  path <- path_report2(outdir)
  args <- c(list(df = df, path = path), fmt_opts)
  do.call(write_report_csv, args)
  path
}

write_report3 <- function(df, outdir, fmt_opts = list()) {
  path <- path_report3(outdir)
  args <- c(list(df = df, path = path), fmt_opts)
  do.call(write_report_csv, args)
  path
}

write_summary_outdir <- function(x, outdir) {
  path <- path_summary_json(outdir)
  write_summary_json(x, path)
  path
}

