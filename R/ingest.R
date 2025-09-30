# ingest.R
# ------------------------------------------------------------------------------
# Purpose   : Ingest the raw DPWH flood-control CSV with strict guards and zero
#             value transformations. Cleaning, typing, and derivations are done
#             in subsequent modules (clean.R, derive.R).
# Contract  : ingest_csv(path) -> tibble (raw table), with parse issues attached
#             as attribute "ingest_problems" (a tibble; may have 0 rows).
# Rubric    : Simplicity (single responsibility), Correctness (fail-fast guards),
#             Performance (readr fast path), Readability (clear messages),
#             UX (structured logs + stable attribute for diagnostics).
# ------------------------------------------------------------------------------

# Suppress startup noise from libraries for clean logs.
suppressPackageStartupMessages({
  # readr provides fast CSV ingestion and parse-problem reporting.
  library(readr)
})

# --------------------------- Configuration section ----------------------------

# Define the maximum number of rows to sample for type guessing (keeps read fast but robust).
DEFAULT_GUESS_MAX <- 10000L  # integer scalar; adequate for ~10k rows CSVs

# Define which string tokens should be considered missing values during read.
NA_TOKENS <- c("", "NA", "N/A", "null", "NULL")  # common NA spellings in real-world CSVs

# Define whether to allow a UTF-8 BOM; readr handles BOM automatically but we keep a flag for clarity.
ALLOW_BOM <- TRUE  # boolean; informational only (readr::read_csv strips BOM safely)

# ----------------------------- Logging utilities ------------------------------

# Emit an informational message; integrates with project logger if present.
.log_info <- function(fmt, ...) {                                        # define function for info logs
  msg <- sprintf(fmt, ...)                                               # format the message string
  if (exists("log_info", mode = "function")) {                           # if project-wide logger exists
    log_info("%s", msg)                                                 # delegate to project logger with printf semantics
  } else {                                                               # otherwise fallback to base message
    message(sprintf("[INFO]  %s", msg))                                  # print standardized info log
  }
  return(invisible(NULL))
}

# Emit a warning message; integrates with project logger if present.
.log_warn <- function(fmt, ...) {                                        # define function for warn logs
  msg <- sprintf(fmt, ...)                                               # format the message string
  if (exists("log_warn", mode = "function")) {                           # if project-wide logger exists
    log_warn("%s", msg)                                                 # delegate to project logger with printf semantics
  } else {                                                               # otherwise fallback to base message
    message(sprintf("[WARN]  %s", msg))                                  # print standardized warn log
  }
  return(invisible(NULL))
}

# ------------------------------- Public API -----------------------------------

#' Ingest a CSV file with strict guards and parse-issue reporting (no transforms).
#'
#' @param path character(1). Filesystem path to the input CSV.
#' @return A tibble as read by readr::read_csv(). An attribute named
#'         "ingest_problems" (a tibble; possibly 0 rows) is attached for
#'         downstream diagnostics and unit tests.
#' @throws Error when the path is invalid, the file does not exist, the file is
#'         empty, the table has zero rows, or when duplicated header names exist.
#' @examples
#' # df <- ingest_csv("dpwh_flood_control_projects.csv")
ingest_csv <- function(path) {                                            # define the main ingestion function
  # ---- Argument validation ----------------------------------------------------
  if (missing(path)) {                                                    # ensure the argument is provided
    stop("ingest_csv(): 'path' is missing; provide a CSV filepath.")      # fail-fast with clear guidance
  }
  if (!is.character(path) || length(path) != 1L || is.na(path)) {         # ensure a scalar, non-NA character input
    stop("ingest_csv(): 'path' must be a non-NA character scalar.")       # fail-fast on invalid type/shape
  }

  # ---- File existence & basic sanity checks ----------------------------------
  if (!file.exists(path)) {                                               # check that the file actually exists
    stop(sprintf("ingest_csv(): file not found at '%s'.", path))          # fail-fast with explicit path
  }
  file_size <- file.info(path)$size                                       # obtain file size in bytes
  if (is.na(file_size) || file_size == 0L) {                              # guard against empty or unreadable files
    stop(sprintf("ingest_csv(): file '%s' is empty or unreadable.", path))# fail-fast if file is empty
  }

  # ---- Read CSV (no value transforms; only NA token handling) ----------------
  # Choose a UTF-8 locale; readr handles BOM automatically (ALLOW_BOM for clarity).
  read_locale <- locale(encoding = "UTF-8")                               # set locale to ensure UTF-8 handling
  df <- readr::read_csv(                                                  # call readr's fast CSV reader
    file           = path,                                                # specify input file path
    guess_max      = DEFAULT_GUESS_MAX,                                   # sample size for robust type guessing
    na             = NA_TOKENS,                                           # strings to treat as missing values
    locale         = read_locale,                                         # fixed UTF-8 locale for stability
    col_names      = TRUE,                                                # keep the original header row intact
    name_repair    = "minimal",                                          # preserve raw header names (allow duplicates)
    show_col_types = FALSE                                                # suppress verbose type printing
  )

  # ---- Header integrity: duplicated column names are disallowed --------------
  dup_names <- names(df)[duplicated(names(df))]                            # find duplicated header names
  if (length(dup_names) > 0L) {                                            # if any duplicates found
    stop(sprintf(                                                          # fail-fast with a precise list
      "ingest_csv(): duplicated column names detected: %s. Fix the header and retry.",
      paste(sort(unique(dup_names)), collapse = ", ")
    ))
  }

  # ---- Shape validation -------------------------------------------------------
  n_rows <- nrow(df)                                                      # capture number of data rows read
  n_cols <- ncol(df)                                                      # capture number of columns read
  if (n_rows == 0L) {                                                     # check for zero rows (only header)
    stop(sprintf("ingest_csv(): '%s' loaded but contains zero data rows.", path)) # fail-fast with cause
  }
  if (n_cols == 0L) {                                                     # check for zero columns (malformed CSV)
    stop(sprintf("ingest_csv(): '%s' loaded but contains zero columns.", path))   # fail-fast if header missing
  }

  # ---- Parse issue collection (diagnostics, not fatal by default) ------------
  problems_tbl <- readr::problems(df)                                      # retrieve parse problems (0+ rows)
  n_problems   <- if (is.null(problems_tbl)) 0L else nrow(problems_tbl)    # count issues defensively
  attr(df, "ingest_problems") <- problems_tbl                              # attach problems as a stable attribute

  # ---- Operational logging (concise, structured) -----------------------------
  .log_info("Ingested '%s' | rows=%d, cols=%d, parse_issues=%d",           # print one-line ingest summary
            path, n_rows, n_cols, n_problems)
  if (n_problems > 0L) {                                                   # if any parse issues occurred
    # Show a tiny preview (first 3) to aid debugging while keeping logs tidy.
    preview_n <- min(3L, n_problems)                                       # compute preview size
    .log_warn("Preview of parse issues (showing %d of %d):", preview_n, n_problems) # header for preview
    utils::capture.output(                                                 # capture print to avoid dumping entire tibble
      print(utils::head(problems_tbl, preview_n)),                         # print only first few issues
      file = "")                                                           # print to console
  }

  # ---- Return the raw dataframe (no transformations applied) -----------------
  return(df)                                                               # yield the ingested tibble to caller
}
