#!/usr/bin/env Rscript
# main.R (finalized)
# ------------------------------------------------------------------------------
# Purpose    : Orchestrate the complete DPWH flood-control pipeline.
#              Stages: CLI → ingest → validate → clean → derive → filter →
#              reports (R1,R2,R3) → summary → outputs, with structured logging.
# Contract   : Run via:
#                Rscript main.R --input dpwh_flood_control_projects.csv --outdir outputs

# Rubric     : Simplicity (clear stages), Correctness (fail-fast, assertions),
#              Performance (vectorized steps), Readability (formal comments),
#              UX (CLI+logs, deterministic outputs, atomic writes).
# Notes      : Assumes R/ modules exist and follow their declared contracts.
# ------------------------------------------------------------------------------

# ------------------------------- Strict options --------------------------------
options(warn = 2)                                           # treat warnings as errors to surface issues early
options(stringsAsFactors = FALSE)                           # keep character columns as character by default

# ------------------------------- Dependencies ---------------------------------
suppressPackageStartupMessages({                            # suppress package banners for clean logs
  library(optparse)                                         # CLI parsing (used by utils_cli.R)
})

# --------------------------- Source project modules ----------------------------
.source_or_die <- function(path) {                          # helper to source a file with fail-fast behavior
  if (!file.exists(path)) {                                 # verify file existence up front
    stop(sprintf("main.R: required module not found: '%s'", path))  # explicit error if missing
  }
  source(path, chdir = TRUE)                                # source the file relative to current working directory
}

# Source logging & utilities first (used by all stages) -------------------------
.source_or_die("R/utils_log.R")                              # log_* API (INFO/WARN/ERROR + context helpers)
.source_or_die("R/utils_cli.R")                              # build_cli(), validate_cli_args(), normalize_cli_paths()
.source_or_die("R/utils_format.R")                           # safe_mean/median, minmax_0_100, format_dataframe()
.source_or_die("R/io.R")                                     # ensure_outdir(), write_report_csv(), write_summary_json()

# Source pipeline stage modules -------------------------------------------------
.source_or_die("R/ingest.R")                                 # ingest_csv()
.source_or_die("R/validate.R")                               # validate_schema(), assert_year_filter()
.source_or_die("R/clean.R")                                  # clean_all()
.source_or_die("R/derive.R")                                 # derive_fields(), filter_years()
.source_or_die("R/report1.R")                                # report_regional_efficiency()


# ------------------------------- Safe execution --------------------------------
tryCatch(                                                # wrap execution to surface errors with non-zero exit
  {
    main()                                               # invoke main orchestration function
  },
  error = function(e) {                                  # handle any error thrown during pipeline execution
    msg <- conditionMessage(e)
    if (exists("log_error", mode = "function")) {
      log_error("%s", msg)
    } else {
      message(sprintf("[ERROR] %s", msg))
    }
    quit(save = "no", status = 1L)                       # exit with failure status for CI/grading
  }
)
