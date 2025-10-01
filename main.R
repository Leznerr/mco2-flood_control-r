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

options(stringsAsFactors = FALSE)                           # keep characters as characters across pipeline

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
.source_or_die("R/constants.R")                              # shared filenames/headings used across modules
.source_or_die("R/utils_log.R")                              # log_* API (INFO/WARN/ERROR + context helpers)
.source_or_die("R/utils_cli.R")                              # build_cli(), validate_cli_args(), normalize_cli_paths()
.source_or_die("R/utils_format.R")                           # safe_mean/median, minmax_0_100, format_dataframe()
.source_or_die("R/io.R")                                     # ensure_outdir(), write_report_csv(), write_summary_json()
.source_or_die("R/interactive.R")                            # .run_interactive_spec() preview helper

# Source pipeline stage modules -------------------------------------------------
.source_or_die("R/ingest.R")                                 # ingest_csv()
.source_or_die("R/validate.R")                               # validate_schema(), assert_year_filter()
.source_or_die("R/clean.R")                                  # clean_all()
.source_or_die("R/derive.R")                                 # derive_fields(), filter_years()
.source_or_die("R/report1.R")                                # report_regional_efficiency()
.source_or_die("R/report2.R")                                # report_contractor_ranking()
.source_or_die("R/report3.R")                                # report_overrun_trends()
.source_or_die("R/summary.R")                                # build_summary()
.source_or_die("R/verify.R")                                 # verify_outputs()

# --------------------------- Pipeline helper stages ---------------------------

.pipeline_prepare <- function(args) {                        # ingest → validate → clean → derive → filter
  df_raw <- with_log_context(list(stage = "ingest"), {
    raw <- ingest_csv(args$input)                            # read raw CSV (parse issues attribute preserved)
    parse_issues <- attr(raw, "ingest_problems")
    n_issues <- if (is.null(parse_issues)) 0L else nrow(parse_issues)
    log_info("diagnostic: class(raw)=%s | rows=%d | parse_issues=%d",
             paste(class(raw), collapse = "/"), NROW(raw), n_issues)
    if (!is.null(parse_issues) && nrow(parse_issues) > 0L) {
      preview <- utils::head(parse_issues, 3L)
      log_warn("parse issue preview (first %d rows):", nrow(preview))
      utils::capture.output(print(preview))
    }
    raw
  })

  with_log_context(list(stage = "validate"), {
    validate_schema(df_raw)                                 # enforce schema before transformations
  })

  df_clean <- with_log_context(list(stage = "clean"), {
    clean_all(df_raw)                                       # normalize money, dates, text, geos
  })

  df_plus <- with_log_context(list(stage = "derive"), {
    derive_fields(df_clean)                                 # append CostSavings + CompletionDelayDays
  })

  df_filtered <- with_log_context(list(stage = "filter"), {
    out <- filter_years(df_plus, years = 2021:2023)         # keep only allowed FundingYear range
    assert_year_filter(out, allowed_years = 2021:2023)      # double-check invariant for safety
    out
  })

  list(
    data = df_filtered,
    rows_loaded = NROW(df_raw),
    rows_filtered = NROW(df_filtered)
  )
}

.pipeline_build_reports <- function(dataset) {               # compute report data frames from filtered dataset
  r1 <- with_log_context(list(stage = "report1"), {
    report_regional_efficiency(dataset)
  })
  r2 <- with_log_context(list(stage = "report2"), {
    report_contractor_ranking(dataset)
  })
  r3 <- with_log_context(list(stage = "report3"), {
    report_overrun_trends(dataset)
  })
  list(report1 = r1, report2 = r2, report3 = r3)
}

.pipeline_preview_reports <- function(reports, fmt_opts) {   # interactive preview helper
  preview_data <- lapply(reports, function(df) {
    do.call(format_dataframe, c(list(df), fmt_opts))
  })
  .run_interactive_spec(preview_data)
}

.pipeline_write_outputs <- function(reports, summary, outdir, fmt_opts) {
  ensure_outdir(outdir)
  paths <- list(
    report1 = write_report1(reports$report1, outdir, fmt_opts),
    report2 = write_report2(reports$report2, outdir, fmt_opts),
    report3 = write_report3(reports$report3, outdir, fmt_opts),
    summary = write_summary_outdir(summary, outdir)
  )
  for (name in names(paths)) {
    log_info("wrote %s -> %s", name, paths[[name]])
  }
  paths
}

.pipeline_verify <- function(dataset, reports, summary, outdir, fmt_opts) {
  with_log_context(list(stage = "verification"), {
    verify_outputs(dataset = dataset,
                   reports = reports,
                   summary = summary,
                   outdir = outdir,
                   fmt_opts = fmt_opts)
  })
}

# ------------------------------- Main routine ---------------------------------

.pipeline_main <- function() {
  start_time <- Sys.time()                                   # timestamp for run duration logging

  cli <- build_cli()                                         # construct CLI parser (flags, help)
  args <- parse_args(cli)                                    # parse command-line arguments
  validate_cli_args(args)                                    # enforce required flags and filesystem checks
  args <- normalize_cli_paths(args)                          # normalize paths for logging clarity
  interactive_mode <- isTRUE(args$interactive)

  env_level <- Sys.getenv("LOG_LEVEL", unset = NA)
  if (!is.na(env_level) && nzchar(env_level)) {
    log_set_level(env_level)
  }
  log_context_set(list(input = args$input, outdir = args$outdir))
  log_banner("MCO2 DPWH Flood-Control Pipeline (R)")
  log_info("Input CSV : %s", args$input)
  log_info("Output dir: %s", args$outdir)

  fmt_opts <- list(
    exclude = c("FundingYear", "Year", "N", "NProjects", "NumProjects", "TotalProjects"),
    comma_strings = TRUE,
    digits = 2,
    exclude_regex = NULL
  )

  prep <- .pipeline_prepare(args)
  log_info("rows loaded=%d | rows retained (2021-2023)=%d", prep$rows_loaded, prep$rows_filtered)

  reports <- .pipeline_build_reports(prep$data)
  summary <- with_log_context(list(stage = "summary"), { build_summary(prep$data) })

  if (interactive_mode) {
    log_info("interactive preview enabled; rendering report excerpts before export")
    .pipeline_preview_reports(reports, fmt_opts)
  }

  paths <- .pipeline_write_outputs(reports, summary, args$outdir, fmt_opts)
  .pipeline_verify(prep$data, reports, summary, args$outdir, fmt_opts)

  elapsed <- round(as.numeric(difftime(Sys.time(), start_time, units = "secs")), 2)
  log_info("Verification report available at %s", file.path(args$outdir, "verification_report.txt"))
  log_info("Pipeline outputs written to: %s", args$outdir)
  log_info("Completed successfully in %.2f seconds.", elapsed)
  invisible(list(paths = paths, elapsed = elapsed))
}

# Retain historical main() alias for compatibility with previous automation.
main <- .pipeline_main

if (!exists(".pipeline_main", mode = "function")) {
  stop("main.R: pipeline entrypoint failed to load; check earlier errors during sourcing.")
}

# ------------------------------- Safe execution --------------------------------
tryCatch(
  {
    .pipeline_main()
  },
  error = function(e) {
    msg <- conditionMessage(e)
    if (exists("log_error", mode = "function")) {
      log_error("%s", msg)
    } else {
      message(sprintf("[ERROR] %s", msg))
    }
    quit(save = "no", status = 1L)
  }
)
