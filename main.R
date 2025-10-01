#!/usr/bin/env Rscript
# main.R (finalized)
# ------------------------------------------------------------------------------
# Purpose    : Orchestrate the complete DPWH flood-control pipeline.
#              Stages: CLI → ingest → validate → clean → derive → filter →
#              reports (R1,R2,R3) → summary → outputs, with structured logging.
# Contract   : Run via:
#                Rscript main.R --input dpwh_flood_control_projects.csv --outdir outputs
# Outputs    : outputs/report1_regional_efficiency.csv
#              outputs/report2_top_contractors.csv
#              outputs/report3_overruns_trend.csv
#              outputs/summary.json
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
.source_or_die("R/report2.R")                                # report_contractor_ranking()
.source_or_die("R/report3.R")                                # report_overrun_trends()
.source_or_die("R/summary.R")                                # build_summary()
.source_or_die("R/verify.R")                                 # verify_outputs()

# --------------------------- Pipeline helper stages ---------------------------

.pipeline_prepare <- function(args) {
  ingest_result <- with_log_context(list(stage = "ingest"), {
    raw <- ingest_csv(args$input)
    log_info(
      "diagnostic: class(raw)=%s; nrow=%s; names[1:5]=%s",
      paste(class(raw), collapse = "/"),
      NROW(raw),
      paste(utils::head(names(raw), 5), collapse = ",")
    )
    stopifnot(is.data.frame(raw), nrow(raw) > 0)
    list(data = raw, rows_loaded = nrow(raw))
  })

  raw <- ingest_result$data
  rows_loaded <- ingest_result$rows_loaded

  with_log_context(list(stage = "validate"), {
    validate_schema(raw)
  })

  cleaned <- with_log_context(list(stage = "clean"), {
    clean_all(raw)
  })

  derived <- with_log_context(list(stage = "derive"), {
    derive_fields(cleaned)
  })

  filter_result <- with_log_context(list(stage = "filter"), {
    filtered <- filter_years(derived, years = 2021:2023)
    assert_year_filter(filtered, allowed_years = 2021:2023)
    list(data = filtered, rows_filtered = nrow(filtered))
  })

  list(
    data = filter_result$data,
    rows_loaded = rows_loaded,
    rows_filtered = filter_result$rows_filtered
  )
}

.pipeline_generate_outputs <- function(prep, args, fmt_opts) {
  df <- prep$data
  if (is.null(df)) stop("pipeline_generate_outputs(): filtered dataset missing.")

  r1 <- with_log_context(list(stage = "report1"), {
    report_regional_efficiency(df)
  })
  r2 <- with_log_context(list(stage = "report2"), {
    report_contractor_ranking(df)
  })
  r3 <- with_log_context(list(stage = "report3"), {
    report_overrun_trends(df)
  })
  sumry <- with_log_context(list(stage = "summary"), {
    build_summary(df)
  })

  r1_fmt <- do.call(format_dataframe, c(list(r1), fmt_opts))
  r2_fmt <- do.call(format_dataframe, c(list(r2), fmt_opts))
  r3_fmt <- do.call(format_dataframe, c(list(r3), fmt_opts))

  paths <- with_log_context(list(stage = "output"), {
    list(
      report1 = write_report1(r1, args$outdir, fmt_opts),
      report2 = write_report2(r2, args$outdir, fmt_opts),
      report3 = write_report3(r3, args$outdir, fmt_opts),
      summary = write_summary_outdir(sumry, args$outdir)
    )
  })

  with_log_context(list(stage = "verification"), {
    verify_outputs(
      dataset = df,
      reports = list(report1 = r1, report2 = r2, report3 = r3),
      summary = sumry,
      outdir = args$outdir,
      fmt_opts = fmt_opts
    )
  })

  list(
    raw = list(report1 = r1, report2 = r2, report3 = r3),
    formatted = list(report1 = r1_fmt, report2 = r2_fmt, report3 = r3_fmt),
    summary = sumry,
    paths = paths
  )
}

.run_interactive_spec <- function(args, fmt_opts) {
  prep <- NULL
  repeat {
    cat("Select Language Implementation:\n")
    cat("[1] Load the file\n")
    cat("[2] Generate Reports\n\n")
    choice <- trimws(readline("Enter choice: "))
    if (identical(choice, "1")) {
      prep <- .pipeline_prepare(args)
      cat(sprintf(
        "Processing dataset... (%d rows loaded, %d filtered for 2021–2023)\n\n",
        prep$rows_loaded, prep$rows_filtered
      ))
    } else if (identical(choice, "2")) {
      if (is.null(prep)) {
        cat("Please load the file first using option 1.\n\n")
        next
      }
      cat("Generating reports...\n")
      results <- .pipeline_generate_outputs(prep, args, fmt_opts)
      cat("Outputs saved to individual files…\n\n")

      cat("Report 1: Regional Flood Mitigation Efficiency Summary\n\n")
      cat("Columns in the final CSV:\n")
      cat("Region, MainIsland, TotalApprovedBudget, MedianSavings, AvgDelay, Delay30Rate, EfficiencyScore\n\n")
      print(utils::head(results$formatted$report1, 2), row.names = FALSE)
      cat(sprintf("\n(Full table exported to %s)\n\n", basename(results$paths$report1)))

      cat("Report 2: Top Contractors Performance Ranking\n\n")
      cat("Columns in the final CSV:\n")
      cat("Contractor, NumProjects, TotalCost, AvgDelay, TotalSavings, ReliabilityIndex, RiskFlag\n\n")
      print(utils::head(results$formatted$report2, 2), row.names = FALSE)
      cat(sprintf("\n(Full table exported to %s)\n\n", basename(results$paths$report2)))

      cat("Report 3: Annual Project Type Cost Overrun Trends\n\n")
      cat("Columns in the final CSV:\n")
      cat("FundingYear, TypeOfWork, TotalProjects, AvgSavings, OverrunRate, YoYChange\n\n")
      print(utils::head(results$formatted$report3, 2), row.names = FALSE)
      cat(sprintf("\n(Full table exported to %s)\n\n", basename(results$paths$report3)))

      fmt_summary <- function(x) {
        if (is.na(x)) "null" else formatC(x, format = "f", digits = 2, big.mark = ",")
      }
      summary_json <- sprintf(
        "{\"global_avg_delay\": %s, \"total_savings\": %s}",
        fmt_summary(results$summary$global_avg_delay),
        fmt_summary(results$summary$total_savings)
      )
      cat(sprintf("Summary Stats (%s): %s\n\n", basename(results$paths$summary), summary_json))

      back <- trimws(readline("Back to Report Selection (Y/N): "))
      if (!identical(tolower(back), "y")) {
        break
      }
      cat("\n")
    } else {
      cat("Invalid choice. Try again.\n\n")
    }
  }
}

# ------------------------------- Main routine ---------------------------------
.pipeline_main <- function() {                               # define primary orchestration function
  start_time <- Sys.time()                                   # capture start timestamp for duration logging

  # ---- CLI parse & normalize --------------------------------------------------
  cli <- build_cli()                                         # construct the CLI parser (flags, help text)
  args <- parse_args(cli)                                    # parse command-line arguments into a named list
  validate_cli_args(args)                                    # fail fast on missing/invalid flags
  args <- normalize_cli_paths(args)                          # best-effort path normalization (no FS touch)
  interactive_mode <- isTRUE(args$interactive)

  # ---- Logging setup & banner -------------------------------------------------
  if (!is.na(Sys.getenv("LOG_LEVEL", unset = NA))) {         # if LOG_LEVEL env var is present
    log_set_level(Sys.getenv("LOG_LEVEL"))                   # set logger verbosity accordingly (DEBUG/INFO/WARN/ERROR)
  }
  log_context_set(list(input = args$input, outdir = args$outdir))  # set structured context fields for all logs
  log_banner("MCO2 DPWH Flood-Control Pipeline (R)")         # visual banner delimiting run start
  log_info("Input CSV : %s", args$input)                     # log the resolved input path
  log_info("Output dir: %s", args$outdir)                    # log the resolved output directory

  fmt_opts <- list(
    exclude = c(
      "FundingYear", "Year", "N", "NProjects", "NumProjects", "TotalProjects"
    ),
    comma_strings = TRUE,
    digits = 2,
    exclude_regex = NULL
  )

  if (!interactive_mode) {
    prep <- .pipeline_prepare(args)
    .pipeline_generate_outputs(prep, args, fmt_opts)
  } else {
    .run_interactive_spec(args, fmt_opts)
  }

  # ---- Epilogue & duration ----------------------------------------------------
  elapsed <- round(as.numeric(difftime(Sys.time(), start_time, units = "secs")), 2)  # compute elapsed seconds
  log_info("Pipeline outputs written to: %s", args$outdir)  # confirm final output directory
  log_info("Completed successfully in %.2f seconds.", elapsed)  # print total run duration
  invisible(TRUE)                                            # return invisibly for CLI usage
}

# Retain historical `main()` alias for compatibility with previous automation.
main <- .pipeline_main

if (!exists(".pipeline_main", mode = "function")) {
  stop("main.R: pipeline entrypoint failed to load; check earlier errors during sourcing.")
}

# ------------------------------- Safe execution --------------------------------
tryCatch(                                                # wrap execution to surface errors with non-zero exit
  {
    .pipeline_main()                                    # invoke main orchestration function
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
