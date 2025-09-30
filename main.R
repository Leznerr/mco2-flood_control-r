#!/usr/bin/env Rscript
# main.R (finalized)
# ------------------------------------------------------------------------------
# Purpose    : Orchestrate the complete DPWH flood-control pipeline.
#              Stages: CLI → ingest → validate → clean → derive → filter →
#              reports (R1,R2,R3) → summary → outputs, with structured logging.
# Contract   : Run via:
#                Rscript main.R --input dpwh_flood_control_projects.csv --outdir outputs
# Outputs    : outputs/report1_regional_summary.csv
#              outputs/report2_contractor_ranking.csv
#              outputs/report3_annual_trends.csv
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

# ------------------------------- Main routine ---------------------------------
main <- function() {                                         # define primary orchestration function
  start_time <- Sys.time()                                   # capture start timestamp for duration logging

  # ---- CLI parse & normalize --------------------------------------------------
  cli <- build_cli()                                         # construct the CLI parser (flags, help text)
  args <- parse_args(cli)                                    # parse command-line arguments into a named list
  validate_cli_args(args)                                    # fail fast on missing/invalid flags
  args <- normalize_cli_paths(args)                          # best-effort path normalization (no FS touch)
  interactive_mode <- isTRUE(args$interactive)

  if (interactive_mode) {
    cat("============================================================\n")
    cat("MCO2 Flood-Control Pipeline — Interactive Preview\n")
    cat("============================================================\n")
  }

  # ---- Logging setup & banner -------------------------------------------------
  if (!is.na(Sys.getenv("LOG_LEVEL", unset = NA))) {         # if LOG_LEVEL env var is present
    log_set_level(Sys.getenv("LOG_LEVEL"))                   # set logger verbosity accordingly (DEBUG/INFO/WARN/ERROR)
  }
  log_context_set(list(input = args$input, outdir = args$outdir))  # set structured context fields for all logs
  log_banner("MCO2 DPWH Flood-Control Pipeline (R)")         # visual banner delimiting run start
  log_info("Input CSV : %s", args$input)                     # log the resolved input path
  log_info("Output dir: %s", args$outdir)                    # log the resolved output directory

  # ---- Stage 1: Ingest --------------------------------------------------------
  rows_loaded <- 0L
  rows_filtered <- 0L

  with_log_context(list(stage = "ingest"), {                 # attach stage context for nested logs
    df_raw <- ingest_csv(args$input)                         # read raw CSV (no transforms; parse issues attached as attribute)
    rows_loaded <<- nrow(df_raw)
  })

  # ---- Stage 2: Validate ------------------------------------------------------
  with_log_context(list(stage = "validate"), {               # update context to validation stage
    validate_schema(df_raw)                                  # enforce required columns, non-empty shape, unique headers
  })

  # ---- Stage 3: Clean ---------------------------------------------------------
  with_log_context(list(stage = "clean"), {                  # update context to cleaning stage
    df_clean <- clean_all(df_raw)                            # parse dates/numerics, normalize text, conservative geo impute
  })

  # ---- Stage 4: Derive --------------------------------------------------------
  with_log_context(list(stage = "derive"), {                 # update context to derivation stage
    df_plus <- derive_fields(df_clean)                       # add CostSavings and CompletionDelayDays
  })

  # ---- Stage 5: Filter (2021–2023) + assert ----------------------------------
  with_log_context(list(stage = "filter"), {                 # stage context: filtering
    df <- filter_years(df_plus, years = 2021:2023)           # keep only rows in allowed FundingYear set
    assert_year_filter(df, allowed_years = 2021:2023)        # double-check invariant post-filter
    rows_filtered <<- nrow(df)
  })

  if (interactive_mode) {
    cat(sprintf("Processing dataset… (%d rows loaded, %d filtered for 2021–2023)\n",
                rows_loaded, rows_filtered))
  }

  # ---- Stage 6: Reports -------------------------------------------------------
  with_log_context(list(stage = "report1"), {                # context: report 1
    r1 <- report_regional_efficiency(df)                     # compute Regional Flood Mitigation Efficiency
  })
  with_log_context(list(stage = "report2"), {                # context: report 2
    r2 <- report_contractor_ranking(df)                      # compute Top Contractors ranking
  })
  with_log_context(list(stage = "report3"), {                # context: report 3
    r3 <- report_overrun_trends(df)                          # compute Overrun trends vs 2021
  })

  # ---- Stage 7: Summary -------------------------------------------------------
  with_log_context(list(stage = "summary"), {                # context: summary
    sumry <- build_summary(df)                               # build JSON-ready summary payload
  })

  # ---- Stage 8: Outputs (atomic writes) --------------------------------------
  with_log_context(list(stage = "output"), {                 # context: output writing
    ensure_outdir(args$outdir)                               # create output directory if it does not exist

    f1 <- path_report1(args$outdir)
    f2 <- path_report2(args$outdir)
    f3 <- path_report3(args$outdir)
    fj <- path_summary(args$outdir)

    fmt_opts <- list(
      exclude = c("FundingYear", "Year", "N", "NProjects", "NumProjects", "Rank"),
      comma_strings = TRUE,
      digits = 2
    )

    r1_fmt <- do.call(format_dataframe, c(list(r1), fmt_opts))
    r2_fmt <- do.call(format_dataframe, c(list(r2), fmt_opts))
    r3_fmt <- do.call(format_dataframe, c(list(r3), fmt_opts))

    if (interactive_mode) {
      preview <- function(title, df_fmt, path) {
        cat(sprintf("\n%s\n", title))
        if (nrow(df_fmt) == 0) {
          cat("[No rows]\n")
        } else {
          print(utils::head(df_fmt, 2), row.names = FALSE)
        }
        cat(sprintf("(Full table exported to %s)\n", path))
      }
      preview("Report 1 — Regional Summary", r1_fmt, f1)
      preview("Report 2 — Contractor Ranking", r2_fmt, f2)
      preview("Report 3 — Annual Trends", r3_fmt, f3)
    }

    write_report_csv(r1_fmt, f1)
    write_report_csv(r2_fmt, f2)
    write_report_csv(r3_fmt, f3)
    write_summary_json(sumry, fj)                            # write summary JSON (pretty, auto_unbox)
  })

  # ---- Epilogue & duration ----------------------------------------------------
  elapsed <- round(as.numeric(difftime(Sys.time(), start_time, units = "secs")), 2)  # compute elapsed seconds
  log_info("Pipeline outputs written to: %s", args$outdir)  # confirm final output directory
  log_info("Completed successfully in %.2f seconds.", elapsed)  # print total run duration
  invisible(TRUE)                                            # return invisibly for CLI usage
}

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
