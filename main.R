#!/usr/bin/env Rscript

source_module <- function(...) {
  rel <- file.path(...)
  if (file.exists(rel)) {
    source(rel, chdir = TRUE)
    return(invisible(TRUE))
  }
  stop(sprintf("Unable to locate module '%s'.", rel))
}

modules <- list(
  c("R", "utils_log.R"),
  c("R", "utils_format.R"),
  c("R", "utils_cli.R"),
  c("R", "io.R"),
  c("R", "ingest.R"),
  c("R", "validate.R"),
  c("R", "clean.R"),
  c("R", "derive.R"),
  c("R", "report1.R"),
  c("R", "report2.R"),
  c("R", "report3.R"),
  c("R", "summary.R")
)

for (path_parts in modules) {
  do.call(source_module, as.list(path_parts))
}

.pipeline_state <- new.env(parent = emptyenv())
.pipeline_state$interactive <- FALSE



.log_stage <- function(name) {
  if (!isTRUE(.pipeline_state$interactive)) {
    log_banner(sprintf("Stage: %s", name))
  }
}

.finish_and_exit <- function(start_time, success = TRUE) {
  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  msg <- if (success) {
    sprintf("Completed successfully in %.2f seconds.\n", elapsed)
  } else {
    sprintf("Pipeline terminated after %.2f seconds.\n", elapsed)
  }
  cat(msg)
  invisible(NULL)
}

.pipeline_process <- function(input_path, interactive = FALSE) {
  .log_stage("Ingest")
  raw <- ingest_csv(input_path)

  .log_stage("Validate")
  validate_schema(raw)

  .log_stage("Clean")
  cln <- clean_all(raw)

  .log_stage("Derive")
  drv <- derive_all(cln)

  .log_stage("Filter (2021-2023)")
  fdf <- filter_window(drv, years = 2021:2023)

  if (interactive) {
    cat(sprintf(
      "Processing dataset... (%d rows loaded, %d filtered for 2021–2023)\n",
      nrow(raw), nrow(fdf)
    ))
  }

  fdf
}

.pipeline_reports <- function(df_filtered, outdir) {
  .log_stage("Reports")
  cat("Generating reports...\n")
  r1 <- build_report1(df_filtered); write_report1(r1, outdir)
  r2 <- build_report2(df_filtered); write_report2(r2, outdir)
  r3 <- build_report3(df_filtered); write_report3(r3, outdir)

  .log_stage("Summary")
  sumlist <- build_summary(df_filtered); write_summary_json(sumlist, path_summary_json(outdir))
  cat("Outputs saved to individual files...\n\n")

  cat("Report 1: Regional Flood Mitigation Efficiency Summary\n")
  cat("Regional Flood Mitigation Efficiency Summary\n")
  cat("(Filtered: 2021–2023 Projects)\n")
  print(utils::head(r1, 3)); cat("(Full table exported to report1_regional_summary.csv)\n\n")

  cat("Report 2: Top Contractors Performance Ranking\n")
  print(utils::head(r2, 3)); cat("(Full table exported to report2_contractor_ranking.csv)\n\n")

  cat("Report 3: Annual Project Type Cost Overrun Trends\n")
  cat("Annual Project Type Cost Overrun Trends\n")
  cat("(Grouped by FundingYear and TypeOfWork)\n")
  print(utils::head(r3, 3)); cat("(Full table exported to report3_annual_trends.csv)\n\n")

  gavg <- round(as.numeric(sumlist$global_avg_delay), 2)
  tots <- format(as.numeric(sumlist$total_savings), scientific = FALSE, trim = TRUE)
  tots <- trimws(tots)
  cat(sprintf('Summary Stats (summary.json): {"global_avg_delay": %.2f, "total_savings": %s}\n', gavg, tots))
}

.run_pipeline <- function(opts, start_time) {
  input_path <- opts$input
  outdir <- opts$outdir

  ensure_outdir(outdir)

  if (isTRUE(opts$interactive)) {
    .pipeline_state$interactive <- TRUE
    df_filtered <- NULL

      if (identical(ch, 1L)) {
        df_filtered <- .pipeline_process(input_path, interactive = TRUE)
      } else if (identical(ch, 2L)) {
        if (is.null(df_filtered)) {
          df_filtered <- .pipeline_process(input_path, interactive = TRUE)
        }
        .pipeline_reports(df_filtered, outdir)

        cat("\n")
        if (!go_back) break
      } else {
        cat("Invalid choice. Please enter 1 or 2.\n\n")
      }
    }
    .pipeline_state$interactive <- FALSE
    .finish_and_exit(start_time, success = TRUE)
    quit(save = "no", status = 0L)
  }

  .pipeline_state$interactive <- FALSE
  df_filtered <- .pipeline_process(input_path, interactive = FALSE)
  .pipeline_reports(df_filtered, outdir)
  .finish_and_exit(start_time, success = TRUE)
  quit(save = "no", status = 0L)
}

main <- function() {
  start_time <- Sys.time()

  parser <- build_cli()
  opts <- parse_args(parser)
  validate_cli_args(opts)
  opts <- normalize_cli_paths(opts)

  tryCatch(
    {
      .run_pipeline(opts, start_time)
    },
    error = function(err) {
      if (exists("log_error", mode = "function")) {
        log_error("Pipeline failed: %s", conditionMessage(err))
      } else {
        message(sprintf("Pipeline failed: %s", conditionMessage(err)))
      }
      .finish_and_exit(start_time, success = FALSE)
      quit(save = "no", status = 1L)
    }
  )
}

if (sys.nframe() == 0L) {
  main()
}

