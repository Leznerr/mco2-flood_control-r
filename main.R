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

for (parts in modules) {
  do.call(source_module, as.list(parts))
}

.log_stage <- function(name, interactive = FALSE) {
  if (interactive) {
    return(invisible(NULL))
  }
  if (exists("log_banner", mode = "function")) {
    log_banner(sprintf("Stage: %s", name))
  } else if (exists("log_info", mode = "function")) {
    log_info("Stage: %s", name)
  } else {
    message(sprintf("Stage: %s", name))
  }
  invisible(NULL)
}

.finish_and_exit <- function(start_time, success) {
  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  if (success) {
    cat(sprintf("Completed successfully in %.2f seconds.\n", elapsed))
  } else {
    cat(sprintf("Pipeline terminated after %.2f seconds.\n", elapsed))
  }
  invisible(NULL)
}

.pipeline_process <- function(input_path, interactive = FALSE) {
  .log_stage("Ingest", interactive)
  raw <- ingest_csv(input_path)

  .log_stage("Validate", interactive)
  validate_schema(raw)

  .log_stage("Clean", interactive)
  cln <- clean_all(raw)

  .log_stage("Derive", interactive)
  drv <- derive_all(cln)

  .log_stage("Filter (2021-2023)", interactive)
  filtered <- filter_window(drv, years = 2021:2023)

  if (interactive) {
    cat(sprintf(
      "Processing dataset... (%d rows loaded, %d filtered for 2021–2023)\n",
      nrow(raw), nrow(filtered)
    ))
  }

  filtered
}

.pipeline_reports <- function(df_filtered, outdir, interactive = FALSE) {
  if (interactive) {
    cat("Generating reports...\n")
  }
  .log_stage("Reports", interactive)
  report1 <- build_report1(df_filtered)
  write_report1(report1, outdir)

  report2 <- build_report2(df_filtered)
  write_report2(report2, outdir)

  report3 <- build_report3(df_filtered)
  write_report3(report3, outdir)

  .log_stage("Summary", interactive)
  summary_list <- build_summary(df_filtered)
  write_summary_json(summary_list, path_summary_json(outdir))

  if (interactive) {
    cat("Outputs saved to individual files...\n\n")

    cat("Report 1: Regional Flood Mitigation Efficiency Summary\n")
    cat("Regional Flood Mitigation Efficiency Summary\n")
    cat("(Filtered: 2021–2023 Projects)\n")
    print(utils::head(report1, 3))
    cat("(Full table exported to report1_regional_summary.csv)\n\n")

    cat("Report 2: Top Contractors Performance Ranking\n")
    print(utils::head(report2, 3))
    cat("(Full table exported to report2_contractor_ranking.csv)\n\n")

    cat("Report 3: Annual Project Type Cost Overrun Trends\n")
    cat("Annual Project Type Cost Overrun Trends\n")
    cat("(Grouped by FundingYear and TypeOfWork)\n")
    print(utils::head(report3, 3))
    cat("(Full table exported to report3_annual_trends.csv)\n\n")

    gavg_val <- suppressWarnings(as.numeric(summary_list$global_avg_delay))
    gavg_fmt <- if (is.na(gavg_val)) {
      "NA"
    } else {
      sprintf("%.2f", round(gavg_val, 2))
    }
    tots_val <- suppressWarnings(as.numeric(summary_list$total_savings))
    tots_fmt <- if (is.na(tots_val)) {
      "NA"
    } else {
      trimws(format(tots_val, scientific = FALSE, trim = TRUE))
    }
    cat(sprintf('Summary Stats (summary.json): {"global_avg_delay": %s, "total_savings": %s}\n', gavg_fmt, tots_fmt))
  }

  invisible(list(report1 = report1, report2 = report2, report3 = report3, summary = summary_list))
}

run_interactive <- function(input_path, outdir) {
  df_filtered <- NULL
  repeat {
    print_menu()
    choice <- read_choice()

    if (identical(choice, 1L)) {
      df_filtered <- .pipeline_process(input_path, interactive = TRUE)
      next
    }

    if (identical(choice, 2L)) {
      if (is.null(df_filtered)) {
        df_filtered <- .pipeline_process(input_path, interactive = TRUE)
      }
      .pipeline_reports(df_filtered, outdir, interactive = TRUE)
      if (!prompt_back_to_menu()) {
        break
      }
      cat("\n")
      next
    }

    cat("Invalid choice. Please enter 1 or 2.\n\n")
  }

  invisible(NULL)
}

run_non_interactive <- function(input_path, outdir) {
  df_filtered <- .pipeline_process(input_path, interactive = FALSE)
  .pipeline_reports(df_filtered, outdir, interactive = FALSE)
  invisible(NULL)
}

main <- function() {
  start_time <- Sys.time()
  success <- FALSE

  parser <- build_cli()
  opts <- parse_args(parser)

  tryCatch(
    {
      validate_cli_args(opts)
      opts <- normalize_cli_paths(opts)

      input_path <- opts$input
      outdir <- opts$outdir

      if (isTRUE(opts$interactive)) {
        run_interactive(input_path, outdir)
      } else {
        run_non_interactive(input_path, outdir)
      }
      success <<- TRUE
    },
    error = function(err) {
      if (exists("log_error", mode = "function")) {
        log_error("Pipeline failed: %s", conditionMessage(err))
      } else {
        message(sprintf("Pipeline failed: %s", conditionMessage(err)))
      }
    }
  )

  .finish_and_exit(start_time, success)
  status <- if (success) 0L else 1L
  quit(save = "no", status = status)
}

if (sys.nframe() == 0L) {
  main()
}

