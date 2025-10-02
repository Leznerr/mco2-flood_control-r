
source_module <- function(...) {
  rel <- file.path(...)
  if (file.exists(rel)) {
    source(rel, chdir = TRUE)
    return(invisible(TRUE))
  }
  alt <- file.path("R", ...)
  if (file.exists(alt)) {
    source(alt, chdir = TRUE)
    return(invisible(TRUE))
  }
  stop(sprintf("Unable to locate module '%s'.", rel))
}

source_module("R", "constants.R")
source_module("R", "utils_log.R")
source_module("R", "utils_cli.R")
source_module("R", "io.R")
source_module("R", "ingest.R")
source_module("R", "validate.R")
source_module("R", "clean.R")
source_module("R", "derive.R")
source_module("R", "report1.R")
source_module("R", "report2.R")
source_module("R", "report3.R")
source_module("R", "summary.R")
source_module("R", "interactive.R")

process_dataset <- function(input_path) {
  raw <- ingest_csv(input_path)
  validate_schema(raw)
  cleaned <- clean_all(raw)
  derived <- derive_fields(cleaned)
  filtered <- filter_years(derived, years = 2021:2023)
  list(raw = raw, cleaned = cleaned, derived = derived, filtered = filtered)
}

build_reports <- function(filtered_df) {
  list(
    report1 = build_report1(filtered_df),
    report2 = build_report2(filtered_df),
    report3 = build_report3(filtered_df)
  )
}

write_reports_and_summary <- function(reports, summary, outdir) {
  ensure_outdir(outdir)
  path1 <- write_report1(reports$report1, outdir)
  path2 <- write_report2(reports$report2, outdir)
  path3 <- write_report3(reports$report3, outdir)
  summary_path <- write_summary_outdir(summary, outdir)
  list(report1 = path1, report2 = path2, report3 = path3, summary = summary_path)
}

format_summary_value <- function(value) {
  if (length(value) == 0L || all(is.na(value))) {
    return("NA")
  }
  if (is.numeric(value)) {
    if (all(abs(value - round(value)) < .Machine$double.eps^0.5, na.rm = TRUE)) {
      return(format(round(value), big.mark = ",", trim = TRUE, scientific = FALSE))
    }
    return(format(value, digits = 4, nsmall = 2, big.mark = ",", trim = TRUE, scientific = FALSE))
  }
  paste(value, collapse = ", ")
}

run_interactive_mode <- function(args) {
  cat(INTERACTIVE_LANGUAGE_PROMPT)
  choice <- trimws(readline())
  while (!identical(choice, "1")) {
    if (!nzchar(choice)) {
      cat(INTERACTIVE_LANGUAGE_PROMPT)
    } else {
      cat("Invalid choice. Please enter 1 for the R implementation.\n")
      cat(INTERACTIVE_LANGUAGE_PROMPT)
    }
    choice <- trimws(readline())
  }

  dataset <- NULL

  repeat {
    cat(INTERACTIVE_REPORT_MENU_TITLE, "\n", sep = "")
    for (line in INTERACTIVE_REPORT_MENU_OPTIONS) {
      cat(line, "\n", sep = "")
    }
    cat("\n", INTERACTIVE_REPORT_MENU_PROMPT, sep = "")
    menu_choice <- trimws(readline())

    if (identical(menu_choice, "1")) {
      dataset <- process_dataset(args$input)
      loaded_rows <- nrow(dataset$raw)
      filtered_rows <- nrow(dataset$filtered)
      cat(sprintf("Processing dataset... (%d rows loaded, %d filtered for 2021–2023)\n", loaded_rows, filtered_rows))
    } else if (identical(menu_choice, "2")) {
      if (is.null(dataset)) {
        dataset <- process_dataset(args$input)
      }
      reports <- build_reports(dataset$filtered)
      summary_list <- build_summary(dataset$filtered)
      paths <- write_reports_and_summary(reports, summary_list, args$outdir)

      .run_interactive_spec(reports, preview_rows = 3L)

      cat(sprintf("Report 1 output: %s\n", basename(paths$report1)))
      cat(sprintf("Report 2 output: %s\n", basename(paths$report2)))
      cat(sprintf("Report 3 output: %s\n", basename(paths$report3)))
      cat(INTERACTIVE_SUMMARY_TITLE, "\n", sep = "")
      for (name in names(INTERACTIVE_SUMMARY_LABELS)) {
        label <- INTERACTIVE_SUMMARY_LABELS[[name]]
        value <- summary_list[[name]]
        cat(sprintf("  %s: %s\n", label, format_summary_value(value)))
      }
    } else {
      cat("Invalid choice. Please select 1 or 2.\n")
      next
    }

    cat("Back to Report Selection (Y/N):")
    again <- toupper(trimws(readline()))
    if (!identical(again, "Y")) {
      break
    }
  }

  invisible(NULL)
}

run_non_interactive_mode <- function(args, start_time) {
  log_info("Stage: Ingesting dataset with ingest_csv()")
  dataset <- ingest_csv(args$input)

  log_info("Stage: Validating schema with validate_schema()")
  validate_schema(dataset)

  log_info("Stage: Cleaning dataset with clean_all()")
  cleaned <- clean_all(dataset)

  log_info("Stage: Deriving analytical fields with derive_fields()")
  derived <- derive_fields(cleaned)

  log_info("Stage: Filtering dataset for years 2021–2023 with filter_years()")
  filtered <- filter_years(derived, years = 2021:2023)

  log_info("Stage: Building report data frames")
  reports <- build_reports(filtered)

  log_info("Stage: Building summary payload")
  summary_list <- build_summary(filtered)

  log_info("Stage: Writing report 1 to disk")
  report1_path <- write_report1(reports$report1, args$outdir)

  log_info("Stage: Writing report 2 to disk")
  report2_path <- write_report2(reports$report2, args$outdir)

  log_info("Stage: Writing report 3 to disk")
  report3_path <- write_report3(reports$report3, args$outdir)

  log_info("Stage: Writing summary JSON to disk")
  summary_path <- write_summary_outdir(summary_list, args$outdir)

  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  log_info("Completed successfully in %.2f seconds.", elapsed)

  invisible(list(
    args = args,
    reports = reports,
    summary = summary_list,
    paths = list(report1 = report1_path, report2 = report2_path, report3 = report3_path, summary = summary_path)
  ))
}

main <- function() {
  start_time <- Sys.time()

  parser <- build_cli()
  parsed_args <- optparse::parse_args(parser, args = commandArgs(trailingOnly = TRUE))
  parsed_args <- normalize_cli_paths(parsed_args)
  validate_cli_args(parsed_args)

  if (isTRUE(parsed_args$interactive)) {
    run_interactive_mode(parsed_args)
  } else {
    log_banner("DPWH Flood Control Pipeline")
    log_info("Stage: Building CLI parser via build_cli()")
    log_info("Stage: Normalising CLI arguments")
    log_info("Stage: Validating CLI arguments")
    run_non_interactive_mode(parsed_args, start_time)
  }
}

if (identical(environment(), globalenv()) && !exists(".__MCO2_MAIN_R_LOADED__", inherits = FALSE)) {
  assign(".__MCO2_MAIN_R_LOADED__", TRUE, envir = globalenv())
  main()
}
