# verify.R
# ------------------------------------------------------------------------------
# Purpose   : Post-generation verification suite that inspects the produced
#             artefacts and emits a human-readable verification report.
# Contract  : verify_outputs(dataset, reports, summary, outdir, fmt_opts)
#             - dataset : filtered/derived data frame (2021-2023 only).
#             - reports : named list with raw report data frames (report1..3).
#             - summary : named list ready for JSON export.
#             - outdir  : output directory containing exported files.
#             - fmt_opts: list with formatting directives (digits, comma strings,
#                        exclude lists) for reference.
#             The function writes outputs/verification_report.txt and stops with
#             an error when a verification step fails.
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(jsonlite)
})

`%||%` <- function(lhs, rhs) if (!is.null(lhs)) lhs else rhs

.status_label <- function(ok) if (ok) "[PASS]" else "[FAIL]"

.verify_numeric_format <- function(values) {
  vals <- values[!is.na(values) & nzchar(values)]
  if (length(vals) == 0L) return(TRUE)
  pattern <- "^-?\\d{1,3}(,\\d{3})*\\.\\d{2}$"
  all(grepl(pattern, vals))
}

.verify_integer_format <- function(values) {
  vals <- values[!is.na(values) & nzchar(values)]
  if (length(vals) == 0L) return(TRUE)
  all(grepl("^-?\\d+$", vals))
}

.read_csv_as_character <- function(path) {
  readr::read_csv(path, col_types = readr::cols(.default = readr::col_character()))
}

.default_report_files <- function() {
  list(
    r1 = "report1_regional_efficiency.csv",
    r2 = "report2_top_contractors.csv",
    r3 = "report3_overrun_trends.csv",
    summary = "summary.json"
  )
}

.expected_preview_titles <- function() {
  c(
    "Report 1: Regional Flood Mitigation Efficiency",
    "Report 2: Top Contractors Performance Ranking",
    "Report 3: Annual Project Type Cost Overrun Trends"
  )
}

verify_outputs <- function(dataset, reports, summary, outdir, fmt_opts) {
  if (!dir.exists(outdir)) {
    stop(sprintf("verify_outputs(): outdir '%s' does not exist.", outdir))
  }
  if (!is.list(reports) || !all(c("report1", "report2", "report3") %in% names(reports))) {
    stop("verify_outputs(): 'reports' must include report1, report2, report3.")
  }
  if (!is.list(summary)) stop("verify_outputs(): 'summary' must be a list.")

  report_files <- if (exists("REPORT_FILES", inherits = TRUE) && is.list(REPORT_FILES)) {
    REPORT_FILES
  } else {
    .default_report_files()
  }

  path1 <- file.path(outdir, report_files$r1)
  path2 <- file.path(outdir, report_files$r2)
  path3 <- file.path(outdir, report_files$r3)
  path_summary_json <- file.path(outdir, report_files$summary)

  report_lines <- c(
    "Verification Report",
    "====================",
    sprintf("Generated: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    ""
  )
  failures <- logical()

  append_check <- function(passed, message) {
    report_lines <<- c(report_lines, sprintf("%s %s", .status_label(passed), message))
    failures <<- c(failures, !passed)
  }

  report_lines <- c(report_lines, "Schema & Formatting", "----------------------")

  r1_file <- .read_csv_as_character(path1)
  r2_file <- .read_csv_as_character(path2)
  r3_file <- .read_csv_as_character(path3)

  expected_r1 <- c("Region", "MainIsland", "TotalApprovedBudget", "MedianSavings", "AvgDelay", "Delay30Rate", "EfficiencyScore")
  expected_r2 <- c("Contractor", "NumProjects", "TotalCost", "AvgDelay", "TotalSavings", "ReliabilityIndex", "RiskFlag")
  expected_r3 <- c("FundingYear", "TypeOfWork", "TotalProjects", "AvgSavings", "OverrunRate", "YoYChange")

  append_check(identical(names(r1_file), expected_r1), "Report 1 header matches expected schema.")
  append_check(identical(names(r2_file), expected_r2), "Report 2 header matches expected schema.")
  append_check(identical(names(r3_file), expected_r3), "Report 3 header matches expected schema.")

  append_check(.verify_numeric_format(r1_file$TotalApprovedBudget), "Report 1 monetary fields formatted with commas and 2 decimals.")
  append_check(.verify_numeric_format(r1_file$MedianSavings), "Report 1 savings column formatted with commas and 2 decimals.")
  append_check(.verify_numeric_format(r1_file$AvgDelay), "Report 1 average delay formatted to two decimals.")
  append_check(.verify_numeric_format(r1_file$Delay30Rate), "Report 1 delay rate formatted to two decimals.")
  append_check(.verify_numeric_format(r1_file$EfficiencyScore), "Report 1 efficiency scores formatted to two decimals.")

  append_check(.verify_integer_format(r2_file$NumProjects), "Report 2 project counts are integers.")
  append_check(.verify_numeric_format(r2_file$TotalCost), "Report 2 total cost formatted with commas and 2 decimals.")
  append_check(.verify_numeric_format(r2_file$AvgDelay), "Report 2 average delay formatted to two decimals.")
  append_check(.verify_numeric_format(r2_file$TotalSavings), "Report 2 total savings formatted with commas and 2 decimals.")
  append_check(.verify_numeric_format(r2_file$ReliabilityIndex), "Report 2 reliability index formatted to two decimals.")

  append_check(.verify_integer_format(r3_file$TotalProjects), "Report 3 total projects column is integer formatted.")
  append_check(.verify_numeric_format(r3_file$AvgSavings), "Report 3 average savings formatted to two decimals.")
  append_check(.verify_numeric_format(r3_file$OverrunRate), "Report 3 overrun rate formatted to two decimals.")
  append_check(.verify_numeric_format(r3_file$YoYChange), "Report 3 YoY change formatted to two decimals (ignoring blanks).")

  report_lines <- c(report_lines, "", "Sorting & Value Integrity", "---------------------------")

  r1_sorted <- reports$report1 %>% arrange(desc(EfficiencyScore), Region, MainIsland)
  r2_sorted <- reports$report2 %>% arrange(desc(TotalCost), Contractor)
  r3_sorted <- reports$report3 %>% arrange(FundingYear, desc(AvgSavings), TypeOfWork)

  append_check(identical(r1_sorted, reports$report1), "Report 1 sorted by EfficiencyScore desc, Region, MainIsland.")
  append_check(identical(r2_sorted, reports$report2), "Report 2 sorted by TotalCost desc then Contractor.")
  append_check(identical(r3_sorted, reports$report3), "Report 3 sorted by FundingYear asc then AvgSavings desc.")

  efficiency_ok <- all(is.na(reports$report1$EfficiencyScore) | (reports$report1$EfficiencyScore >= 0 & reports$report1$EfficiencyScore <= 100))
  delayrate_ok <- all(is.na(reports$report1$Delay30Rate) | (reports$report1$Delay30Rate >= 0 & reports$report1$Delay30Rate <= 100))
  reliability_ok <- all(is.na(reports$report2$ReliabilityIndex) | (reports$report2$ReliabilityIndex <= 100))
  overrun_ok <- all(is.na(reports$report3$OverrunRate) | (reports$report3$OverrunRate >= 0 & reports$report3$OverrunRate <= 100))

  append_check(efficiency_ok, "EfficiencyScore within [0,100].")
  append_check(delayrate_ok, "Delay30Rate within [0,100].")
  append_check(reliability_ok, "ReliabilityIndex ≤ 100 (negatives allowed).")
  append_check(overrun_ok, "OverrunRate within [0,100].")

  risk_flag_expected <- ifelse(is.na(reports$report2$ReliabilityIndex) | reports$report2$ReliabilityIndex < 50, "High Risk", "Low Risk")
  append_check(identical(reports$report2$RiskFlag, risk_flag_expected), "RiskFlag aligns with ReliabilityIndex threshold.")

  yoy_check <- reports$report3 %>% filter(FundingYear == 2021) %>% pull(YoYChange)
  append_check(all(is.na(yoy_check)), "YoYChange is NA for FundingYear 2021.")

  report_lines <- c(report_lines, "", "Data Integrity", "----------------")

  years_ok <- all(dataset$FundingYear >= 2021 & dataset$FundingYear <= 2023)
  append_check(years_ok, "Dataset limited to FundingYear 2021–2023.")

  cost_savings_expected <- dataset$ApprovedBudgetForContract - dataset$ContractCost
  savings_diff <- cost_savings_expected - dataset$CostSavings
  savings_ok <- all(is.na(savings_diff) | abs(savings_diff) < 1e-6)
  append_check(savings_ok, "CostSavings matches ApprovedBudgetForContract − ContractCost.")

  delay_expected <- as.numeric(dataset$ActualCompletionDate - dataset$StartDate)
  delay_diff <- delay_expected - dataset$CompletionDelayDays
  delay_ok <- all(is.na(delay_diff) | abs(delay_diff) < 1e-6)
  append_check(delay_ok, "CompletionDelayDays equals ActualCompletionDate − StartDate.")

  summary_file <- jsonlite::fromJSON(path_summary_json)
  summary_keys_ok <- all(sort(names(summary_file)) == sort(c("total_projects", "total_contractors", "total_provinces", "global_avg_delay", "total_savings")))
  append_check(summary_keys_ok, "summary.json contains required keys.")

  summary_match <- TRUE
  for (nm in names(summary)) {
    left <- summary[[nm]]
    right <- summary_file[[nm]]
    if (is.numeric(left) && is.numeric(right)) {
      summary_match <- summary_match && (isTRUE(all.equal(left, right, tolerance = 1e-6)) || (is.na(left) && is.na(right)))
    } else {
      summary_match <- summary_match && identical(left, right)
    }
  }
  append_check(summary_match, "summary.json values match in-memory summary list.")

  report_lines <- c(report_lines, "", "UX & Documentation", "----------------------")

  preview_expected <- .expected_preview_titles()
  preview_map <- if (exists("REPORT_PREVIEW_HEADINGS", inherits = TRUE) && is.list(REPORT_PREVIEW_HEADINGS)) {
    REPORT_PREVIEW_HEADINGS
  } else {
    stats::setNames(as.list(preview_expected), c("report1", "report2", "report3"))
  }
  preview_order <- if (exists("REPORT_PREVIEW_ORDER", inherits = TRUE)) REPORT_PREVIEW_ORDER else names(preview_map)
  preview_headings <- unname(unlist(preview_map[preview_order]))
  preview_ok <- length(preview_headings) == length(preview_expected) && all(preview_headings == preview_expected)
  append_check(preview_ok, "Interactive preview headings mirror specification titles.")

  readme_text <- tryCatch(readr::read_file("README.md"), error = function(e) "")
  rubric_ok <- grepl("Rubric", readme_text, ignore.case = TRUE) && grepl("Simplicity", readme_text, ignore.case = TRUE)
  append_check(rubric_ok, "README documents rubric alignment for Simplicity/Performance/Readability/Correctness/UX.")

  report_lines <- c(report_lines, "", "Formatting Parameters", "-----------------------")
  report_lines <- c(
    report_lines,
    sprintf("Comma formatting enabled: %s", isTRUE(fmt_opts$comma_strings)),
    sprintf("Numeric digits: %s", fmt_opts$digits)
  )

  report_lines <- c(report_lines, "", "Rubric Mapping", "--------------")

  append_rubric <- function(passed, message) {
    report_lines <<- c(report_lines, sprintf("- %s %s", .status_label(passed), message))
    failures <<- c(failures, !passed)
  }

  readme_lines <- strsplit(readme_text, "\\r?\\n", perl = TRUE)[[1]]
  readme_lines <- readme_lines[nzchar(readme_lines)]

  grab_bullet <- function(pattern, fallback_label) {
    idx <- which(grepl(pattern, readme_lines, perl = TRUE))
    if (length(idx) > 0L) {
      list(text = sub("^\\s*-\\s+", "", readme_lines[idx[1]]), found = TRUE)
    } else {
      list(text = sprintf("**%s** – README rubric entry missing.", fallback_label), found = FALSE)
    }
  }

  rubric_bullets <- list(
    Simplicity   = grab_bullet("^\\s*-\\s+\\*\\*Simplicity\\*\\*\\s+[-\\u2013]\\s+.+$", "Simplicity"),
    Performance  = grab_bullet("^\\s*-\\s+\\*\\*Performance\\*\\*\\s+[-\\u2013]\\s+.+$", "Performance"),
    Readability  = grab_bullet("^\\s*-\\s+\\*\\*Readability\\*\\*\\s+[-\\u2013]\\s+.+$", "Readability"),
    Correctness  = grab_bullet("^\\s*-\\s+\\*\\*Correctness\\*\\*\\s+[-\\u2013]\\s+.+$", "Correctness"),
    UX           = grab_bullet("^\\s*-\\s+\\*\\*(?:UX|User Experience)\\*\\*\\s+[-\\u2013]\\s+.+$", "UX")
  )

  module_files <- file.path("R", c("ingest.R", "validate.R", "clean.R", "derive.R", "report1.R", "report2.R", "report3.R", "summary.R", "verify.R"))
  missing_modules <- setdiff(module_files, module_files[file.exists(module_files)])
  simplicity_ok <- rubric_bullets$Simplicity$found && length(missing_modules) == 0L
  simplicity_evidence <- if (simplicity_ok) {
    sprintf("Modules present: %s", paste(basename(module_files), collapse = ", "))
  } else if (length(missing_modules) > 0L) {
    sprintf("Missing modules: %s", paste(basename(missing_modules), collapse = ", "))
  } else {
    "No module files enumerated."
  }

  helper_functions <- c("safe_mean", "safe_median", "safe_sum", "minmax_0_100")
  helpers_available <- helper_functions[vapply(helper_functions, exists, logical(1), mode = "function", inherits = TRUE)]
  missing_helpers <- setdiff(helper_functions, helpers_available)
  performance_ok <- rubric_bullets$Performance$found && length(missing_helpers) == 0L
  performance_evidence <- if (performance_ok) {
    sprintf("Vectorised helpers detected: %s", paste(helper_functions, collapse = ", "))
  } else if (length(missing_helpers) > 0L) {
    sprintf("Missing helpers: %s", paste(missing_helpers, collapse = ", "))
  } else {
    "No helper functions enumerated."
  }

  doc_files <- file.path("R", c("ingest.R", "clean.R", "derive.R", "report1.R", "report2.R", "report3.R"))
  doc_checks <- vapply(doc_files, function(path) {
    if (!file.exists(path)) return(FALSE)
    any(grepl("^#\\s+Purpose", readLines(path, warn = FALSE)))
  }, logical(1))
  readability_ok <- rubric_bullets$Readability$found && all(doc_checks)
  readability_evidence <- if (readability_ok) {
    sprintf("Purpose headers detected in: %s", paste(basename(doc_files), collapse = ", "))
  } else {
    missing_docs <- basename(doc_files[!doc_checks])
    if (length(missing_docs) > 0L) {
      sprintf("Missing purpose headers in: %s", paste(missing_docs, collapse = ", "))
    } else {
      "No documentation files enumerated."
    }
  }

  previous_failures <- any(failures)
  correctness_ok <- rubric_bullets$Correctness$found && !previous_failures
  correctness_evidence <- if (correctness_ok) {
    "Schema, data, and UX checks above passed."
  } else {
    "See failure entries above for context."
  }

  ux_defined <- preview_ok && exists("REPORT_PREVIEW_HEADINGS", inherits = TRUE)
  ux_ok <- rubric_bullets$UX$found && ux_defined
  ux_evidence <- if (ux_defined) {
    sprintf("Preview headings configured: %s", paste(preview_headings, collapse = " | "))
  } else {
    "REPORT_PREVIEW_HEADINGS constant unavailable or mismatched."
  }

  rubric_results <- list(
    list(ok = simplicity_ok, text = rubric_bullets$Simplicity$text, evidence = simplicity_evidence),
    list(ok = performance_ok, text = rubric_bullets$Performance$text, evidence = performance_evidence),
    list(ok = readability_ok, text = rubric_bullets$Readability$text, evidence = readability_evidence),
    list(ok = correctness_ok, text = rubric_bullets$Correctness$text, evidence = correctness_evidence),
    list(ok = ux_ok, text = rubric_bullets$UX$text, evidence = ux_evidence)
  )

  for (entry in rubric_results) {
    message <- entry$text
    if (!is.null(entry$evidence) && nzchar(entry$evidence)) {
      message <- sprintf("%s (%s)", message, entry$evidence)
    }
    append_rubric(entry$ok, message)
  }

  verification_path <- file.path(outdir, "verification_report.txt")
  readr::write_lines(report_lines, verification_path)

  if (any(failures)) {
    stop(sprintf("Verification failed; see %s for details.", verification_path))
  }

  invisible(verification_path)
}
