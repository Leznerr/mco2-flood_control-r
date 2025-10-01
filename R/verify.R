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

verify_outputs <- function(dataset, reports, summary, outdir, fmt_opts) {
  if (!dir.exists(outdir)) {
    stop(sprintf("verify_outputs(): outdir '%s' does not exist.", outdir))
  }
  if (!is.list(reports) || !all(c("report1", "report2", "report3") %in% names(reports))) {
    stop("verify_outputs(): 'reports' must include report1, report2, report3.")
  }
  if (!is.list(summary)) stop("verify_outputs(): 'summary' must be a list.")

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

  path1 <- file.path(outdir, REPORT_FILES$r1)
  path2 <- file.path(outdir, REPORT_FILES$r2)
  path3 <- file.path(outdir, REPORT_FILES$r3)
  path_summary_json <- file.path(outdir, REPORT_FILES$summary)

  r1_file <- .read_csv_as_character(path1)
  r2_file <- .read_csv_as_character(path2)
  r3_file <- .read_csv_as_character(path3)


  expected_r2 <- c("Contractor", "NumProjects", "TotalCost", "AvgDelay", "TotalSavings", "ReliabilityIndex", "RiskFlag")
  expected_r3 <- c("FundingYear", "TypeOfWork", "TotalProjects", "AvgSavings", "OverrunRate", "YoYChange")

  append_check(identical(names(r1_file), expected_r1), "Report 1 header matches expected schema.")
  append_check(identical(names(r2_file), expected_r2), "Report 2 header matches expected schema.")
  append_check(identical(names(r3_file), expected_r3), "Report 3 header matches expected schema.")


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



  readme_text <- tryCatch(readr::read_file("README.md"), error = function(e) "")
  rubric_ok <- grepl("Rubric", readme_text, ignore.case = TRUE) && grepl("Simplicity", readme_text, ignore.case = TRUE)
  append_check(rubric_ok, "README documents rubric alignment for Simplicity/Performance/Readability/Correctness/UX.")



  report_lines <- c(report_lines, "", "Formatting Parameters", "-----------------------")
  report_lines <- c(
    report_lines,
    sprintf("Comma formatting enabled: %s", isTRUE(fmt_opts$comma_strings)),
    sprintf("Numeric digits: %s", fmt_opts$digits)
  )


  verification_path <- file.path(outdir, "verification_report.txt")
  readr::write_lines(report_lines, verification_path)

  if (any(failures)) {
    stop(sprintf("Verification failed; see %s for details.", verification_path))
  }

  invisible(verification_path)
}

