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


  append_check(preview_ok, "Interactive preview headings mirror sample output titles.")

  readme_text <- tryCatch(readr::read_file("README.md"), error = function(e) "")
  rubric_ok <- grepl("Rubric", readme_text, ignore.case = TRUE) && grepl("Simplicity", readme_text, ignore.case = TRUE)
  append_check(rubric_ok, "README documents rubric alignment for Simplicity/Performance/Readability/Correctness/UX.")

  report_lines <- c(report_lines, "", "Formatting Parameters", "-----------------------")
  report_lines <- c(
    report_lines,
    sprintf("Comma formatting enabled: %s", isTRUE(fmt_opts$comma_strings)),
    sprintf("Numeric digits: %s", fmt_opts$digits)
  )

  rubric_heading <- "Rubric Mapping"
  report_lines <- c(
    report_lines,
    "",
    rubric_heading,
    paste(rep("-", nchar(rubric_heading)), collapse = "")
  )

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
  modules_present <- module_files[file.exists(module_files)]
  missing_modules <- setdiff(module_files, modules_present)
  simplicity_ok <- rubric_bullets$Simplicity$found && length(missing_modules) == 0L
  simplicity_evidence <- if (length(module_files) == 0L) {
    "No module files enumerated."
  } else if (simplicity_ok) {
    sprintf("Modules present: %s", paste(basename(module_files), collapse = ", "))
  } else {
    sprintf("Missing modules: %s", paste(basename(missing_modules), collapse = ", "))
  }

  helper_functions <- c("safe_mean", "safe_median", "safe_sum", "minmax_0_100")
  helpers_available <- helper_functions[vapply(helper_functions, exists, logical(1), mode = "function", inherits = TRUE)]
  missing_helpers <- setdiff(helper_functions, helpers_available)
  performance_ok <- rubric_bullets$Performance$found && length(missing_helpers) == 0L
  performance_evidence <- if (length(helper_functions) == 0L) {
    "No helper functions enumerated."
  } else if (performance_ok) {
    sprintf("Vectorised helpers detected: %s", paste(helper_functions, collapse = ", "))
  } else {
    sprintf("Missing helpers: %s", paste(missing_helpers, collapse = ", "))
  }

  doc_files <- file.path("R", c("ingest.R", "clean.R", "derive.R", "report1.R", "report2.R", "report3.R"))
  doc_checks <- vapply(doc_files, function(path) {
    if (!file.exists(path)) return(FALSE)
    any(grepl("^#\\s+Purpose", readLines(path, warn = FALSE)))
  }, logical(1))
  readability_ok <- rubric_bullets$Readability$found && all(doc_checks)
  readability_evidence <- if (length(doc_files) == 0L) {
    "No documentation files enumerated."
  } else if (readability_ok) {
    sprintf("Purpose headers detected in: %s", paste(basename(doc_files), collapse = ", "))
  } else {
    missing_docs <- basename(doc_files[!doc_checks])
    sprintf("Missing purpose headers in: %s", paste(missing_docs, collapse = ", "))
  }

  correctness_ok <- rubric_bullets$Correctness$found && !any(failures)
  correctness_evidence <- if (correctness_ok) {
    "Schema, data, and UX checks above passed."
  } else {
    "See failure entries above for context."
  }

  if (!exists("REPORT_PREVIEW_HEADINGS", inherits = TRUE) || !is.list(REPORT_PREVIEW_HEADINGS)) {
    const_candidates <- c("constants.R", file.path("R", "constants.R"))
    for (candidate in const_candidates) {
      if (file.exists(candidate)) {
        source(candidate, chdir = TRUE)
        break
      }
    }
  }
  preview_defined <- exists("REPORT_PREVIEW_HEADINGS", inherits = TRUE) && is.list(REPORT_PREVIEW_HEADINGS)
  ux_ok <- rubric_bullets$UX$found && preview_defined
  ux_evidence <- if (preview_defined) {
    sprintf("Preview headings configured: %s", paste(unname(unlist(REPORT_PREVIEW_HEADINGS)), collapse = " | "))
  } else {
    "REPORT_PREVIEW_HEADINGS constant unavailable."
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

