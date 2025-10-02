# constants.R
# ------------------------------------------------------------------------------
# Purpose   : Provide shared literal strings used by the interactive CLI and
#             verification suite so that user-facing copy stays in sync with the
#             specification and automated checks. All constants defined here are
#             treated as the single source of truth for transcript headings and
#             column descriptions referenced in `main.R` and `verify.R`.
# ------------------------------------------------------------------------------

INTERACTIVE_LANGUAGE_PROMPT <- "Select language implementation:\n[1] R\n\nEnter choice:"

INTERACTIVE_REPORT_MENU_TITLE <- "Report Selection Menu"
INTERACTIVE_REPORT_MENU_OPTIONS <- c(
  "[1] Process dataset (ingest to filter)",
  "[2] Generate reports and summary"
)
INTERACTIVE_REPORT_MENU_PROMPT <- "Enter choice:"

REPORT_PREVIEW_TITLE <- "Report Preview"
REPORT_PREVIEW_RULE <- paste(rep("=", 72L), collapse = "")
REPORT_PREVIEW_ORDER <- c("report1", "report2", "report3")
REPORT_PREVIEW_HEADINGS <- list(
  report1 = "Report 1 Preview — Regional Flood Mitigation Efficiency Summary",
  report2 = "Report 2 Preview — Top Contractors Performance Ranking",
  report3 = "Report 3 Preview — Annual Project Type Cost Overrun Trends"
)

INTERACTIVE_SUMMARY_TITLE <- "Summary Metrics (summary.json)"
INTERACTIVE_SUMMARY_LABELS <- c(
  total_projects = "Total Projects",
  total_contractors = "Unique Contractors",
  total_provinces = "Unique Provinces",
  global_avg_delay = "Global Average Delay",
  total_savings = "Total Savings"
)

SUMMARY_FILENAME_LABEL <- "summary.json"

