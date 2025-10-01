# constants.R
# ------------------------------------------------------------------------------
# Purpose   : Provide shared literal strings used by the interactive CLI and
#             verification suite so that user-facing copy stays in sync with the
#             specification and automated checks. All constants defined here are
#             treated as the single source of truth for transcript headings and
#             column descriptions referenced in `main.R` and `verify.R`.
# ------------------------------------------------------------------------------

INTERACTIVE_MENU_TITLE <- "Select Language Implementation:"
INTERACTIVE_MENU_OPTIONS <- c(
  "[1] Load the file",
  "[2] Generate Reports"
)

INTERACTIVE_REPORT_HEADINGS <- c(
  "Report 1: Regional Flood Mitigation Efficiency Summary",
  "Report 2: Top Contractors Performance Ranking",
  "Report 3: Annual Project Type Cost Overrun Trends"
)

INTERACTIVE_REPORT_COLUMNS <- list(
  report1 = "Region, MainIsland, TotalApprovedBudget, MedianSavings, AvgDelay, Delay30Rate, EfficiencyScore",
  report2 = "Contractor, NumProjects, TotalCost, AvgDelay, TotalSavings, ReliabilityIndex, RiskFlag",
  report3 = "FundingYear, TypeOfWork, TotalProjects, AvgSavings, OverrunRate, YoYChange"
)

SUMMARY_FILENAME_LABEL <- "summary.json"

