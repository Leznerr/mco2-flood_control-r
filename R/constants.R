# constants.R
# ------------------------------------------------------------------------------
# Purpose   : Central place for shared string constants used by the pipeline,
#             especially values that must stay in sync between the interactive
#             CLI preview and the verification suite.
# ------------------------------------------------------------------------------

REPORT_FILES <- list(
  r1 = "report1_regional_efficiency.csv",
  r2 = "report2_top_contractors.csv",
  r3 = "report3_overrun_trends.csv",
  summary = "summary.json"
)

REPORT_PREVIEW_TITLE <- "Interactive Preview"
REPORT_PREVIEW_RULE  <- "==================="

REPORT_PREVIEW_HEADINGS <- list(
  report1 = "Report 1: Regional Flood Mitigation Efficiency",
  report2 = "Report 2: Top Contractors Performance Ranking",
  report3 = "Report 3: Annual Project Type Cost Overrun Trends"
)

REPORT_PREVIEW_ORDER <- c("report1", "report2", "report3")

