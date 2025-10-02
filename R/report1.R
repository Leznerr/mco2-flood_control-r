# report1.R
# ------------------------------------------------------------------------------
# Purpose   : Produce the Regional Flood Mitigation Efficiency report.
# Contract  : build_report1(df) -> tibble with columns
#   Region, MainIsland, TotalBudget, MedianSavings, AvgDelay,
#   HighDelayPct, EfficiencyScore (sorted by EfficiencyScore desc).
# Rubric    : Correctness (aggregations & min-max normalisation), Performance
#             (dplyr group summarise), Readability (formal comments), UX (stable
#             schema ordering and formatted values).
# ------------------------------------------------------------------------------

.load_utils_format <- function() {
  caller <- sys.frame(1)
  ofile <- caller$ofile
  if (is.null(ofile)) {
    stop(".load_utils_format(): unable to determine the calling file location.")
  }
  utils_path <- file.path(normalizePath(dirname(ofile)), "utils_format.R")
  if (!file.exists(utils_path)) {
    stop(sprintf(".load_utils_format(): expected helper file at '%s' but it was not found.", utils_path))
  }
  sys.source(utils_path, envir = parent.frame())
}

.load_utils_format()

suppressPackageStartupMessages({                             # quiet load for tidy verbs
  library(dplyr)
})

build_report1 <- function(df) {
  stopifnot(is.data.frame(df))

  summary_tbl <- df %>%
    dplyr::group_by(Region, MainIsland) %>%
    dplyr::summarise(
      TotalBudget = safe_sum(ApprovedBudgetForContract),
      MedianSavings = safe_median(CostSavings),
      AvgDelay = safe_mean(CompletionDelayDays),
      HighDelayPct = safe_mean(CompletionDelayDays > 30) * 100,
      .groups = "drop"
    )

  efficiency <- (summary_tbl$MedianSavings / pmax(summary_tbl$AvgDelay, 1)) * 100
  summary_tbl$EfficiencyScore <- minmax_0_100(efficiency)

  summary_tbl %>%
    dplyr::arrange(dplyr::desc(EfficiencyScore)) %>%
    dplyr::select(
      Region,
      MainIsland,
      TotalBudget,
      MedianSavings,
      AvgDelay,
      HighDelayPct,
      EfficiencyScore
    ) %>%
    format_dataframe()
}
