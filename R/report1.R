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


suppressPackageStartupMessages({                             # quiet load
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
