# report2.R
# ------------------------------------------------------------------------------
# Purpose   : Produce the Top Contractors Performance Ranking report.
# Contract  : report_contractor_ranking(df) -> tibble with columns
#   Rank, Contractor, TotalCost, NumProjects, AvgDelay, TotalSavings,
#   ReliabilityIndex, RiskFlag. Keeps contractors with ≥5 projects, ranks top 15
#   by TotalCost (descending).
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({                             # quiet load
  library(dplyr)
})

report_contractor_ranking <- function(df) {                  # build contractor leaderboard
  if (!is.data.frame(df)) stop("report_contractor_ranking(): 'df' must be a data frame.")
  df %>%
    group_by(Contractor) %>%
    summarise(
      TotalCost = safe_sum(ContractCost),
      NumProjects = dplyr::n(),
      AvgDelay = safe_mean(CompletionDelayDays),
      TotalSavings = safe_sum(CostSavings),
      .groups = "drop"
    ) %>%
    filter(NumProjects >= 5) %>%
    arrange(desc(TotalCost), Contractor) %>%
    slice_head(n = 15) %>%
    mutate(
      ri_raw = (1 - pmax(0, AvgDelay) / 90) * pmax(0, TotalSavings) / pmax(1, TotalCost),
      ReliabilityIndex = pmax(0, pmin(100, 100 * ri_raw)),
      RiskFlag = ifelse(ReliabilityIndex < 50, "High Risk", "Low Risk"),
      Rank = dplyr::row_number()
    ) %>%
    select(Rank, Contractor, TotalCost, NumProjects, AvgDelay, TotalSavings, ReliabilityIndex, RiskFlag)
}
