# report2.R
# ------------------------------------------------------------------------------
# Purpose   : Produce the Top Contractors Performance Ranking report.
# Contract  : report_contractor_ranking(df) -> tibble with columns
#   Contractor, NumProjects, TotalCost, AvgDelay, TotalSavings,
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
      ReliabilityIndex = {
        ri <- (1 - (AvgDelay / 90)) * (TotalSavings / TotalCost) * 100
        bad <- !is.finite(ri) | is.na(TotalCost) | TotalCost <= 0
        ri[bad] <- NA_real_
        ri <- pmin(pmax(ri, 0), 100)
        ri
      },
      RiskFlag = dplyr::if_else(is.na(ReliabilityIndex) | ReliabilityIndex < 50, "High Risk", "Low Risk")
    ) %>%
    select(Contractor, NumProjects, TotalCost, AvgDelay, TotalSavings, ReliabilityIndex, RiskFlag)
}
