# report2.R
# ------------------------------------------------------------------------------
# Purpose   : Produce the Top Contractors Performance Ranking report.
# Contract  : build_report2(df) -> tibble with columns Rank, Contractor,
#   TotalCost, NumProjects, AvgDelay, TotalSavings, ReliabilityIndex, RiskFlag.
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({                             # quiet load
  library(dplyr)
})

build_report2 <- function(df) {                              # build contractor leaderboard
  if (!is.data.frame(df)) stop("build_report2(): 'df' must be a data frame.")

  summary_tbl <- df %>%
    dplyr::group_by(Contractor) %>%
    dplyr::summarise(
      TotalCost = sum(ContractCost, na.rm = TRUE),
      NumProjects = dplyr::n(),
      AvgDelay = mean(CompletionDelayDays, na.rm = TRUE),
      TotalSavings = sum(CostSavings, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      AvgDelay = ifelse(is.nan(AvgDelay), NA_real_, AvgDelay)
    ) %>%
    dplyr::filter(NumProjects >= 5)

  ranked <- summary_tbl %>%
    dplyr::arrange(dplyr::desc(TotalCost), Contractor) %>%
    dplyr::slice_head(n = 15) %>%
    dplyr::mutate(
      ReliabilityIndex = {
        ri <- (1 - (AvgDelay / 90)) * (TotalSavings / pmax(TotalCost, 1)) * 100
        ri[is.nan(ri) | is.infinite(ri)] <- NA_real_
        ri <- pmax(ri, 0)
        ri <- pmin(ri, 100)
        ri
      },
      RiskFlag = ifelse(is.na(ReliabilityIndex) | ReliabilityIndex < 50, "High Risk", "Low Risk"),
      Rank = dplyr::row_number()
    )

  ranked %>%
    dplyr::select(
      Rank,
      Contractor,
      TotalCost,
      NumProjects,
      AvgDelay,
      TotalSavings,
      ReliabilityIndex,
      RiskFlag
    )
}

report_contractor_ranking <- function(df) {                  # backwards-compatible entry point
  build_report2(df)
}
