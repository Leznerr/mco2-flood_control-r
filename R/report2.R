# report2.R
# ------------------------------------------------------------------------------
# Purpose   : Produce the Top Contractors Performance Ranking report.
# Contract  : build_report2(df) -> tibble with columns Rank, Contractor,
#   TotalCost, NumProjects, AvgDelay, TotalSavings, ReliabilityIndex, RiskFlag.
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({                             # quiet load
  library(dplyr)
})

build_report2 <- function(df) {                               # build report 2 contractor ranking
  if (!is.data.frame(df)) stop("build_report2(): 'df' must be a data frame.")

  df %>%
    dplyr::group_by(Contractor) %>%
    dplyr::summarise(
      TotalCost = sum(ContractCost, na.rm = TRUE),
      NumProjects = dplyr::n(),
      AvgDelay = mean(CompletionDelayDays, na.rm = TRUE),
      TotalSavings = sum(CostSavings, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      ReliabilityIndex = pmin(
        100,
        pmax(
          0,
          (1 - (AvgDelay / 90)) * (TotalSavings / pmax(TotalCost, 1)) * 100
        )
      ),
      RiskFlag = ifelse(is.na(ReliabilityIndex) | ReliabilityIndex < 50, "High Risk", "Low Risk")
    ) %>%
    dplyr::filter(NumProjects >= 5) %>%
    dplyr::arrange(dplyr::desc(TotalCost)) %>%
    dplyr::slice_head(n = 15) %>%
    dplyr::mutate(Rank = dplyr::row_number()) %>%
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

report_contractor_ranking <- function(df) {
  build_report2(df)
}
