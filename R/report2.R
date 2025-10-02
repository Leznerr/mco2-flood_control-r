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


      TotalCost = sum(ContractCost, na.rm = TRUE),
      NumProjects = dplyr::n(),
      AvgDelay = mean(CompletionDelayDays, na.rm = TRUE),
      TotalSavings = sum(CostSavings, na.rm = TRUE),
      .groups = "drop"
    ) %>%

        ri <- pmax(ri, 0)
        ri <- pmin(ri, 100)
        ri
      },

}
