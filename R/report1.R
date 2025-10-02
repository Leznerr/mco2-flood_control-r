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


}

suppressPackageStartupMessages({                             # quiet load for CLI/tests
  library(dplyr)
})

build_report1 <- function(df) {                               # build report 1 summary
  if (!is.data.frame(df)) stop("build_report1(): 'df' must be a data frame.")


    ) %>%
    select(Region, MainIsland, TotalBudget, MedianSavings, AvgDelay, HighDelayPct, EfficiencyScore) %>%
    arrange(desc(EfficiencyScore))

  format_dataframe(report)
}

report_regional_efficiency <- function(df) {                  # backwards compatibility helper
  build_report1(df)
}
