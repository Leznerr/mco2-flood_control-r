# report3.R
# ------------------------------------------------------------------------------
# Purpose   : Produce the Annual Project Type Cost Overrun Trends report.
# Contract  : report_overrun_trends(df) -> tibble with columns
#   FundingYear, TypeOfWork, TotalProjects, AvgSavings, OverrunRate, YoYChange.
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({                             # quiet load
  library(dplyr)
})

report_overrun_trends <- function(df) {                      # build report 3 time-series summary
  if (!is.data.frame(df)) stop("report_overrun_trends(): 'df' must be a data frame.")
  summary <- df %>%
    group_by(FundingYear, TypeOfWork) %>%
    summarise(
      TotalProjects = dplyr::n(),
      AvgSavings = safe_mean(CostSavings),
      OverrunRate = 100 * safe_mean(CostSavings < 0),
      .groups = "drop"
    )

  baseline <- summary %>%
    filter(FundingYear == 2021) %>%
    transmute(TypeOfWork, Base2021 = AvgSavings)

  summary %>%
    left_join(baseline, by = "TypeOfWork") %>%
    mutate(
      YoYChange = dplyr::if_else(
        FundingYear == 2021 | is.na(Base2021) | Base2021 == 0,
        NA_real_,
        ((AvgSavings - Base2021) / abs(Base2021)) * 100
      )
    ) %>%
    select(FundingYear, TypeOfWork, TotalProjects, AvgSavings, OverrunRate, YoYChange) %>%
    arrange(FundingYear, desc(AvgSavings), TypeOfWork)
}
