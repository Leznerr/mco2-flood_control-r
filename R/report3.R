# report3.R
# ------------------------------------------------------------------------------
# Purpose   : Produce the Annual Project Type Cost Overrun Trends report.
# Contract  : report_overrun_trends(df) -> tibble with columns
#   FundingYear, TypeOfWork, TotalProjects, AvgSavings, OverrunRate, YoYChange.
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({                             # quiet load
  library(dplyr)
})

build_report3 <- function(df) {                              # build report 3 time-series summary
  if (!is.data.frame(df)) stop("build_report3(): 'df' must be a data frame.")

  df %>%
    group_by(TypeOfWork, FundingYear) %>%
    summarise(
      TotalProjects = dplyr::n(),
      AvgSavings = mean(CostSavings, na.rm = TRUE),
      OverrunRate = 100 * mean(CostSavings < 0, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      AvgSavings = ifelse(is.nan(AvgSavings), NA_real_, AvgSavings),
      OverrunRate = ifelse(is.nan(OverrunRate), NA_real_, OverrunRate)
    ) %>%
    arrange(TypeOfWork, FundingYear) %>%
    group_by(TypeOfWork) %>%
    mutate(
      PrevAvg = dplyr::lag(AvgSavings),
      YoYChange = dplyr::if_else(
        is.na(PrevAvg) | PrevAvg == 0,
        NA_real_,
        100 * (AvgSavings - PrevAvg) / abs(PrevAvg)
      ),
      YoYChange = ifelse(is.na(YoYChange), "", as.character(YoYChange))
    ) %>%
    ungroup() %>%
    select(-PrevAvg) %>%
    arrange(FundingYear, desc(AvgSavings), TypeOfWork) %>%
    select(FundingYear, TypeOfWork, TotalProjects, AvgSavings, OverrunRate, YoYChange)
}

report_overrun_trends <- build_report3
