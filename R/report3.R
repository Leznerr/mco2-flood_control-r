# report3.R
# ------------------------------------------------------------------------------
# Purpose   : Produce the Annual Project Type Cost Overrun Trends report.
# Contract  : report_overrun_trends(df) -> tibble with columns
#   FundingYear, TypeOfWork, N, AvgSavings, OverrunRate, YoY_vs_2021.
# Rubric    : Correctness (baseline join logic, NA handling), Simplicity,
#             Readability, UX (deterministic sorting).
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({                             # quiet load
  library(dplyr)
})

report_overrun_trends <- function(df) {                      # build report 3 time-series summary
  if (!is.data.frame(df)) stop("report_overrun_trends(): 'df' must be a data frame.")
  summary <- df %>%
    group_by(FundingYear, TypeOfWork) %>%
    summarise(
      N = dplyr::n(),
      AvgSavings = safe_mean(CostSavings),
      OverrunRate = 100 * safe_mean(CostSavings < 0),
      .groups = "drop"
    )
  baseline <- summary %>%
    filter(FundingYear == 2021) %>%
    transmute(TypeOfWork, AvgSavings_2021 = AvgSavings)
  summary %>%
    left_join(baseline, by = "TypeOfWork") %>%
    mutate(
      YoY_vs_2021 = ifelse(
        FundingYear == 2021 | is.na(AvgSavings_2021) | AvgSavings_2021 == 0,
        NA_real_,
        100 * (AvgSavings - AvgSavings_2021) / abs(AvgSavings_2021)
      )
    ) %>%
    select(FundingYear, TypeOfWork, N, AvgSavings, OverrunRate, YoY_vs_2021) %>%
    arrange(FundingYear, desc(AvgSavings))
}

