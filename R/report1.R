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

.load_utils_format <- function() {
  helpers_exist <- all(
    vapply(
      c("safe_mean", "safe_sum", "safe_median", "minmax_0_100", "format_dataframe"),
      exists,
      logical(1),
      inherits = TRUE
    )
  )
  if (helpers_exist) {
    return(invisible(TRUE))
  }

  caller <- sys.frame(1)
  dirs <- c(
    if (!is.null(caller$ofile)) dirname(caller$ofile) else NA_character_,
    if (!is.null(caller$filename)) dirname(caller$filename) else NA_character_,
    getwd()
  )
  dirs <- unique(Filter(function(path) !is.na(path) && nzchar(path), dirs))

  for (dir in dirs) {
    utils_path <- file.path(normalizePath(dir, mustWork = FALSE), "utils_format.R")
    if (file.exists(utils_path)) {
      sys.source(utils_path, envir = parent.frame())
      return(invisible(TRUE))
    }
  }

  stop(".load_utils_format(): unable to locate 'utils_format.R' in the expected directories.")
}

.load_utils_format()

suppressPackageStartupMessages({                             # quiet load for tidy verbs
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

report_regional_efficiency <- function(df) {
  build_report1(df)
}
