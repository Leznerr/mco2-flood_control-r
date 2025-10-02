# report2.R
# ------------------------------------------------------------------------------
# Purpose   : Produce the Top Contractors Performance Ranking report.
# Contract  : build_report2(df) -> tibble with columns Rank, Contractor,
#   TotalCost, NumProjects, AvgDelay, TotalSavings, ReliabilityIndex, RiskFlag.
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({                             # quiet load
  library(dplyr)
})

.load_utils_format <- function() {
  if (exists("safe_sum", mode = "function") && exists("safe_mean", mode = "function")) {
    return(invisible(TRUE))
  }
  caller <- sys.frame(1)
  ofile <- caller$ofile
  candidates <- c(
    if (!is.null(ofile)) file.path(normalizePath(dirname(ofile)), "utils_format.R"),
    "utils_format.R",
    file.path("R", "utils_format.R")
  )
  candidates <- unique(candidates[!is.na(candidates)])
  for (path in candidates) {
    if (file.exists(path)) {
      sys.source(path, envir = parent.frame())
      return(invisible(TRUE))
    }
  }
  stop(".load_utils_format(): unable to locate utils_format.R for helper imports.")
}

.load_utils_format()

build_report2 <- function(df) {                               # build report 2 contractor ranking
  if (!is.data.frame(df)) stop("build_report2(): 'df' must be a data frame.")

  df %>%
    dplyr::group_by(Contractor) %>%
    dplyr::summarise(
      TotalCost = safe_sum(ContractCost),
      NumProjects = dplyr::n(),
      AvgDelay = safe_mean(CompletionDelayDays),
      TotalSavings = safe_sum(CostSavings),
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
      RiskFlag = dplyr::if_else(
        is.na(ReliabilityIndex) | ReliabilityIndex < 50,
        "High Risk",
        "Low Risk"
      )
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
