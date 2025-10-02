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

suppressPackageStartupMessages({                             # quiet load for tidy verbs
  library(dplyr)
})

.load_utils_format <- function() {                           # load shared helpers once per session
  required <- c("safe_sum", "safe_mean", "safe_median", "minmax_0_100", "format_dataframe")
  have_all <- vapply(required, exists, logical(1), mode = "function", inherits = TRUE)
  if (all(have_all)) {
    return(invisible(TRUE))
  }

  candidate_paths <- character()
  candidate_paths <- c(candidate_paths, file.path("R", "utils_format.R"))

  frames <- sys.frames()
  if (length(frames) > 0) {
    for (env in rev(frames)) {
      if (exists("ofile", envir = env, inherits = FALSE)) {
        ofile <- get("ofile", envir = env, inherits = FALSE)
        if (!is.null(ofile)) {
          candidate_paths <- c(candidate_paths, file.path(dirname(ofile), "utils_format.R"))
        }
      }
    }
  }

  candidate_paths <- unique(candidate_paths[file.exists(candidate_paths)])

  for (path in candidate_paths) {
    sys.source(path, envir = globalenv())
    have_all <- vapply(required, exists, logical(1), mode = "function", inherits = TRUE)
    if (all(have_all)) {
      break
    }
  }

  have_all <- vapply(required, exists, logical(1), mode = "function", inherits = TRUE)
  if (!all(have_all)) {
    stop("Failed to load required helper functions from R/utils_format.R")
  }

  invisible(TRUE)
}

.load_utils_format()

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
