# utils_format.R
# ------------------------------------------------------------------------------
# Purpose   : Provide common numeric helpers (safe aggregates, min-max scaling)
#             and presentation formatting for report data frames.
# Contract  :
#   - safe_mean(x), safe_median(x), safe_sum(x) return NA when all inputs NA.
#   - minmax_0_100(x) rescales numeric vector to [0,100] range (NA preserving).
#   - format_dataframe(df, exclude = NULL, exclude_regex = NULL) converts
#     numeric columns to comma-formatted strings with two decimals, except for
#     identifiers/counts specified in the excludes.
# Rubric    : Simplicity, Correctness (NA handling, deterministic formatting),
#             UX (human-friendly numbers), Readability (formal comments).
# ------------------------------------------------------------------------------

safe_mean <- function(x) {                                  # compute mean with NA when no finite values
  x <- as.numeric(x)
  if (all(is.na(x))) return(NA_real_)
  mean(x, na.rm = TRUE)
}

safe_median <- function(x) {                                # compute median with NA fallback
  x <- as.numeric(x)
  if (all(is.na(x))) return(NA_real_)
  stats::median(x, na.rm = TRUE)
}

safe_sum <- function(x) {                                   # compute sum with NA fallback
  x <- as.numeric(x)
  if (all(is.na(x))) return(NA_real_)
  sum(x, na.rm = TRUE)
}

minmax_0_100 <- function(x) {                               # rescale numeric vector to [0,100]
  if (length(x) == 0L) return(numeric())                    # empty input -> empty output
  x <- as.numeric(x)
  if (!any(is.finite(x))) {                                 # all values missing/non-finite -> return all NA
    return(rep(NA_real_, length(x)))
  }
  range <- range(x, na.rm = TRUE)
  if (any(is.infinite(range))) {                            # handle degenerate case with all NA
    return(rep(NA_real_, length(x)))
  }
  min_val <- range[1]
  max_val <- range[2]
  if (is.infinite(min_val) || is.infinite(max_val)) {
    return(rep(NA_real_, length(x)))
  }
  if (is.na(min_val) || is.na(max_val)) {
    return(rep(NA_real_, length(x)))
  }
  if (max_val - min_val < .Machine$double.eps) {            # constant vector -> return 100 for all finite entries
    out <- rep(NA_real_, length(x))
    out[!is.na(x)] <- 100
    return(out)
  }
  scaled <- (x - min_val) / (max_val - min_val)
  scaled <- pmax(pmin(scaled, 1), 0)                        # clamp due to potential floating error
  scaled * 100
}

format_dataframe <- function(df,
                             exclude = NULL,
                             exclude_regex = NULL,
                             comma_strings = TRUE,
                             digits = 2) {
  if (!is.data.frame(df)) stop("format_dataframe(): 'df' must be a data frame.")
  numeric_cols <- vapply(df, is.numeric, logical(1))
  if (!any(numeric_cols)) return(df)
  default_exclude <- c(

  )
  excl <- unique(c(default_exclude, exclude %||% character(0)))
  regex <- exclude_regex
  for (col in names(df)[numeric_cols]) {
    if (col %in% excl || (!is.null(regex) && grepl(regex, col))) {
      next
    }
    values <- round(df[[col]], digits)
    if (comma_strings) {
      formatted <- formatC(values, format = "f", digits = digits, big.mark = ",", drop0trailing = FALSE)
    } else {
      formatted <- formatC(values, format = "f", digits = digits, drop0trailing = FALSE)
    }
    formatted[is.na(values)] <- ""                               # keep NA as empty string for CSV readability
    df[[col]] <- formatted
  }
  df
}

`%||%` <- function(lhs, rhs) {                                # fallback helper identical to utils_cli but local copy
  if (!is.null(lhs)) lhs else rhs
}

