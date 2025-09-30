# validate.R
# ------------------------------------------------------------------------------
# Purpose : Schema/type/invariant checks for the DPWH flood-control CSV.
# Contract: validate_schema(df) -> invisible(TRUE) or stop() on violations
#           assert_year_filter(df, allowed_years=2021:2023) -> invisible(TRUE)
# Notes   : Accepts either {Latitude,Longitude} OR {ProjectLatitude,ProjectLongitude}.
#           No value transformations here; only structure/invariants.
# ------------------------------------------------------------------------------

# Helper: assert data.frame-like inputs ---------------------------------------
assert_is_df <- function(x, var = deparse(substitute(x))) {
  if (!is.data.frame(x)) {
    stop(
      sprintf(
        "assert_is_df(): '%s' is not a data.frame/tibble (class: %s).",
        var,
        paste(class(x), collapse = "/")
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

# Helper: does ANY pair in a matrix of pairs exist fully in `nms`? ------------
.has_pair <- function(nms, pairs) {
  any(apply(pairs, 1L, function(p) all(p %in% nms)))
}

#' Validate the raw schema (presence, duplicates, non-empty).
#' @param df data.frame/tibble read by ingest_csv().
#' @return invisible(TRUE) if valid; otherwise stop() with a precise message.
validate_schema <- function(df) {
  if (missing(df)) {
    stop("validate_schema(): argument 'df' is missing.", call. = FALSE)
  }

  # Unwrap a common {data=..., problems=...} list shape -----------------------
  if (is.list(df) && !is.data.frame(df) && !is.null(df$data)) {
    df <- df$data
  }

  assert_is_df(df, var = "df")

  nms <- names(df)
  if (length(nms) == 0L) {
    stop("validate_schema(): input has zero columns.", call. = FALSE)
  }

  if (nrow(df) == 0L) {
    stop("validate_schema(): input has zero rows.", call. = FALSE)
  }

  # No duplicated headers -----------------------------------------------------
  dups <- nms[duplicated(nms)]
  if (length(dups) > 0L) {
    stop(
      sprintf(
        "validate_schema(): duplicated column names: %s.",
        paste(sort(unique(dups)), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # Strict required columns (excluding coordinates, which accept synonyms) ----
  required_strict <- c(
    "Region", "MainIsland", "Province",
    "FundingYear", "TypeOfWork",
    "StartDate", "ActualCompletionDate",
    "ApprovedBudgetForContract", "ContractCost",
    "Contractor"
  )
  missing_strict <- setdiff(required_strict, nms)
  if (length(missing_strict) > 0L) {
    stop(
      sprintf(
        "validate_schema(): missing required columns: %s.",
        paste(missing_strict, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # Coordinates: accept canonical OR synonyms; fail only if neither pair exists
  latlon_pairs <- rbind(
    c("Latitude", "Longitude"),
    c("ProjectLatitude", "ProjectLongitude")
  )
  if (!.has_pair(nms, latlon_pairs)) {
    stop(
      "validate_schema(): missing coordinates; expected either {Latitude,Longitude} or {ProjectLatitude,ProjectLongitude}.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

#' Assert that all FundingYear values are within the allowed set (post-filter).
#' @param df data.frame with FundingYear present.
#' @param allowed_years integer vector of allowed years (default 2021:2023).
#' @return invisible(TRUE) or stop() listing offending values.
assert_year_filter <- function(df, allowed_years = 2021:2023) {
  if (missing(df)) {
    stop("assert_year_filter(): argument 'df' is missing.", call. = FALSE)
  }
  assert_is_df(df, var = "df")
  if (!"FundingYear" %in% names(df)) {
    stop("assert_year_filter(): 'FundingYear' column is missing.", call. = FALSE)
  }
  vals <- unique(stats::na.omit(df$FundingYear))
  bad  <- setdiff(vals, allowed_years)
  if (length(bad) > 0L) {
    stop(sprintf(
      "assert_year_filter(): found disallowed FundingYear values: %s; allowed: %s.",
      paste(sort(bad), collapse = ", "), paste(allowed_years, collapse = ", ")
    ), call. = FALSE)
  }
  invisible(TRUE)
}
