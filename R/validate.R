# validate.R
# ------------------------------------------------------------------------------
# Purpose : Schema/type/invariant checks for the DPWH flood-control CSV.
# Contract: validate_schema(df) -> invisible(TRUE) or stop() on violations
#           assert_year_filter(df, allowed_years=2021:2023) -> invisible(TRUE)
# Notes   : Strictly enforces presence of canonical coordinate columns.
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

  row_count <- nrow(df)
  if (row_count == 0L) {
    stop("validate_schema(): input has zero rows.", call. = FALSE)
  }

  if (exists("log_info", mode = "function")) {
    log_info("validate_schema(): rows=%d", row_count)
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

  # Strict required columns ---------------------------------------------------
  required_cols <- c(
    "Region", "MainIsland", "Province",
    "FundingYear", "TypeOfWork",
    "StartDate", "ActualCompletionDate",
    "ApprovedBudgetForContract", "ContractCost",
    "Contractor"
  )
  missing_required <- setdiff(required_cols, nms)
  if (length(missing_required) > 0L) {
    stop(
      sprintf(
        "validate_schema(): missing required columns: %s.",
        paste(missing_required, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  has_canonical_coords <- all(c("Latitude", "Longitude") %in% nms)
  has_synonym_coords <- all(c("ProjectLatitude", "ProjectLongitude") %in% nms)
  if (!has_canonical_coords && !has_synonym_coords) {
    stop(
      "validate_schema(): missing coordinates (Latitude/Longitude or ProjectLatitude/ProjectLongitude).",
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
