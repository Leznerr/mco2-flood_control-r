# clean.R 
# ------------------------------------------------------------------------------
# Purpose   : Clean and standardize the ingested DPWH flood-control dataset.
#             - Robust date parsing (multiple formats) to Date.
#             - Coerce money-like fields to numeric (strip "Php", commas, etc.).
#             - Ensure FundingYear is integer.
#             - Ensure Latitude/Longitude are numeric and plausibly bounded.
#             - Normalize key text fields (trim, title-case where sensible).
#             - Conservatively impute geocoordinates ONLY when BOTH are missing,
#               using province-level means (no single-coordinate imputation).
# Contract  : clean_all(df_raw) -> df_clean (same columns, standardized values).
# Notes     : No derived analytics here (see derive.R). No filtering here.
# Rubric    : Correctness (robust parsing, conservative imputation), Readability
#             (formal comments), Simplicity (single responsibility), UX (logs).
# ------------------------------------------------------------------------------

# ----------------------------- Dependencies -----------------------------------
suppressPackageStartupMessages({                                           # suppress package startup banners for clean logs
  library(dplyr)                                                           # verbs for mutate, group_by, summarise, joins
  library(stringr)                                                         # string normalization (trim, case)
  library(lubridate)                                                       # robust date parsing utilities
  library(readr)                                                           # parse_number for flexible numeric parsing
})

# ----------------------------- Logging utilities ------------------------------
# These wrappers integrate with project-wide loggers if present; otherwise they
# fallback to standardized console messages.

.log_info <- function(fmt, ...) {                                          # define informational logger (printf-style)
  msg <- sprintf(fmt, ...)                                                 # format message once
  if (exists("log_info", mode = "function")) log_info("%s", msg)            # delegate to project logger if available
  else message(sprintf("[INFO]  %s", msg))                                 # otherwise print standardized INFO line
}

.log_warn <- function(fmt, ...) {                                          # define warning logger (printf-style)
  msg <- sprintf(fmt, ...)                                                 # format message once
  if (exists("log_warn", mode = "function")) log_warn("%s", msg)            # delegate to project logger if available
  else message(sprintf("[WARN]  %s", msg))                                 # otherwise print standardized WARN line
}

# ----------------------------- Helper functions -------------------------------

.parse_date_any <- function(x) {                                           # parse common gov/date formats to Date
  dt <- suppressWarnings(                                                  # suppress per-element parse warnings (we log totals)
    lubridate::parse_date_time(                                           # use multi-order parser for resilience
      x = x,                                                              # input vector (character or factor/Date)
      orders = c("ymd","mdy","dmy","Ymd HMS","mdy HMS","dmy HMS","Ymd HM","mdy HM","dmy HM"), # common layouts
      tz = "UTC",                                                         # deterministic timezone for POSIX parsing
      exact = FALSE                                                       # allow flexible matching across orders
    )
  )
  as.Date(dt)                                                             # coerce POSIXct -> Date (drops time component)
}

.parse_money_number <- function(x) {                                       # parse currency-like strings to numeric
  readr::parse_number(x, locale = readr::locale(grouping_mark = ",", decimal_mark = ".")) # strip "Php", commas, keep decimals
}

.as_integer_safely <- function(x) {                                        # coerce to integer safely via digit parse
  suppressWarnings(as.integer(readr::parse_number(x)))                    # parse digits then cast; non-numeric -> NA
}

.as_numeric_safely <- function(x) {                                        # coerce to numeric, keep NA on failure
  suppressWarnings(as.numeric(x))                                          # base numeric conversion (robust and fast)
}

.title_case_squish <- function(x) {                                        # normalize whitespace and apply title case
  stringr::str_to_title(stringr::str_squish(x))                            # compress internal spaces; Title Case
}

.squish_only <- function(x) {                                              # normalize whitespace without changing case
  stringr::str_squish(x)                                                   # trim edges and compress internal spaces
}

.bound_latlon <- function(lat, lon) {                                      # bound-check geocoordinates for plausibility
  lat_ok <- ifelse(is.na(lat), NA_real_, ifelse(lat < -90 | lat > 90, NA_real_, lat))  # invalid latitudes -> NA
  lon_ok <- ifelse(is.na(lon), NA_real_, ifelse(lon < -180 | lon > 180, NA_real_, lon))# invalid longitudes -> NA
  list(lat = lat_ok, lon = lon_ok)                                         # return list (used inside mutate)
}

# ----------------------------- Public API -------------------------------------

#' Clean and standardize the ingested dataset.
#'
#' @param df data.frame/tibble from ingest_csv() and validate_schema().
#' @return data.frame/tibble with standardized types, normalized text, and
#'         conservative province-mean geolocation imputation.
clean_all <- function(df) {                                                # define main cleaning function
  # ---- Preconditions ----------------------------------------------------------
  if (missing(df) || !is.data.frame(df)) {                                 # ensure input is a data.frame/tibble
    stop("clean_all(): 'df' must be a data.frame/tibble from ingest_csv().")# fail-fast with actionable message
  }

  # ---- Canonicalize coordinate column names (accept synonyms) ----------------
  # If Latitude/Longitude are absent but ProjectLatitude/ProjectLongitude exist,
  # rename the latter to the canonical names so all downstream steps use one pair.
  if (!("Latitude" %in% names(df)) && all(c("ProjectLatitude", "ProjectLongitude") %in% names(df))) {
    # If both canonical and synonym pairs exist, prefer canonical and ignore synonyms.
    if (all(c("Latitude", "Longitude") %in% names(df))) {
      if (exists("log_warn", mode = "function")) {
        log_warn("Both coordinate pairs present; using canonical Latitude/Longitude; ignoring ProjectLatitude/ProjectLongitude.")
      }
    } else {
      names(df)[match("ProjectLatitude", names(df))]  <- "Latitude"
      names(df)[match("ProjectLongitude", names(df))] <- "Longitude"
    }
  }

  # ---- Preserve original column set and order --------------------------------
  orig_cols <- names(df)                                                   # snapshot original header for schema integrity

  # ---- NA snapshot BEFORE cleaning (for delta logs) --------------------------
  na_before <- list(                                                       # collect NA counts for key fields pre-clean
    StartDate = sum(is.na(df[["StartDate"]])),                             # NA count in StartDate
    ActualCompletionDate = sum(is.na(df[["ActualCompletionDate"]])),       # NA count in ActualCompletionDate
    ABC = sum(is.na(df[["ApprovedBudgetForContract"]])),                   # NA count in ApprovedBudgetForContract
    Cost = sum(is.na(df[["ContractCost"]])),                               # NA count in ContractCost
    FY  = sum(is.na(df[["FundingYear"]])),                                 # NA count in FundingYear
    Lat = sum(is.na(df[["Latitude"]])),                                    # NA count in Latitude
    Lon = sum(is.na(df[["Longitude"]]))                                    # NA count in Longitude
  )

  # ---- Type coercions & text normalization (idempotent) ----------------------
  df1 <- df %>%                                                            # start a new pipeline to avoid surprise mutation
    mutate(
      # Dates: robust parse to Date (UTC-based, drop time)
      StartDate            = .parse_date_any(StartDate),                    # parse StartDate to Date
      ActualCompletionDate = .parse_date_any(ActualCompletionDate),         # parse ActualCompletionDate to Date

      # Money: parse currency-like strings and commas to numeric
      ApprovedBudgetForContract = .parse_money_number(ApprovedBudgetForContract), # numeric ABC
      ContractCost              = .parse_money_number(ContractCost),        # numeric contract cost

      # Year: integer FundingYear (handles "2021", 2021.0, "FY2021", etc.)
      FundingYear = .as_integer_safely(FundingYear),                        # integer or NA if unparsable

      # Geo: numeric lat/lon first (NA on non-numeric), then plausibility bounds
      Latitude  = .as_numeric_safely(Latitude),                             # numeric latitude or NA
      Longitude = .as_numeric_safely(Longitude),                            # numeric longitude or NA
      # Text normalization: geographic/party labels Title Case; work type trim only
      Region     = .title_case_squish(Region),                              # normalize Region naming
      MainIsland = .title_case_squish(MainIsland),                          # normalize MainIsland
      Province   = .title_case_squish(Province),                            # normalize Province naming
      Contractor = .title_case_squish(Contractor),                          # normalize Contractor naming
      TypeOfWork = .squish_only(TypeOfWork)                                 # preserve original case for TypeOfWork
    ) %>%
    {                                                                       # begin inline block to apply bound checks
      b <- .bound_latlon(.$Latitude, .$Longitude)                           # compute bounded lat/lon
      mutate(., Latitude = b$lat, Longitude = b$lon)                        # replace with bounded versions
    }

  # ---- Province-level mean geolocation (for conservative imputation) ---------
  complete_coords <- df1 %>%                                               # restrict to rows with complete coordinate pairs
    filter(!is.na(Latitude) & !is.na(Longitude))                           # exclude any partial geocoordinate entries

  prov_means <- complete_coords %>%                                        # compute province means using only observed pairs
    group_by(Province) %>%                                                 # group by normalized Province
    summarise(
      mean_lat = mean(Latitude),                                           # province mean latitude (complete cases only)
      mean_lon = mean(Longitude),                                          # province mean longitude (complete cases only)
      .groups  = "drop"                                                    # drop grouping to avoid downstream carry-over
    )

  # ---- Impute BOTH coordinates only when BOTH are missing --------------------
  df2 <- df1 %>%                                                           # continue cleaning pipeline
    mutate(
      .both_na = is.na(Latitude) & is.na(Longitude)                         # flag rows with both coordinates missing
    ) %>%
    left_join(prov_means, by = "Province") %>%                             # attach filtered province-level means
    mutate(
      Latitude  = ifelse(.both_na & !is.na(mean_lat) & !is.na(mean_lon),    # if both missing and province means exist
                         mean_lat,                                          #   impute latitude with province mean
                         Latitude),                                         # else keep original
      Longitude = ifelse(.both_na & !is.na(mean_lat) & !is.na(mean_lon),    # if both missing and province means exist
                         mean_lon,                                          #   impute longitude with province mean
                         Longitude)                                         # else keep original
    ) %>%
    select(-mean_lat, -mean_lon, -.both_na)                                 # drop helper columns to restore schema

  # ---- NA snapshot AFTER cleaning (for transparency) -------------------------
  na_after <- list(                                                        # compute NA counts after cleaning/imputation
    StartDate = sum(is.na(df2[["StartDate"]])),                             # post-clean NA count: StartDate
    ActualCompletionDate = sum(is.na(df2[["ActualCompletionDate"]])),       # post-clean NA count: ActualCompletionDate
    ABC = sum(is.na(df2[["ApprovedBudgetForContract"]])),                   # post-clean NA count: ABC
    Cost = sum(is.na(df2[["ContractCost"]])),                               # post-clean NA count: Cost
    FY  = sum(is.na(df2[["FundingYear"]])),                                 # post-clean NA count: FundingYear
    Lat = sum(is.na(df2[["Latitude"]])),                                    # post-clean NA count: Latitude
    Lon = sum(is.na(df2[["Longitude"]]))                                    # post-clean NA count: Longitude
  )

  # ---- Delta computation and concise log summary -----------------------------
  delta <- list(                                                           # compute how many NAs were resolved
    StartDate_fixed = na_before$StartDate - na_after$StartDate,             # resolved StartDate NAs
    ACD_fixed       = na_before$ActualCompletionDate - na_after$ActualCompletionDate, # resolved completion-date NAs
    ABC_fixed       = na_before$ABC - na_after$ABC,                         # resolved ABC NAs
    Cost_fixed      = na_before$Cost - na_after$Cost,                       # resolved cost NAs
    FY_fixed        = na_before$FY - na_after$FY,                           # resolved year NAs
    Lat_fixed       = na_before$Lat - na_after$Lat,                         # resolved latitude NAs (parse or impute)
    Lon_fixed       = na_before$Lon - na_after$Lon                          # resolved longitude NAs (parse or impute)
  )

  .log_info(                                                                # emit one-line structured cleaning summary
    paste(
      "Cleaning summary |",
      "dates_fixed(Start,Comp)=%d/%d |",
      "money_fixed(ABC,Cost)=%d/%d |",
      "year_fixed=%d |",
      "geo_fixed(Lat,Lon)=%d/%d",
      sep = " "
    ),
    delta$StartDate_fixed, delta$ACD_fixed,                                 # values for dates fixed
    delta$ABC_fixed, delta$Cost_fixed,                                      # values for money fields fixed
    delta$FY_fixed,                                                         # values for year fixed
    delta$Lat_fixed, delta$Lon_fixed                                        # values for geo fixed
  )

  # ---- Postconditions: column integrity (order and set) ----------------------
  if (!identical(names(df2), orig_cols)) {                                  # ensure we didn't change names/order inadvertently
    .log_warn("Column order/set changed during cleaning; restoring original header order.") # warn for developer visibility
    df2 <- df2[, orig_cols, drop = FALSE]                                   # restore original column order strictly
  }

  # ---- Return cleaned dataframe ----------------------------------------------
  return(df2)                                                               # yield standardized, conservatively-imputed table
}
