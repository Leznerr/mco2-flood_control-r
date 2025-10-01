# interactive_preview.R
# ------------------------------------------------------------------------------
# Purpose   : Provide interactive console previews mirroring the specification
#             transcript. The helper prints a heading per report followed by a
#             small sample of rows, allowing operators to sanity check outputs
#             before writing to disk.
# Contract  :
#   - .run_interactive_spec(reports, summary, fmt_opts, preview_limit = 5)
#     prints previews to stdout and returns (invisibly) the headings used.
#   - preview_headings() returns the ordered headings vector so other modules
#     can validate against the same source of truth.
# ------------------------------------------------------------------------------

if (!exists("INTERACTIVE_PREVIEW_HEADINGS", inherits = TRUE)) {
  candidates <- c("R/constants.R", "../R/constants.R", file.path("..", "R", "constants.R"), "constants.R")
  for (path in candidates) {
    if (file.exists(path)) {
      source(path, chdir = TRUE)
      break
    }
  }
  if (!exists("INTERACTIVE_PREVIEW_HEADINGS", inherits = TRUE)) {
    stop("interactive_preview.R: unable to load INTERACTIVE_PREVIEW_HEADINGS; ensure R/constants.R is present.")
  }
}

preview_headings <- function() INTERACTIVE_PREVIEW_HEADINGS  # expose for verification/tests

.render_preview_block <- function(title, df, limit = 5) {    # internal printer for each report
  cat(title, "\n", sep = "")
  cat(strrep("-", nchar(title)), "\n", sep = "")
  if (is.data.frame(df) && nrow(df) > 0) {
    print(utils::head(df, limit))
  } else {
    cat("(no data available)\n")
  }
  cat("\n")
}

.run_interactive_spec <- function(reports,
                                  summary,
                                  fmt_opts = list(),
                                  preview_limit = 5) {
  headings <- preview_headings()
  blocks <- list(
    report1 = reports$report1,
    report2 = reports$report2,
    report3 = reports$report3
  )
  for (idx in seq_along(headings)) {
    df <- blocks[[idx]]
    .render_preview_block(headings[[idx]], df, limit = preview_limit)
  }
  invisible(headings)
}
