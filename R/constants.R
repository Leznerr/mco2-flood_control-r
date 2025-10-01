# constants.R
# ------------------------------------------------------------------------------
# Purpose   : Central location for shared constants across pipeline modules.
#             Hosts file-name mappings and user-facing preview headings so that
#             the interactive preview helper and verification suite stay in
#             sync.
# Contract  : Modules may source this file to obtain immutable values.
#             - REPORT_FILES: named list of output artefacts.
#             - INTERACTIVE_PREVIEW_HEADINGS: ordered character vector of
#               headings printed by the interactive preview helper.
#             - path_report{1,2,3}(), path_summary(): convenience accessors for
#               constructing output paths without repeating file-name literals.
# ------------------------------------------------------------------------------

REPORT_FILES <- list(                                        # exported filenames for outputs
  r1 = "report_regional_efficiency.csv",
  r2 = "report_contractor_ranking.csv",
  r3 = "report_overrun_trends.csv",
  summary = "summary.json"
)

INTERACTIVE_PREVIEW_HEADINGS <- c(                           # interactive preview section titles
  "Preview • Report 1 – Regional Efficiency",
  "Preview • Report 2 – Contractor Reliability",
  "Preview • Report 3 – Cost Overrun Trends"
)

.path_join <- function(...) {                                # helper to join paths without forcing existence
  normalizePath(file.path(...), winslash = "/", mustWork = FALSE)
}

path_report1 <- function(outdir) .path_join(outdir, REPORT_FILES$r1)
path_report2 <- function(outdir) .path_join(outdir, REPORT_FILES$r2)
path_report3 <- function(outdir) .path_join(outdir, REPORT_FILES$r3)
path_summary <- function(outdir)  .path_join(outdir, REPORT_FILES$summary)
