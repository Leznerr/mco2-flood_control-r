# utils_cli.R
# ------------------------------------------------------------------------------
------------------------------------------------------------

suppressPackageStartupMessages({
  library(optparse)
})



build_cli <- function() {
  option_list <- list(
    make_option(c("-i", "--input"), type = "character", metavar = "FILE",
                help = "Path to the DPWH flood-control CSV dataset."),
    make_option(c("-o", "--outdir"), type = "character", default = "outputs",
                metavar = "DIR", help = "Directory for generated reports [default %default]."),
    make_option("--interactive", action = "store_true", default = FALSE,
                help = "Enable interactive preview of report outputs.")
  )

  OptionParser(option_list = option_list, usage = "%prog --input <file> [--outdir <dir>]")
}


validate_cli_args <- function(args) {
  if (is.null(args$input) || is.na(args$input) || !nzchar(args$input)) {
    stop("CLI: --input <file> is required.")
  }
  if (!file.exists(args$input)) {
    stop(sprintf("CLI: input file not found -> %s", args$input))
  }

  outdir <- args$outdir %||% "outputs"
  if (!is.null(outdir) && (is.na(outdir) || !nzchar(outdir))) {
    stop("CLI: --outdir must be a non-empty string when provided.")
  }
  if (file.exists(outdir) && !dir.exists(outdir)) {
    stop(sprintf("CLI: --outdir path exists but is not a directory -> %s", outdir))
  }
  if (!dir.exists(outdir)) {
    ok <- dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
    if (!ok) {
      stop(sprintf("CLI: unable to create --outdir '%s'.", outdir))
    }
  }

  invisible(args)
}

normalize_cli_paths <- function(args) {
  args$input <- normalizePath(args$input, winslash = "/", mustWork = FALSE)
  args$outdir <- normalizePath(args$outdir %||% "outputs", winslash = "/", mustWork = FALSE)
  args
}


print_menu <- function() {
  cat("Select Language Implementation:\n")
  cat("[1] Load the file\n")
  cat("[2] Generate Reports\n\n")
  cat("Enter choice: ")

}

prompt_back_to_menu <- function() {
  cat("Back to Report Selection (Y/N): ")


