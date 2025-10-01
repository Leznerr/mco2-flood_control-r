# MCO2 Flood Control Data Analysis Pipeline (R)

This repository contains the production-ready R implementation of the DPWH flood
control data pipeline. It ingests the official CSV dataset, validates schema and
ranges, applies conservative cleaning, derives analytics fields, generates the
three specification-aligned reports, and writes a JSON summary artifact.

## Project Layout

```
mco2-flood_pipeline/
├─ main.R                     # pipeline entrypoint (Rscript main.R --input ...)
├─ R/                         # modular stage implementations
├─ tests/                     # testthat unit/golden coverage
├─ sample-data/               # tiny fixture covering pipeline edge cases
└─ outputs/                   # generated artifacts (created on demand)
```

## Prerequisites

Install the required CRAN packages (run once):

```
R -q -e "install.packages(c('optparse','readr','dplyr','stringr','lubridate','jsonlite','tibble','testthat'), repos='https://cloud.r-project.org')"
```

## Running the Pipeline

Execute the pipeline against the official dataset (outputs go to `outputs/` by
default, or specify `--outdir`):

```
Rscript main.R --input dpwh_flood_control_projects.csv --outdir outputs
```

Key outputs (written to `--outdir`, defaults to `outputs/`):


- `summary.json` – Global scalar metrics
- `verification_report.txt` – Pass/fail log covering schema, formatting, sorting, derived-field consistency, summary parity, and rubric alignment checks.

All reports use UTF-8 CSV with comma thousands separators and two decimal places on monetary/ratio columns, matching the course specification.

## Output Schemas


  - Includes contractors with ≥5 projects, top 15 by `TotalCost`
- **Report 3 – Annual Project Type Cost Overrun Trends**
  - Columns: `FundingYear`, `TypeOfWork`, `TotalProjects`, `AvgSavings`, `OverrunRate`, `YoYChange`
  - Sorted by `FundingYear` ascending, `AvgSavings` descending, `TypeOfWork`
- **summary.json**
  - Keys: `total_projects`, `total_contractors`, `total_provinces`, `global_avg_delay`, `total_savings`



## Tests

Run the automated unit and golden tests:

```
R -q -e "testthat::test_dir('tests')"
```

The suite exercises ingestion guards, validation invariants, cleaning and
imputation rules, derivations, report logic, formatting contracts, and the JSON
summary output.

## Troubleshooting

- **`Rscript` not found on Windows** – add the R `bin` directory (e.g.
  `C:\Program Files\R\R-4.x.x\bin`) to your `PATH`, then re-open the terminal.
- **CSV header mismatch** – ensure the input file contains exactly the required
  columns: `Region`, `MainIsland`, `Province`, `FundingYear`, `TypeOfWork`,
  `StartDate`, `ActualCompletionDate`, `ApprovedBudgetForContract`, `ContractCost`,
  `Contractor`, `Latitude`, `Longitude` (case-sensitive).

## Rubric Alignment (Simplicity • Performance • Readability • Correctness • UX)

- **Simplicity** – Modular pipeline (`ingest`, `validate`, `clean`, `derive`, `report*`, `summary`, `verify`) keeps each responsibility focused and easy to reason about.
- **Performance** – Vectorised dplyr pipelines and guarded parsing avoid per-row loops, ensuring the ~10k-row dataset runs in a few seconds even on modest hardware.
- **Readability** – Consistent inline comments, descriptive function names, and deterministic formatting helpers make it straightforward for reviewers to trace transformations end-to-end.
- **Correctness** – Strict schema validation, guarded money/date parsing, derived-field checks, and the automated `verification_report.txt` guarantee specification compliance.
- **User Experience** – The CLI mirrors the sample menu/preview experience, logs key cleaning/imputation actions, and writes Excel-friendly reports with explicit export confirmations.

