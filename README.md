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

Key outputs:

- `report1_regional_efficiency.csv` – Regional Flood Mitigation Efficiency
- `report2_top_contractors.csv` – Top Contractors Performance Ranking
- `report3_overruns_trend.csv` – Annual Project Type Cost Overrun Trends
- `summary.json` – global scalar metrics

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

