# Purpose: run the definitive analysis in dependency order.
# Run from the project root in a fresh session, with COHERO_DATA_DIR set in
#   your .Renviron. Scripts in R/exploratory/ are not run from here.
# Produces: every object listed in the scripts' headers, and every table as
#   a Word file in tables/ (tables.R). Set COHERO_OUTPUT_DIR to save outputs
#   in another folder; the synthetic-data runs do. Figures are kept in memory
#   and not yet saved.

source("R/setup.R")
source("R/functions.R")
source("R/plot_style.R")
source("R/01_import_data.R")
source("R/02_build_visit_data.R")
source("R/03_derive_medication_outcomes.R")
source("R/04_fit_count_models.R")
source("R/05_fit_mixed_models.R")
source("R/tables.R")
source("R/figures.R")
