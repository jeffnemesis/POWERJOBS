#' Generate Capacity CSVs using Plutus
#'
#' This function uses the `plutus` package to generate CSV files for capacity additions and retirements
#' based on the specified GCAM database and other input parameters. If the `plutus` package is not
#' installed, it will be installed automatically along with its dependencies.
#'
#' @param workdir The working directory where outputs will be saved.
#' @param gcam_database_path The path to the GCAM database.
#' @param scenario_names A vector of scenario names to process. Default is `NULL`, meaning all scenarios in the database are selected.
#' @param regions A vector of region names to include in the analysis. Default is `NULL`, meaning all regions in the database are selected.
#' @param output_dir The directory where the output CSV files will be saved (default: 'generated_csvs' in `workdir`).
#' @return The path to the directory where the generated CSV files are saved.
#' @export
generate_capacity_csvs <- function(
    workdir,
    gcam_database_path,
    scenario_names = NULL,
    regions = NULL,
    output_dir = NULL
) {
  # Check and install `plutus` if not available
  if (!requireNamespace("plutus", quietly = TRUE)) {
    message("`plutus` package not found. Installing `plutus` and dependencies...")
    install.packages("devtools")
    devtools::install_github("JGCRI/rgcam")
    devtools::install_github("JGCRI/plutus")
  }

  # Load the plutus package
  library(plutus)

  # Set the output directory
  if (is.null(output_dir)) {
    output_dir <- file.path(workdir, "generated_csvs")
  }
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Call the plutus::gcamInvest function to generate the CSVs
  plutus::gcamInvest(
    gcamdatabase = gcam_database_path,
    dataProjFile = file.path(workdir, "dataProj.proj"),
    dirOutputs = output_dir,
    reReadData = TRUE,
    scenOrigNames = scenario_names,  # NULL means all scenarios are included
    regionsSelect = regions,        # NULL means all regions are included
    saveData = TRUE
  )

  # Notify the user that the files have been saved
  message("Capacity addition and retirement CSVs have been saved to: ", output_dir)

  # Return the path to the generated CSVs
  return(output_dir)
}
