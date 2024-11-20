#' Calculate Power Jobs
#'
#' This function calculates job potentials for new capacity additions and retirements
#' using predefined job intensities and integrates with Plutus' gcamInvest.
#'
#' @param gcamdatabase Path to the GCAM database directory.
#' @param dataProjFile Path to the GCAM `.proj` file.
#' @param dirOutputs Path to the directory for output files.
#' @param scenOrigNames A vector of scenario names to include. Default is NULL (all scenarios).
#' @param regionsSelect A vector of regions to include. Default is NULL (all regions).
#' @param reReadData Logical; if TRUE, forces reloading of GCAM data. Default is TRUE.
#' @param saveData Logical; if TRUE, saves the output files. Default is TRUE.
#' @return A dataframe containing job potentials by region, technology, and scenario.
#' @export
calculate_power_jobs <- function(
    gcamdatabase,
    dataProjFile,
    dirOutputs,
    scenOrigNames = NULL,
    regionsSelect = NULL
) {
  # Ensure Plutus is rerun to generate updated CSVs
  message("Running Plutus to generate updated GCAM data...")
  plutus::gcamInvest(
    gcamdatabase = gcamdatabase,
    dataProjFile = dataProjFile,
    dirOutputs = dirOutputs,
    reReadData = TRUE,
    scenOrigNames = scenOrigNames,
    regionsSelect = regionsSelect,
    saveData = TRUE
  )

  # Locate the GCAM Data Table
  gcam_data_path <- file.path(dirOutputs, "readGCAM/Tables_gcam/gcamDataTable.csv")
  if (!file.exists(gcam_data_path)) {
    stop("The gcamDataTable.csv file could not be found or generated. Please ensure Plutus ran successfully.")
  }

  # Load the GCAM Data Table
  gcam_data <- read.csv(gcam_data_path)

  # Filter and process data
  gcam_filtered <- gcam_data %>%
    filter(param %in% c("elecNewCapGW", "elecAnnualRetPrematureGW")) %>%
    filter(!is.na(value) & value != 0)

  # Merge with job intensities
  gcam_merged <- gcam_filtered %>%
    rename(technology = class1) %>%
    left_join(job_intensities, by = c("region", "technology"))

  # Add job potential calculation
  gcam_merged <- gcam_merged %>%
    mutate(job_potential = value * jobs_per_GW) %>%
    mutate(unit = "Job-Years")  # Add unit column

  # Generate various result data frames
  jobs_tech_year <- gcam_merged %>%
    group_by(scenario, region, technology, x) %>%
    summarise(job_potential = sum(job_potential, na.rm = TRUE), .groups = "drop") %>%
    mutate(unit = "Job-Years")  # Add unit

  jobs_tech_cum <- gcam_merged %>%
    group_by(scenario, region, technology) %>%
    summarise(job_potential = sum(job_potential, na.rm = TRUE), .groups = "drop") %>%
    mutate(unit = "Job-Years")  # Add unit

  jobs_year <- gcam_merged %>%
    group_by(scenario, region, x) %>%
    summarise(job_potential = sum(job_potential, na.rm = TRUE), .groups = "drop") %>%
    mutate(unit = "Job-Years")  # Add unit

  jobs_cum <- gcam_merged %>%
    group_by(scenario, region) %>%
    summarise(job_potential = sum(job_potential, na.rm = TRUE), .groups = "drop") %>%
    mutate(unit = "Job-Years")  # Add unit

  # Add global results
  jobs_tech_year_global <- jobs_tech_year %>%
    group_by(scenario, technology, x) %>%
    summarise(job_potential = sum(job_potential, na.rm = TRUE), .groups = "drop") %>%
    mutate(region = "Global", unit = "Job-Years")

  jobs_tech_cum_global <- jobs_tech_cum %>%
    group_by(scenario, technology) %>%
    summarise(job_potential = sum(job_potential, na.rm = TRUE), .groups = "drop") %>%
    mutate(region = "Global", unit = "Job-Years")

  jobs_year_global <- jobs_year %>%
    group_by(scenario, x) %>%
    summarise(job_potential = sum(job_potential, na.rm = TRUE), .groups = "drop") %>%
    mutate(region = "Global", unit = "Job-Years")

  jobs_cum_global <- jobs_cum %>%
    group_by(scenario) %>%
    summarise(job_potential = sum(job_potential, na.rm = TRUE), .groups = "drop") %>%
    mutate(region = "Global", unit = "Job-Years")

  # Combine all results into a list
  results <- list(
    jobs_tech_year = jobs_tech_year,
    jobs_tech_cum = jobs_tech_cum,
    jobs_year = jobs_year,
    jobs_cum = jobs_cum,
    jobs_tech_year_global = jobs_tech_year_global,
    jobs_tech_cum_global = jobs_tech_cum_global,
    jobs_year_global = jobs_year_global,
    jobs_cum_global = jobs_cum_global
  )

  return(results)
}
