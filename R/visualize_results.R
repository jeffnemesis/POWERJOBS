#' Visualize Results
#'
#' Visualizes outputs of the `calculate_power_jobs` function and saves them as PNG files.
#'
#' @param results A named list containing data frames of the different result types.
#' @param output_dir A character string specifying the directory where the PNG files will be saved.
#' @export
visualize_results <- function(results, output_dir) {
  # Check if the output directory exists; create it if not
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Define a consistent theme
  custom_theme <- theme_minimal(base_size = 14) +
    theme(
      panel.background = element_rect(fill = "white", color = "black"),
      panel.border = element_rect(fill = NA, color = "black", size = 0.2),
      strip.background = element_rect(fill = "lightgrey", color = "black"),
      plot.background = element_rect(fill = "white", color = NA),
      strip.text = element_text(face = "bold", size = 12),
      axis.text.x = element_text(size = 8, color = "black", face = "bold", angle = 90, hjust = 1),
      axis.text.y = element_text(size = 10),
      axis.title = element_text(size = 12, face = "bold"),
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      legend.text = element_text(size = 10)
    )

  # Define a custom color palette for "technology"
  tech_colors <- c(
    "Bioenergy" = "#66c2a5",
    "Bioenergy CCS" = "#a6d854",
    "Coal" = "#4d4d4d",
    "Coal CCS" = "#bababa",
    "Gas" = "#762a83",
    "Gas CCS" = "#b2abd2",
    "Geothermal" = "#8c510a",
    "Nuclear" = "#e78ac3",
    "Oil" = "#800026",
    "Oil CCS" = "#bd0026",
    "Solar" = "#ffd92f",
    "Wind" = "#ff7f00",
    "Hydro" = "#d1e5f0"
  )

  # Iterate over result types
  for (result_name in names(results)) {
    data <- results[[result_name]]

    # Divide job potential by 1e6 for "Million Job-Years"
    data <- data %>%
      mutate(job_potential = job_potential / 1e6)

    # Add "Global" region for global datasets
    if ("global" %in% result_name) {
      data <- data %>% mutate(region = "Global")
    }

    # Determine plot logic based on result type
    if (result_name %in% c("jobs_tech_year", "global_jobs_tech_year") && "technology" %in% colnames(data)) {
      plot <- ggplot(data, aes(x = x, y = job_potential, fill = technology)) +
        geom_bar(stat = "identity", position = "stack") +
        scale_fill_manual(values = tech_colors) +  # Apply custom colors
        facet_grid(region ~ scenario, scales = "free_y") +
        labs(
          title = paste("Jobs by Technology per Year:", result_name),
          x = "Year",
          y = "Job Potential (Million Job-Years)",
          fill = "Technology"
        ) +
        custom_theme
    } else if (result_name %in% c("jobs_tech_cum", "global_jobs_tech_cum") && "technology" %in% colnames(data)) {
      plot <- ggplot(data, aes(x = scenario, y = job_potential, fill = technology)) +
        geom_bar(stat = "identity", position = "stack") +
        scale_fill_manual(values = tech_colors) +  # Apply custom colors
        facet_grid(~ region, scales = "free_y") +
        labs(
          title = paste("Cumulative Jobs by Technology:", result_name),
          x = "Scenario",
          y = "Cumulative Job Potential (Million Job-Years)",
          fill = "Technology"
        ) +
        custom_theme
    } else if (result_name %in% c("jobs_year", "global_jobs_year") && "region" %in% colnames(data)) {
      plot <- ggplot(data, aes(x = x, y = job_potential, fill = scenario)) +
        geom_bar(stat = "identity", position = "stack") +
        facet_grid(~region, scales = "free_y") +
        labs(
          title = paste("Total Jobs per Year:", result_name),
          x = "Year",
          y = "Job Potential (Million Job-Years)",
          fill = "Scenario"
        ) +
        custom_theme
    } else if (result_name %in% c("jobs_cum", "global_jobs_cum") && "region" %in% colnames(data)) {
      plot <- ggplot(data, aes(x = region, y = job_potential, fill = scenario)) +
        geom_bar(stat = "identity", position = "dodge") +
        facet_grid(~ scenario, scales = "free_y") +
        labs(
          title = paste("Cumulative Jobs Across Regions:", result_name),
          x = "Region",
          y = "Cumulative Job Potential (Million Job-Years)",
          fill = "Scenario"
        ) +
        custom_theme
    } else {
      warning("Unknown or unsupported result type: ", result_name)
      next  # Skip unsupported result types
    }

    # Save the plot as a PNG file
    output_file <- file.path(output_dir, paste0(result_name, "_plot.png"))
    ggsave(output_file, plot, width = 12, height = 8, dpi = 300)
  }

  message("All visualizations have been saved to: ", output_dir)
}
