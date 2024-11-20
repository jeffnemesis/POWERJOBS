#' Run Visualization App
#'
#' Launches a Shiny app for interactive exploration of the results.
#'
#' @param results A named list containing data frames of the different result types.
#' @export
run_visualization_app <- function(results) {
  library(shiny)
  library(ggplot2)
  library(dplyr)

  # Filter non-global results
  valid_results <- results[!grepl("_global", names(results))]

  # Define a consistent theme
  custom_theme <- theme_minimal(base_size = 14) +
    theme(
      panel.background = element_rect(fill = "white", color = "black"),
      panel.border = element_rect(fill = NA, color = "grey"),
      strip.background = element_rect(fill = "lightgrey", color = "black"),
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

  # Define the Shiny UI
  ui <- fluidPage(
    titlePanel("Interactive Visualization of Power Jobs"),
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "result_type",
          "Select Result Type:",
          choices = names(valid_results),
          selected = names(valid_results)[1]
        ),
        selectInput(
          "scenario",
          "Select Scenario:",
          choices = NULL,  # Updated dynamically based on data
          multiple = TRUE
        ),
        selectInput(
          "region",
          "Select Region:",
          choices = NULL,  # Updated dynamically based on data
          multiple = TRUE
        ),
        uiOutput("year_slider")  # Dynamically show/hide the year slider
      ),
      mainPanel(
        plotOutput("plot", height = "600px")
      )
    )
  )

  # Define the Shiny server logic
  server <- function(input, output, session) {
    # Dynamically update scenario and region based on selected result type
    observeEvent(input$result_type, {
      data <- valid_results[[input$result_type]]

      # Update scenario choices
      updateSelectInput(session, "scenario",
                        choices = unique(data$scenario),
                        selected = unique(data$scenario))

      # Update region choices
      updateSelectInput(session, "region",
                        choices = unique(data$region),
                        selected = unique(data$region))
    })

    # Dynamically show or hide the year slider based on the result type
    output$year_slider <- renderUI({
      if (input$result_type %in% c("jobs_tech_cum", "jobs_cum")) {
        return(NULL)  # Hide the year slider for cumulative datasets
      } else {
        sliderInput(
          "yearRange",
          "Select Year Range:",
          min = 2015,
          max = 2100,
          value = c(2025, 2050),
          step = 5
        )
      }
    })

    # Generate the plot based on user inputs
    output$plot <- renderPlot({
      req(input$result_type)  # Ensure result type is selected
      data <- valid_results[[input$result_type]]

      # Apply filtering for scenario and region
      filtered_data <- data %>%
        filter(
          scenario %in% input$scenario,
          region %in% input$region
        )

      # Apply year filtering only for non-cumulative datasets
      if (!input$result_type %in% c("jobs_tech_cum", "jobs_cum")) {
        filtered_data <- filtered_data %>%
          filter(x >= input$yearRange[1], x <= input$yearRange[2])
      }

      # Determine plot logic based on result type
      if (input$result_type == "jobs_tech_year" && "technology" %in% colnames(filtered_data)) {
        ggplot(filtered_data, aes(x = x, y = job_potential / 1e6, fill = technology)) +
          geom_bar(stat = "identity", position = "stack") +
          scale_fill_manual(values = tech_colors) +
          facet_grid(region ~ scenario, scales = "free_y") +
          labs(
            title = "Jobs by Technology per Year",
            x = "Year",
            y = "Job Potential (Million Job-Years)",
            fill = "Technology"
          ) +
          custom_theme
      } else if (input$result_type == "jobs_tech_cum" && "technology" %in% colnames(filtered_data)) {
        ggplot(filtered_data, aes(x = scenario, y = job_potential / 1e6, fill = technology)) +
          geom_bar(stat = "identity", position = "stack") +
          scale_fill_manual(values = tech_colors) +
          facet_grid(~ region, scales = "free_y") +
          labs(
            title = "Cumulative Jobs by Technology",
            x = "Scenario",
            y = "Cumulative Job Potential (Million Job-Years)",
            fill = "Technology"
          ) +
          custom_theme
      } else if (input$result_type == "jobs_year" && "region" %in% colnames(filtered_data)) {
        ggplot(filtered_data, aes(x = x, y = job_potential / 1e6, fill = scenario)) +
          geom_bar(stat = "identity", position = "stack") +
          facet_grid(~region, scales = "free_y") +
          labs(
            title = "Total Jobs per Year",
            x = "Year",
            y = "Job Potential (Million Job-Years)",
            fill = "Scenario"
          ) +
          custom_theme
      } else if (input$result_type == "jobs_cum" && "region" %in% colnames(filtered_data)) {
        ggplot(filtered_data, aes(x = region, y = job_potential / 1e6, fill = scenario)) +
          geom_bar(stat = "identity", position = "dodge") +
          facet_grid(~ scenario, scales = "free_y") +
          labs(
            title = "Cumulative Jobs Across Regions",
            x = "Region",
            y = "Cumulative Job Potential (Million Job-Years)",
            fill = "Scenario"
          ) +
          custom_theme
      } else {
        ggplot() +
          labs(title = "Unsupported result type or insufficient data") +
          theme_void()
      }
    })
  }

  # Run the Shiny app
  shinyApp(ui = ui, server = server)
}
