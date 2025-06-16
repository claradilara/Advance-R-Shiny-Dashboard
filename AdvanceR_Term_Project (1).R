
# # # # # # # #  # # # # # # # #  # # # # # # # #  # # # # # # # 
#                                                               #
#                   Advance R Term Project                      #
#                                                               #  
#                 Crime Simulator Application                   #
#                                                               #
#                                                               #
#                      Dilara Ozdil                             #
#                     Eljan Abbaszada                           #
#                                                               #
#                                                               #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 



# # # # # # # # # C++  and Advance Functions# # # # # # # # # # 

if (!requireNamespace("crimedata", quietly = TRUE)) {
  install.packages("crimedata")
}
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
if (!requireNamespace("lubridate", quietly = TRUE)) {
  install.packages("lubridate")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}

library(crimedata)
library(dplyr)
library(lubridate)
library(ggplot2)

# Step 1: Load and clean the crime data
load_and_clean_crime_data <- function(cities = c("Chicago"), type = "core", start_year = 2015, end_year = 2020) {
  # Defensive checks
  if (!is.character(cities) || length(cities) == 0) stop("cities must be a non-empty character vector.")
  if (!type %in% c("core", "extended", "sample")) stop("type must be 'core', 'extended', or 'sample'.")
  if (!is.numeric(start_year) || !is.numeric(end_year)) stop("start_year and end_year must be numeric.")
  
  # Download crime data for the cities
  raw_data <- get_crime_data(cities = cities, type = type)
  
  # Check if the necessary columns exist
  required_columns <- c("date_single", "offense_type", "longitude", "latitude")
  missing_cols <- setdiff(required_columns, names(raw_data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns in the downloaded dataset: ", paste(missing_cols, collapse = ", "))
  }
  
  # Convert date column to POSIXct format
  raw_data$date_single <- as.POSIXct(raw_data$date_single, tz = "UTC")
  
  # Filter the data by the date range
  crime_data <- raw_data %>%
    filter(!is.na(latitude) & !is.na(longitude))  # Remove rows with missing coordinates
  
  return(crime_data)
}

# Step 2: Summarize the data
summarize_crime_data <- function(data) {
  # Basic summary statistics for numeric columns
  print(summary(data))
  
  # Summary of crime types
  crime_type_summary <- data %>%
    group_by(offense_type) %>%
    summarise(crime_count = n()) %>%
    arrange(desc(crime_count))
  print(head(crime_type_summary))
  
  # Crime count by year
  crime_year_summary <- data %>%
    mutate(year = year(date_single)) %>%
    group_by(year) %>%
    summarise(crime_count = n())
  print(crime_year_summary)
  
  # Missing data check
  missing_values <- colSums(is.na(data))
  print("Missing values:")
  print(missing_values[missing_values > 0])
  
  # Visualize crime count by year
  ggplot(crime_year_summary, aes(x = year, y = crime_count)) +
    geom_line() +
    theme_minimal() +
    labs(title = "Crime Count by Year", x = "Year", y = "Number of Crimes")
  
  # Crime count by offense type (bar plot)
  ggplot(crime_type_summary, aes(x = reorder(offense_type, crime_count), y = crime_count)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    theme_minimal() +
    labs(title = "Crime Count by Offense Type", x = "Offense Type", y = "Number of Crimes")
}

# Step 3: Load the data for Chicago (or any valid city) and summarize it
crime_data <- load_and_clean_crime_data(cities = "Chicago", start_year = 2018, end_year = 2020)

# Print the summary
summarize_crime_data(crime_data)

crime_data <- load_and_clean_crime_data(cities = "Chicago", start_year = 2018, end_year = 2020)


crime <- get_crime_data()

colnames(crime)
summary(crime)

ggplot(crime)





## # # # # # # # # # #  Shiny and R6 Section# # # # # # # # # # # # # # 

library(shiny)
library(dplyr)
library(lubridate)
library(ggplot2)
library(leaflet)
library(crimedata)
library(R6)
library(Rcpp)

cppFunction('
int custom_round(double x) {
  return (x >= 0.0) ? static_cast<int>(x + 0.5) : static_cast<int>(x - 0.5);
}')

# Predefine city choices
available_cities <- c("Austin", "Boston", "Charlotte", "Chicago", "Colorado Springs", 
                      "Detroit", "Houston", "Kansas City", "Los Angeles", 
                      "Louisville", "Memphis", "Mesa", "Minneapolis", "Nashville", 
                      "New York", "San Francisco", "Seattle")

CrimeSimulator <- R6::R6Class("CrimeSimulator",
                              public = list(
                                budget = 100,
                                crime_data = NULL,
                                
                                set_budget = function(new_budget) {
                                  self$budget <- new_budget
                                },
                                
                                set_crime_data = function(data) {
                                  self$crime_data <- data
                                },
                                
                                simulate_effects = function() {
                                  if (is.null(self$crime_data)) {
                                    return("No crime data available.")
                                  }
                                  
                                  crime_count <- nrow(self$crime_data)
                                  
                                  # Simulate effect based on the budget
                                  if (self$budget > 120) {
                                    # Assuming a budget above 120 reduces crime by 20%
                                    new_count <- crime_count * 0.8
                                    return(paste("Expected crime reduction: Crime count decreases to", custom_round(new_count)))
                                  } else if (self$budget < 80) {
                                    # Assuming a budget below 80 increases crime by 30%
                                    new_count <- crime_count * 1.3
                                    return(paste("Expected crime increase: Crime count increases to", custom_round(new_count)))
                                  } else {
                                    return("Neutral effect on crime expected.")
                                  }
                                }
                              )
)

# ---- R6 Class for Specialized Output ----
CrimeOutputGenerator <- R6Class("CrimeOutputGenerator",
                                public = list(
                                  data = NULL,
                                  
                                  initialize = function(data) {
                                    self$data <- data
                                  },
                                  
                                  generate_trend_plot = function() {
                                    req(nrow(self$data) > 0)
                                    data <- self$data %>%
                                      mutate(month = floor_date(date_single, "month")) %>%
                                      group_by(month, offense_type) %>%
                                      summarise(count = n(), .groups = 'drop')
                                    
                                    most_common <- data %>% group_by(offense_type) %>%
                                      summarise(total = sum(count)) %>%
                                      arrange(desc(total)) %>% slice(1) %>% pull(offense_type)
                                    
                                    ggplot(data, aes(x = month, y = count, color = offense_type, group = offense_type)) +
                                      geom_line(aes(linewidth = offense_type == most_common)) +
                                      scale_linewidth_manual(values = c("TRUE" = 1.5, "FALSE" = 0.7)) +
                                      theme_minimal() +
                                      labs(title = "Monthly Crime Trends", x = "Month", y = "Crime Count")
                                  },
                                  
                                  generate_bar_plot = function() {
                                    req(nrow(self$data) > 0)
                                    offense_counts <- self$data %>% count(offense_type) %>% arrange(desc(n))
                                    most_common <- offense_counts$offense_type[1]
                                    
                                    ggplot(offense_counts, aes(x = reorder(offense_type, n), y = n, fill = offense_type == most_common)) +
                                      geom_col() +
                                      coord_flip() +
                                      scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "steelblue")) +
                                      theme_minimal() +
                                      theme(legend.position = "none") +
                                      labs(title = "Crime Count by Offense Type", x = "Offense Type", y = "Count")
                                  },
                                  
                                  generate_map = function() {
                                    req(nrow(self$data) > 0)
                                    bounds <- list(
                                      lat1 = min(self$data$latitude, na.rm = TRUE),
                                      lat2 = max(self$data$latitude, na.rm = TRUE),
                                      lng1 = min(self$data$longitude, na.rm = TRUE),
                                      lng2 = max(self$data$longitude, na.rm = TRUE)
                                    )
                                    
                                    leaflet(self$data) %>%
                                      addTiles() %>%
                                      addMarkers(~longitude, ~latitude,
                                                 popup = ~paste("<b>Offense:</b>", offense_type, "<br><b>Date:</b>", date_single),
                                                 clusterOptions = markerClusterOptions()) %>%
                                      fitBounds(bounds$lng1, bounds$lat1, bounds$lng2, bounds$lat2)
                                  }
                                )
)

# ---- UI ----
ui <- fluidPage(
  titlePanel("Crime Data Explorer"),
  fluidRow(
    column(4,
           selectInput("city", "Select City", choices = available_cities, selected = "Chicago"),
           uiOutput("date_slider"),
           uiOutput("offense_selector"),
           downloadButton("downloadData", "Download Filtered CSV"),
           actionButton("select_all", "Select All Offenses"),
           actionButton("deselect_all", "Deselect All Offenses"),
           sliderInput("budget_slider", "Police Budget (%)", min = 0, max = 200, value = 100, step = 10),
           textOutput("sim_effect")
    ),
    column(8,
           tabsetPanel(
             tabPanel("Trend Plot", plotOutput("trendPlot")),
             tabPanel("Bar Chart", plotOutput("barPlot")),
             tabPanel("Map", leafletOutput("crimeMap"))
           )
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  
  # Loading and cleaning raw data
  raw_data <- reactive({
    get_crime_data(cities = input$city, type = "core")
  })
  
  offense_choices <- reactive({
    req(raw_data())
    sort(unique(raw_data()$offense_type))
  })
  
  # Dynamically update date range based on raw data
  observeEvent(raw_data(), {
    dates <- as.Date(raw_data()$date_single)
    updateDateRangeInput(session, "date_range", start = min(dates, na.rm = TRUE), end = max(dates, na.rm = TRUE))
  })
  
  output$date_slider <- renderUI({
    req(raw_data())
    dateRangeInput("date_range", "Select Date Range",
                   start = min(as.Date(raw_data()$date_single), na.rm = TRUE),
                   end = max(as.Date(raw_data()$date_single), na.rm = TRUE))
  })
  
  output$offense_selector <- renderUI({
    req(offense_choices())
    selectInput("offense_type", "Select Offense Types",
                choices = offense_choices(),
                selected = offense_choices(), multiple = TRUE)
  })
  
  observeEvent(input$select_all, {
    updateSelectInput(session, "offense_type", selected = offense_choices())
  })
  
  observeEvent(input$deselect_all, {
    updateSelectInput(session, "offense_type", selected = character(0))
  })
  
  filtered_data <- reactive({
    req(raw_data(), input$date_range, input$offense_type)
    
    data <- raw_data()
    
    # Ensure date_single is properly formatted
    data$date_single <- as.POSIXct(data$date_single, tz = "UTC", format = "%Y-%m-%d")
    
    # Defensive programming: remove rows with invalid dates or required fields
    data <- data %>%
      filter(!is.na(latitude), !is.na(longitude), !is.na(date_single))
    
    # Handle edge case: input$date_range may not be valid
    if (length(input$date_range) != 2 || any(is.na(input$date_range))) {
      return(NULL)
    }
    
    # Filter based on date and offense type
    data <- data %>%
      filter(date_single >= input$date_range[1],
             date_single <= input$date_range[2],
             offense_type %in% input$offense_type)
    
    # Return NULL if no rows remain
    if (nrow(data) == 0) {
      return(NULL)
    }
    
    return(data)
  })
  
  
  # Initialize CrimeSimulator
  simulator <- CrimeSimulator$new()
  
  # Update CrimeSimulator with filtered data
  observe({
    simulator$set_crime_data(filtered_data())
  })
  
  # Handle budget changes
  observeEvent(input$budget_slider, {
    simulator$set_budget(input$budget_slider)
  })
  
  # Reactive simulation effect output
  sim_effect <- reactive({
    # Only compute when filtered data and budget are valid
    data <- filtered_data()
    req(data, input$budget_slider)
    
    simulator$set_crime_data(data)
    simulator$set_budget(input$budget_slider)
    
    simulator$simulate_effects()
  })
  
  output$sim_effect <- renderText({
    sim_effect()
  })
  
  
  # Generate Trend Plot
  output$trendPlot <- renderPlot({
    req(filtered_data())  # Ensure there is data before plotting
    generator <- CrimeOutputGenerator$new(filtered_data())
    generator$generate_trend_plot()
  })
  
  # Generate Bar Plot
  output$barPlot <- renderPlot({
    req(filtered_data())  # Ensure there is data before plotting
    generator <- CrimeOutputGenerator$new(filtered_data())
    generator$generate_bar_plot()
  })
  
  # Generate Crime Map
  output$crimeMap <- renderLeaflet({
    req(filtered_data())  # Ensure there is data before plotting
    generator <- CrimeOutputGenerator$new(filtered_data())
    generator$generate_map()
  })
  
  # Data download handler
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("filtered_crime_data", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)