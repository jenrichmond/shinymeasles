library(shiny)
library(shinydashboard)
library(tidyverse)


# read data

m <- read_csv("m.csv") 

# ui
ui <- dashboardPage(
  dashboardHeader(title = "Measles Cases"),
  
  
  dashboardSidebar(

    div(
      style = "padding: 15px; margin-bottom: 20px; font-size: 12px; line-height: 1.4;",
      p(strong("Data Source:"), style = "margin-bottom: 8px; color: #fff;"),
      p("Provisional monthly data reported to the World Health Organisation (Geneva) as of June 2025.", 
        style = "margin-bottom: 8px; color: #ddd;"),
      p(a("View data source", 
          href = "https://immunizationdata.who.int/global?topic=Provisional-measles-and-rubella-data&location=", 
          target = "_blank",
          style = "color: #3c8dbc; text-decoration: none;"),
        style = "margin-bottom: 0px;")
    ),

    selectInput("region", "Choose a region:", 
                choices = c("All Regions" = "all", unique(m$region)),
                selected = "all"),
      # Country filter (will be updated based on region)
      selectInput("country", 
                  "Choose a country:", 
                  choices = unique(m$country),
                  selected = "United States of America"),
      
                  # Add date range input
      dateRangeInput("date_range",
                     "Select date range:",
                     start = min(m$date, na.rm = TRUE),
                     end = max(m$date, na.rm = TRUE),
                     min = min(m$date, na.rm = TRUE),
                     max = max(m$date, na.rm = TRUE),
                     format = "yyyy-mm-dd",
                     separator = " to "),


   # Add "Made by" section at the bottom
    div(
      style = "position: absolute; bottom: 60px; left: 15px; right: 15px; font-size: 11px; line-height: 1.3; border-top: 1px solid #444; padding-top: 10px;",
      p(strong("This dashboard was made by:"), style = "margin-bottom: 5px; color: #fff;"),
      p("Jen Richmond", style = "margin-bottom: 5px; color: #ddd;"),
      p(a("GitHub", 
          href = "https://github.com/jenrichmond/", 
          target = "_blank",
          style = "color: #3c8dbc; text-decoration: none; font-size: 11px;"),
        style = "margin-bottom: 0px;")
    )
    ),
    
 dashboardBody(
  fluidRow(
    column(12, 
      h1(HTML("World Health Organisation<br>Provisional Monthly Measles Cases"),
   style = "text-align: center; margin-bottom: 20px; color: #3c8dbc;")
    )
  ),

  tags$head(tags$style(HTML("
    .small-box h3 {
      font-size: 24px !important;
      font-weight: bold !important;
    }
    .small-box p {
      font-size: 16px !important;
    }
    .small-box .icon-large {
      font-size: 60px !important;
    }
    .small-box .icon {
      font-size: 45px !important;
    }
  "))),
    fluidRow(
      valueBoxOutput("total_cases", width = 3),
      valueBoxOutput("peak_month", width = 3),
      valueBoxOutput("avg_cases", width = 3),
      valueBoxOutput("recent_trend", width = 3)
    ),
    
    fluidRow(
      box(width = 12, plotOutput("measles_plot"))
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive expression to filter countries based on selected region
  filtered_countries <- reactive({
    if (input$region == "all") {
      unique(m$country)
    } else {
      unique(m$country[m$region == input$region])
    }
  })
  
  # Update country choices when region changes
  observe({
    updateSelectInput(session, "country",
                      choices = filtered_countries(),
                      selected = filtered_countries()[1])  # Select first country in the list
  })

  # Create reactive filtered data for reuse
  filtered_data <- reactive({
    m %>%
      filter(country == input$country,
             date >= input$date_range[1],
             date <= input$date_range[2]) %>%
      arrange(date)
  })
  
  # Summary Statistics Outputs
  output$total_cases <- renderValueBox({
    total <- sum(filtered_data()$measles_total, na.rm = TRUE)
    valueBox(
      value = format(total, big.mark = ","),
      subtitle = "Total Cases",
      icon = icon("virus"),
      color = "light-blue"
    )
  })
  
  output$peak_month <- renderValueBox({
    data <- filtered_data()
    if(nrow(data) > 0) {
      peak_date <- data$date[which.max(data$measles_total)]
      peak_value <- max(data$measles_total, na.rm = TRUE)
      peak_text <- paste0(format(peak_date, "%b %Y"))
    } else {
      peak_text <- "No data"
    }
    
    valueBox(
      value = peak_text,
      subtitle = "Peak Month",
      icon = icon("chart-line"),
      color = "light-blue"
    )
  })
  
  output$avg_cases <- renderValueBox({
    avg <- round(mean(filtered_data()$measles_total, na.rm = TRUE), 1)
    valueBox(
      value = format(avg, big.mark = ","),
      subtitle = "Average Cases/Month",
      icon = icon("calculator"),
      color = "light-blue"
    )
  })
  
  output$recent_trend <- renderValueBox({
    data <- filtered_data()
    if(nrow(data) >= 6) {  # Need at least 6 months to calculate trend
      recent_6 <- tail(data$measles_total, 6)
      recent_6_mean <- mean(recent_6[1:6], na.rm = TRUE)
      first_3 <- mean(recent_6[1:3], na.rm = TRUE)
      last_3 <- mean(recent_6[4:6], na.rm = TRUE)
      
      if(recent_6_mean < 3) {
        trend <- "Stable"
        trend_icon <- "minus"
        trend_color <- "yellow"
      } else if(last_3 > first_3) {
        trend <- "Increasing"
        trend_icon <- "arrow-up"
        trend_color <- "red"
      } else if(last_3 < first_3) {
        trend <- "Decreasing"  
        trend_icon <- "arrow-down"
        trend_color <- "green"
      } else {
        trend <- "Stable"
        trend_icon <- "minus"
        trend_color <- "yellow"
      }
    } else {
      trend <- "Insufficient data"
      trend_icon <- "question"
      trend_color <- "gray"
    }
    
    valueBox(
      value = trend,
      subtitle = "Recent Trend",
      icon = icon(trend_icon),
      color = trend_color
    )
  })
  
  # Generate plot
  output$measles_plot <- renderPlot({
    m %>%
     filter(country == input$country,
             date >= input$date_range[1],
             date <= input$date_range[2]) %>% 
      ggplot(aes(x = date, y = measles_total)) +
      geom_point(colour = "red") +
      geom_line(colour = "red") +
      theme_minimal() +
      scale_x_date(date_labels = "%b \n%Y", date_breaks = "1 year") +
      scale_y_continuous(limits = c(0, max(filtered_data()$measles_total, na.rm = TRUE) * 1.1)) +
      labs(x = "Month", y = "Cases", title = "Total Measles cases",
           subtitle = paste(input$country, 
                           format(input$date_range[1], "%Y"), 
                           "to", 
                           format(input$date_range[2], "%Y")))
  })
}

# Run the app
shinyApp(ui = ui, server = server)
