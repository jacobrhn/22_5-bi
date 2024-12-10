# Laden der notwendigen Pakete
library(shiny)
library(shinydashboard)
library(leaflet)
library(plotly)
library(dplyr)
library(readr)
library(lubridate)
library(prophet)

# Datensatz laden
data <- read_csv("european_dashboard_dataset.csv")
data[sapply(data, is.numeric)] <- round(data[sapply(data, is.numeric)])

# Definieren der Koordinaten für \u20ACpäische Länder
country_coords <- data.frame(
  Country = c("Germany", "France", "Italy", "Spain", "Netherlands", 
              "Sweden", "Finland", "Norway", "Denmark", "Poland"),
  Longitude = c(10.0, 2.3522, 12.4964, -3.7038, 4.9041, 18.0686, 24.9355, 10.7522, 12.5683, 21.0122),
  Latitude = c(51.1657, 48.8566, 41.9028, 40.4168, 52.3676, 59.3293, 60.1695, 59.9139, 55.6761, 52.2297)
)

# UI Definition
ui <- dashboardPage(
  dashboardHeader(title = "Sales Enterprise Dashboard"),
  dashboardSidebar(
    sidebarMenu(id = "sidebarmenu",
                menuItem("Sales", tabName = "sales", icon = icon("chart-line")),
                conditionalPanel("input.sidebarmenu === 'sales'",
                                 selectInput("year", "Year", choices = unique(data$Date %>% format("%Y")), selected = "2020"),
                                 selectInput("country", "Country", choices = c("All Countries", unique(data$Country)), selected = "All Countries")),
                menuItem("Predictions", tabName = "predictions", icon = icon("line-chart")),
                conditionalPanel("input.sidebarmenu === 'predictions'",
                                 selectInput("pred_country", "Country", choices = unique(data$Country), selected = "Germany"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "sales",
              fluidRow(
                valueBoxOutput("salesRevenueBox", width = 3),
                valueBoxOutput("productionCostsBox", width = 3),
                valueBoxOutput("activeUsersBox", width = 3),
                valueBoxOutput("openedComplaintsBox", width = 3)
              ),
              fluidRow(
                box(
                  title = "Sales Revenue",
                  status = "primary",
                  plotlyOutput("salesRevenuePlot", height = 250),
                  width = 6
                ),
                box(
                  title = "Sales Revenue by Country",
                  status = "primary",
                  leafletOutput("salesMap", height = 250),
                  width = 6
                )
              ),
              fluidRow(
                box(
                  title = "Breakdown",
                  status = "primary",
                  plotlyOutput("breakdownPlot", height = 300),
                  width = 12
                )
              )
      ),
      tabItem(tabName = "predictions",
              fluidRow(
                box(
                  title = "Sales Revenue Predictions",
                  status = "primary",
                  plotlyOutput("predictionsPlot", height = 400),
                  width = 12
                )
              )
      )
    )
  )
)

# Server Definition
server <- function(input, output) {
  # Filter der Daten basierend auf Benutzereingabe
  filteredData <- reactive({
    req(input$year)  # sicherstellen, dass Jahr ausgewählt ist
    data %>%
      filter(format(Date, "%Y") == input$year) %>%
      { if (input$country != "All Countries") filter(., Country == input$country) else . }
  })
  
  # Value Boxes anzeigen
  output$salesRevenueBox <- renderValueBox({
    valueBox(
      paste0("\u20AC ", sum(filteredData()$Sales_Revenue)), "Sales Revenue", icon = icon("chart-line"), color = "light-blue"
    )
  })
  
  output$productionCostsBox <- renderValueBox({
    valueBox(
      paste0("\u20AC ", sum(filteredData()$Production_Costs)), "Production Costs", icon = icon("shopping-cart"), color = "purple"
    )
  })
  
  output$activeUsersBox <- renderValueBox({
    valueBox(
      sum(filteredData()$Active_Users), "Active Users", icon = icon("users"), color = "green"
    )
  })
  
  output$openedComplaintsBox <- renderValueBox({
    valueBox(
      sum(filteredData()$Opened_Complaints), "Opened Complaints", icon = icon("envelope"), color = "teal"
    )
  })
  
  # Sales Revenue Plot erstellen
  output$salesRevenuePlot <- renderPlotly({
    monthly_data <- filteredData() %>%
      group_by(Month = format(Date, "%Y-%m")) %>%
      summarise(Sales_Revenue = sum(Sales_Revenue), .groups = 'drop')
    
    plot_ly(
      data = monthly_data,
      x = ~Month,
      y = ~Sales_Revenue,
      type = 'scatter',
      mode = 'lines+markers',
      name = 'Revenue',
      marker = list(color = 'blue')
    ) %>%
      layout(title = paste("Total Sales Revenue in", input$year),
             xaxis = list(title = "Month"),
             yaxis = list(title = "Sales Revenue (\u20AC)"))
  })
  
  # Sales Revenue by Country Map erstellen
  output$salesMap <- renderLeaflet({
    leaflet() %>%
      addTiles()
  })
  
  observe({
    leafletProxy("salesMap") %>% clearMarkers()
    
    if (input$country == "All Countries") {
      # Aggregierte Daten für alle Länder
      all_countries_data <- filteredData() %>%
        group_by(Country) %>%
        summarise(
          Sales_Revenue = sum(Sales_Revenue),
          Production_Costs = sum(Production_Costs),
          Active_Users = sum(Active_Users),
          .groups = 'drop'
        ) %>%
        left_join(country_coords, by = "Country")
      
      leafletProxy("salesMap") %>%
        addCircleMarkers(
          data = all_countries_data,
          lng = ~Longitude,
          lat = ~Latitude,
          popup = ~paste("<b>Country:</b>", Country,
                         "<br><b>Sales Revenue:</b> \u20AC", Sales_Revenue,
                         "<br><b>Production Costs:</b> \u20AC", Production_Costs,
                         "<br><b>Active Users:</b>", Active_Users),
          color = "blue",
          fillOpacity = 0.7,
          radius = 8
        )
    } else {
      # Daten für das ausgewählte Land
      selected_country_data <- filteredData() %>%
        summarise(
          Sales_Revenue = sum(Sales_Revenue),
          Production_Costs = sum(Production_Costs),
          Active_Users = sum(Active_Users),
          .groups = 'drop'
        )
      
      country_coords_selected <- country_coords %>%
        filter(Country == input$country)
      
      if (nrow(country_coords_selected) > 0 && !is.null(selected_country_data)) {
        leafletProxy("salesMap") %>%
          addCircleMarkers(
            lng = country_coords_selected$Longitude,
            lat = country_coords_selected$Latitude,
            popup = paste("<b>Country:</b>", input$country,
                          "<br><b>Sales Revenue:</b> \u20AC", selected_country_data$Sales_Revenue,
                          "<br><b>Production Costs:</b> \u20AC", selected_country_data$Production_Costs,
                          "<br><b>Active Users:</b>", selected_country_data$Active_Users),
            color = "red",
            fillOpacity = 0.7,
            radius = 10
          )
      }
    }
  })
  
  # Breakdown Plot erstellen
  output$breakdownPlot <- renderPlotly({
    metrics <- filteredData() %>%
      summarise(
        Sales_Revenue = sum(Sales_Revenue),
        Production_Costs = sum(Production_Costs),
        Active_Users = sum(Active_Users),
        Opened_Complaints = sum(Opened_Complaints),
        .groups = 'drop'
      )
    
    plot_ly(
      x = c("Sales Revenue", "Production Costs", "Active Users", "Opened Complaints"),
      y = c(metrics$Sales_Revenue, metrics$Production_Costs, 
            metrics$Active_Users, metrics$Opened_Complaints),
      type = 'bar',
      marker = list(color = 'light-blue')
    ) %>%
      layout(title = "Breakdown of Key Metrics")
  })
  
  # Vorhersagen erstellen
  output$predictionsPlot <- renderPlotly({
    # Daten für Vorhersagen basierend auf dem ausgewählten Land
    sales_data <- data %>%
      filter(Country == input$pred_country) %>%
      arrange(Date) %>%
      group_by(Month = format(Date, "%Y-%m")) %>%
      summarise(Sales_Revenue = sum(Sales_Revenue), .groups = 'drop')
    
    # Konvertiere den Monat in das Date-Format
    sales_data <- sales_data %>% 
      mutate(ds = as.Date(paste0(Month, "-01")), y = Sales_Revenue) %>% 
      select(ds, y)
    
    # Vorhersagemodell erstellen
    model <- prophet(sales_data)
    future <- make_future_dataframe(model, periods = 12, freq = "month")
    forecast <- predict(model, future)
    
    # Plot der Vorhersagen
    plot_ly() %>%
      add_trace(data = sales_data, x = ~ds, y = ~y, type = 'scatter', mode = 'lines', name = 'Tatsächliche Werte', line = list(color = 'grey')) %>%
      add_trace(data = forecast, x = ~ds, y = ~yhat, type = 'scatter', mode = 'lines', name = 'Vorhersage', line = list(color = 'blue')) %>%
      add_ribbons(data = forecast, x = ~ds, ymin = ~yhat_lower, ymax = ~yhat_upper, name = 'Konfidenzintervall', fillcolor = 'rgba(0, 0, 255, 0.2)') %>%
      layout(title = paste("Sales Revenue Forecast for", input$pred_country),
             xaxis = list(title = "Date"),
             yaxis = list(title = "Sales Revenue (\u20AC)"))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
