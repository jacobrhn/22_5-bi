# Business Intelligence
# Anna-Lehna Grittke, Jacob Ruhnau
# Aufgabe 5 - gemeinsames Dashboard 

setwd("/Users/jacob/data/dhbw/22_5-bi/99-portfolio")

# Lade die notwendigen Bibliotheken
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(dplyr)
library(ggplot2)
library(lubridate)
library(plotly)
library(choroplethrMaps)
library(maps)
library(DT)

retail_data <- read.csv("cleaned_data.csv", stringsAsFactors = FALSE, fileEncoding = "UTF-8")

retail_data$InvoiceDate <- ymd_hms(retail_data$InvoiceDate)
retail_data$Year <- year(retail_data$InvoiceDate)

# Berechnungen für KPIs
total_sales <- sum(retail_data$TotalRevenue, na.rm = TRUE)
total_quantity <- sum(retail_data$Quantity, na.rm = TRUE)
total_orders <- nrow(retail_data)
total_customers <- length(unique(retail_data$CustomerID))

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Online Retail Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Filter", tabName = "filter", icon = icon("filter")),
      pickerInput("country_filter", "Land(er):", 
                  choices = unique(retail_data$Country), 
                  selected = unique(retail_data$Country),
                  options = pickerOptions(
                    actionsBox = TRUE, 
                    size = 10,
                    selectedTextFormat = "count > 3"
                  ), 
                  multiple = TRUE
                  ),
      
      # Filter nach Jahren als Checkboxen
      pickerInput("year_filter", "Jahr(e):", 
                  choices = unique(retail_data$Year), 
                  selected = unique(retail_data$Year),
                  options = pickerOptions(
                    actionsBox = TRUE, 
                    size = 10,
                    selectedTextFormat = "count > 3"
                  ), 
                  multiple = TRUE
      ),

      sliderInput("price_filter", "UnitPrice (EUR):", 
                  min = min(retail_data$UnitPrice, na.rm = TRUE),
                  max = max(retail_data$UnitPrice, na.rm = TRUE),
                  value = c(min(retail_data$UnitPrice, na.rm = TRUE), max(retail_data$UnitPrice, na.rm = TRUE)),
                  step = 1
      ),
      
      actionButton("reset_filters", "Filter zurücksetzen", icon = icon("refresh")),

      
      tags$hr(),
      
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"), selected = TRUE),
      menuItem("Preis und Verkaufszahlen", tabName = "price_quantity", icon = icon("chart-line")),
      menuItem("Preisverteilung", tabName = "price_distribution", icon = icon("chart-bar")),
      menuItem("Top-Produkte", tabName = "top_products", icon = icon("star")),
      menuItem("Top-Länder", tabName = "top_countries", icon = icon("globe")),
      menuItem("Data Browser", tabName = "data_browser", icon = icon("table"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        @media (max-width: 1200px) {
          .value-box-custom {
            width: 50%;
            float: left;
          }
        }
        @media (min-width: 1201px) {
          .value-box-custom {
            width: 25%;
            float: left;
          }
        }
      "))
    ),
    tabItems(
      tabItem(tabName = "dashboard",
        fluidRow(
          div(class = "value-box-custom",
            valueBoxOutput("total_sales", width = 12)
          ),
          div(class = "value-box-custom",
            valueBoxOutput("total_quantity", width = 12)
          ),
          div(class = "value-box-custom",
            valueBoxOutput("total_customers", width = 12)
          ),
          div(class = "value-box-custom",
            valueBoxOutput("total_products", width = 12)
          )
        ),
        
        fluidRow(
          # Vier Boxen für Grafiken
          box(
            title = "Zusammenhang zwischen Preis und Verkaufszahlen", width = 6, status = "primary", solidHeader = TRUE,
            plotlyOutput("price_quantity_scatter")
          ),
          box(
            title = "Preisverteilung", width = 6, status = "primary", solidHeader = TRUE,
            plotlyOutput("price_distribution_plot")
          )
        ),
        
        fluidRow(
          box(
            title = "Top-Produkte", width = 6, status = "primary", solidHeader = TRUE,
            plotlyOutput("top_products_plot")
          ),
          box(
            title = "Top-Länder", width = 6, status = "primary", solidHeader = TRUE,
            plotlyOutput("top_countries_plot")
          )
        )
      ),
      
      tabItem(tabName = "price_quantity",
        fluidRow(
          box(
            title = "Zusammenhang zwischen Preis und Verkaufszahlen", width = 12, status = "primary", solidHeader = TRUE,
            plotlyOutput("price_quantity_scatter_full")
          )
        )
      ),
      
      tabItem(tabName = "price_distribution",
        fluidRow(
          box(
            title = "Preisverteilung", width = 12, status = "primary", solidHeader = TRUE,
            plotlyOutput("price_distribution_plot_full")
          )
        )
      ),
      
      tabItem(tabName = "top_products",
        fluidRow(
          box(
            title = "Top-Produkte", width = 12, status = "primary", solidHeader = TRUE,
            plotlyOutput("top_products_plot_full")
          )
        )
      ),
      
      tabItem(tabName = "top_countries",
        fluidRow(
          box(
            title = "Top-Länder", width = 12, status = "primary", solidHeader = TRUE,
            plotlyOutput("top_countries_plot_full")
          )
        )
      ),

      tabItem(tabName = "data_browser",
        DT::dataTableOutput("data_table")
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Filtere die Daten nach den ausgewählten Ländern und Jahren
  filtered_data <- reactive({
    req(input$country_filter, input$year_filter, input$price_filter)
    retail_data %>%
      filter(Country %in% input$country_filter, Year %in% input$year_filter, UnitPrice >= input$price_filter[1], UnitPrice <= input$price_filter[2])
  })
  
  # Total Sales (Gesamtumsatz)
  output$total_sales <- renderValueBox({
    data <- filtered_data()
    valueBox(
      formatC(sum(data$TotalRevenue, na.rm = TRUE), format = "f", big.mark = ",", digits = 2),
      "Gesamtumsatz (EUR)",
      icon = icon("piggy-bank"),
      color = "green"
    )
  })
  
  # Total Quantity (Verkaufsmenge)
  output$total_quantity <- renderValueBox({
    data <- filtered_data()
    valueBox(
      formatC(sum(data$Quantity, na.rm = TRUE), format = "f", big.mark = ",", digits = 0),
      "Verkaufte Artikel",
      icon = icon("shopping-cart"),
      color = "yellow"
    )
  })
  
  # Total Customers (Kundenanzahl)
  output$total_customers <- renderValueBox({
    data <- filtered_data()
    valueBox(
      formatC(length(unique(data$CustomerID)), format = "f", big.mark = ",", digits = 0),
      "Kunden",
      icon = icon("users"),
      color = "blue"
    )
  })
  
  # Total Products (Anzahl Produkte)
  output$total_products <- renderValueBox({
    data <- filtered_data()
    valueBox(
      formatC(length(unique(data$Description)), format = "f", big.mark = ",", digits = 0),
      "Produkte",
      icon = icon("tag"),
      color = "purple"
    )
  })
  
  # Zusammenhang zwischen Preis und Verkaufszahlen (Streudiagramm mit Regressionslinie)
  output$price_quantity_scatter <- renderPlotly({
    data <- filtered_data()
    
    # Streudiagramm
    scatter_plot <- plot_ly(data, x = ~UnitPrice, y = ~Quantity, type = 'scatter', mode = 'markers', 
                            marker = list(size = 5, color = 'blue', opacity = 0.5)) %>%
      layout(title = "Zusammenhang Preis und Verkaufszahlen",
             xaxis = list(title = "Preis"),
             yaxis = list(title = "Verkäufte Menge"))
    
    # Regressionslinie hinzufügen
    model <- lm(Quantity ~ UnitPrice, data = data)
    scatter_plot <- scatter_plot %>%
      add_lines(x = ~UnitPrice, y = fitted(model), line = list(color = 'red'), name = 'Regressionslinie')
    
    scatter_plot
  })
  
  output$price_quantity_scatter_full <- renderPlotly({
    data <- filtered_data()
    
    # Streudiagramm
    scatter_plot <- plot_ly(data, x = ~UnitPrice, y = ~Quantity, type = 'scatter', mode = 'markers', 
                            marker = list(size = 5, color = 'blue', opacity = 0.5)) %>%
      layout(title = "Zusammenhang Preis und Verkaufszahlen",
             xaxis = list(title = "Preis"),
             yaxis = list(title = "Verkäufte Menge"))
    
    # Regressionslinie hinzufügen
    model <- lm(Quantity ~ UnitPrice, data = data)
    scatter_plot <- scatter_plot %>%
      add_lines(x = ~UnitPrice, y = fitted(model), line = list(color = 'red'), name = 'Regressionslinie')
    
    scatter_plot
  })
  
  # Preisverteilung als Histogramm und Boxplot
  output$price_distribution_plot <- renderPlotly({
    data <- filtered_data()
    price_histogram <- plot_ly(data, x = ~UnitPrice, type = 'histogram', nbinsx = 50, 
                               marker = list(color = 'rgba(255, 165, 0, 0.6)', line = list(color = 'orange'))) %>%
      layout(title = "Preisverteilung der Produkte",
             xaxis = list(title = "Preis"),
             yaxis = list(title = "Häufigkeit"))
    
    price_boxplot <- plot_ly(data, y = ~UnitPrice, type = 'box', 
                             boxmean = TRUE, 
                             marker = list(color = 'orange')) %>%
      layout(title = "Preisverteilung (Boxplot)",
             yaxis = list(title = "Preis (UnitPrice)"))
    
    subplot(price_histogram, price_boxplot, nrows = 1, shareX = TRUE) %>%
      layout(title = "Verteilung der Verkaufspreise")
  })
  
  output$price_distribution_plot_full <- renderPlotly({
    data <- filtered_data()
    price_histogram <- plot_ly(data, x = ~UnitPrice, type = 'histogram', nbinsx = 50, 
                               marker = list(color = 'rgba(255, 165, 0, 0.6)', line = list(color = 'orange'))) %>%
      layout(title = "Preisverteilung der Produkte",
             xaxis = list(title = "Preis"),
             yaxis = list(title = "Häufigkeit"))
    
    price_boxplot <- plot_ly(data, y = ~UnitPrice, type = 'box', 
                             boxmean = TRUE, 
                             marker = list(color = 'orange')) %>%
      layout(title = "Preisverteilung (Boxplot)",
             yaxis = list(title = "Preis (UnitPrice)"))
    
    subplot(price_histogram, price_boxplot, nrows = 1, shareX = TRUE) %>%
      layout(title = "Verteilung der Verkaufspreise")
  })
  
  # Top-Produkte
  output$top_products_plot <- renderPlotly({
    data <- filtered_data()
    top_products <- data %>%
      group_by(Description) %>%
      summarise(TotalRevenue = sum(TotalRevenue, na.rm = TRUE)) %>%
      arrange(desc(TotalRevenue)) %>%
      head(10)
    
    plot_ly(top_products, x = ~Description, y = ~TotalRevenue, type = 'scatter', mode = 'markers', 
            marker = list(size = 15, color = 'lightgreen', symbol = 'circle')) %>%
      layout(title = "Top-Produkte nach Umsatz",
             xaxis = list(title = "Produkt"),
             yaxis = list(title = "Umsatz"))
  })
  
  output$top_products_plot_full <- renderPlotly({
    data <- filtered_data()
    top_products <- data %>%
      group_by(Description) %>%
      summarise(TotalRevenue = sum(TotalRevenue, na.rm = TRUE)) %>%
      arrange(desc(TotalRevenue)) %>%
      head(10)
    
    plot_ly(top_products, x = ~Description, y = ~TotalRevenue, type = 'scatter', mode = 'markers', 
            marker = list(size = 15, color = 'lightgreen', symbol = 'circle')) %>%
      layout(title = "Top-Produkte nach Umsatz",
             xaxis = list(title = "Produkt"),
             yaxis = list(title = "Umsatz"))
  })
  
  # Top-Länder
  output$top_countries_plot <- renderPlotly({
    data <- filtered_data()
    top_countries <- data %>%
      group_by(Country) %>%
      summarise(TotalRevenue = sum(TotalRevenue, na.rm = TRUE)) %>%
      arrange(desc(TotalRevenue))
    
    # Liste aller Länder der Welt
    all_countries <- data.frame(
      Country = unique(map_data("world")$region)
    )
    
    # Zusammenführen der Daten
    cartogram_data <- merge(all_countries, top_countries, by = "Country", all.x = TRUE)
    cartogram_data$TotalRevenue[is.na(cartogram_data$TotalRevenue)] <- 0
    
    plot_ly(
      type = 'choropleth',
      locations = cartogram_data$Country,
      locationmode = 'country names',
      z = cartogram_data$TotalRevenue,
      colorscale = 'YlOrRd_r',
      colorbar = list(title = 'Umsatz')
    ) %>%
      layout(
        title = "Top-Länder nach Umsatz",
        geo = list(
          showframe = FALSE,
          showcoastlines = TRUE,
          coastlinecolor = 'rgb(255, 255, 255)',
          projection = list(type = 'equirectangular')
        )
      )
  })
  
  output$top_countries_plot_full <- renderPlotly({
    data <- filtered_data()
    top_countries <- data %>%
      group_by(Country) %>%
      summarise(TotalRevenue = sum(TotalRevenue, na.rm = TRUE)) %>%
      arrange(desc(TotalRevenue))
    
    # Liste aller Länder der Welt
    all_countries <- data.frame(
      Country = unique(map_data("world")$region)
    )
    
    # Zusammenführen der Daten
    cartogram_data <- merge(all_countries, top_countries, by = "Country", all.x = TRUE)
    cartogram_data$TotalRevenue[is.na(cartogram_data$TotalRevenue)] <- 0
    
    plot_ly(
      type = 'choropleth',
      locations = cartogram_data$Country,
      locationmode = 'country names',
      z = cartogram_data$TotalRevenue,
      colorscale = 'YlOrRd_r',
      colorbar = list(title = 'Umsatz')
    ) %>%
      layout(
        title = "Top-Länder nach Umsatz",
        geo = list(
          showframe = FALSE,
          showcoastlines = TRUE,
          coastlinecolor = 'rgb(255, 255, 255)',
          projection = list(type = 'equirectangular')
        )
      )
  })

  output$data_table <- DT::renderDataTable({
    data <- filtered_data()
    DT::datatable(data, options = list(pageLength = 25))
  })
  
  observeEvent(event_data("plotly_click", source = "price_quantity_scatter"), {
    updateTabItems(session, "tabs", "price_quantity")
  })
  
  observeEvent(event_data("plotly_click", source = "price_distribution_plot"), {
    updateTabItems(session, "tabs", "price_distribution")
  })
  
  observeEvent(event_data("plotly_click", source = "top_products_plot"), {
    updateTabItems(session, "tabs", "top_products")
  })
  
  observeEvent(event_data("plotly_click", source = "top_countries_plot"), {
    updateTabItems(session, "tabs", "top_countries")
  })

  observeEvent(input$reset_filters, {
    updatePickerInput(session, "country_filter", selected = unique(retail_data$Country))
    updatePickerInput(session, "year_filter", selected = unique(retail_data$Year))
    updateSliderInput(session, "price_filter", value = c(min(retail_data$UnitPrice, na.rm = TRUE), max(retail_data$UnitPrice, na.rm = TRUE)))
  })
}

# Shiny App im Viewer starten
shinyApp(ui = ui, server = server)