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
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      
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
      )
    )
  ),
  
  dashboardBody(
    fluidRow(
      # Vier Boxen für KPIs
      valueBoxOutput("total_sales"),
      valueBoxOutput("total_quantity"),
      valueBoxOutput("total_customers"),
      valueBoxOutput("total_products")
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
  )
)

# Server
server <- function(input, output) {
  
  # Filtere die Daten nach den ausgewählten Ländern und Jahren
  filtered_data <- reactive({
    req(input$country_filter, input$year_filter)
    retail_data %>%
      filter(Country %in% input$country_filter, Year %in% input$year_filter)
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
  
  # Top-Länder
  output$top_countries_plot <- renderPlotly({
    
    # Filtere die Daten nach den ausgewählten Ländern und Jahren
    filtered_data <- reactive({
      req(input$country_filter, input$year_filter)
      retail_data %>%
        filter(Country %in% input$country_filter, Year %in% input$year_filter)
    })
    
    data <- filtered_data()
    data <- retail_data
    top_countries <- data %>%
      group_by(Country) %>%
      summarise(TotalRevenue = sum(TotalRevenue, na.rm = TRUE)) %>%
      arrange(desc(TotalRevenue))
    
    # Liste aller Länder der Welt
    all_countries <- data.frame(
      region = unique(map_data("world")$region),
      value = 0
    )
    
    # Zusammenführen der Daten
    cartogram_data <- merge(all_countries, top_countries, by.x = "region", by.y = "Country", all.x = TRUE)
    cartogram_data$TotalRevenue[is.na(cartogram_data$TotalRevenue)] <- 0
    
    plot_ly(
      type = 'choropleth',
      locations = cartogram_data$region,
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
}

# Shiny App im Viewer starten
shinyApp(ui = ui, server = server)
