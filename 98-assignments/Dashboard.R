# Installiere und lade notwendige Pakete, falls noch nicht installiert
install.packages(c("shiny", "shinydashboard", "dplyr", "ggplot2", "lubridate", "plotly", "choroplethr", "choroplethrMaps"))

# Lade die notwendigen Bibliotheken
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(lubridate)
library(plotly)
library(choroplethr)
library(choroplethrMaps)

# Lade die CSV-Datei mit einer spezifischen Kodierung (UTF-8 oder latin1)
retail_data <- read.csv("OnlineRetail.csv", stringsAsFactors = FALSE, fileEncoding = "UTF-8")

# 1. Entferne Zeilen mit fehlenden wichtigen Daten
retail_data <- retail_data %>%
  filter(!is.na(InvoiceDate), !is.na(Quantity), !is.na(UnitPrice), !is.na(Country))

# 2. Konvertiere die InvoiceDate-Spalte in ein Datum
retail_data$InvoiceDate <- as.POSIXct(retail_data$InvoiceDate, format="%m/%d/%Y %H:%M", tz = "GMT")

# 3. Berechne die TotalPrice-Spalte (UnitPrice * Quantity)
retail_data$TotalPrice <- retail_data$UnitPrice * retail_data$Quantity

# 4. Entferne ungültige oder nicht-finite Werte aus den relevanten Spalten
retail_data <- retail_data %>%
  filter(is.finite(TotalPrice), is.finite(Quantity), is.finite(UnitPrice))

# 5. Entferne Zeilen mit negativen oder null Werten in den relevanten Spalten
retail_data <- retail_data %>%
  filter(Quantity > 0, UnitPrice > 0)

# 6. Optional: Entfernen von leeren oder nicht sinnvollen Country-Werten
retail_data <- retail_data %>%
  filter(Country != "")

# Berechnungen für KPIs
total_sales <- sum(retail_data$TotalPrice, na.rm = TRUE)
total_quantity <- sum(retail_data$Quantity, na.rm = TRUE)
total_orders <- nrow(retail_data)
total_customers <- length(unique(retail_data$CustomerID))

# UI - Benutzeroberfläche
ui <- dashboardPage(
  dashboardHeader(title = "Online Retail Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      
      # Filter nach Ländern als Checkboxen
      checkboxGroupInput("country_filter", "Wählen Sie Land(er):", 
                         choices = unique(retail_data$Country), selected = unique(retail_data$Country), inline = TRUE)
    )
  ),
  
  dashboardBody(
    fluidRow(
      # Vier Boxen für Kennzahlen
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
        title = "Top-Länder (Cartogram)", width = 6, status = "primary", solidHeader = TRUE,
        plotlyOutput("top_countries_plot")
      )
    )
  )
)

# Server - Serverlogik
server <- function(input, output) {
  
  # Filtere die Daten nach den ausgewählten Ländern
  filtered_data <- reactive({
    req(input$country_filter)
    retail_data %>%
      filter(Country %in% input$country_filter)
  })
  
  # Total Sales (Gesamtumsatz)
  output$total_sales <- renderValueBox({
    data <- filtered_data()
    valueBox(
      formatC(sum(data$TotalPrice, na.rm = TRUE), format = "f", big.mark = ",", digits = 2),
      "Gesamtumsatz",
      icon = icon("piggy-bank"),
      color = "green"
    )
  })
  
  # Total Quantity (Verkaufsmenge)
  output$total_quantity <- renderValueBox({
    data <- filtered_data()
    valueBox(
      formatC(sum(data$Quantity, na.rm = TRUE), format = "f", big.mark = ",", digits = 0),
      "Verkäufte Menge",
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
  
  # Zusammenhang zwischen Preis und Verkaufszahlen (Streudiagramm)
  output$price_quantity_scatter <- renderPlotly({
    data <- filtered_data()
    
    # Streudiagramm für den Zusammenhang zwischen UnitPrice und Quantity
    plot_ly(data, x = ~UnitPrice, y = ~Quantity, type = 'scatter', mode = 'markers', 
            marker = list(size = 5, color = 'blue', opacity = 0.5)) %>%
      layout(title = "Zusammenhang Preis und Verkaufszahlen",
             xaxis = list(title = "Preis"),
             yaxis = list(title = "Verkäufte Menge"))
  })
  
  # Preisverteilung als Histogramm und Boxplot
  output$price_distribution_plot <- renderPlotly({
    data <- filtered_data()
    
    # Histogramm der UnitPrice-Verteilung
    price_histogram <- plot_ly(data, x = ~UnitPrice, type = 'histogram', nbinsx = 50, 
                               marker = list(color = 'rgba(255, 165, 0, 0.6)', line = list(color = 'orange'))) %>%
      layout(title = "Preisverteilung der Produkte",
             xaxis = list(title = "Preis", dtick = 5),
             yaxis = list(title = "Häufigkeit"))
    
    # Boxplot zur Überprüfung der Ausreißer
    price_boxplot <- plot_ly(data, y = ~UnitPrice, type = 'box', 
                             boxmean = TRUE, 
                             marker = list(color = 'orange')) %>%
      layout(title = "Preisverteilung (Boxplot)",
             yaxis = list(title = "Preis (UnitPrice)"))
    
    # Beide Diagramme nebeneinander
    subplot(price_histogram, price_boxplot, nrows = 1, shareX = TRUE) %>%
      layout(title = "Verteilung der Verkaufspreise")
  })
  
  # Top-Produkte (Kreisdiagramm mit Punkten)
  output$top_products_plot <- renderPlotly({
    data <- filtered_data()
    top_products <- data %>%
      group_by(Description) %>%
      summarise(TotalRevenue = sum(TotalPrice, na.rm = TRUE)) %>%
      arrange(desc(TotalRevenue)) %>%
      head(10)
    
    # Skalierte Punkte für jedes Produkt
    plot_ly(top_products, x = ~Description, y = ~TotalRevenue, type = 'scatter', mode = 'markers', 
            marker = list(size = 15, color = 'lightgreen', symbol = 'circle')) %>%
      layout(title = "Top-Produkte nach Umsatz",
             xaxis = list(title = "Produkt"),
             yaxis = list(title = "Umsatz"))
  })
  
  # Top-Länder (Cartogram)
  output$top_countries_plot <- renderPlotly({
    data <- filtered_data()
    top_countries <- data %>%
      group_by(Country) %>%
      summarise(TotalRevenue = sum(TotalPrice, na.rm = TRUE)) %>%
      arrange(desc(TotalRevenue)) %>%
      head(10)
    
    # Erstelle ein Cartogram mit den Umsatzdaten der Top-Länder
    cartogram_data <- data.frame(region = top_countries$Country, value = top_countries$TotalRevenue)
    
    # Zeichne das Cartogram
    plot_ly(
      type = 'choropleth',
      locations = cartogram_data$region,
      locationmode = 'country names',
      z = cartogram_data$value,
      colorscale = 'YlOrRd',
      colorbar = list(title = 'Umsatz')
    ) %>%
      layout(title = "Top-Länder nach Umsatz (Cartogram)",
             geo = list(showcoastlines = TRUE, coastlinecolor = 'rgb(255, 255, 255)', projection = list(type = 'mercator')))
  })
}

# Shiny App im Viewer starten
shinyApp(ui = ui, server = server)
