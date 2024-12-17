setwd("/Users/jacob/data/dhbw/22_5-bi/99-portfolio")

library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Basic Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Übersicht", tabName = "dashboard", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                valueBoxOutput("total_revenue"),
                valueBoxOutput("total_sales"),
                valueBoxOutput("avg_order_value"),
                valueBoxOutput("total_customers")
              ),
              fluidRow(
                box(title = "Umsatz pro Monat", status = "primary", solidHeader = TRUE, 
                    plotOutput("revenue_per_month")),
                box(title = "Top 5 Länder nach Umsatz", status = "primary", solidHeader = TRUE, 
                    plotOutput("top_countries")),
                box(title = "Produktkategorien nach Verkaufsmenge", status = "primary", solidHeader = TRUE, 
                    plotOutput("product_categories"))
              )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Daten laden
  data_path <- "OnlineRetail.csv"
  if (file.exists(data_path)) {
    raw_data <- read.csv(data_path, encoding = "latin1")
    clean_data <- raw_data %>%
      filter(!is.na(Description)) %>%
      mutate(InvoiceDate = as.Date(InvoiceDate, format = "%d-%m-%Y"))
  } else {
    showNotification("CSV-Datei nicht gefunden. Bitte überprüfen Sie den Pfad.", type = "error")
    return(NULL)
  }
  
  # KPIs berechnen
  total_revenue <- sum(clean_data$Quantity * clean_data$UnitPrice, na.rm = TRUE)
  total_sales <- nrow(clean_data)
  avg_order_value <- total_revenue / total_sales
  total_customers <- n_distinct(clean_data$CustomerID)
  
  # Umsatz pro Monat
  revenue_per_month <- clean_data %>%
    group_by(Month = format(InvoiceDate, "%Y-%m")) %>%
    summarise(Revenue = sum(Quantity * UnitPrice, na.rm = TRUE))
  
  # Top 5 Länder nach Umsatz
  top_countries <- clean_data %>%
    group_by(Country) %>%
    summarise(Revenue = sum(Quantity * UnitPrice, na.rm = TRUE)) %>%
    top_n(5, Revenue)
  
  # Produktkategorien nach Verkaufsmenge
  product_categories <- clean_data %>%
    group_by(Description) %>%
    summarise(Quantity = sum(Quantity, na.rm = TRUE)) %>%
    arrange(desc(Quantity)) %>%
    head(10)
  
  # KPIs anzeigen
  output$total_revenue <- renderValueBox({
    valueBox(format(total_revenue, big.mark = ","), "Gesamte Einnahmen", icon = icon("euro"), color = "green")
  })
  
  output$total_sales <- renderValueBox({
    valueBox(total_sales, "Anzahl der Verkäufe", icon = icon("shopping-cart"), color = "blue")
  })
  
  output$avg_order_value <- renderValueBox({
    valueBox(format(avg_order_value, big.mark = ","), "Durchschnittlicher Bestellwert", icon = icon("calculator"), color = "yellow")
  })
  
  output$total_customers <- renderValueBox({
    valueBox(total_customers, "Anzahl der Kunden", icon = icon("users"), color = "purple")
  })
  
  # Diagramme anzeigen
  output$revenue_per_month <- renderPlot({
    ggplot(revenue_per_month, aes(x = Month, y = Revenue)) +
      geom_line() +
      theme_minimal() +
      labs(title = "Umsatz pro Monat", x = "Monat", y = "Umsatz")
  })
  
  output$top_countries <- renderPlot({
    ggplot(top_countries, aes(x = reorder(Country, Revenue), y = Revenue)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      theme_minimal() +
      labs(title = "Top 5 Länder nach Umsatz", x = "Land", y = "Umsatz")
  })
  
  output$product_categories <- renderPlot({
    ggplot(product_categories, aes(x = reorder(Description, Quantity), y = Quantity)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      theme_minimal() +
      labs(title = "Produktkategorien nach Verkaufsmenge", x = "Produktkategorie", y = "Verkaufsmenge")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)