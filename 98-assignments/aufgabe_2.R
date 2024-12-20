#' 
#'                         AUFGABE 2
#' 
#'              Business Intelligence (AM413.1)
#'                Prof. Dr. Aikatarini Nakou
#'                   WWI22 - DHBW Lörrach
#' 
#'            Bearbeitet von Jacob Ruhnau (2441453)
#' 


setwd("/Users/jacob/data/dhbw/22_5-bi/98-assignments")
#getwd()

Sys.setlocale('LC_ALL','C')
library(plotly)


# Load OnlineRetail data
#online_retail_data <- read.csv("OnlineRetail.csv", header = TRUE, sep = ",", dec = ".")
online_retail_data <- read.csv("online_retail_data.csv", header = TRUE, sep = ",", dec = ".")
str(online_retail_data)
online_retail_data$InvoiceDate = ymd_hms(online_retail_data$InvoiceDate)

# Visualisierungen

#' A2_1 VERGLEICH DER VERKAUFSZAHLEN
#' Stellen Sie grafisch die Anzahl der verkauften Artikel nach Ländern dar. 

A2_1_data = online_retail_data %>%
  group_by(Country) %>%
  summarise(TotalQuantity = sum(Quantity)) %>%
  filter(TotalQuantity > 100) %>%
  arrange(desc(TotalQuantity))

#' Unterscheiden Sie zwischen den Ländern, die mehr als 100 Artikel verkauft haben, und den anderen.
A2_1_fig = plot_ly(
  data = A2_1_data,
  x = ~reorder(Country, TotalQuantity, decreasing = TRUE),
  y = ~TotalQuantity,
  type = "bar",
  marker = list(color = 'rgba(222,45,38,0.8)', 
                line = list(color = 'rgba(8,48,107,1.0)', width = 2))
) %>%
  layout(
    title = list(
      text = "Verkaufte Artikel nach Ländern<br><sup>(über 100 Artikel)</sup>",
      x = 0.5
    ),
    xaxis = list(title = 'Land'), 
    yaxis = list(title = 'Anzahl verkaufter Artikel')
  )
A2_1_fig


#' PRODUKTKATWGORIE NACH VERKAUFSZAHLEN
#' Visualisieren Sie die Verkaufszahlen (Quantity) nach den verschiedenen Produktkategorien (Description). 
#' Welche Produkte wurden am häufigsten verkauft?
A2_2_data = online_retail_data %>%
  group_by(Description) %>%
  summarise(TotalQuantity = sum(Quantity)) %>%
  filter(TotalQuantity > 0) %>%
  arrange(desc(TotalQuantity))

A2_2_fig = plot_ly(
  data = A2_2_data,
  x = ~reorder(Description, TotalQuantity, decreasing = TRUE),
  y = ~TotalQuantity,
  type = "scatter",
  mode = "markers"
) %>%
  layout(
    title = "Verkaufte Artikel nach Produktkategorien",
    xaxis = list(title = 'Produktkategorie'), 
    yaxis = list(title = 'Anzahl verkaufter Artikel')
  )
A2_2_fig


#' PREISVERTEILUNG
#' Überprüfen Sie grafisch die Verteilung der Verkaufspreise (UnitPrice) der Produkte. 
#' Gibt es Ausreißer? 
#' Wie sieht die allgemeine Preisstruktur aus?
A2_3_data = online_retail_data %>%
  filter(UnitPrice > 0) %>%
  group_by(Description) %>%
  summarise(TotalUnitPrice = sum(UnitPrice)) %>%
  filter(TotalUnitPrice > 0) %>%
  arrange(desc(TotalUnitPrice))

A2_3_fig = plot_ly(
  data = A2_3_data,
  x = ~reorder(Description, TotalUnitPrice, decreasing = TRUE),
  y = ~TotalUnitPrice,
  type = "scatter",
  mode = "markers"
) %>%
  layout(
    title = "Verkaufspreise nach Produktkategorien",
    xaxis = list(title = 'Produktkategorie'), 
    yaxis = list(title = 'Verkaufspreis')
  )
A2_3_fig


#' ZEITLICHE ENTWICKLUNG DER VERKÄUFE
#' Stellen Sie grafisch dar, wie sich die Verkaufszahlen über die Monate hinweg verändert haben. 
#' Nutzen Sie dazu die InvoiceDate-Spalte, um die Verkäufe nach Monaten zu aggregieren.

A2_4_data = online_retail_data %>%
  group_by(InvoiceDate) %>%
  summarise(TotalQuantity = sum(Quantity)) %>%
  filter(TotalQuantity > 0) %>%
  arrange(desc(TotalQuantity))

A2_4_fig = plot_ly(
  data = A2_4_data,
  x = ~InvoiceDate,
  y = ~TotalQuantity,
  type = "scatter",
  mode = "lines"
) %>%
  layout(
    title = "Verkaufte Artikel über die Zeit",
    xaxis = list(title = 'Datum'), 
    yaxis = list(title = 'Anzahl verkaufter Artikel')
  )
A2_4_fig


#' ZUSAMMENHANG ZWISCHEN PREIS UND VERKAUFSZAHLEN
#' Überprüfen Sie grafisch, ob es einen Zusammenhang zwischen dem Preis (UnitPrice) 
#' und der Anzahl der verkauften Artikel (Quantity) gibt. 
#' Verwenden Sie dazu ein Streudiagramm.

A2_5_data = online_retail_data %>%
  filter(UnitPrice > 0) %>%
  group_by(Description) %>%
  summarise(TotalQuantity = sum(Quantity), TotalUnitPrice = sum(UnitPrice)) %>%
  filter(TotalQuantity > 0) %>%
  arrange(desc(TotalQuantity))

A2_5_fig = plot_ly(
  data = A2_5_data,
  x = ~TotalUnitPrice,
  y = ~TotalQuantity,
  type = "scatter",
  mode = "markers"
) %>%
  layout(
    title = "Zusammenhang zwischen Preis und Verkaufszahlen",
    xaxis = list(title = 'Verkaufspreis'), 
    yaxis = list(title = 'Anzahl verkaufter Artikel')
  )
A2_5_fig
