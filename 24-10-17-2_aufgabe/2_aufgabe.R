#' AUFGABE 2
#' 
#' Business Intelligence (AM413.1) - WWI22
#' Eingereicht von Jacob Ruhnau (ruhnauja@dhbw-loerrach.de)
#' 
#' Visualisierungen der OnlineRetail.csv-Datenquelle, anhand der in Aufgabe 1 aufgearbeiteten Daten

setwd("/Users/jacob/data/dhbw/22_5-bi/24-10-17-2_aufgabe")
#getwd()

library(plotly)

# Load OnlineRetail data
online_retail_data <- read.csv("OnlineRetail.csv", header = TRUE, sep = ",", dec = ".")
str(online_retail_data)
online_retail_data$InvoiceDate = ymd_hms(online_retail_data$InvoiceDate)

# Visualisierungen

#' A2_1 VERGLEICH DER VERKAUFSZAHLEN
#' Stellen Sie grafisch die Anzahl der verkauften Artikel nach Ländern dar. 

data_A2_1 = online_retail_data %>%
  group_by(Country) %>%
  summarise(TotalQuantity = sum(Quantity)) %>%
  filter(TotalQuantity > 100) %>%
  arrange(desc(TotalQuantity))

#' Unterscheiden Sie zwischen den Ländern, die mehr als 100 Artikel verkauft haben, und den anderen.
fig_A2_1 = plot_ly(
  data = data_A2_1,
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
fig_A2_1


#' PRODUKTKATWGORIE NACH VERKAUFSZAHLEN
#' Visualisieren Sie die Verkaufszahlen (Quantity) nach den verschiedenen Produktkategorien (Description). 
#' Welche Produkte wurden am häufigsten verkauft?


#' PREISVERTEILUNG
#' Überprüfen Sie grafisch die Verteilung der Verkaufspreise (UnitPrice) der Produkte. 
#' Gibt es Ausreißer? 
#' Wie sieht die allgemeine Preisstruktur aus?


#' ZEITLICHE ENTWICKLUNG DER VERKÄUFE
#' Stellen Sie grafisch dar, wie sich die Verkaufszahlen über die Monate hinweg verändert haben. 
#' Nutzen Sie dazu die InvoiceDate-Spalte, um die Verkäufe nach Monaten zu aggregieren.


#' ZUSAMMENHANG ZWISCHEN PREIS UND VERKAUFSZAHLEN
#' Überprüfen Sie grafisch, ob es einen Zusammenhang zwischen dem Preis (UnitPrice) 
#' und der Anzahl der verkauften Artikel (Quantity) gibt. 
#' Verwenden Sie dazu ein Streudiagramm.

