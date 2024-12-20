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
library(dplyr)
library(lubridate)

# Load OnlineRetail data
#online_retail_data <- read.csv("OnlineRetail.csv", header = TRUE, sep = ",", dec = ".")
online_retail_data <- read.csv("online_retail_data.csv", header = TRUE, sep = ",", dec = ".")
str(online_retail_data)
online_retail_data$InvoiceDate <- ymd_hms(online_retail_data$InvoiceDate)

# Visualisierungen

#' A2_1: VERGLEICH DER LÄNDER (VERKAUFSZAHLEN)
#' Stellen Sie grafisch die Anzahl der verkauften Artikel nach Ländern dar. 

A2_1_data <- online_retail_data %>%
  group_by(Country) %>%
  summarise(TotalQuantity = sum(Quantity)) %>%
  filter(TotalQuantity > 100) %>%
  arrange(desc(TotalQuantity))

#' Unterscheiden Sie zwischen den Ländern, die mehr als 100 Artikel verkauft haben, und den anderen.
A2_1_fig <- plot_ly(
  data = A2_1_data,
  x = ~reorder(Country, TotalQuantity, decreasing = TRUE),
  y = ~TotalQuantity,
  type = "bar",
  marker = list(color = 'rgba(222,45,38,0.8)', 
                line = list(color = 'rgba(8,48,107,1.0)', width = 2))
) %>%
  layout(
    title = list(
      text = "[2.1] Verkaufte Artikel nach Ländern<br><sup>(über 100 Artikel)</sup>",
      x = 0.5
    ),
    xaxis = list(title = "Land"),
    yaxis = list(title = "Verkaufte Artikel")
  )

# Plot anzeigen
A2_1_fig

#%%

#' A2_2: PRODUKTKATEGORIE NACH VERKAUFSZAHLEN
#' Visualisieren Sie die Verkaufszahlen (Quantity) nach den verschiedenen Produktkategorien (Description). 
#' Welche Produkte wurden am häufigsten verkauft?

# Daten aggregieren und sortieren
A2_2_data <- online_retail_data %>%
  group_by(Description) %>%
  summarise(TotalQuantity = sum(Quantity)) %>%
  filter(TotalQuantity > 0) %>%
  arrange(desc(TotalQuantity))

# Plot erstellen
A2_2_fig <- plot_ly(
  data = A2_2_data,
  x = ~reorder(Description, TotalQuantity, decreasing = TRUE),
  y = ~TotalQuantity,
  type = "bar",
  marker = list(color = 'rgba(222,45,38,0.8)', 
                line = list(color = 'rgba(8,48,107,1.0)', width = 2))
) %>%
  layout(
    title = "[2.2] Verkaufte Artikel nach Produktkategorien",
    xaxis = list(title = 'Produktkategorie'), 
    yaxis = list(title = 'Anzahl verkaufter Artikel'),
    margin = list(b = 150)  # Platz für lange Beschriftungen
  )

# Plot anzeigen
A2_2_fig


#' A2_3: PRODUKTKATEGORIE NACH VERKAUFSPREIS
#' Visualisieren Sie die Verkaufspreise (UnitPrice) nach den verschiedenen Produktkategorien (Description). 
#' Welche Produkte haben den höchsten Gesamtverkaufspreis?

# Daten aggregieren und sortieren
A2_3_data <- online_retail_data %>%
  group_by(Description) %>%
  summarise(TotalUnitPrice = sum(UnitPrice * Quantity)) %>%
  filter(TotalUnitPrice > 0) %>%
  arrange(desc(TotalUnitPrice))

# Plot erstellen
A2_3_fig <- plot_ly(
  data = A2_3_data,
  x = ~reorder(Description, TotalUnitPrice, decreasing = TRUE),
  y = ~TotalUnitPrice,
  type = "bar",
  marker = list(color = 'rgba(222,45,38,0.8)', 
                line = list(color = 'rgba(8,48,107,1.0)', width = 2))
) %>%
  layout(
    title = "[2.3] Verkaufspreise nach Produktkategorien",
    xaxis = list(title = 'Produktkategorie'), 
    yaxis = list(title = 'Verkaufspreis'),
    margin = list(b = 150)  # Platz für lange Beschriftungen
  )

# Plot anzeigen
A2_3_fig

#%%


#' A2_4: ZEITLICHE ENTWICKLUNG DER VERKÄUFE
#' Stellen Sie grafisch dar, wie sich die Verkaufszahlen über die Monate hinweg verändert haben. 
#' Nutzen Sie dazu die InvoiceDate-Spalte, um die Verkäufe nach Monaten zu aggregieren.

# Daten nach Monaten aggregieren und sortieren
A2_4_data <- online_retail_data %>%
  mutate(Month = floor_date(InvoiceDate, "month")) %>%
  group_by(Month) %>%
  summarise(TotalQuantity = sum(Quantity)) %>%
  filter(TotalQuantity > 0) %>%
  arrange(Month)

# Plot erstellen
A2_4_fig <- plot_ly(
  data = A2_4_data,
  x = ~Month,
  y = ~TotalQuantity,
  type = "scatter",
  mode = "lines+markers",
  line = list(shape = "linear")
) %>%
  layout(
    title = "[2.4] Verkaufte Artikel über die Zeit",
    xaxis = list(title = 'Monat'), 
    yaxis = list(title = 'Anzahl verkaufter Artikel')
  )

# Plot anzeigen
A2_4_fig

#%%

#' A2_5: ZUSAMMENHANG ZWISCHEN PREIS UND VERKAUFSZAHLEN
#' Überprüfen Sie grafisch, ob es einen Zusammenhang zwischen dem Preis (UnitPrice) 
#' und der Anzahl der verkauften Artikel (Quantity) gibt. 
#' Verwenden Sie dazu ein Streudiagramm.

A2_5_data <- online_retail_data %>%
  filter(UnitPrice > 0) %>%
  group_by(Description) %>%
  summarise(
    TotalQuantity = sum(Quantity, na.rm = TRUE),
    AvgUnitPrice = mean(UnitPrice, na.rm = TRUE)
  ) %>%
  filter(TotalQuantity > 0) %>%
  arrange(desc(TotalQuantity))

A2_5_fig <- plot_ly(
  data = A2_5_data,
  x = ~AvgUnitPrice,
  y = ~TotalQuantity,
  type = "scatter",
  mode = "markers",
  marker = list(
    color = 'rgba(222,45,38,0.8)',
    line = list(color = 'rgba(8,48,107,1.0)', width = 2)
  )
) %>%
  layout(
    title = "[2.5] Zusammenhang zwischen Preis und Verkaufszahlen",
    xaxis = list(title = 'Durchschnittlicher Preis (UnitPrice)'), 
    yaxis = list(title = 'Anzahl verkaufter Artikel'),
    margin = list(b = 80)
  )

# Plot anzeigen
A2_5_fig
