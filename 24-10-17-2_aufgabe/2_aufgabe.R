#' AUFGABE 2
#' 
#' Business Intelligence (AM413.1) - WWI22
#' Eingereicht von Jacob Ruhnau (ruhnauja@dhbw-loerrach.de)
#' 
#' Visualisierungen der OnlineRetail.csv-Datenquelle, anhand der in Aufgabe 1 aufgearbeiteten Daten

setwd("/Users/jacob/data/dhbw/22_5-bi/24-10-17-2_aufgabe")
#getwd()

library(plotly)

# OnlineRetail dat
online_retail_data <- read.csv("online_retail_data.csv", header = TRUE, sep = ",", dec = ".")

# Visualisierungen

#' VERGLEICH DER VERKAUFSZAHLEN
#' Stellen Sie grafisch die Anzahl der verkauften Artikel nach Ländern dar. 
#' Unterscheiden Sie zwischen den Ländern, die mehr als 100 Artikel verkauft haben, und den anderen.


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

