### 
###                         AUFGABE 3 
### 
###              Business Intelligence (AM413.1)
###                Prof. Dr. Aikatarini Nakou
###                   WWI22 - DHBW Lörrach
### 
###            Bearbeitet von Jacob Ruhnau (2441453)
### 

#' Laden Sie die Daten aus OnlineRetail_v2.csv in R.
#' Erstellen Sie eine neue Spalte TotalSales, die den Gesamtumsatz pro Bestellung (Quantity * UnitPrice) berechnet.
#' Aggregieren Sie die Daten nach Monat und berechnen Sie den monatlichen Gesamtumsatz.
#' Verwenden Sie die monatlichen Verkaufszahlen, um ein Zeitreihenmodell mit prophet zu erstellen.
#' Erstellen Sie ein Modell, das die monatlichen Verkaufszahlen vorhersagt, und berechnen Sie die Vorhersagen für die nächsten 6 Monate.
#' 

setwd("/Users/jacob/data/dhbw/22_5-bi/98-assignments")
#getwd()

library(dplyr)
library(stringr)
library(plotly)
library(onewaytests)
library(car)
library(prophet)
library(lubridate)

#' Laden Sie die Daten aus OnlineRetail_v2.csv in R.
online_retail_data <- read.csv("OnlineRetail_v2.csv", header = TRUE, sep = ",", dec = ".")
str(online_retail_data)

# Erstellen Sie eine neue Spalte TotalSales, die den Gesamtumsatz pro Bestellung (Quantity * UnitPrice) berechnet.
online_retail_data$TotalSales <- online_retail_data$Quantity * online_retail_data$UnitPrice

#' Aggregieren Sie die Daten nach Monat und berechnen Sie den monatlichen Gesamtumsatz.
online_retail_data$InvoiceDate <- ymd_hms(online_retail_data$InvoiceDate)
online_retail_data$Month <- floor_date(online_retail_data$InvoiceDate, "month")

#' Verwenden Sie die monatlichen Verkaufszahlen, um ein Zeitreihenmodell mit prophet zu erstellen.
online_retail_data_monthly <- online_retail_data %>%
  group_by(Month) %>%
  summarise(TotalSales = sum(TotalSales))

#' Erstellen Sie ein Modell, das die monatlichen Verkaufszahlen vorhersagt, und berechnen Sie die Vorhersagen für die nächsten 6 Monate.
online_retail_data_monthly_prophet <- online_retail_data_monthly %>%
  rename(ds = Month, y = TotalSales)
