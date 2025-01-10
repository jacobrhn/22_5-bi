# Business Intelligence
# Anna-Lehna Grittke - 8322504
# Aufgabe 1 

setwd("/Users/jacob/data/dhbw/22_5-bi/98-assignments")

library(dplyr)
library(readr)
library(DBI)
library(RSQLite)

# 1. Extraktion: Daten aus CSV-Datei einlesen
file_path <- "OnlineRetail.csv"
data <- read_csv(file_path)

# 2. Transformation: Daten bereinigen
# Entfernen von fehlenden Werten
cleaned_data <- data %>%
  filter(!is.na(InvoiceNo) & !is.na(StockCode) & !is.na(Description) & !is.na(CustomerID))

# Sicherstellen, dass Quantity und UnitPrice positiv sind
cleaned_data <- cleaned_data %>%
  filter(Quantity > 0 & UnitPrice > 0)

# Spalten umformatieren
cleaned_data$InvoiceDate <- ymd_hms(cleaned_data$InvoiceDate)

# Berechnung wichtiger Kennzahlen
# Gesamtumsatz (TotalRevenue)
cleaned_data <- cleaned_data %>%
  mutate(TotalRevenue = Quantity * UnitPrice)

# Aggregierte Kennzahlen: Gesamtumsatz pro Land
revenue_by_country <- cleaned_data %>%
  group_by(Country) %>%
  summarise(
    TotalRevenue = sum(TotalRevenue),
    TotalQuantity = sum(Quantity),
    NumberOfTransactions = n_distinct(InvoiceNo)
  )

# 3. Laden: Ergebnisse speichern
# a) In einer SQLite-Datenbank
# Verbindung zur SQLite-Datenbank herstellen
conn <- dbConnect(SQLite(), "sales_data.db")

# Daten in die Datenbank schreiben
dbWriteTable(conn, "cleaned_data", cleaned_data, overwrite = TRUE)
dbWriteTable(conn, "revenue_by_country", revenue_by_country, overwrite = TRUE)

# Verbindung schließen
dbDisconnect(conn)

# b) In einer CSV-Datei
write_csv(cleaned_data, "cleaned_data.csv")
write_csv(revenue_by_country, "revenue_by_country.csv")

# Abschlussmeldung
cat("ETL-Prozess abgeschlossen. Die Daten wurden bereinigt, analysiert und gespeichert.")
