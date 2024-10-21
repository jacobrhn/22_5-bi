setwd("/Users/jacob/data/dhbw/22_5-bi/24-10-10-vl")
#getwd()

library(RSQLite)
library(dplyr)
library(Hmisc)
library(lubridate)

# Einlesen der Produktionskosten-Daten aus der CSV-Datei
production_costs_data <- read.csv("productioncosts.csv", header = TRUE, sep= ",", dec=".")

# Einen Blick auf die ersten Zeilen des CSV-Datensatzes werfen
head(production_costs_data)

# Verbindung zur SQLite-Datenbank herstellen
con <- dbConnect(SQLite(), dbname = "sales_database.sqlite")

# Extrahieren der Verkaufsdaten aus der Datenbanktabelle 'sales'
sales_data <- dbReadTable(con, "sales")

# ?berpr?fen der ersten Zeilen der Verkaufsdaten
head(sales_data)


# Schlie?en der Verbindung zur Datenbank
dbDisconnect(con)

# Zusammenf?hren der Produktionskosten- und Verkaufsdaten
merged_data <- production_costs_data %>%
  left_join(sales_data, by = c("Date", "Country"))

# ?berpr?fen der ersten Zeilen der zusammengef?hrten Daten
head(merged_data)

# ?berpr?fung auf fehlende Werte in jedem Feld des zusammengef?hrten Datensatzes
colSums(is.na(merged_data))

#Verwendung der Hmisc-Packet und Ersetzen fehlender Produktionskosten mit dem Mittelwert:
#merged_data$Production_Costs=impute(merged_data$Production_Costs, mean)
merged_data$Production_Costs=impute(merged_data$Production_Costs, mean)

# Filterung der Daten f?r das Jahr 2020
merged_data$dates=ymd(merged_data$Date)
filtered_data_2020 <- merged_data %>%
  filter(between(dates, as.Date("2020-01-01"), as.Date("2020-12-31")))

# Anzeige der gefilterten Daten
head(filtered_data_2020)


# Filterung der Daten f?r Germany
filtered_data_germany <- merged_data %>%
  filter(Country == "Germany")


# Filterung der Daten f?r Verkaufsumsatz gr??er als 1000
high_revenue_data <- merged_data %>%
  filter(Sales_Revenue > 10000)

# Kombinierte Filterung: Daten aus Deutschland und Verkaufsumsatz gr??er als 10000
filtered_combined <- merged_data %>%
  filter(Country == "Germany" & Sales_Revenue > 10000)


# Berechnung des Gewinns
merged_data$Profit <- merged_data$Sales_Revenue - merged_data$Production_Costs


# Berechnung der Beschwerdequote
merged_data$Complaint_Rate <- merged_data$Opened_Complaints / merged_data$Active_Users



# Verbindung zur SQLite-Datenbank herstellen (oder erstellen, falls nicht vorhanden)
output <- "output.sqlite"
con <- dbConnect(SQLite(), dbname = output)

# Daten in die Datenbank schreiben (?berschreiben, falls die Tabelle bereits existiert)
dbWriteTable(con, "merged_data", merged_data, overwrite = TRUE, row.names = FALSE)

# Schlie?en der Verbindung zur Datenbank
dbDisconnect(con)


# Definieren des Pfades zur CSV-Datei
csv_file_path <- "merged_data.csv"

# Daten in eine CSV-Datei schreiben
write.csv(merged_data, file = csv_file_path, row.names = FALSE)




