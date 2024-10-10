library(RSQLite)
library(dplyr)
library(Hmisc)


# Einlesen der Produktionskosten-Daten aus der CSV-Datei
production_costs_data <- read.csv("productioncosts.csv", header = TRUE, sep= ",", dec=".")

# Einen Blick auf die ersten Zeilen des CSV-Datensatzes werfen
head(production_costs_data)

# Verbindung zur SQLite-Datenbank herstellen
con <- dbConnect(SQLite(), dbname = "sales_database.sqlite")

# Extrahieren der Verkaufsdaten aus der Datenbanktabelle 'sales'
sales_data <- dbReadTable(con, "sales")

# Überprüfen der ersten Zeilen der Verkaufsdaten
head(sales_data)


# Schließen der Verbindung zur Datenbank
dbDisconnect(con)

# Zusammenführen der Produktionskosten- und Verkaufsdaten
merged_data <- production_costs_data %>%
  left_join(sales_data, by = c("Date", "Country"))

# Überprüfen der ersten Zeilen der zusammengeführten Daten
head(merged_data)

# Überprüfung auf fehlende Werte in jedem Feld des zusammengeführten Datensatzes
colSums(is.na(merged_data))


#Verwendung der Hmisc-Packet und Ersetzen fehlender Produktionskosten mit dem Mittelwert:
merged_data$Production_Costs=impute(merged_data$Production_Costs, mean)

# Filterung der Daten für das Jahr 2020
filtered_data_2020 <- merged_data %>%
  filter(Year == 2020)

# Anzeige der gefilterten Daten
head(filtered_data_2020)


# Filterung der Daten für Germany
filtered_data_germany <- merged_data %>%
  filter(Country == "Germany")


# Filterung der Daten für Verkaufsumsatz größer als 1000
high_revenue_data <- merged_data %>%
  filter(Sales_Revenue > 10000)

# Kombinierte Filterung: Daten aus Deutschland und Verkaufsumsatz größer als 10000
filtered_combined <- merged_data %>%
  filter(Country == "Germany" & Sales_Revenue > 10000)


# Berechnung des Gewinns
merged_data$Profit <- merged_data$Sales_Revenue - merged_data$Production_Costs


# Berechnung der Beschwerdequote
merged_data$Complaint_Rate <- merged_data$Opened_Complaints / merged_data$Active_Users



# Verbindung zur SQLite-Datenbank herstellen (oder erstellen, falls nicht vorhanden)
output <- "output.sqlite"
con <- dbConnect(SQLite(), dbname = output)

# Daten in die Datenbank schreiben (Überschreiben, falls die Tabelle bereits existiert)
dbWriteTable(con, "merged_data", merged_data, overwrite = TRUE, row.names = FALSE)

# Schließen der Verbindung zur Datenbank
dbDisconnect(con)


# Definieren des Pfades zur CSV-Datei
csv_file_path <- "merged_data.csv"

# Daten in eine CSV-Datei schreiben
write.csv(merged_data, file = csv_file_path, row.names = FALSE)




