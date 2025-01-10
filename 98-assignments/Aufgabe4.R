# Business Intelligence
# Anna-Lehna Grittke - 8322504
# Aufgabe 4

setwd("/Users/jacob/data/dhbw/22_5-bi/98-assignments")

# Lade notwendige Pakete
library(dplyr)
library(lubridate)
library(prophet)

# Daten laden (ersetze den Dateipfad mit dem tatsächlichen Pfad zur Datei)
cleaned_data <- read.csv("cleaned_data.csv", header = TRUE, sep = ",", dec = ".")
cleaned_data$InvoiceDate <- ymd_hms(cleaned_data$InvoiceDate)
cleaned_data$Month <- floor_date(cleaned_data$InvoiceDate, "month")

# Struktur der Daten überprüfen
str(cleaned_data)

# 1. Berechne den Gesamtumsatz pro Bestellung
cleaned_data <- cleaned_data %>%
  mutate(TotalSales = Quantity * UnitPrice)

# 2. Füge eine Spalte 'Month' hinzu und berechne den monatlichen Gesamtumsatz
monthly_sales <- cleaned_data %>%
  group_by(Month) %>%
  summarise(MonthlyTotalSales = sum(TotalSales, na.rm = TRUE))

# Ausgabe der aggregierten Daten (zur Überprüfung)
print("Aggregierte Verkaufsdaten:")
print(monthly_sales)

# Überprüfe auf fehlende Werte in den aggregierten Daten
monthly_sales <- monthly_sales %>%
  filter(!is.na(MonthlyTotalSales))

# Ausgabe der bereinigten Daten
print("Bereinigte aggregierte Verkaufsdaten ohne NA-Werte:")
print(monthly_sales)

# Überprüfe, ob genügend Daten vorhanden sind
if (nrow(monthly_sales) < 2) {
  stop("Es gibt weniger als zwei gültige Datenzeilen für das Modell.")
}

# 3. Zeitreihenmodell mit Prophet erstellen
# Prophet benötigt Spalten "ds" (Datum) und "y" (Wert)
prophet_data <- monthly_sales %>%
  rename(ds = Month, y = MonthlyTotalSales)

# Überprüfe auf fehlende Werte im prophet_data-DataFrame
if (any(is.na(prophet_data$ds)) | any(is.na(prophet_data$y))) {
  stop("Es gibt fehlende Werte in den Daten.")
}

# Erstelle ein Prophet-Modell
model <- prophet()
model <- add_seasonality(model, name = "monthly", period = 30.5, fourier.order = 3)
model <- fit.prophet(model, prophet_data)

# 4. Vorhersagen für die nächsten 6 Monate berechnen
future <- make_future_dataframe(model, periods = 6, freq = "month")
forecast <- predict(model, future)

# 5. Visualisiere die Ergebnisse
plot(model, forecast) +
  ggtitle("Vorhersage der monatlichen Verkaufszahlen")

# 6. Zeige die Vorhersagen für die nächsten 6 Monate
forecast_next_6_months <- forecast %>%
  filter(ds > max(prophet_data$ds)) %>%
  select(ds, yhat, yhat_lower, yhat_upper)

print("Vorhersagen für die nächsten 6 Monate:")
print(forecast_next_6_months)
