setwd("/Users/jacob/data/dhbw/22_5-bi/24-10-10-1_aufgabe")

library(Hmisc)
library(lubridate)
library(dplyr)
library(RSQLite)

# ETL Prozess "OnlineRetail.csv"

# EXTRACT
online_retail_data = read.csv("OnlineRetail.csv", header = TRUE, sep= ",", dec=".")
summary(online_retail_data)
head(online_retail_data)


# TRANSFORM

## Bereinigen
## Wandle InvoiceDate von chr to DateTime (POSIXlt)
online_retail_data$InvoiceDate = dmy_hm(online_retail_data$InvoiceDate)
str(online_retail_data$InvoiceDate)

### na-Werte filtern
colSums(is.na(online_retail_data)) 

#### Nur CustomerID hat na-Werte (Anzahl: 135080 = 24,93%), diese werden mit Konstante belegt (impute)
online_retail_data$CustomerID=impute(online_retail_data$CustomerID, "None")
summary(online_retail_data$CustomerID)

### Negative Werte in Quantiy und UnitPrice mit Median belegen
colSums(online_retail_data < 0)
online_retail_data$Quantity[online_retail_data$Quantity < 0] = median(online_retail_data$Quantity[online_retail_data$Quantity > 0])
online_retail_data$UnitPrice[online_retail_data$UnitPrice < 0] = median(online_retail_data$UnitPrice[online_retail_data$UnitPrice > 0])


## Kennzahlen berechnen 
### Bestellungen pro Customer und sortieren
orders_by_customer = online_retail_data %>% group_by(CustomerID) %>% summarise(Anzahl_Bestellungen = n())
orders_by_customer = orders_by_customer[order(orders_by_customer$Anzahl_Bestellungen, decreasing = TRUE),]
summary(orders_by_customer)
head(orders_by_customer)

### Wert pro Invoice
### spalte OrderValueSum (Gesamtwert aller Items mit selber InvoiceNo) in originale Tabelle einfügen
value_per_invoice = online_retail_data %>% 
  group_by(InvoiceNo) %>% 
  summarise(OrderValueSum = sum(Quantity * UnitPrice))
online_retail_data = merge(online_retail_data, value_per_invoice, by = "InvoiceNo")

### Wert pro OrderRow
online_retail_data$RowValue = online_retail_data$Quantity * online_retail_data$UnitPrice

### Orders aus Deutschland mit OrderValueSum über 1000 Euro
orders_germany_over_100 = online_retail_data %>% 
  filter(Country == "Germany") %>% 
  group_by(InvoiceNo) %>%  
  filter(OrderValueSum > 1000) 

### orders_per_stock_item columns: StockCode, Description, SumOrders, 2011_Q1_Orders, 2011_Q2_Orders, 2011_Q3_Orders, 2011_Q4_Orders
orders_per_stock_item = online_retail_data %>% 
  group_by(StockCode, Description) %>% 
  summarise(
    SumOrders = n(), 
    `2010Q4Orders` = sum(InvoiceDate >= as.Date("2010-10-01") & InvoiceDate <= as.Date("2010-12-31")),
    `2011Q1Orders` = sum(InvoiceDate >= as.Date("2011-01-01") & InvoiceDate <= as.Date("2011-03-31")), 
    `2011Q2Orders` = sum(InvoiceDate >= as.Date("2011-04-01") & InvoiceDate <= as.Date("2011-06-30")), 
    `2011Q3Orders` = sum(InvoiceDate >= as.Date("2011-07-01") & InvoiceDate <= as.Date("2011-09-30")), 
    `2011Q4Orders` = sum(InvoiceDate >= as.Date("2011-10-01") & InvoiceDate <= as.Date("2011-12-31"))
  ) %>% 
  ungroup()

summary(order(orders_per_stock_item$SumOrders, decreasing = TRUE))


# LOAD

## Ergebnisse in einer SQLite-Datenbank speichern  
### Verbindung zur SQLite-Datenbank erstellen
output <- "1_aufgabe_output.sqlite"
con <- dbConnect(SQLite(), dbname = output)
### Daten in die Datenbank schreiben
dbWriteTable(con, "online_retail_data", online_retail_data, overwrite = TRUE, )
dbWriteTable(con, "orders_by_customer", orders_by_customer, overwrite = TRUE, row.names = FALSE)
dbWriteTable(con, "orders_germany_over_100", orders_germany_over_100, overwrite = TRUE, row.names = FALSE)
dbWriteTable(con, "orders_per_stock_item", orders_per_stock_item, overwrite = TRUE, row.names = FALSE)
### Schliessen der Verbindung zur Datenbank
dbDisconnect(con)

## online_retail_data neuem CSV-File speichern  
### Definieren des Pfades zur CSV-Datei
csv_file_path <- "online_retail_data.csv"
### Daten in eine CSV-Datei schreiben
write.csv(online_retail_data, file = csv_file_path, row.names = FALSE)

# Save Data for Assignement 2
setwd("/Users/jacob/data/dhbw/22_5-bi/24-10-17-2_aufgabe")
write.csv(online_retail_data, file = csv_file_path, row.names = FALSE)
