### 
###                         AUFGABE 2
### 
###              Business Intelligence (AM413.1)
###                Prof. Dr. Aikatarini Nakou
###                   WWI22 - DHBW Lörrach
### 
###            Bearbeitet von Jacob Ruhnau (2441453)
### 

setwd("/Users/jacob/data/dhbw/22_5-bi/98-assignments")
#getwd()

library(dplyr)
library(stringr)
library(plotly)
library(onewaytests)
library(car)

### Statistische Überprüfung (Unterschiede und Zusammenhänge der Variablen)
###   - Anhand der Visualisierungen aus aufgabe_2.R
###   - Methoden an den Schemata aus "BI_4". 
###   - Umsetzung in "hypothese_testen_korrelation.R"

# Load OnlineRetail data
#online_retail_data <- read.csv("OnlineRetail.csv", header = TRUE, sep = ",", dec = ".")
online_retail_data <- read.csv("online_retail_data.csv", header = TRUE, sep = ",", dec = ".")
online_retail_data$InvoiceDate <- ymd_hms(online_retail_data$InvoiceDate)

#%%
#   A3_1:   Überprüfung A2_1: A2_1: VERGLEICH DER LÄNDER (VERKAUFSZAHLEN)
#           (Am Beispiel Greece und Netherlands)

A3_1_data <- online_retail_data %>%
  filter(Country == "Greece" | Country == "Netherlands") %>%
  group_by(Country) %>%
  filter(n() >= 3 & n() <= 5000) %>% # sonst "Error in FUN(X[[i]], ...) : sample size must be between 3 and 5000"
  ungroup()

# 1.  Testen, ob die Daten normalverteilt sind (Shapiro-Wilk)
onewaytests::nor.test(Quantity ~ Country, data = A3_1_data)

# 2.  Testen, ob beide Verteilungen die gleiche Varianz haben oder nicht (Levene)
onewaytests::var.test(Quantity ~ Country, data = A3_1_data)

# 3.A Bei Normalverteilung
# Daten sind nicht normalverteilt.

# 3.B. Bei Nicht-Normalverteilung
# 3.B.2 Testen, ob die Mittelwerte der beiden Gruppen gleich sind (Wilcoxon-Test)
# 3.B.3 Testen, ob die Mittelwerte der beiden Gruppen gleich sind (ANOVA)
# 3.B.4 Testen, ob die Mittelwerte der beiden Gruppen gleich sind (Kruskal-Wallis-Test)

# 4. Pearson-Korrelation

# 5. Spearman-Korrelation

# 6. Kontingenzkoeffizient



#%%

# A3_2: Überprüfung A2_2: PRODUKTKATEGORIE NACH VERKAUFSZAHLEN
# A3_3: Überprüfung A2_3: PRODUKTKATEGORIE NACH VERKAUFSPREIS
# A3_4: Überprüfung A2_4: ZEITLICHE ENTWICKLUNG DER VERKÄUFE
# A3_5: Überprüfung A2_5: ZUSAMMENHANG ZWISCHEN PREIS UND VERKAUFSZAHLEN
