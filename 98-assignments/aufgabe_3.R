### 
###                         AUFGABE 3
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

# Daten laden
df <- read.csv("online_retail_data.csv", header = TRUE, sep = ",", dec = ".")

# Datenstruktur überprüfen
str(df)
summary(df)

# 1. Hypothese: Unterschiede im Verkaufsumsatz zwischen Ländern (z.B. Deutschland und Frankreich)
# Filtere Daten für Deutschland und Frankreich
A1_df <- df %>% filter(Country %in% c("Greece", "Netherlands"))

### Schritt 1: Normalverteilung überprüfen (Shapiro-Wilk-Test)
A1_normality_test <- onewaytests::nor.test(Quantity ~ Country, data = A1_df)

# Ergebnis: Wenn p < 0.05, ist die Verteilung nicht normal.

### Schritt 2: Varianzhomogenität testen (Levene-Test)
variance_test <- leveneTest(Quantity ~ Country, data = df_gf)
print(variance_test)

# Ergebnis: Wenn p > 0.05, sind die Varianzen homogen.

### Schritt 3: Unterschied testen
# a) Wenn normal verteilt und gleiche Varianzen -> t-Test
t_test_result <- t.test(Quantity ~ Country, data = df_gf)
print(t_test_result)

# b) Wenn nicht normal verteilt oder unterschiedliche Varianzen -> Mann-Whitney-Test
mann_whitney_result <- wilcox.test(Quantity ~ Country, data = df_gf, exact = TRUE)
print(mann_whitney_result)

# 2. Hypothese: Zusammenhang zwischen Preis (UnitPrice) und Verkaufszahlen (Quantity)
### Berechnung der Pearson-Korrelation (metrische Variablen, normalverteilt)
pearson_correlation <- cor(df$UnitPrice, df$Quantity, method = "pearson")
print(paste("Pearson-Korrelation: ", round(pearson_correlation, 2)))

### Berechnung der Spearman-Korrelation (nicht normalverteilte oder ordinale Variablen)
spearman_correlation <- cor(df$UnitPrice, df$Quantity, method = "spearman")
print(paste("Spearman-Korrelation: ", round(spearman_correlation, 2)))

# Visualisierung: Scatterplot Preis vs. Verkaufszahlen
ggplot(df, aes(x = UnitPrice, y = Quantity)) +
  geom_point(color = "blue") +
  labs(
    title = "Zusammenhang zwischen Preis und Verkaufszahlen",
    x = "Preis (UnitPrice)",
    y = "Verkaufszahlen (Quantity)"
  ) +
  theme_minimal()

# 3. Hypothese: Unterschied in Verkaufsumsätzen zwischen mehr als zwei Ländern
# Aggregiere Daten für alle Länder
df_country <- df %>% group_by(Country) %>% summarise(Total_Sales = sum(Sales_Revenue))

### ANOVA-Test für Normalverteilung und Varianzgleichheit
anova_result <- aov(Total_Sales ~ Country, data = df_country)
summary(anova_result)

### Kruskal-Wallis-Test für nicht-normalverteilte Daten
kruskal_result <- kruskal.test(Total_Sales ~ Country, data = df_country)
print(kruskal_result)

# Visualisierung: Balkendiagramm für Gesamtumsätze
ggplot(df_country, aes(x = reorder(Country, Total_Sales), y = Total_Sales)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(
    title = "Gesamtverkaufsumsätze nach Ländern",
    x = "Land",
    y = "Gesamtumsatz"
  ) +
  theme_minimal()

# 4. Hypothese: Zusammenhang zwischen diskreten Variablen (z. B. Länder und Nutzeraktivität)
# Erstelle Kontingenztabelle
df_gf$Active_Users_cat <- cut(df_gf$Active_Users, breaks = 3, labels = c("low", "medium", "high"))
table_data <- table(df_gf$Active_Users_cat, df_gf$Country)

# Berechne Chi²-Test
chi_squared_result <- chisq.test(table_data)
print(chi_squared_result)

# Berechne Kontingenzkoeffizient
library(vcd)
assoc_result <- assocstats(table_data)
print(assoc_result)


#%%

# A3_2: Überprüfung A2_2: PRODUKTKATEGORIE NACH VERKAUFSZAHLEN
# A3_3: Überprüfung A2_3: PRODUKTKATEGORIE NACH VERKAUFSPREIS
# A3_4: Überprüfung A2_4: ZEITLICHE ENTWICKLUNG DER VERKÄUFE
# A3_5: Überprüfung A2_5: ZUSAMMENHANG ZWISCHEN PREIS UND VERKAUFSZAHLEN
