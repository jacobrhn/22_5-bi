setwd("/Users/jacob/data/dhbw/22_5-bi/24-10-24")
#getwd()
# Lade notwendige Pakete
library(dplyr)
library(stringr)
library(plotly)
library(onewaytests)
library(car)

# Lade die Daten
df <- read.csv("sales_gesamtdf.csv", header = TRUE, sep = ",", dec = ".")

# Der Datensatz enth?lt 6 Spalten mit mehreren Eintr?gen.
# 4 Spalten mit kontinuierlichen Werten und 2 Spalten mit diskreten Werten.

# ?berpr?fe die Struktur der Daten
str(df)

# Siehe Informationen f?r die Daten
summary(df)

# Kombinierte Filterung: Daten aus Deutschland und Frankreich
df_gf <- df %>%
  filter(Country == "Germany" | Country == "France")
## Two Sample Tests

#%% md

### Deutschland vs. Frankreich Sales Revenue



### Schritt 1 - Pr?fen, ob die Daten normalverteilt sind ###
### Der Shapiro-Wilk-Test testet die Nullhypothese, dass die ###
### Daten aus einer Normalverteilung gezogen wurden ###
### Wenn pValue < 0,05 ist, ist die Verteilung nicht normal ###

# nor.test ist bei R der Shapiro-Wilk-Test
onewaytests::nor.test(Sales_Revenue ~ Country, data = df_gf)



#%%




fig <- plot_ly(x = df_gf$Sales_Revenue, color = df_gf$Country, type = "histogram")


fig



print('Hier sehen wir deutlich, dass beide Variablen Normalverteilt sind')

#%%

### Schritt 2: Testen, ob beide Verteilungen die gleiche Varianz haben oder nicht ###
###Der Levene-Test testet die Nullhypothese, dass alle eingegebenen Stichproben###
### aus Populationen mit gleichen Varianzen stammen.  ###

leveneTest(df_gf$Sales_Revenue, df_gf$Country)

#### Ergebnis: Pr(>F) ist grösser als 0,05 -> sind homogen


#%%

### Schritt 3: Da die Verteilungen normal sind ###
### und die gleiche Varianz haben, verwenden wir den T Test ###


t.test(Sales_Revenue ~ Country, data = df_gf)

#### Ergebnis: p-value = 0,7017 


df_country <- df_gf %>%
  group_by(Country) %>%
  summarise(Total_Sales = sum(Sales_Revenue))

fig <- plot_ly(
  data = df_country,
  x = ~Country,
  y = ~Total_Sales,
  type = "bar",
  color = ~Country
) %>%
  layout(title = "Gesamtverkaufsumsatz nach Land", 
         xaxis = list(title = 'Land'), 
         yaxis = list(title = 'Gesamtumsatz'))

fig


### Falls: die Verteilungen nicht normal sind ###
### und nicht die gleiche Varianz haben, verwenden wir den MannWhitney Test ###
data_germany= filter(df_gf, Country == "Germany")
data_france= filter(df_gf, Country == "France")

wilcox.test(data_germany$Sales_Revenue, data_france$Sales_Revenue, exact = TRUE, correct = TRUE, conf.int = TRUE)

#Testen wenn Diskrete Variable mehrere Klassen hat
### Anova erfordert Normalit?t und Homogenit?t ###
### Hier ist unsere Verteilung normal, sondern homogen ###

onewaytests::nor.test(Sales_Revenue  ~ Country, data= df)

summary(aov(df$Sales_Revenue ~ df$Country))

TukeyHSD(aov(df$Sales_Revenue ~ df$Country)) 



# berechne Correlation Pearson

cor(df[c("Sales_Revenue", "Active_Users", "Production_Costs")])

# berechne Correlation Spearman

cor(df[c("Sales_Revenue", "Active_Users", "Production_Costs")], method="spearman")


# berechne Kontigenzkoeffizient


library(vcd)
# Active_Users kategorisieren (z.B. in "low", "medium", "high")
# Hier kategorisiere ich die Werte willk?rlich als Beispiel
df_gf$Active_Users_cat <- cut(df_gf$Active_Users, 
                             breaks = 3, 
                             labels = c("low", "medium", "high"))


table_data <- table(df_gf$Active_Users_cat, df_gf$Country)

result <- assocstats(table_data)
print(result)
