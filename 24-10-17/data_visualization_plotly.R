setwd("/Users/jacob/data/dhbw/22_5-bi/24-10-17")
# Lade notwendige Pakete
library(dplyr)
library(stringr)
library(plotly)
library(reshape2)

# Lade die Daten
# 
df <- read.csv("sales_gesamtdf.csv", header = TRUE, sep = ",", dec = ".")

# Der Datensatz enth?lt 6 Spalten mit mehreren Eintr?gen.
# 4 Spalten mit kontinuierlichen Werten und 2 Spalten mit diskreten Werten.

# ?berpr?fe die Struktur der Daten
str(df)

# Siehe Informationen f?r die Daten
summary(df)

# ### Berechnung der Gesamtgewinn
df$total_profit <- df$Sales_Revenue - df$Production_Costs

# ### Darstellung der Korrelation von Spalten
mydata <- subset(df, select = c('Sales_Revenue', 'Production_Costs', 'Active_Users', 'Opened_Complaints'))

cormat <- cor(mydata)
head(cormat)
# -> es gibt seht starke (lineare) Zusammenhänge!

melted_cormat <- melt(cormat)
head(melted_cormat)

# Visualisierung der Korrelationsmatrix
ggplot(data = melted_cormat, aes(x = Var1, y = Var2, fill = value)) + 
  geom_tile() + 
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", limit = c(-1, 1), 
                       name = "Korrelation") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1))

# ### Visualisierung der Daten

# ## Balkendiagramm: Vergleich der Verkaufsums?tze nach Land
df_country <- df %>%
  group_by(Country) %>%
  summarise(Total_Sales = sum(Sales_Revenue))

fig <- plot_ly(
  data = df_country,
  x = ~Country,
  y = ~Total_Sales,
  type = "bar",
  marker = list(color = 'rgba(222,45,38,0.8)', 
                line = list(color = 'rgba(8,48,107,1.0)', width = 2))
) %>%
  layout(title = "Gesamtverkaufsumsatz nach Land", 
         xaxis = list(title = 'Land'), 
         yaxis = list(title = 'Gesamtumsatz'))

fig

# ## Kuchendiagramm: Anteil der Verkaufsums?tze nach Land
fig <- plot_ly(
  data = df_country,
  labels = ~Country,
  values = ~Total_Sales,
  type = "pie"
) %>%
  layout(title = "Anteil der Verkaufsums?tze nach Land")

fig

# ## Histogramm: Verteilung der Verkaufsums?tze
fig <- plot_ly(
  x = df$Sales_Revenue, type = "histogram"
) %>%
  layout(title = "Verteilung der Verkaufsums?tze", 
         xaxis = list(title = 'Verkaufsumsatz'), 
         yaxis = list(title = 'H?ufigkeit'))

fig

# ## Boxplot: Produktionskosten
fig <- plot_ly(y = df$Production_Costs, type = "box") %>%
  layout(title = "Boxplot der Produktionskosten", 
         yaxis = list(title = 'Produktionskosten'))

fig

# ## Punktdiagramm: Beziehung zwischen Verkaufsumsatz und aktiven Nutzern
fig <- plot_ly(
  data = df, 
  x = ~Active_Users, 
  y = ~Sales_Revenue,
  size = ~Production_Costs,
  type = 'scatter',
  mode = 'markers'
) %>%
  layout(title = "Beziehung zwischen aktiven Nutzern und Verkaufsumsatz", 
         xaxis = list(title = 'Aktive Nutzer'), 
         yaxis = list(title = 'Verkaufsumsatz'))

fig

# ## Blasendiagramm: Verkaufsumsatz und Produktionskosten
fig <- plot_ly(
  data = df, 
  x = ~Sales_Revenue, 
  y = ~Production_Costs, 
  size = ~Active_Users, 
  color = ~Country,
  type = 'scatter',
  mode = 'markers'
) %>%
  layout(title = "Verkaufsumsatz vs. Produktionskosten (mit aktiven Nutzern)", 
         xaxis = list(title = 'Verkaufsumsatz'), 
         yaxis = list(title = 'Produktionskosten'))

fig

# ### Beziehungen zwischen Variablen und Korrelation
axis = list(showline = FALSE,
            zeroline = FALSE,
            gridcolor = '#ffff',
            ticklen = 4)

fig <- df %>%
  plot_ly() 

fig <- fig %>%
  add_trace(
    type = 'splom',
    dimensions = list(
      list(label = 'Verkaufsumsatz', values = ~Sales_Revenue),
      list(label = 'Produktionskosten', values = ~Production_Costs),
      list(label = 'Aktive Nutzer', values = ~Active_Users),
      list(label = '?ffnete Beschwerden', values = ~Opened_Complaints)
    )
  ) 

fig <- fig %>%
  layout(title = 'Korrelation zwischen Variablen')

fig

#####Interaktive Grafiken

#library(esquisse)

#esquisse::esquisser()

#ggplotly() Transformation ggplot zu plotly

