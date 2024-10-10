setwd('/Users/jacob/data/dhbw/22_5-bi/24-10-04')
getwd()
print(getwd())


#Vektoren

zahlen <- c(1, 2, 3, 4, 5)
namen <- c('Anna', 'Ben', 'Chris')

#Matrizen

matrix_data <- matrix(1:6, nrow=2, ncol=3)


# Data Frames

daten <- data.frame(Name=c('Anna', 'Ben'), Alter=c(25, 30), Beruf=c('Informatiker', 'Analyst'))
  
# If-Else Bedingung:
Alter <- 25
if (Alter > 30) {
  print('Über 30')
} else {
  print('Unter 30')
37
}

# For-Schleifen:
for (i in 1:5) {
  print(i)
}

# Funktionen
addiere <- function(a, b) {
  print(a + b)
}
ergebnis <- addiere(5, 10)



# Pakete installieren 
#install.packages('dplyr')

#lade notwendige Paketen
library(dplyr)

#lade die Daten

supermarket_sales <- read.csv2('./supermarket_sales.csv',
                               header = TRUE,
                               sep= ',',
                               dec='.')

print('head')
head(supermarket_sales)   # Zeigt die ersten 6 Zeilen

print('str')
str(supermarket_sales)    # Zeigt die Struktur des Datenrahmens

print('summary')
summary(supermarket_sales) # Zusammenfassung der Daten