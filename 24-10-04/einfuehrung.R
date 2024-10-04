setwd('C:/Users/Dozen/Documents/BI_Vorlesung_Loerrach')
#getwd()


#Vektoren

zahlen <- c(1, 2, 3, 4, 5)
namen <- c("Anna", "Ben", "Chris")

#Matrizen

matrix_data <- matrix(1:6, nrow=2, ncol=3)


# Data Frames

daten <- data.frame(Name=c("Anna", "Ben"), Alter=c(25, 30), Beruf=c("Informatiker", "Analyst"))

# If-Else Bedingung:

if (Alter > 30) {
  print("Über 30")
} else {
  print("Unter 30")
}



# For-Schleifen:

for (i in 1:5) {
  print(i)
}


# Funktionen

addiere <- function(a, b) {
  return(a + b)
}
ergebnis <- addiere(5, 10)


# Pakete installieren 

install.packages("dplyr")


#lade notwendige Paketen
library(dplyr)



#lade die Daten

supermarket_sales <- read.csv2("supermarket_sales.csv", header = TRUE, sep= ",", dec=".")


head(supermarket_sales)   # Zeigt die ersten 6 Zeilen
str(supermarket_sales)    # Zeigt die Struktur des Datenrahmens
summary(supermarket_sales) # Zusammenfassung der Daten




