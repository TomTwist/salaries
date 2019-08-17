# Aufrufen der Pakete
library(readr)
library(mosaic)
library(dplyr)

# Einlesen und Benennen des Datensatzes
salaries <- read_csv("http://vincentarelbundock.github.io/Rdatasets/csv/carData/Salaries.csv")

# Auswahl der benötigten Variablen. DF wird als neuer, reduzierter Datensatz angelegt
DF <- salaries %>%
  select(rank, yrs.service, sex, salary)

# Ausgabe der vorselektierten Variablen
names(DF)
