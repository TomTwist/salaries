# Aufrufen der Pakete
library(readr)
library(mosaic)
library(dplyr)

# Einlesen und Benennen des Datensatzes
salaries <- read_csv("http://vincentarelbundock.github.io/Rdatasets/csv/carData/Salaries.csv")

DF <- salaries %>%
  select(rank, yrs.service, sex, salary)