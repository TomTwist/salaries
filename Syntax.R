# Aufrufen der Pakete
library(readr)
library(mosaic)
library(dplyr)
library(ggplot2)


# Einlesen und Benennen des Datensatzes
salaries <- read_csv("http://vincentarelbundock.github.io/Rdatasets/csv/carData/Salaries.csv")

# Auswahl der benötigten Variablen. DF wird als neuer, reduzierter Datensatz angelegt
DF <- salaries %>%
  select(rank, yrs.service, sex, salary)

# Ausgabe der vorselektierten Variablen
names(DF)

# Statistiken inkl min und max von einzelnen Variblen
favstats(~yrs.service, data = DF)
favstats(~salary, data = DF)

# inspect liefert eine Übersicht über den Datensatz, mit $ über die einzelnen Variablen.
# qplot gibt die Histogramme aus
inspect(DF)

inspect(DF$rank)
DF_rank <- factor(DF$rank , levels=c("AsstProf", "AssocProf", "Prof"))
tally(~rank, DF)
qplot(DF_rank,
      main = "Histogramm für rank", 
      xlab = "Stellenbezeichung",  
      ylab = "Anzahl Befragte",
      fill=I("darkgreen"), 
      col=I("black"), 
      alpha=I(.5)) 


inspect(DF$yrs.service)
qplot(DF$yrs.service,
      binwidth = 1,  
      main = "Histogramm für yrs.service", 
      xlab = "Jahre im Dienst",  
      ylab = "Anzahl Befragte",
      fill=I("darkred"), 
      col=I("black"), 
      alpha=I(.5))

bwplot(DF$yrs.service,
       fill="lightblue")

inspect(DF$sex)
tally(~sex, DF)
qplot(DF$sex,
      main = "Histogramm für sex", 
      xlab = "Geschlecht",  
      ylab = "Anzahl Befragte",
      fill=I("darkblue"), 
      col=I("black"), 
      alpha=I(.5))

inspect(DF$salary)
qplot(DF$salary,
      main = "Histogramm für salary", 
      xlab = "Gehalt bei einem 9-Monatsvertrag",  
      ylab = "Anzahl Befragte",
      fill=I("darkgoldenrod1"), 
      col=I("black"), 
      alpha=I(.5))

# Korrelationen berechnen
cor(yrs.service ~ salary, data = DF)
cor(salary ~ yrs.service, data = DF)

cor(yrs.service ~ salary, data = filter(DF, rank == "Prof"))
cor(yrs.service ~ salary, data = filter(DF, rank == "AssocProf"))
cor(yrs.service ~ salary, data = filter(DF, rank == "AsstProf"))
# Plpot zur Visualisierung des Zusammenhangs
qplot(data = DF,
  main = "Streudiagramm mit Trendgerade zu cor(yrs.service ~ salary, data = DF)",
  x = yrs.service,
  xlab = "Jahre im Dienst",
  y = salary,
  ylab = "Gehalt",
  geom = "point") + 
  geom_smooth(
    method = "lm",
    color ="firebrick1")

# Signifikanztest
H1 <- lm(yrs.service ~ salary, data = DF)
summary(H1)
