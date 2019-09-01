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
DF$rank <- factor(DF$rank , levels=c("AsstProf", "AssocProf", "Prof"))
tally(~rank, DF)
qplot(DF$rank,
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

# bisher nicht berücksichtigt in Hausarbeit
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

# Korrelation berechnen
cor(yrs.service ~ salary, data = DF)

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



# Mittelwertvergleiche M und W im Gehalt
DF_Fem <- filter(DF, sex =="Female")
DF_Mal <- filter(DF, sex =="Male")
mean(DF_Fem$salary)
mean(DF_Mal$salary)
mean(DF_Mal$salary) - mean(DF_Fem$salary)
(mean(DF_Mal$salary) / mean(DF_Fem$salary))

qplot(x = sex, y = salary,
      data = DF,
      geom = "boxplot",
      main = "Boxplot Gehalt und Geschlecht",
      xlab = "Geschlecht",
      ylab = "Gehalt",
      alpha=I(.5),
      colour=sex)

#Sal_SD_Fem <- sd(DF_Fem$salary)
#Sal_SD_Mal <- sd(DF_Mal$salary)
#Sal_Mean_Fem <- mean(DF_Fem$salary)
#Sal_Mean_Mal <- mean(DF_Mal$salary)
mes(Sal_Mean_Fem, Sal_Mean_Mal, Sal_SD_Fem, Sal_SD_Mal, 39, 358)

qplot(x = rank, y = salary,
      data = DF,
      main = "Boxplot Gehalt und Geschlecht nach Stellenbezeichnung",
      xlab = "Stellenbezeichnung",
      ylab = "Gehalt",
      facets = ~sex,
      geom = "boxplot",
      alpha=I(.5),
      colour=sex)

mean(filter(DF_Fem, rank == "AsstProf")$salary)
mean(filter(DF_Mal, rank == "AsstProf")$salary)
mean(filter(DF_Mal, rank == "AsstProf")$salary) / mean(filter(DF_Fem, rank == "AsstProf")$salary)

mean(filter(DF_Fem, rank == "AssocProf")$salary)
mean(filter(DF_Mal, rank == "AssocProf")$salary)
mean(filter(DF_Mal, rank == "AssocProf")$salary) / mean(filter(DF_Fem, rank == "AssocProf")$salary)

mean(filter(DF_Fem, rank == "Prof")$salary)
mean(filter(DF_Mal, rank == "Prof")$salary)
mean(filter(DF_Mal, rank == "Prof")$salary) / mean(filter(DF_Fem, rank == "Prof")$salary)
