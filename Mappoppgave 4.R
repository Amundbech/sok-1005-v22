library(rvest)
library(tidyverse)
library(rlist)
library(purrr)

# Gjort i samarbeid Herman

#Laster inn timeplanen
timer <- paste0("https://timeplan.uit.no/emne_timeplan.php?sem=22v&module%5B%5D=SOK-1005-1&week=1-20&View=list",
                      "https://timeplan.uit.no/emne_timeplan.php?sem=22v&module%5B%5D=SOK-1006-1&week=1-20&View=list",
                      "https://timeplan.uit.no/emne_timeplan.php?sem=22v&module%5B%5D=SOK-1016-1&week=1-20&View=list")



f <- function(timer) { # Lager en funksjon som leses senere 
  page <- read_html(timer)
  table <- html_nodes(page, 'table')  # En tabell per uke
  table <- html_table(table, fill=TRUE) # Tvinger de inn i en liste
  dataset <- list.stack(table) # Gjør listen om til et datasett
  colnames(dataset) <- dataset[1,] # Definerer den første raden som variabelnavn
  dataset <- dataset %>% filter(!Dato=="Dato")
  dataset <- dataset %>% separate(Dato, 
                                into = c("Dag", "Dato"), 
                                sep = "(?<=[A-Za-z])(?=[0-9])") # separerer datasettet inn i to kolonner
  dataset$Dato <- as.Date(dataset$Dato, format="%d.%m.%Y") # Koder inn hvilke datoformat som skal brukes
  dataset$Uke <- strftime(dataset$Dato, format = "%V") # Lager en ukesvariebl
  dataset <- dataset %>% select(Dag,Dato,Uke,Tid,Rom,Emnekode,Beskrivelse,Lærer) # Velger hvilke variabler som skal vises
  return(dataset)
}

#Gjør først om til en liste
timetable <- map(timer, f)

#Gjør listen om til et leselig datasett
timetable <- bind_rows(timetable)

# jør dag kolonnen til karakter
timetable$Dag <- as.character(timetable$Dag)

#Putter inn NA i blanke kolonner på Dag raden
timetable$Dag[timetable$Dag ==""] <- NA

# Legger inn manglende data
timetable <- timetable %>% fill(c(Dag,Dato,Uke))


