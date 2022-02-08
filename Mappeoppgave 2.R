library(data.table)
library(rjson)
library(tidyverse)
library(ggrepel)
library(ggeasy)

# Oppgavene gjort i sammarbeid med Herman Kvale og Knut Bakken

# Oppgave 1 

#Laster inn json filen
data <- fromJSON(file = "https://static01.nyt.com/newsgraphics/2021/12/20/us-coronavirus-deaths-2021/ff0adde21623e111d8ce103fedecf7ffc7906264/scatter.json")


#Gjør det om til et datasett slik at det blir leselig
covidnumbers <- do.call(rbind, data) %>% 
  as.data.frame


# Gjør om til numerisk fra karakter
covidnumbers$fully_vaccinated_pct_of_pop = as.numeric(covidnumbers$fully_vaccinated_pct_of_pop)
covidnumbers$deaths_per_100k = as.numeric(covidnumbers$deaths_per_100k)
covidnumbers$name = as.character(covidnumbers$name)


#Forkorter navnene 
covidnumbers$name <- state.abb[match(covidnumbers$name, state.name)]


# Lager plottet 
plot1 <- covidnumbers %>%
  ggplot(aes(x = fully_vaccinated_pct_of_pop, y = deaths_per_100k, label=name), col = "green", ) +
  geom_point() +
  labs("Covid-19 deaths since universal adult vaccine eligibility compared with \n vaccination rates",
       x= "Prosen fullvaksinert",
       y= "Dødsrate") +
  scale_x_continuous(labels=scales::percent, 
                     breaks = seq(from = .45, to = .81, by = 0.05)) +
  geom_label_repel(label.size = 0,
                   label.padding = 0,
                   label.r = 0,
                   )  +
  theme_gray() +
  annotate(geom="text", x=0.54, y=17, 
           label=" 🢆Lower Vaccination rate,\n13 higher death rate") +
  annotate(geom="text", x=0.78, y=8, 
           label="🢆 Higer Vaccination rate,\n13 Lower death rate")
plot1
  
# Oppgave 2
# Lager lm funksjonen
lm(deaths_per_100k ~ fully_vaccinated_pct_of_pop, data = covidnumbers)

# Lager plot nummer 2 
plot1 + geom_smooth(method = lm, se = FALSE)

# I plottet ser vi 

# Tolker dette plottet slik at det er cirka 63% av befolkningen som er fullvaksinert 
# Og da -36.66 som ikke er vaksinert. Så dør det 31.15 personer per 100k innbyggere 