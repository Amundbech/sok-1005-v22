library(tidyverse)
library(rvest)
# Gjort i samarbeid med Herman og Knut

#Skraper tabellen fra nett
cars <- read_html("https://www.motor.no/aktuelt/motors-store-vintertest-av-rekkevidde-pa-elbiler/217132")

tables <- cars %>% html_table(fill=TRUE)

#laster inn tabellen 
cartable <-  tables[[1]]

#fjerner øvereste rad
cartable <- cartable[-1,]

#Endrer navn slik at det korresnponderer slik det gjør på nett
colnames(cartable) <- c("Modell (temp. varierte fra 0° til -10°)", "WLTP-tall", "STOPP", "Avvik")

#Fjerner observasjoner med x
cartable <- cartable %>%
  slice(1:18, 20:25, 27:33)


#Rydder i tabbel slik at det kan plottes, da det ikke vil plottes der det er km og kWh 
cartable <- cartable %>% 
  separate(`WLTP-tall`, sep = "/", into=c("wltp","kWh"))

cartable$STOPP <- gsub("km","", as.character(cartable$STOPP))

cartable$wltp <- gsub("km","", as.character(cartable$wltp))

# Gjør tallene om til numeric slik at de kan plottes
cartable$wltp = as.numeric(cartable$wltp)

cartable$STOPP = as.numeric(cartable$STOPP)

# Lager plottet 
plot1 <- cartable %>% 
  ggplot() +
  geom_point(aes(x = wltp, y = STOPP)) +
  scale_x_continuous(limits = c(200,600)) +
  scale_y_continuous(limits = c(200,600)) +
  geom_abline(col="red") +
  labs(x = "Rekkevidde",
       y = "Faktiske rekkevidde") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        panel.grid.minor.y = element_blank())
plot1

#Oppgave 2

# Lager datasett med lm fulnsjon
df <- lm(STOPP ~ wltp, data = cartable)

summary(df)

#plotter lm funksjonen 
plot1 + geom_smooth(method = lm, aes(x = wltp, y = STOPP))

# Intercept som er -26.64 forteller oss at den reelle rekkevidden er lavere enn de 
#hadde reklamert for. wltp blir 0.867 er da dette den relle rekkevidden for hver kjørte km

