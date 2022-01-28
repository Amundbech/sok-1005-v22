# Oppgave 1
library(zoo)
library(tidyverse)
library(ggplot2)
library(patchwork)
library(data.table)
low_trop <- fread("https://www.nsstc.uah.edu/data/msu/v6.0/tlt/uahncdc_lt_6.0.txt",) 

low_trop <- head(low_trop, -1)

low_trop$Globe = as.numeric(low_trop$Globe)
low_trop$Year = as.numeric(low_trop$Year)
low_trop$Mo = as.numeric(low_trop$Mo)


low_trop2 = low_trop %>% 
  select(Year:Globe) %>% 
  mutate(Date = paste(Year, Mo, sep = "-")) %>%
  mutate(Date = lubridate::ym(Date)) %>%
  mutate(average_temp = zoo::rollmean(Globe, 13, 
                                      fill = NA, align = "center"))

text_box_label = "UAH Satelite based\nTemperature of the\nGlobal Lower Atmosphere\n(Version 6.0)"

low_trop2 %>% 
  ggplot(aes(x = Date)) +
  geom_hline(yintercept = 0) +  
  geom_point(aes(y = Globe), colour = "blue4", shape = 21) + 
  geom_line(aes(y = Globe), colour = "blue4", alpha = 0.5) +
  geom_line(aes(y = average_temp, group = 1), 
            colour = "red", size = 1)  +
  scale_y_continuous(breaks = seq(from= -0.7,to=0.9, by = 0.1) , 
                     labels = scales::comma) + 
  scale_x_date(date_breaks = "year", date_labels = "%Y",
               expand = c(0,0.1)) + 
  labs(title = "Latest Global Average Tropospheric Temperatures",
       x = NULL,
       y = "Departure from '91-'20 Avg. (deg. C)") +
  theme_bw() +
  annotate(geom="text", x=as.Date("2004-01-01"), y=-0.5, 
           label="Running, centered\n13 month average", 
           colour = "red") + 
  geom_segment(x = as.Date("2004-01-01"), y=-0.45,
               xend = as.Date("2008-01-01"), yend=-0.2,
               arrow = arrow(angle = 20, type = "closed",
                             length = unit(0.15, "inches")),
               colour = "red", size = 1) +
  annotate(geom="text", 
           x=as.Date("1987-01-01"), 
           y = 0.5, hjust = 0.5,
           label = text_box_label,
           colour = "blue4" ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        panel.grid.minor.y = element_blank()) 

# Oppgave 2
low_trop1 <- fread("https://www.nsstc.uah.edu/data/msu/v6.0/tlt/uahncdc_lt_6.0.txt",) 
mid_trop <- fread("https://www.nsstc.uah.edu/data/msu/v6.0/tmt/uahncdc_mt_6.0.txt")
trop <- fread("https://www.nsstc.uah.edu/data/msu/v6.0/ttp/uahncdc_tp_6.0.txt")
low_strat <- fread("https://www.nsstc.uah.edu/data/msu/v6.0/tls/uahncdc_ls_6.0.txt")

colnames(low_trop1) <- c("Year", "Mo", "Globe_lowtrop", "Land1_lowtrop", "Ocean1_lowtrop", "NH_lowtrop",
                         "Land2_lowtrop", "Ocean2_lowtrop", "SH_lowtrop", "Land3_lowtrop", "Ocean3_lowtrop",
                         "Trpcs_lowtrop", "Land4_lowtrop", "Ocean4_lowtrop", "NoExt_lowtrop",  "Land5_lowtrop",
                         "Ocean5_lowtrop", "SoExt_lowtrop",  "Land6_lowtrop", "Ocean6_lowtrop", "NoPol_lowtrop",
                         "Land7_lowtrop", "Ocean7_lowtrop", "SoPol_lowtrop", "Land8_lowtrop",
                         "Ocean8_lowtrop","USA48_lowtrop", "USA49_lowtrop",  "AUST_lowtrop")
colnames(mid_trop) <- c("Year", "Mo", "Globe_midtrop", "Land1_midtrop", "Ocean1_midtrop", "NH_midtrop",
                        "Land2_midtrop", "Ocean2_midtrop", "SH_midtrop", "Land3_midtrop", "Ocean3_midtrop",
                        "Trpcs_midtrop", "Land4_midtrop", "Ocean4_midtrop", "NoExt_midtrop",  "Land5_midtrop",
                        "Ocean5_midtrop", "SoExt_midtrop",  "Land6_midtrop", "Ocean6_midtrop", "NoPol_midtrop",
                        "Land7_midtrop", "Ocean7_midtrop", "SoPol_midtrop", "Land8_midtrop",
                        "Ocean8_midtrop","USA48_midtrop", "USA49_midtrop",  "AUST_midtrop")
colnames(trop) <- c("Year", "Mo", "Globe_trop", "Land1_trop", "Ocean1_trop", "NH_trop",
                    "Land2_trop", "Ocean2_trop", "SH_trop",  "Land3_trop", "Ocean3_trop",
                    "Trpcs_trop", "Land4_trop", "Ocean4_trop", "NoExt_trop",  "Land5_trop",
                    "Ocean5_trop", "SoExt_trop",  "Land6_trop", "Ocean6_trop", "NoPol_trop",
                    "Land7_trop", "Ocean7_trop", "SoPol_trop", "Land8_trop",
                    "Ocean8_trop","USA48_trop", "USA49_trop",  "AUST_trop")
colnames(low_strat) <- c("Year", "Mo", "Globe_lowstrat", "Land1_lowstrat", "Ocean1_lowstrat", "NH_lowstrat",
                         "Land2_lowstrat", "Ocean2_lowstrat", "SH_lowstrat", "Land3_lowstrat", "Ocean3_lowstrat",
                         "Trpcs_lowstrat", "Land4_lowstrat", "Ocean4_lowstrat", "NoExt_lowstrat",  "Land5_lowstrat",
                         "Ocean5_lowstrat", "SoExt_lowstrat",  "Land6_lowstrat", "Ocean6_lowstrat", "NoPol_lowstrat",
                         "Land7_lowstrat", "Ocean7_lowstrat", "SoPol_lowstrat", "Land8_lowstrat", 
                         "Ocean8_lowstrat","USA48_lowstrat", "USA49_lowstrat",  "AUST_lowstrat")
low_strat <- as.data.frame(lapply(low_strat, as.character))

full_liste1 <- full_join(low_trop1, mid_trop)
full_liste2 <- full_join(full_liste1, trop)
full_list <- full_join(full_liste2, low_strat)
full_list <- as.data.frame(lapply(full_list, as.numeric))
full_list <- slice(full_list, 1:(n()-1))

full_list$Date <- as.yearmon(paste(full_list$Year, full_list$Mo), "%Y %m")
full_list$Date <- as.Date(full_list$Date) 

full_list <- full_list %>% 
  mutate(Avg_all = (NoPol_lowtrop+NoPol_midtrop 
                    +NoPol_trop+NoPol_lowstrat)/4)

p1 <- full_list %>% 
  ggplot(aes(x =Year, y = NoPol_lowtrop)) +
  geom_point(size = 0.5) +
  scale_x_continuous(breaks = seq(from= 1979,to=2021, by = 10)) +
  labs(subtitle="Lavere Troposfære", 
       y="Temperatur", 
       x="År", 
       title="") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        panel.grid.minor.y = element_blank()) 

p2 <- full_list %>% 
  ggplot(aes(x = Year, y = NoPol_lowstrat)) +
  geom_point(size = 0.5) +
  scale_x_continuous(breaks = seq(from= 1979,to=2021, by = 10)) +
  labs(subtitle = "Midtre Troposfære",
       y="Temperatur", 
       x="År", 
       title="") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        panel.grid.minor.y = element_blank()) 

p3 <- full_list %>% 
  ggplot(aes(x =Year, y = NoPol_midtrop)) +
  geom_point(size = 0.5) +
  scale_x_continuous(breaks = seq(from= 1979,to=2021, by = 10)) +
  labs(subtitle = "Tropospause",
       y="Temperatur", 
       x="År", 
       title="") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        panel.grid.minor.y = element_blank()) 

p4 <- full_list %>% 
  ggplot(aes(x =Year, y = NoPol_trop)) +
  geom_point(size = 0.5) +
  scale_x_continuous(breaks = seq(from= 1979,to=2021, by = 10)) +
  labs(subtitle = "Lavere stratosfære",
       y="Temperatur", 
       x="År", 
       title="") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        panel.grid.minor.y = element_blank()) 

p5 <- full_list %>% 
  ggplot(aes(x =Year, y = Avg_all)) +
  geom_point(size = 0.5) + 
  scale_x_continuous(breaks = seq(from= 1979,to=2021, by = 10)) +
  labs(subtitle = "Gjennomsnittlig temperatur",
       y="Temperatur", 
       x="År", 
       title="") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        panel.grid.minor.y = element_blank()) 

p1 + p2 + p3 + p4 + p5 +
  plot_layout(ncol = 3, guides = "collect")+
  plot_annotation(title = "Temperatur mellom 60° og 90° nord",
                  subtitle = "")

