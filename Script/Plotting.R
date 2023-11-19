#load all packages needed
library(readxl)
library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(plotly)
library(gganimate)
library(gifski)
library(sf)
library(ggspatial)


#Plot the crime rate in 2019 of the most criminal states and least criminal states.
Crime_2019 <- read.csv(here::here("data_end/Everything_by_dep.csv"))

Crime_2019 <- Crime_2019 |> select(Dep_number, Dep_name, Crime_rate_1k) |>
  filter(Dep_number %in% c("75","93", "13", "69", "92", "12", "15", "48", "32", "50"))

crime_plot <- ggplot(Crime_2019, aes(x= Crime_rate_1k, y = reorder(Dep_name, -Crime_rate_1k))) +
  geom_col(fill = "darkred") + 
  labs(title ="Crime rate for the 5 highest and lowest departments", x= "Crime rate per a thousand people", y= "Department name")

#Plot the evolution of crime in the least criminal states and in the most criminal states
Crime <- read_csv(here::here("data_end/Crime_to_use.csv"))
List_depart <- read_csv(here::here("data_end/List_depart.csv"))

Most <- Crime[-1] |>
  full_join(List_depart, join_by("Dep_number")) |>
  filter(Dep_number %in% c("75","93","13","69","92")) |>
  pivot_longer(as.character(1996:2021), names_to = "Year", values_to = "Tot_crime")

Least <- Crime[-(1)] |> 
  full_join(List_depart, join_by("Dep_number")) |>
  filter(Dep_number %in% c("12","15","48","32","50")) |>
  pivot_longer(as.character(1996:2021), names_to = "Year", values_to = "Tot_crime")

Most_plot <- ggplot(Most, aes(x = as.numeric(Year), y= Tot_crime, color =Dep_name,)) +
  geom_line() +
  labs(title= "Evolution of crime in the most criminal departments", x= "Year", y= "Total Crime")+
  theme_minimal()

Least_plot <- ggplot(Least, aes(x = as.numeric(Year), y= Tot_crime, color =Dep_name,)) +
  geom_line() +
  labs(title= "Evolution of crime in the least criminal departments", x= "Year", y= "Total Crime")+
  theme_minimal()

Most_plot <- ggplotly(Most_plot)
Least_plot <- ggplotly(Least_plot)

#Histogram of the evolution of crimes per year
Type_per_year <- read.csv(here::here("data_end/Type_per_year.csv"))

Five_common <- Type_per_year %>%
  filter(Type %in% c("Destructions et dégradations volontaires",
                     "Vols sans violence contre des personnes",
                     "Coups et blessures volontaires",
                     "Cambriolages de logement",
                     "Usage de stupéfiants")) 

Five_hist <- Five_common %>%
  group_by(Year, Type) %>%
  summarize(Rate_per_1k = mean(Rate_per_1k, na.rm = TRUE))

evol5 <- ggplot(Five_hist) +
  geom_col(aes(x = as.factor(Year), y = Rate_per_1k, fill = Type), position = "dodge") +
  labs(title="Evolution of types of crime by year", y = "Rate per thousand", x = "Years")
evol5 <- ggplotly(evol5)

#Comparison of Paris and a rural departement
Dep75 <- Five_common %>%
  group_by(Departement, Type, Year) %>%
  summarize(Rate_per_1k= mean(Rate_per_1k, na.rm=TRUE)) %>%
  filter(Departement=="75")

Dep18 <- Five_common %>%
  group_by(Departement, Type, Year) %>%
  summarize(Rate_per_1k= mean(Rate_per_1k, na.rm=TRUE)) %>%
  filter(Departement=="18")

plot75 <-  ggplot(Dep75, aes(x=Year, y=Rate_per_1k, color=Type)) + 
  geom_line() + 
  labs(title = "Types of crime by year in the Paris Region")
plot18 <- ggplot(Dep18, aes(x=Year, y=Rate_per_1k, color=Type)) + 
  geom_line() + 
  labs(title = "Types of crime by year in the Cher departement")



#Evolution of unemployement by year
Unemployment_T <- read_csv(here::here("data_end/Unemployment_quarterly.csv"))

Unemployment_fr <- Unemployment_T[-(1)] |> 
  pivot_longer(-(1:2), names_to = "Year", values_to = "Rate") |>
  group_by(Year) |>
  summarize(Rate = mean(Rate))


unemploy_overall <- ggplot(Unemployment_fr, aes(x = Year, y = Rate, group = 1)) + 
  geom_rect(xmin="1990.T3", xmax="1991.T1", ymin=-Inf, ymax = Inf, fill= "gray", alpha = 0.5)+
  geom_text(x="1990.T3", y = 10.2, nudge_x = -0.4, label= "First Gulf War")+
  geom_rect(xmin="2008.T3", xmax="2009.T3", ymin=-Inf, ymax = Inf, fill= "gray", alpha = 0.5)+
  geom_text(x="2007.T3", y = 10.2, label= "2008 crisis")+
  geom_rect(xmin="2020.T1", xmax="2021.T4", ymin=-Inf, ymax = Inf, fill= "gray", alpha = 0.5)+
  geom_text(x="2020.T1", y = 10.1, label= "COVID-19 \n Pandemic")+
  geom_line() +
  labs(title = "Unemployment rate trough the years", y = "Unemployement rate in %") +
  theme(panel.grid = element_blank(), 
        panel.background = element_rect(color = "black", fill = "white")) +
  scale_x_discrete("Years", 
                   breaks = Unemployment_fr$Year[seq(1, nrow(Unemployment_fr), 8)], 
                   labels=seq(1982, 2023, 2))




#Unemployment of every departement in 2022 interactive graph
Unemployment <- read.csv(here::here("data_end/Unemployment_year.csv"))
Unemployment_2019 <- select(Unemployment, Dep_name, 40)
colnames(Unemployment_2019) <- c("Departement", "Rate")
Unemp_2019 <- arrange(Unemployment_2019, desc(Rate))



Bars <- ggplot(Unemp_2019, aes(x = reorder(Departement, Rate), y = Rate, fill = Departement)) +
  geom_col() + 
  theme(axis.text.y=element_blank(), legend.position = "none")+
  labs(title = "Unemployment rate in 2019 by Departement", x = "Rate(%)") +
  theme_minimal() +
  coord_flip()

Bars <- ggplotly(Bars)


#Unemp map
border <- st_read(here::here("data_end/Departement_geoson_carte.geojson"))
colnames(border) <- c("Dep_number", "Dep_name", "geometry")
Everything_by_dep <- read_csv(here::here("data_end/Everything_by_dep.csv"))
both <- left_join(border, Everything_by_dep, join_by("Dep_number"))

plot_map <- ggplot() +
  geom_sf(data = both, aes(fill = Unemp_2019)) + 
  scale_fill_gradient(low = "green", high = "red")+
  theme(axis.text.x  = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x  = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_rect(fill = "aliceblue"),
        panel.grid = element_blank()) +
  labs(title = "Unemployment rate in France")

#map for the crime rate,
crime_map <- ggplot() +
  geom_sf(data = both, aes(fill = Crime_rate_1k)) + 
  scale_fill_gradient2(midpoint = 48,low = "green", high = "red")+
  #if we run a summary function, we find that the mean crime rate, is 48, we put it as our midpoint to have colors show us how far from the mean each dep is.
  theme(axis.text.x  = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x  = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_rect(fill = "aliceblue"),
        panel.grid = element_blank()) +
  labs(title = "Crime rate in France")

#map for lepen
lepen_map <- ggplot() +
  geom_sf(data = both, aes(fill = Lepen_score)) + 
  scale_fill_gradient(low = "white", high = "darkblue")+
  #if we run a summary function, we find that the mean crime rate, is 48, we put it as our midpoint to have colors show us how far from the mean each dep is.
  theme(axis.text.x  = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x  = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_rect(fill = "aliceblue"),
        panel.grid = element_blank()) +
  labs(title = "Score of Marine Lepen")

#Map immigration rate
immig_map <- ggplot() +
  geom_sf(data = both, aes(fill = Immig_rate)) + 
  scale_fill_gradient2(midpoint = 0.08, low = "white", high = "darkolivegreen")+
  theme(axis.text.x  = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x  = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_rect(fill = "aliceblue"),
        panel.grid = element_blank()) +
  labs(title = "Immigration rate")

immig_map

#Map school pass rates
school_map <- ggplot() +
  geom_sf(data = both, aes(fill = Pass_rate)) + 
  scale_fill_gradient2(midpoint = 0.87, low = "red", high = "green")+
  theme(axis.text.x  = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x  = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_rect(fill = "aliceblue"),
        panel.grid = element_blank()) +
  labs(title = "Middle school pass rate")

