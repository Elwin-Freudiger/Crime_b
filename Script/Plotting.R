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
library(leaflet)
library(leaflet.extras)
library(treemap)
library(scales)

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
  geom_col(aes(x = as.factor(Year+2000), y = Rate_per_1k, fill = Type), position = "dodge") +
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


#Immigration total by state
Immigration_2019 <- read_excel(here::here("Raw_data/Immigration_2019.xlsx"), 
                               sheet = "Pop0_R", skip = 2)
Immig_tree <- Immigration_2019[1:13, 1:2] |>
  rename(Reg_name = `...1`)

Tree <- treemap(Immig_tree, 
                index = "Reg_name", 
                vSize = "Immigrés", 
                type = "index", 
                width = 800, 
                height = 400, 
                title = "Repartition of total of Immigrants by Region")


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


#Create a dataframe whith a geometry column, the geometry is department borders
border <- st_read(here::here("data_end/Departement_geoson_carte.geojson"))
colnames(border) <- c("Dep_number", "Dep_name", "geometry")
Everything_by_dep <- read_csv(here::here("data_end/Everything_by_dep.csv"))
both <- left_join(border, Everything_by_dep, join_by("Dep_number"))


#Unemp map
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

#plot the map with leaflet
#By department
#We first divide the values in 10 deciles. This means that our data is divided into 10% of the data ordered
both_category <- both |> select(Dep_number, Dep_name.x, Unemp_2019, 
                                Crime_rate_1k, Pass_rate, Lepen_score, 
                                Immig_rate, Density_2019, geometry) |>
  rename(Dep_name = Dep_name.x) |>
  mutate(Crime_rate_1k = ifelse(Crime_rate_1k >= 100, NA, Crime_rate_1k),
         Density_2019 = ifelse(Density_2019 > 1000, NA, Density_2019))

pal <- colorNumeric("YlOrRd", na.color = "darkgray", NULL)

leaf_map <- leaflet(both_category) %>%
  addTiles() %>%
  addPolygons(group = "Unemployment", stroke = TRUE, smoothFactor = 0.3, fillOpacity = 0.6,
              fillColor = ~pal(rescale(Unemp_2019)),
              color = "white",
              weight = 0.3,
              label = ~paste0(Dep_name, ": ", Unemp_2019)) %>% 
  addPolygons(group = "School fail rate", stroke = TRUE, smoothFactor = 0.3, fillOpacity = 0.6,
              fillColor = ~pal(rescale(1-Pass_rate)),
              color = "white",
              weight = 0.3,
              label = ~paste0(Dep_name, ": ", (1-Pass_rate))) %>% 
  addPolygons(group = "Lepen Score", stroke = TRUE, smoothFactor = 0.3, fillOpacity = 0.6,
              fillColor = ~pal(rescale(Lepen_score)),
              color = "white",
              weight = 0.3,
              label = ~paste0(Dep_name, ": ", Lepen_score)) %>% 
  addPolygons(group = "Immigration", stroke = TRUE, smoothFactor = 0.3, fillOpacity = 0.6,
              fillColor = ~pal(rescale(Immig_rate)),
              color = "white",
              weight = 0.3,
              label = ~paste0(Dep_name, ": ", Immig_rate)) %>% 
  addPolygons(group = "Density", stroke = TRUE, smoothFactor = 0.3, fillOpacity = 0.6,
              fillColor = ~pal(rescale(Density_2019)),
              color = "white",
              weight = 0.3,
              label = ~paste0(Dep_name, ": ", Density_2019)) %>% 
  addPolygons(group = "Crime rate", stroke = TRUE, smoothFactor = 0.3, fillOpacity = 0.6,
              fillColor = ~pal(rescale(Crime_rate_1k)),
              color = "white",
              weight = 0.3,
              label = ~paste0(Dep_name, ": ", Crime_rate_1k)) %>% 
  addLegend(
    position = "bottomright",
    pal = pal,
    values = ~rescale(Crime_rate_1k),  # Values are set to the Unemp_2019 column for this legend
    title = "Color scale") |>
  addLayersControl(baseGroups = c("Crime rate", "Unemployment", "School fail rate", "Lepen score", "Immigration", "Density"),
                   options = layersControlOptions(collapsed = FALSE))


#Now we will create a leaflet map at the Town level.
#We load a dataset that gives us the town center, in latitude and longitude
cities <- read_csv(here::here("Raw_data/cities_hope.csv"))
Center_city <- cities |> 
  select(insee_code, latitude, longitude) |>
  rename(Town_code = insee_code, 
         Latitude = latitude,
         Longitude = longitude)

#Load the data by town
Everything_by_town <- read_csv("data_end/Everything_by_town.csv")
#extract location, join it with our previous dataset.
#omit NA values, and duplicates, we also filter any non metropolitan values
both_town_center <- left_join(Center_city, Everything_by_town, join_by("Town_code")) |>
  na.omit() |>
  filter(!grepl("^97", Town_code)) |>
  distinct()

#Create the map for each town
circle_map <- leaflet() %>%
  addTiles()

#list of department number and names
grouped <- unique(both_town_center$Dep_name)
list <- unique(both_town_center$Dep_number)
#add circles for departments using a for loop. and create a group by department
for (i in 1:length(grouped)) {
  # Filter data for the  department needed
  data <- filter(both_town_center, Dep_number == list[i])
  
  # Add circles for the  department
  circle_map <- circle_map %>%
    addCircles(
      group = as.character(grouped[i]),
      data = data,
      lng = ~Longitude,
      lat = ~Latitude,
      weight = 1,
      fillColor = "red",
      stroke = TRUE,
      color = "red",
      fillOpacity = 0.2,
      radius = ~(Rate_per_1k)*1000, #chose the circles radius
      popup = ~paste(Town_name, "<br/> <b>", round(data$Rate_per_1k/1000, 3), " </b>Crimes per capita")
    )
}

# Layer control to have the opportunity to show each layer/department
circle_map <- circle_map %>%
  addLayersControl(
    overlayGroups = as.character(grouped),  # Convert department numbers to character
    options = layersControlOptions(collapsed = TRUE)) |>
  hideGroup(grouped) #start with every layer hidden by default


#Data distribution and Plotting by town
#Load the cleaned dataset, with only towns above 1700 inhabitants
Crime_per_type_town <- read_csv("data_end/Crime_per_type_town.csv")
#Total crime rate
Density_crime_rate <- ggplot(Everything_by_town, aes(x = Rate_per_1k)) +
  geom_density(fill = "skyblue", color = "black") +
  labs(title = "Distribution of the crime Rate", x = "Crime rate per thousand people")

#Total crime
Density_crime_total <- ggplot(Everything_by_town, aes(x = Total_crime)) +
  geom_density(fill = "skyblue", color = "black") +
  labs(title = "Distribution of the total crime", x = "Total number of crimes")

#Total population
Density_population <- ggplot(Everything_by_town, aes(x = Total_pop)) +
  geom_density(fill = "skyblue", color = "black") +
  labs(title = "Distribution of the population", x = "Number of inhabitants per town")

#Density
Density_density <- ggplot(Everything_by_town, aes(x = Density_2019)) +
  geom_density(fill = "skyblue", color = "black") +
  labs(title = "Distribution of the Density", x = "Number of inhabitants per square kilometers")

#Score of Lepen
Density_Lepen <- ggplot(Everything_by_town, aes(x = Lepen)) +
  geom_density(fill = "skyblue", color = "black") +
  labs(title = "Distribution of Marine Lepen's Score", x = "Score of Marine Lepen in %")

#Poverty
Density_poverty <- ggplot(Everything_by_town, aes(x = Povrety_2019)) +
  geom_density(fill = "skyblue", color = "black") +
  labs(title = "Distribution of Poverty levels", x = "Share of people below the poverty line in %")

#Intensity Poverty
Density_intensity_poverty <- ggplot(Everything_by_town, aes(x = Intensity_povrety)) +
  geom_density(fill = "skyblue", color = "black") +
  labs(title = "Distribution of Intensity of Poverty", x = "Difference between Median earning level of poor people and Poverty line")

#No diploma
Density_no_diploma <- ggplot(Everything_by_town, aes(x = No_diploma_rate1k)) +
  geom_density(fill = "skyblue", color = "black") +
  labs(title = "Distribution of people without a diploma", x = "Number of people without a diploma per thousand people")

#Immigration rate
Density_immig <- ggplot(Everything_by_town, aes(x = Immig_rate)) +
  geom_density(fill = "skyblue", color = "black") +
  labs(title = "Distribution of Immigration rate", x = "Number of immigrants per thousand people")

#Unemployment
Density_unemp <- ggplot(Everything_by_town, aes(x = Unemp_2019/100)) +
  geom_density(fill = "skyblue", color = "black") +
  labs(title = "Distribution of Unemployment numbers", x = "Share of people unemployed in %")