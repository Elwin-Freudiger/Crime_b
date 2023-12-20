###########################
###########################
#This script is our plot file
###########################
###########################

# all packages are loaded in the setup
source(here::here("Script/setup.R"))

#Plot the crime rate in 2019 of the most criminal states and least criminal states.
Crime_2019 <- read.csv(here::here("data_end/Everything_by_dep.csv"))

Crime_2019 <- Crime_2019 |> select(Dep_number, Dep_name, Crime_rate_1k) |>
  filter(Dep_number %in% c("75","93", "13", "69", "92", "12", "15", "48", "32", "50")) #select the 5 most and least criminal departments

crime_plot <- ggplot(Crime_2019, aes(x= Crime_rate_1k, y = reorder(Dep_name, -Crime_rate_1k))) +
  geom_col(fill = "darkred") + #plot it as a barplot
  labs(title ="Crime rate for the 5 highest and lowest departments", x= "Crime rate per a thousand people", y= "Department name")
#################################################################################
#Plot the evolution of crime in the least criminal states and in the most criminal states
Crime <- read_csv(here::here("data_end/Crime_to_use.csv"))
List_depart <- read_csv(here::here("data_end/List_depart.csv"))

#create a dataframe showing the evolution of crime in the most criminal departments
Most <- Crime[-1] |>
  full_join(List_depart, join_by("Dep_number")) |>
  filter(Dep_number %in% c("75","93","13","69","92")) |>
  pivot_longer(as.character(1996:2021), names_to = "Year", values_to = "Tot_crime")

#create a dataframe showing the evolution of crime in the least criminal departments
Least <- Crime[-(1)] |> 
  full_join(List_depart, join_by("Dep_number")) |>
  filter(Dep_number %in% c("12","15","48","32","50")) |>
  pivot_longer(as.character(1996:2021), names_to = "Year", values_to = "Tot_crime")

#plot the dataframe
Most_plot <- ggplot(Most, aes(x = as.numeric(Year), y= Tot_crime, color =Dep_name,)) +
  geom_line() +
  labs(title= "Evolution of crime in the most criminal departments", x= "Year", y= "Total Crime")+
  theme_minimal()

#plot the dataframe
Least_plot <- ggplot(Least, aes(x = as.numeric(Year), y= Tot_crime, color =Dep_name,)) +
  geom_line() +
  labs(title= "Evolution of crime in the least criminal departments", x= "Year", y= "Total Crime")+
  theme_minimal()

#make these plots interactive
Most_plot <- ggplotly(Most_plot) 
Least_plot <- ggplotly(Least_plot)
#################################################################################
#Histogram of the evolution of crimes per year
Type_per_year <- read.csv(here::here("data_end/Type_per_year.csv"))

#only select the five most common types of crime
Five_common <- Type_per_year %>% 
  filter(Type %in% c("Destructions et dégradations volontaires",
                     "Vols sans violence contre des personnes",
                     "Coups et blessures volontaires",
                     "Cambriolages de logement",
                     "Usage de stupéfiants"))

#create a vector, to translate the crime names to english
English <- c("Cambriolages de logement" = "Burglaries",
                      "Coups et blessures volontaires" = "Assault",
                      "Destructions et dégradations volontaires" = "Intentional damage",
                      "Usage de stupéfiants" = "Drug use",
                      "Vols sans violence contre des personnes" = "Theft without violence")
#compute the mean rate of these types of crime for France overall
Five_hist <- Five_common %>%
  group_by(Year, Type) %>%
  summarize(Rate_per_1k = mean(Rate_per_1k, na.rm = TRUE))
#translate crime types to english
Five_hist$Type <- English[Five_hist$Type]
#plot the evolution of each type of crime by year
evol5 <- ggplot(Five_hist) +
  geom_col(aes(x = as.factor(Year+2000), y = Rate_per_1k, fill = reorder(Type, Rate_per_1k)), position = "dodge") +
  labs(title="Evolution of types of crime by year", y = "Rate per thousand", x = "Years")
#make the plot interactive
evol5 <- ggplotly(evol5)|>
  layout(legend = list(title = list(text = "Type of Crime")))
#################################################################################
# Comparison of Paris and a rural department, the cher

Compare_dep <- Five_common %>% 
  group_by(Departement, Type, Year) %>%
  summarize(Rate_per_1k= mean(Rate_per_1k, na.rm=TRUE)) %>% #compute the mean rate
  filter(Departement %in% c("75", "18")) #select our2 departments
Compare_dep$Type <- English[Compare_dep$Type] #translate crime types to english

#plot it. 
Compare_plot <-  ggplot(Compare_dep, aes(x=(Year+2000), y=Rate_per_1k, color=Type)) + 
  geom_line() + #draw lines for the evolution of each crimes
  labs(title = "Types of crime by year in the Cher(18) and in Paris(75)", x = "Year", y = "Crime rate per thousand inhabitants") +
  theme(legend.position = "bottom") +
  facet_wrap(~Departement, ncol = 2) #separate each department using a facet wrap
#################################################################################
#Evolution of unemployment by year
Unemployment_T <- read_csv(here::here("data_end/Unemployment_quarterly.csv"))
#compute the quarterly unempoyment rate for France overall.
Unemployment_fr <- Unemployment_T[-(1)] |> 
  pivot_longer(-(1:2), names_to = "Year", values_to = "Rate") |>
  group_by(Year) |>
  summarize(Rate = mean(Rate))


unemploy_overall <- ggplot(Unemployment_fr, aes(x = Year, y = Rate, group = 1)) +
  geom_rect(xmin="1990.T3", xmax="1991.T1", ymin=-Inf, ymax = Inf, fill= "gray", alpha = 0.5)+ #create a rectangle representing the Gulf War
  geom_text(x="1990.T3", y = 10.2, nudge_x = -0.4, label= "First Gulf War")+ #give it a name
  geom_rect(xmin="2008.T3", xmax="2009.T3", ymin=-Inf, ymax = Inf, fill= "gray", alpha = 0.5)+ #this rectangle represents the 2008 recession
  geom_text(x="2007.T3", y = 10.2, label= "2008 crisis")+
  geom_rect(xmin="2020.T1", xmax="2021.T4", ymin=-Inf, ymax = Inf, fill= "gray", alpha = 0.5)+ #this rectangle plots the COVID-19 pandemic
  geom_text(x="2020.T1", y = 10.1, label= "COVID-19 \n Pandemic")+
  geom_line() +  #plot unemployment as a line
  labs(title = "Unemployment rate trough the years", y = "Unemployement rate in %") +
  theme(panel.grid = element_blank(), 
        panel.background = element_rect(color = "black", fill = "white")) +
  scale_x_discrete("Years", 
                   breaks = Unemployment_fr$Year[seq(1, nrow(Unemployment_fr), 8)], 
                   labels=seq(1982, 2023, 2)) #change the scale to only show a lower number of values, done to avoid clutter
#################################################################################
#Unemployment of every departement in 2022 interactive graph
Unemployment <- read.csv(here::here("data_end/Unemployment_year.csv"))
Unemployment_2019 <- select(Unemployment, Dep_name, 40) #only select the year 2019
colnames(Unemployment_2019) <- c("Departement", "Rate")
Unemp_2019 <- arrange(Unemployment_2019, desc(Rate)) #arrange the values in decreasing order

#plot unemployement as a bar a reorder them by rate
Bars <- ggplot(Unemp_2019, aes(x = reorder(Departement, Rate), y = Rate, fill = Departement)) +
  geom_col() +
  labs(title = "Unemployment rate in 2019 by Department", y = "Rate(%)", x = "Department name", color = "Department") +
  theme_minimal() +
  coord_flip() #make the plot horizontal

#make the plot interactive
Bars <- ggplotly(Bars) |>
  layout(showlegend = FALSE) 

#################################################################################
#load a dataset representing the number of immigrants by region in 2019
Immigration_2019 <- read_excel(here::here("Raw_data/Immigration_2019.xlsx"), 
                               sheet = "Pop0_R", skip = 2)
#only select 13 metropolitan regions, and only the total number of immigrants
Immig_tree <- Immigration_2019[1:13, 1:2] |>
  rename(Reg_name = `...1`) 

#creation of the treemap. it will then be saved as an image, done using the "treemap" package
treemap(Immig_tree, 
        index = "Reg_name", 
        vSize = "Immigrés", 
        type = "index", 
        width = 800, 
        height = 400, 
        title = "Repartition of total of Immigrants by Region")
#################################################################################
#Create a dataframe whith a geometry column, the geometry is department borders
border <- st_read(here::here("data_end/Departement_geoson_carte.geojson")) #read a geojson file
colnames(border) <- c("Dep_number", "Dep_name", "geometry")
Everything_by_dep <- read_csv(here::here("data_end/Everything_by_dep.csv"))
both <- left_join(border, Everything_by_dep, join_by("Dep_number")) #add the geometry to our final dataframe


#Unemployment map
#the process is the same for every non interactive map, it will only be explained once
plot_map <- ggplot() +
  geom_sf(data = both, aes(fill = Unemp_2019)) + #plot the borders, fill the using our variable
  scale_fill_gradient(low = "green", high = "red")+ #chose fill colors depending on the variable
  theme(axis.text.x  = element_blank(), #remove the axis, gridlines, ticks and set the background to lightblue
        axis.text.y = element_blank(),
        axis.ticks.x  = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_rect(fill = "aliceblue"),
        panel.grid = element_blank()) +
  labs(title = "Unemployment rate in France", fill = "Unemployment rate (%)")

#map for the crime rate
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
  labs(title = "Crime rate in France", fill = "Crime rate (Crime/thousand people)")

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
  labs(title = "Score of Marine Le Pen", fill = "Marine Le Pen score (%)")

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
  labs(title = "Immigration rate", fill = "Immigration rate (Immigrants per capita)")

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
  labs(title = "Middle school pass rate", fill = "Exam pass rate (%)")

#Map density
density_map <- ggplot() +
  geom_sf(data = both, aes(fill = Density_2019)) + 
  scale_fill_gradient2(low = "green", high = "red")+
  theme(axis.text.x  = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x  = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_rect(fill = "aliceblue"),
        panel.grid = element_blank()) +
  labs(title = "Density by department", fill = "Density (People/square km)")
#################################################################################
#plot an interactive map with leaflet
#By department
both_category <- both |> select(Dep_number, Dep_name.x, Unemp_2019, 
                                Crime_rate_1k, Pass_rate, Lepen_score, 
                                Immig_rate, Density_2019, geometry) |> #only select some columns from the dataset
  rename(Dep_name = Dep_name.x) |>
  mutate(Crime_rate_1k = ifelse(Crime_rate_1k >= 100, NA, Crime_rate_1k), #set a limit for our values, set them as NA if they are over a threshold
         Density_2019 = ifelse(Density_2019 > 5000, NA, Density_2019)) #this is done so that the difference can be seen between the remaining values

pal <- colorNumeric("YlOrRd", na.color = "black", NULL) #set the palette

leaf_map <- leaflet(both_category) %>%
  addTiles() %>% #initialize the map
  #for each variable, add the border polygons, fill it with values of the variable and add a popup label, also assign it into a group
  addPolygons(group = "Lepen", stroke = TRUE, smoothFactor = 0.3, fillOpacity = 0.6,
              fillColor = ~pal(rescale(Lepen_score)),
              color = "white",
              weight = 0.3,
              label = ~paste0(Dep_name, ": ", Lepen_score)) %>% 
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
  addPolygons(group = "Immigration", stroke = TRUE, smoothFactor = 0.3, fillOpacity = 0.6,
              fillColor = ~pal(rescale(Immig_rate)),
              color = "white",
              weight = 0.3,
              label = ~paste0(Dep_name, ": ", Immig_rate)) %>% 
  addPolygons(group = "Density", stroke = TRUE, smoothFactor = 0.3, fillOpacity = 0.6,
              fillColor = ~pal(rescale(Density_2019)),
              color = "white",
              weight = 0.3,
              label = ~paste0(Dep_name, ": ", both$Density_2019)) %>% 
  addPolygons(group = "Crime rate", stroke = TRUE, smoothFactor = 0.3, fillOpacity = 0.6,
              fillColor = ~pal(rescale(Crime_rate_1k)),
              color = "white",
              weight = 0.3,
              label = ~paste0(Dep_name, ": ", both$Crime_rate_1k)) %>% 
  addLegend( #add a legend ad the bottom, all values rescaled are between 0 and 1.
    position = "bottomright",
    pal = pal,
    values = ~rescale(Crime_rate_1k), #values shown in the legend are the rescaled Crime rate
    title = "Color scale") |>
  addLayersControl(baseGroups = c("Crime rate", "Unemployment", "School fail rate", "Lepen", "Immigration", "Density"),
                   options = layersControlOptions(collapsed = FALSE)) #add layers control, with base group. This allows user to switch between layers
#################################################################################
#Now we will create a leaflet map at the Town level.
cities <- read_csv(here::here("Raw_data/cities_hope.csv")) #this dataset gives us the geographic center of each town
Center_city <- cities |> 
  select(insee_code, latitude, longitude) |> #we select the town code, latitude and longitude for each town
  rename(Town_code = insee_code, 
         Latitude = latitude,
         Longitude = longitude)

#Load the data by town
Everything_by_town <- read_csv(here::here("data_end/Everything_by_town.csv"))
both_town_center <- left_join(Center_city, Everything_by_town, join_by("Town_code")) |> #join this dataset with our centers location
  na.omit() |>
  filter(!grepl("^97", Town_code)) |> #select onlymetropolitan values
  distinct() |>
  arrange(Dep_number) #arrange departments in increasing order

#Create the map for each town
circle_map <- leaflet() %>%
  addTiles() #initialize the map

#list of department number and names, will be sued to name the groups
grouped <- paste0(unique(both_town_center$Dep_name), "(", unique(both_town_center$Dep_number), ")")
list <- unique(both_town_center$Dep_number)

#using a loop, we add circle centers of towns inside each departments, will allow us to choose which departments to display
for (i in 1:length(grouped)) {
  # Filter data for the  department needed
  data <- filter(both_town_center, Dep_number == list[i]) #filter the list with one department each time
  
  # Add circles for the  department
  circle_map <- circle_map %>%
    addCircles(
      group = as.character(grouped[i]), #name the groups using the list
      data = data,
      lng = ~Longitude,
      lat = ~Latitude,
      weight = 1,
      fillColor = "red",
      stroke = TRUE,
      color = "darkred",
      fillOpacity = 0.1,
      radius = ~(Rate_per_1k)*500, #chose the circles radius
      popup = ~paste(Town_name, "<br/> <b>", round(data$Rate_per_1k/1000, 3), " </b>Crimes per capita") #display the rounded crime per capita value as a popup
    )
}

# Layer control to have the opportunity to show each layer/department
circle_map <- circle_map %>%
  addLayersControl(
    overlayGroups = as.character(grouped),  # chose which groups to display
    options = layersControlOptions(collapsed = TRUE)) |> #the list is collapsed by default
  hideGroup(grouped) #start with every layer hidden by default, avoids clutter
#################################################################################
#Data distribution and Plotting by town

#load the dataset by town
Crime_per_type_town <- read_csv(here::here("data_end/Crime_per_type_town.csv"))

#For each variable, we plot the density using geom_density()
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