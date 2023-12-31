###########################
###########################
#This script is our wrangling file
###########################
###########################

#Load required packages and options
source(here::here("Script/setup.R"))
####################################################################################

#Create a list of metropolitan departements
Pop_by_depart <- 
  read_excel(here::here("Raw_data/population par département.xls"), sheet = "2023")

#Create List of departments 
List_depart <- Pop_by_depart[5:100, 1:2]#select metropolitan departments and only name/number
colnames(List_depart) <- c("Dep_number", "Dep_name")#rename columns

####################################################################################

#Compute population by year, by department
years_pop <- as.character(1975:2023) #every year we need to extract a sheet
Pop_by_year <- List_depart[1] #list with depart numbers 
Col_name <-  paste0("pop_", years_pop) #List to rename the columns

#loop for every year, each excel sheet is one year
for (i in 1:length(years_pop)) {
  pop <- 
    read_excel(here::here("Raw_data/population par département.xls"), 
               sheet = years_pop[i]) 
  Pop_by_year <- cbind(Pop_by_year, pop[5:100, 8])#add them to the df
  colnames(Pop_by_year)[i+1] <- Col_name[i] #rename cols
}
#only select the population in 2019
Pop_2019 <- Pop_by_year |> 
  select(Dep_number, pop_2019)

####################################################################################

#Crime

crimes_depart_first <- read_excel(here::here("Raw_data/crimes_depart.xlsx"), sheet = "01") #open excel sheet

years_crime <- 1996:2021 #years needed
Crime_total <- t(data.frame(years_crime))#new dataframe with years as the first column
colnames(Crime_total) <- Crime_total[1,]

#Use a for loop to open the excel sheet for each department and add it to our dataframe
for (i in 1:nrow(List_depart)) {
  crimes_depart <- read_excel(here::here("Raw_data/crimes_depart.xlsx"), 
                              sheet = List_depart$Dep_number[i]) 
  #Load each datasheet with the department list
  crimes_depart <- crimes_depart |>
    select(-(1:10)) |> 
    summarise_all(sum) |>
    pivot_longer(cols = everything(), names_to = "Year", values_to = "Dep_number") |>
    mutate(Year = substr(Year, 2, 5)) |>
    group_by(Year) |>
    summarize(Dep_number = sum(Dep_number)) |> t()
  
  Crime_total <- rbind(Crime_total, crimes_depart[2,])#add our result to the dataframe
}

Crime_total <-  Crime_total[-1, ]
Crime_total <- cbind(List_depart[1], Crime_total)

Crime_2019 <- Crime_total |>
  select(Dep_number, `2019`) |>
  rename(Crime_tot_2019 = `2019`)#only select crime in 2019

Crime_2019 <- full_join(Crime_2019, Pop_2019, join_by("Dep_number"))#add the population

Crime_2019 <- Crime_2019 |>
  mutate(Crime_rate_1k = (as.numeric(Crime_tot_2019)/as.numeric(pop_2019))*1000)#compute crime rate(Total crime/Population*1000)

####################################################################################

#Unemployment 
Unemployment_start <- 
  read.csv(here::here("Raw_data/valeurs_trimestrielles.csv"), 
           sep = ";")#open csv


dummy <- seq_len(nrow(Unemployment_start)) %% 2 #Dummy variable equal to 1 if the row is an odd number
Unemployment <- cbind(dummy, Unemployment_start)#add dummy variable.

Unemployment <- Unemployment |>
  filter(dummy == 1) |>
  select(-c(1, 3,4,5))
Unemployment <-  Unemployment[-(1:15),]#Only keep odd rows, the only rows with data

Unemployment <- Unemployment |> 
  separate(Libellé, into = c("X", "Dep_name") ,sep=" - ") |>#Extract department name
  select(-1)

Unemployment <- Unemployment[-(97:100), ]#select metropolitan departments
#we want to rename those columns
Unemployment_quarterly <- Unemployment |> 
  pivot_longer(cols = -Dep_name, names_to = "Year", values_to = "Total") |>
  mutate(Total = as.numeric(Total)) |>
  mutate(Year = substr(Year, 2, 8)) |> #remove the X at the beginning
  group_by(Dep_name, Year) |>
  summarize(Total= mean(Total)) |>
  pivot_wider(names_from = Year, values_from = Total)#Compute the quarterly unemployment rate

Unemployment_quarterly <- full_join(List_depart, Unemployment_quarterly, join_by(Dep_name))#Add department name and number

Unemployment_year <- Unemployment_quarterly |>
  pivot_longer(cols = -(Dep_number:Dep_name), names_to = "Year", values_to = "Total") |>
  mutate(Year = substr(Year, 1, 4)) |>
  group_by(Dep_number, Dep_name, Year) |>
  summarize(Total = mean(Total)) |>
  pivot_wider(names_from = Year, values_from = Total)#Compute the unemployment by year, which is the average of each quarterly unemployment rate.

Unemp <- Unemployment_year |> 
  select(Dep_number, `2019`) |>
  rename(Unemp_2019 = `2019`)#Only select 2019, and the department name, to be added to the final dataset


####################################################################################

#Middle school final exam results
Middle_results <- read.csv(here::here("Raw_data/fr-en-dnb-par-etablissement.csv"), 
                           sep = ";")#Load the csv

Middle <- Middle_results |>
  filter(Session == 2019) |>#Only select the year 2019
  select(Code.département, Admis, Inscrits) |>#select Department, number of people who passed and number of registered students
  mutate(Pass_rate = (Admis/Inscrits)) |>#compute pass rate
  group_by(Code.département) |>#group by department
  summarize(Pass_rate_2019 = mean(Pass_rate)) |>#compute department average
  rename(Dep_number = Code.département) |>
  mutate(Dep_number = substr(Dep_number, 2, 3))#standardize department number

Middle <- Middle[(2:97), ]

####################################################################################

#For elections by department
Election_dep <- read_excel(here::here("Raw_data/Election_dep.xls"), 
                           sheet = "Départements Tour 2", skip = 2) #load excel sheet

Election_by_dep <- Election_dep |>
  select(c(1,28)) |>
  rename(Dep_number = `Code du département`,
         Lepen_score = `% Voix/Exp...28`) |>
  mutate(Win_lepen = ifelse(Lepen_score>50, 1, 0))#create dummy variable


Election_by_dep <- Election_by_dep[1:96, ] #only take metropolitan departments

#Need to change the dep notation
for (i in 1:nrow(Election_by_dep)) { #create a for loop that will add a leading 0 in front of single digit departments
  if(nchar(Election_by_dep$Dep_number[i]) == 1) {
    Election_by_dep$Dep_number[i] <- paste0("0",  Election_by_dep$Dep_number[i])
  }
}

####################################################################################

#Immigration
Immigration_2019 <- read_excel(here::here("Raw_data/Immigration_2019.xlsx"), 
                               sheet = "Pop0_D", skip = 1)#open excel

Immig_2019 <- Immigration_2019 |>
  slice(1:96) |> #select metropolitan departments
  select(c(1, 2, 4)) |>
  rename(Dep_name = `...1`,
         Immig_tot = Immigrés,
         pop_2019 = `Ensemble...4`) |>
  mutate(Immig_rate = Immig_tot/pop_2019) |>#compute immigration rate
  select(Dep_name, Immig_rate)#only select Department name and immigration rate

####################################################################################

#Density
population_2019 <- read_excel(here::here("Raw_data/population_2019.xlsx"), 
                              sheet = "Figure 3", skip = 2)#open excel sheet

Dens_2019 <- population_2019 |>
  rename(Dep_number = Département,
         Density_2019 = Densité) |>
  select(Dep_number, Density_2019) |> #only select department number and density
  slice(1:96) #only select metropolitan departments

#####################################################################################

#Join all of them together
#use a full join, the key is the department number
Full_data_dep <- Unemp |>
  full_join(Crime_2019, join_by("Dep_number")) |>
  full_join(Middle, join_by("Dep_number")) |>
  full_join(Election_by_dep, join_by("Dep_number")) |>
  full_join(Immig_2019, join_by("Dep_name")) |>
  full_join(Dens_2019, join_by("Dep_number"))


#####################################################################################
#####################################################################################
#On town level
#####################################################################################
#####################################################################################

#Crimes 
Crimes <- read.csv(here::here("Raw_data/try.csv"), sep = ";")#open crime csv

#Get a feeling of the way Towns are encoded,
Towncode <- Crimes |> 
  filter(annee == 21) |>#select 2021
  filter(valeur.publiée == "diff") |> #select published values
  filter(!grepl("^97", CODGEO_2023)) |> #select metropolitan departments
  select(CODGEO_2023, classe) |> 
  group_by(CODGEO_2023) |> 
  summarize(classe = n_distinct(classe)) |>
  select(CODGEO_2023)#we can see the town key. It seems that the code is department number first and a number for each town, creating a unique key for each town

#Find the rate at which towns publish data in a departement
Missing_in_dep <- Crimes |>
  filter(!grepl("^97", CODGEO_2023)) |>#only select published data
  select(CODGEO_2023, valeur.publiée)

Missing_in_dep <- cbind(Missing_in_dep$CODGEO_2023, Missing_in_dep)
colnames(Missing_in_dep) <- c("Departement", "Town", "Published") #rename columns

Missing_in_dep <- Missing_in_dep |> 
  mutate(Departement = substr(Departement, 1, 2)) |>
  mutate(Town = substr(Town, 3, 5)) |> #extract town number
  group_by(Departement) |>
  summarize(Publishing_rate = sum(Published=="diff")/n()) #Compute the average publishing rate

#Create a dataframe with values per year per type of crime
Type_per_year <- Crimes |> 
  filter(valeur.publiée == "diff") |> #select published values
  filter(!grepl("^97", CODGEO_2023)) |> #select metropolitan dep
  select(CODGEO_2023, annee, classe, faits, tauxpourmille)

Type_per_year <- cbind(Type_per_year[1], Type_per_year)#duplicate the town code column
colnames(Type_per_year) <- c("Departement", "Town", "Year", "Type", "Number", "Rate_per_1k")

Type_per_year <- Type_per_year |> 
  mutate(Departement = substr(Departement, 1, 2)) |> #use the duplicated column to extract the dep number
  mutate(Rate_per_1k = str_replace_all(Rate_per_1k, ",", ".")) |> #every rate uses commas, replace them with dots
  mutate(Rate_per_1k = as.numeric(Rate_per_1k)) |> #convert the rate to numeric
  group_by(Departement, Town, Year, Number, Type) |>
  summarize(Rate_per_1k = mean(Rate_per_1k, na.rm = TRUE), ) |> #compute the mean rate
  group_by(Departement, Town, Year, Type, Rate_per_1k) |> 
  summarize(Number = sum(Number)) #add the total number of crimes.

Common_crimes <- Type_per_year |>
  group_by(Type) |>
  summarize(Rate_per_1k = mean(Rate_per_1k, na.rm = TRUE)) |> #compute the crime rate for France, by type of crime
  arrange(desc(Rate_per_1k)) #arrange to find the most common crimes
#We find that the 5 most common crimes are 
#Destructions et dégradations volontaires
#Vols sans violence contre des personnes 
#Coups et blessures volontaires
#Cambriolages de logement 
#Usage de stupéfiants

Five_common <- Type_per_year|> 
  filter(Type %in% c("Destructions et dégradations volontaires",
                     "Vols sans violence contre des personnes",
                     "Coups et blessures volontaires",
                     "Cambriolages de logement",
                     "Usage de stupéfiants")) #only select the top 5 crimes


#Load in 2019 only
Crime_2019_town <- Type_per_year |>
  filter(Year == 19) |> #filter the crime rate
  group_by(Departement, Town) |>
  summarize(Rate_per_1k = mean(Rate_per_1k),
         Total_crime = sum(Number)) |> #compute the overall crime rate and number of crimes per town
  rename(Town_code = Town, Dep_number = Departement) 


#Create a dataset where we look at the rate for each type of crime
Crime_type_town <- Type_per_year |>
  filter(Year == 19) |>
  rename(Town_code = Town, Dep_number = Departement) |>
  select(Dep_number, Town_code, Type, Rate_per_1k) |>
  pivot_wider(names_from = Type, values_from = Rate_per_1k) |>
  select(Dep_number, Town_code, "Cambriolages de logement", 
         "Coups et blessures volontaires", 
         "Destructions et dégradations volontaires", 
         "Usage de stupéfiants",
         "Vols sans violence contre des personnes") #only select top 5 crimes

Crime_type_town[is.na(Crime_type_town)] <- 0 #Replace any non-published value with 0

#####################################################################################

#Dataset of 2017 election results by town
Presidentielle_2017 <- read_excel(here::here("Raw_data/Presidentielle_2017_Resultats_Communes_Tour_2_c.xls"), 
                                                              sheet = "Feuil1", skip =3) #open the excel sheet

Vote_2017 <- Presidentielle_2017 |>
  select(c(1, 2, 3, 25, 32)) #select only the values important to us in that case, the department, the towncode and the result
Vote_2017 <- Vote_2017[1:35281, ]
colnames(Vote_2017) <- c("Department", "Dep_name", "Town_num", "Macron", "Lepen") #rename columns

#Need to add the Corse departement number(2A/2B)
Vote_2017$Department <- as.character(Vote_2017$Department)

for (i in 1:nrow(Vote_2017)) { #using a for loop, look for the dep name and give it the appropriate number
  if (Vote_2017$Dep_name[i] == "Corse-du-Sud") {
    Vote_2017$Department[i] <-  "2A"
  }
  if (Vote_2017$Dep_name[i] == "Haute-Corse") {
    Vote_2017$Department[i] <-  "2B"
  }  
}

#Now let's standardize the notation of departments and town codes by adding leading zeroes.
for (i in 1:nrow(Vote_2017)) { #using a for loop
  if (nchar(Vote_2017$Department[i]) == 1) { #add one 0 to single digit departments
    Vote_2017$Department[i] <- paste0("0", Vote_2017$Department[i])
  }
  if (nchar(Vote_2017$Town_num[i])<=2) { # add 2 zeroes to single digit towns
    if (nchar(Vote_2017$Town_num[i]) <= 1) { 
      Vote_2017$Town_num[i] <- paste0("00", Vote_2017$Town_num[i])
    }
    else {Vote_2017$Town_num[i] <- paste0("0", Vote_2017$Town_num[i]) #add 1 zero to double digit towns
    }
  }
}

#Create the town key
Vote_2017 <- Vote_2017 |> 
  mutate(Town_code = paste0(Department, Town_num)) |>
  mutate(Win_Lepen = ifelse(Lepen>50, 1, 0)) |>
  select(Department, Town_code, Macron, Lepen, Win_Lepen)#Create a dummy variable that equals 1 if LePen "won" the town (scored more than 50%)

#####################################################################################

#Density
Dense <- read_csv(here::here("Raw_data/villes_france.csv"), col_names = FALSE) #open csv

Density_town <- Dense |>
  select(c(4, 11, 15, 19)) |>
  rename(Town_name = `X4`, 
         Town_code = `X11`,
         Pop_2012 = `X15`,
         Size = `X19`) |> #select population, size of each town
  filter(!str_detect(Town_code, "^97")) |> # remove metropolitan values
  mutate(Density_2019 = Pop_2012/Size) |> #compute density (Population/town size)
  select(Town_code, Town_name, Density_2019)
#we use population data from 2012.

#####################################################################################

#Population 2019
Pop_2019 <- read_delim(here::here("Raw_data/Pop_2019.csv"), 
                       delim = ";", escape_double = FALSE, trim_ws = TRUE) #open csv

Pop_by_town <- Pop_2019 |>
  group_by(CODGEO) |>
  summarise(Total = sum(NB)) |> #sum of population by town, it is divided by gender and age.
  rename(Town_code = CODGEO, Total_pop = Total) 

#####################################################################################

#People over 25 with no diploma 
Pop_16_no_diploma <- read_excel(here::here("Raw_data/pop-16ans-dipl6819_v2.xls"), 
                                sheet = "COM_2019", skip = 15) #open the excel sheet

Pop_no_diploma <- Pop_16_no_diploma |>
  select(c(2, 3, 6, 8, 10)) |>
  slice(-1) |>
  rename(Dep_number = `Département\nen géographie courante`,
         Town_number = `Commune\nen géographie courante`,
         Town_name = `Libellé de commune`,
         no_diploma_M = `Aucun diplôme\nHommes\n25 ans ou plus\nRP2019`,
         no_diploma_F = `Aucun diplôme\nFemmes\n25 ans ou plus\nRP2019`) |> #rename variables
  group_by(Dep_number, Town_number) |>
  summarize(no_diploma = sum(as.numeric(no_diploma_F), as.numeric(no_diploma_M), na.rm = TRUE)) |> #summarize the value, it was divided by gender
  mutate(Town_code = paste0(Dep_number, Town_number)) |> 
  select(Dep_number, Town_code, no_diploma) |>
  filter(!str_detect(Dep_number, "^97")) |> 
  left_join(Pop_by_town, join_by(Town_code)) |>
  na.omit() |>
  mutate(No_diploma_rate1k = (no_diploma/Total_pop)*1000) #compute the rate (Total number/Population*1000)

#We add the total for only Paris, this is because, this dataset divided Paris by city divisions, it is the only dataset to do this.
Paris_tot <- Pop_no_diploma |> 
  filter(Dep_number == "75") |>
  summarize(No_diploma_rate1k = mean(No_diploma_rate1k)) |> #find Paris mean rate
  rename(Town_code = Dep_number) |>
  mutate(Town_code = "75056") #This is Paris town code

#Add our Paris total at the end
Pop_no_diploma <- Pop_no_diploma |>
  group_by(Town_code, No_diploma_rate1k) |>
  select(Town_code, No_diploma_rate1k)

Pop_no_diploma <- rbind(Pop_no_diploma, Paris_tot) #add the row for Paris

#####################################################################################

#Add poverty
Pov_2019 <- read_delim(here::here("Raw_data/FILO2019_DEC_Pauvres_COM.csv"), 
                       delim = ";", escape_double = FALSE, trim_ws = TRUE) #Open csv

Povr_2019 <- Pov_2019 |>
  select(c(1, 4, 6)) |> #select poverty and intensity of poverty
  na.omit() |>
  rename(Town_code = CODGEO,
         Povrety_2019 = TP6019,
         Intensity_povrety = TP60IP19) #We know that poverty is not correctly written, but changing it now would be too much work :)

#####################################################################################

#Immigration
Immig_by_town <- read_delim(here::here("Raw_data/Immig_by_town.csv"), 
                            delim = ";", escape_double = FALSE, trim_ws = TRUE) #open the csv 
#we haven't mentioned it before, 
#but these French(European) datasets use semicolons because they use commas for decimals. It is a little bit annoying, but easily fixed 
#by specifiying that the delimitation is semicolons and not commas.

Immig_2019_town <- Immig_by_town |>
  select(CODGEO, IMMI, NB) |> #select number of immigrants and total of population per town
  rename(Town_code = CODGEO) |>
  filter(IMMI == 1) |> #Immig = 1 means that the row inidicates the number of immigrants
  filter(!str_detect(Town_code, "^97")) |> #only select metropolitan values
  group_by(Town_code) |>
  summarize(NB = sum(NB)) |> #calculate the total number of immigrants
  left_join(Pop_by_town, join_by(Town_code)) |>
  mutate(Immig_rate = (NB/Total_pop)*1000 ) |> #calculate the immigration rate (Total/Population*1000)
  na.omit() |>
  select(Town_code, Immig_rate) #only select town code and immigration rate

#####################################################################################

#Add them all together
#use a full join, the town code is the key
Everything_by_town <- Crime_2019_town |>
  full_join(Pop_by_town, join_by(Town_code)) |> 
  full_join(Density_town, join_by(Town_code)) |>
  full_join(Vote_2017, join_by(Town_code)) |>
  full_join(Povr_2019, join_by(Town_code)) |>
  full_join(Pop_no_diploma, join_by(Town_code)) |>
  full_join(Immig_2019_town, join_by(Town_code)) |>
  left_join(Unemp, join_by(Dep_number)) #We also add unemployment from our department dataset, the only value we cannot find by town. 

#remove the Na's, and only select specific values
Everything_by_town_clean <- Everything_by_town |>
  na.omit() |> 
  select(Dep_number, Town_code, Town_name, 
         Total_pop, Total_crime, Rate_per_1k,
         Density_2019, Lepen, Win_Lepen, 
         Povrety_2019, Intensity_povrety, No_diploma_rate1k, 
         Immig_rate, Unemp_2019)

#Similar operation but this dataset has values with type of crimes
Crime_per_type_town <- Crime_type_town |>
  full_join(Everything_by_town_clean, join_by(Town_code)) |>
  select(Dep_number.x, Town_code, Town_name, 
         Total_pop, Total_crime, Rate_per_1k, 
         "Cambriolages de logement", "Coups et blessures volontaires", "Destructions et dégradations volontaires", 
         "Usage de stupéfiants", "Vols sans violence contre des personnes",
         Density_2019, Lepen, Win_Lepen, 
         Povrety_2019, Intensity_povrety, No_diploma_rate1k, 
         Immig_rate, Unemp_2019) |>
  na.omit() |>
  rename(Assault = `Coups et blessures volontaires`,
         Burglary = `Cambriolages de logement`,
         Damage = `Destructions et dégradations volontaires`,
         Drugs = `Usage de stupéfiants`,
         Theft = `Vols sans violence contre des personnes`,
         Dep_number = Dep_number.x
         ) |> #translate variables into english
  select(-Year) |>
  mutate(Lepen= Lepen/100,
         Povrety_2019 = Povrety_2019/100,
         No_diploma_rate1k = No_diploma_rate1k/1000,
         Immig_rate = Immig_rate/1000,
         Unemp_2019 = Unemp_2019/100) #Everything on a percent basis

#Put DT interactive tables.
Interact_everything_by_dep <- datatable(Full_data_dep) |> #create interactive table
  formatRound(columns = c(3,6,7,10), digits = 3) #round some values with 3 digits

Interact_crime_per_type_town <- datatable(Crime_per_type_town[-1]) |> #create interactive tables
  formatRound(columns = 4, digits = 0) |> 
  formatRound(columns = c(6:13, 17:18), digits = 4) #round values