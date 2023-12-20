###########################
###########################
#This script is for our regression analysis
###########################
###########################

#Load our packages and options for our regression analysis
source(here::here("Script/setup.R"))

#Load the dataset
Crime_type <- read.csv(here::here("data_end/Crime_per_type_town.csv"))
#only select numerical variables for a correlation analysis
Crime_num <- Crime_type[5:20]

#We first run a correlation analysis
corr_matrix <- cor(Crime_num)
#visualize the corrplot
corplot <- ggcorrplot(corr_matrix)

############
#Total crime
############

#Create a model with every variable  
Full_model <- lm(formula = Rate_per_1k ~ Density_2019 + 
                   Win_Lepen + 
                   Povrety_2019 + 
                   No_diploma_rate1k + 
                   Immig_rate + 
                   Unemp_2019 + 
                   Intensity_povrety +
                   Total_pop, 
                 data = Crime_type)

#Create a Null model
Null_model <- lm(formula = Rate_per_1k ~ 1, data = Crime_type)

#Run stepwise regression
step(Full_model, scope = list(lower= Null_model, upper=Full_model), direction = "backward")

#Create a reduced model based on stepwise results
Reduce_model <- lm(formula = Rate_per_1k ~ Density_2019 + Immig_rate + Unemp_2019 + Total_pop, data = Crime_type)

############
#Crime by type
############

#Now run a regression by type of crime, with top 5 crimes.

#Assault and battery
#Full model
Assault_model_full <- lm(formula= Assault ~ 
                       Density_2019 + Povrety_2019 + No_diploma_rate1k + 
                       Immig_rate + Unemp_2019 + Win_Lepen +
                       Intensity_povrety, data = Crime_type)
#null model
Assault_model_null <- lm(formula = Assault ~ 1, data =  Crime_type)

#stepwise
step(Assault_model_full, scope = list(lower= Assault_model_null, upper=Assault_model_full),
     direction = "backward")

#Final model based on the results of stepwise
Assault_model_final <- lm(formula= Assault ~ 
                                 Density_2019 +
                                 Povrety_2019 +
                                 No_diploma_rate1k +
                                 Immig_rate + 
                                 Intensity_povrety, data = Crime_type)

#Burglary
#Full model
Burglary_model_full <- lm(formula=  Burglary ~ 
                            Density_2019 +  
                            Povrety_2019 +  
                            No_diploma_rate1k + 
                            Immig_rate +  
                            Unemp_2019 +
                            Win_Lepen +
                            Intensity_povrety, data = Crime_type)

#Null model
Burglary_model_null <- lm(formula = Burglary ~ 1, data = Crime_type)

#Stepwise
step(Burglary_model_full, scope = list(lower = Burglary_model_null, upper = Burglary_model_full),
     direction = "backward")
#Final model based on the results of stepwise
Burglary_model_final <-  lm(formula=  Burglary ~ 
                              Povrety_2019 +  
                              No_diploma_rate1k + 
                              Immig_rate +  
                              Unemp_2019 +
                              Intensity_povrety, data = Crime_type)



#Intentional destruction and damage
#Full model
Damage_model_full <- lm(formula=  Damage ~ 
                          Density_2019 +
                          Povrety_2019 +  
                          No_diploma_rate1k + 
                          Immig_rate +  
                          Unemp_2019 +
                          Win_Lepen +
                          Intensity_povrety, data = Crime_type)

#Null model
Damage_model_null <- lm(formula =  Damage ~1, data = Crime_type)

#stepwise
step(Damage_model_full, scope = list(lower= Damage_model_null, upper=Damage_model_full),
     direction = "backward")
#Final model based on the results of stepwise
Damage_model_final <-  lm(formula =  Damage ~ 
                            Povrety_2019 +  
                            Win_Lepen +
                            Intensity_povrety +  
                            Density_2019, data = Crime_type)


#Drug use
#Full model
Drug_model_full <- lm(formula=  Drugs ~ 
                        Density_2019 +
                        Povrety_2019 + 
                        No_diploma_rate1k + 
                        Immig_rate + 
                        Unemp_2019 +
                        Win_Lepen +
                        Intensity_povrety, data = Crime_type)

#Null model
Drug_model_null <- lm(formula =  Drugs ~ 1, data = Crime_type)

#Stepwise
step(Drug_model_full, scope = list(lower= Drug_model_null, upper = Drug_model_full),
     direction = "backward")
#Final model based on the results of stepwise, all variables are kept
Drug_model_final <-  Drug_model_full



#Theft
#Full model
Theft_model_full <-  lm(formula=  Theft ~ 
                          Density_2019 +
                          Povrety_2019 +  
                          No_diploma_rate1k + 
                          Immig_rate +  
                          Unemp_2019 +
                          Win_Lepen +
                          Intensity_povrety, data = Crime_type)
#Null model
Theft_model_null <-  lm(formula =  Theft ~ 1, data = Crime_type)

#stepwise
step(Theft_model_full, scope = list(lower= Theft_model_null, upper=Theft_model_full),
     direction = "backward")
#Final model based on the results of stepwise
Theft_model_final <- lm(formula =  Theft ~ 
                          Immig_rate + 
                          Win_Lepen +
                          Density_2019 + 
                          Unemp_2019 +
                          Intensity_povrety, data = Crime_type)