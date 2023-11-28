#load packages
library(readxl)
library(readr)
library(dplyr)
library(tidyverse)
library(caret)
library(lmtest)
library(car)
library(ggcorrplot)
library(stargazer)

############
############
#Crime by type
############
############

#Load the dataset with types of crime

Crime_type <- read.csv(here::here("data_end/Crime_per_type_town.csv"))
Crime_num <- Crime_type[5:20]

#We first run a correlation analysis

corr_matrix <- cor(Crime_num)
#visualize the corrplot
corplot <- ggcorrplot(corr_matrix)

#First run the thing with overall crime
#We first create a model with all the explicative variables  
Full_model <- lm(formula = Rate_per_1k ~ Density_2019 + 
                   Win_Lepen + 
                   Povrety_2019 + 
                   No_diploma_rate1k + 
                   Immig_rate + 
                   Unemp_2019 + 
                   Intensity_povrety +
                   Total_pop, 
                 data = Crime_type)


#computing the VIF also tells us that there is currently no strong sign of collinearity

#We then create a Null model to mesure the impact of each variable with a stepwise regression 
Null_model <- lm(formula = Rate_per_1k ~ 1, data = Crime_type)

step(Full_model, scope = list(lower= Null_model, upper=Full_model), direction = "backward")
#The model with forward stepwise shows us that the variables to keep are Density, Poverty, Total pop, 
#immigration rate, unemployment, poverty intensity

#We will now create a reduce model excluding the no diploma variable 
Reduce_model <- lm(formula = Rate_per_1k ~ Density_2019 + Immig_rate + Unemp_2019 + Total_pop, data = Crime_type)


#Linear regressions with the type of crimes committed + vif 

#Coups et blessures volontaires/(Assault and battery)

Assault_model_full <- lm(formula= Assault ~ 
                       Density_2019 + Povrety_2019 + No_diploma_rate1k + 
                       Immig_rate + Unemp_2019 + Win_Lepen +
                       Intensity_povrety, data = Crime_type)

Assault_model_null <- lm(formula = Assault ~ 1, data =  Crime_type)

#stepwise regression
step(Assault_model_full, scope = list(lower= Assault_model_null, upper=Assault_model_full),
     direction = "backward")

#after computing the stepwise regression with the backward
#method we decided to keep only the Poverty, Immigration, Density, NoDiploma, IntensityPoverty 
#variables as they are the most useful in the model 

Assault_model_final <- lm(formula= Assault ~ 
                                 Density_2019 +
                                 Povrety_2019 +
                                 No_diploma_rate1k +
                                 Immig_rate + 
                                 Intensity_povrety, data = Crime_type)


#Cambriolages(Burglary)
Burglary_model_full <- lm(formula=  Burglary ~ 
                            Density_2019 +  
                            Povrety_2019 +  
                            No_diploma_rate1k + 
                            Immig_rate +  
                            Unemp_2019 +
                            Win_Lepen +
                            Intensity_povrety, data = Crime_type)

Burglary_model_null <- lm(formula = Burglary ~ 1, data = Crime_type)

step(Burglary_model_full, scope = list(lower = Burglary_model_null, upper = Burglary_model_full),
     direction = "backward")
#With AIC rate we decide to keep only the following variables: NoDIploma, 
#Unemployment, Poverty, Immigration, Intensity poverty
Burglary_model_final <-  lm(formula=  Burglary ~ 
                              Povrety_2019 +  
                              No_diploma_rate1k + 
                              Immig_rate +  
                              Unemp_2019 +
                              Intensity_povrety, data = Crime_type)

#Destructions et dégradations volontaires(willful destruction and damage)


Damage_model_full <- lm(formula=  Damage ~ 
                          Density_2019 +
                          Povrety_2019 +  
                          No_diploma_rate1k + 
                          Immig_rate +  
                          Unemp_2019 +
                          Win_Lepen +
                          Intensity_povrety, data = Crime_type)


Damage_model_null <- lm(formula =  Damage ~1, data = Crime_type)


step(Damage_model_full, scope = list(lower= Damage_model_null, upper=Damage_model_full),
     direction = "backward")

#With the AIC we only need to keep the following variables: Poverty, Win Lepen, Density, Intensity Poverty

Damage_model_final <-  lm(formula =  Damage ~ 
                            Povrety_2019 +  
                            Win_Lepen +
                            Intensity_povrety +  
                            Density_2019, data = Crime_type)


#USAGE DE STUPS(Drug use)
Drug_model_full <- lm(formula=  Drugs ~ 
                        Density_2019 +
                        Povrety_2019 + 
                        No_diploma_rate1k + 
                        Immig_rate + 
                        Unemp_2019 +
                        Win_Lepen +
                        Intensity_povrety, data = Crime_type)

Drug_model_null <- lm(formula =  Drugs ~ 1, data = Crime_type)

step(Drug_model_full, scope = list(lower= Drug_model_null, upper = Drug_model_full),
     direction = "backward")
#With AIC test with stepwise regression we choose to compute all the previous variables established
Drug_model_final <-  Drug_model_full



#Vols(Theft)

Theft_model_full <-  lm(formula=  Theft ~ 
                          Density_2019 +
                          Povrety_2019 +  
                          No_diploma_rate1k + 
                          Immig_rate +  
                          Unemp_2019 +
                          Win_Lepen +
                          Intensity_povrety, data = Crime_type)

Theft_model_null <-  lm(formula =  Theft ~ 1, data = Crime_type)

step(Theft_model_full, scope = list(lower= Theft_model_null, upper=Theft_model_full),
     direction = "backward")
#With AIC test with stepwise regression we chose to keep the following variables for our regression:
#Immigration, Win Lepen, Density, Unemployment, IntensityPoverty
Theft_model_final <- lm(formula =  Theft ~ 
                          Immig_rate + 
                          Win_Lepen +
                          Density_2019 + 
                          Unemp_2019 +
                          Intensity_povrety, data = Crime_type)