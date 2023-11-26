#load packages
library(readxl)
library(readr)
library(dplyr)
library(tidyverse)
install.packages("caret")
library(caret)
library(lmtest)
library(car)
library(ggcorrplot)

Everything_by_town_clean <- read_csv(here::here("data_end/Everything_by_town_clean.csv"))

#We first create a model with all the explicative variables  
Full_model <- lm(formula = Rate_per_1k ~ Density_2019 + 
                   Win_Lepen + 
                   Povrety_2019 + 
                   No_diploma_rate1k + 
                   Immig_rate + 
                   Unemp_2019 + 
                   Intensity_povrety +
                   Total_pop, 
                 data = Everything_by_town_clean)
summary(Full_model)
#We can observe that not all the variables are significant

vif(Full_model)
#computing the VIF also tells us that there is currently no strong sign of collinearity

#We then create a Null model to mesure the impact of each variable with a stepwise regression 
Null_model <- lm(formula = Rate_per_1k ~ 1, data = Everything_by_town_clean)

step(Full_model, scope = list(lower= Null_model, upper=Full_model), direction = "both")

#The model with forward stepwise shows us that the variables to keep are Density, Poverty, Total pop, 
#immigration rate, unemployment, poverty intensity

#We will now create a reduce model excluding the no diploma variable 
Reduce_model <- lm(formula = Rate_per_1k ~ Density_2019 + Immig_rate + Unemp_2019 + Total_pop, data = Every)
summary(Reduce_model)

#We will now compare the previous full modelModelByTown with the reduce one ReduceModel

lrtest(Full_model, Reduce_model)
#"The model with the higher likelihood ratio will be the best model "


Single_model <- lm(Rate_per_1k ~ Intensity_povrety, data = Everything_by_town_clean)
summary(Single_model)

#compute correlation matrix

corr_matrix <- cor(Everything_by_town_clean[5:14])


############
############
#Crime by type
############
############

#Load the dataset with types of crime

Crime_type <- read.csv(here::here("data_end/Crime_per_type_town.csv"))
Crime_num <- Crime_type[6:21]

#We first run a correlation analysis

corr_matrix <- cor(Crime_num)
#visualize the corrplot
corplot <- ggcorrplot(corr_matrix)
corr_matrix

#Linear regressions with the type of crimes committed + vif 

#Coups et blessures volontaires/(Assault and battery)

Assault_model_full <- lm(formula= Coups.et.blessures.volontaires ~ 
                       Density_2019 + Povrety_2019 + No_diploma_rate1k + 
                       Immig_rate + Unemp_2019 + Win_Lepen +
                       Intensity_povrety, data = Crime_type)

Assault_model_null <- lm(formula = Coups.et.blessures.volontaires ~ 1, data =  Crime_type)

#stepwise regression
step(Assault_model_full, scope = list(lower= Assault_model_null, upper=Assault_model_full),
     direction = "backward")

#after computing the stepwise regression with the backward
#method we decided to keep only the Poverty, Immigration, Density, NoDiploma, IntensityPoverty 
#variables as they are the most useful in the model 

Assault_model_final <- lm(formula= Coups.et.blessures.volontaires ~ 
                                 Density_2019 +
                                 Povrety_2019 +
                                 No_diploma_rate1k +
                                 Immig_rate + 
                                 Intensity_povrety, data = Crime_type)

#how do they look ? Print summary, vif and Plot it
summary(Assault_model_final)
vif(Assault_model_final)

#Cambriolages(Burglary)
Burglary_model_full <- lm(formula=  Cambriolages.de.logement ~ 
                            Density_2019 +  
                            Povrety_2019 +  
                            No_diploma_rate1k + 
                            Immig_rate +  
                            Unemp_2019 +
                            Win_Lepen +
                            Intensity_povrety, data = Crime_type)

Burglary_model_null <- lm(formula = Cambriolages.de.logement ~ 1, data = Crime_type)

step(Burglary_model_full, scope = list(lower = Burglary_model_null, upper = Burglary_model_full),
     direction = "backward")
#With AIC rate we decide to keep only the following variables: NoDIploma, 
#Unemployment, Poverty, Immigration, Intensity poverty
Burglary_model_final <-  lm(formula=  Cambriolages.de.logement ~ 
                              Povrety_2019 +  
                              No_diploma_rate1k + 
                              Immig_rate +  
                              Unemp_2019 +
                              Intensity_povrety, data = Crime_type)

summary(Burglary_model_final)
vif(Burglary_model_final)

#Destructions et dégradations volontaires(willful destruction and damage)


Damage_model_full <- lm(formula=  Destructions.et.degradations.volontaires ~ 
                          Density_2019 +
                          Povrety_2019 +  
                          No_diploma_rate1k + 
                          Immig_rate +  
                          Unemp_2019 +
                          Win_Lepen +
                          Intensity_povrety, data = Crime_type)


Damage_model_null <- lm(formula =  Destructions.et.dégradations.volontaires ~1, data = Crime_type)


step(Damage_model_full, scope = list(lower= Damage_model_null, upper=Damage_model_full),
     direction = "backward")

#With the AIC we only need to keep the following variables: Poverty, Win Lepen, Density, Intensity Poverty

Damage_model_final <-  lm(formula =  Destructions.et.dégradations.volontaires ~ 
                            Povrety_2019 +  
                            Win_Lepen +
                            Intensity_povrety +  
                            Density_2019, data = Crime_type)


summary(Damage_model_final)
vif(Damage_model_final)

#USAGE DE STUPS(Drug use)
Drug_model_full <- lm(formula=  Usage.de.stupéfiants ~ 
                        Density_2019 +
                        Povrety_2019 + 
                        No_diploma_rate1k + 
                        Immig_rate + 
                        Unemp_2019 +
                        Win_Lepen +
                        Intensity_povrety, data = Crime_type)

Drug_model_null <- lm(formula =  Usage.de.stupéfiants ~ 1, data = Crime_type)

step(Drug_model_full, scope = list(lower= Drug_model_null, upper = Drug_model_full),
     direction = "backward")
#With AIC test with stepwise regression we choose to compute all the previous variables established
Drug_model_final <-  Drug_model_full

summary(Drug_model_final)
vif(Drug_model_final)

#Vols(Theft)

Theft_model_full <-  lm(formula=  Vols.sans.violence.contre.des.personnes ~ 
                          Density_2019 +
                          Povrety_2019 +  
                          No_diploma_rate1k + 
                          Immig_rate +  
                          Unemp_2019 +
                          Win_Lepen +
                          Intensity_povrety, data = Crime_type)

Theft_model_null <-  lm(formula =  Vols.sans.violence.contre.des.personnes ~ 1, data = Crime_type)

step(Theft_model_full, scope = list(lower= Theft_model_null, upper=Theft_model_full),
     direction = "backward")
#With AIC test with stepwise regression we chose to keep the following variables for our regression:
#Immigration, Win Lepen, Density, Unemployment, IntensityPoverty
Theft_model_final <- lm(formula =  Vols.sans.violence.contre.des.personnes ~ 
                          Immig_rate + 
                          Win_Lepen +
                          Density_2019 + 
                          Unemp_2019 +
                          Intensity_povrety, data = Crime_type)

summary(Theft_model_final)
vif(Theft_model_final)









