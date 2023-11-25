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
Full_model <- lm(formula = Rate_per_1k ~ Density_2019 + Win_Lepen + 
                   Povrety_2019 + No_diploma_rate1k + Immig_rate + 
                   Unemp_2019 + Intensity_povrety + Total_pop, 
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
"The model with the higher likelihood ratio will be the best model "


Single_model <- lm(Rate_per_1k ~ Intensity_povrety, data = Everything_by_town_clean)
summary(Single_model)

#compute correlation matrix

corr_matrix <- cor(Everything_by_town_clean[5:14])
corr_matrix


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
ggcorrplot(corr_matrix)

#Linear regressions with the type of crimes committed + vif 

#Coups et blessures volontaires/(Assault and battery)

Modelfullcoups <- lm(formula= Coups.et.blessures.volontaires ~ 
                       Density_2019 + Povrety_2019 + No_diploma_rate1k + 
                       Immig_rate + Unemp_2019 + Win_Lepen +
                       Intensity_povrety, data = Crime_per_type_town)


ModelCoupsBlessuresNULL <- lm(formula = Coups.et.blessures.volontaires ~ 1, data =  Crime_per_type_town)

#stepwise regression
step(Modelfullcoups, scope = list(lower= ModelCoupsBlessuresNULL, upper=Modelfullcoups),
     direction = "backward")

#after computing the stepwise regression with the backward
#method we decided to keep only the Poverty, Immigration, Density, NoDiploma, IntensityPoverty 
#variables as they are the most useful in the model 

ModelCoupsBlessuresFINAL <- lm(formula= Coups.et.blessures.volontaires ~ 
                                 Density_2019 +
                                 Povrety_2019 +
                                 No_diploma_rate1k +
                                 Immig_rate + 
                                 Intensity_povrety, data = Crime_per_type_town)


summary(ModelCoupsBlessuresFINAL)
vif(ModelCoupsBlessuresFINAL)

#Cambriolages




Modelfullcambriolages <- lm(formula=  Cambriolages.de.logement ~ 
                               Density_2019
                            +  Povrety_2019 +  No_diploma_rate1k + 
                               Immig_rate +  Unemp_2019 +
                               Win_Lepen +
                               Intensity_povrety)




ModelCambriolagesNULL <- lm(formula =  Cambriolages.de.logement ~ 1)




step(Modelfullcambriolages, scope = list(lower= ModelCambriolagesNULL, upper=Modelfullcambriolages),
     direction = "backward")


#With AIC rate we decide to keep only the following variables: NoDIploma, 
#Unemployment, Poverty, Immigration, Intensity poverty


ModelCambriolagesFINAL <-  lm(formula=  Cambriolages.de.logement ~ 
                                 Povrety_2019 +  No_diploma_rate1k + 
                                 Immig_rate +  Unemp_2019 +
                                 Intensity_povrety)


summary(ModelCambriolagesFINAL)
vif(ModelCambriolagesFINAL)

#Destructions et dégradations volontaires


ModelfullDegradations<- lm(formula=  Destructions.et.degradations.volontaires ~ 
                              Density_2019
                           +  Povrety_2019 +  No_diploma_rate1k + 
                              Immig_rate +  Unemp_2019 +
                              Win_Lepen +
                              Intensity_povrety)


ModelDegradationsNULL <- lm(formula =  Destructions.et.dégradations.volontaires ~1)


step(ModelfullDegradations, scope = list(lower= ModelDegradationsNULL, upper=ModelfullDegradations),
     direction = "backward")

#With the AIC we only need to keep the following variables: Poverty, Win Lepen, Density, Intensity Poverty

ModelDegradationsFINAL <-  lm(formula =  Destructions.et.dégradations.volontaires ~ 
                                 Povrety_2019 +  Win_Lepen
                              +  Intensity_povrety +  Density_2019)


summary(ModelDegradationsFINAL)
vif(ModelDegradationsFINAL)

#USAGE DE STUPS 


ModelFullStups <- lm(formula=  Usage.de.stupéfiants ~ 
                        Density_2019
                     +  Povrety_2019 +  No_diploma_rate1k + 
                        Immig_rate +  Unemp_2019 +
                        Win_Lepen +
                        Intensity_povrety)




ModelNULLStups <- lm(formula =  Usage.de.stupéfiants ~ 1)


step(ModelFullStups, scope = list(lower= ModelNULLStups, upper=ModelFullStups),
     direction = "backward")

#With AIC test with stepwise regression we choose to compute all the previous variables established

ModelFINALStups <-  ModelFullStups


summary(ModelFINALStups)


vif(ModelFINALStups)

#Vols 

ModelFullVols <-  lm(formula=  Vols.sans.violence.contre.des.personnes ~ 
                        Density_2019
                     +  Povrety_2019 +  No_diploma_rate1k + 
                        Immig_rate +  Unemp_2019 +
                        Win_Lepen +
                        Intensity_povrety)
ModelNULLVols <-  lm(formula =  Vols.sans.violence.contre.des.personnes ~ 1)


step(ModelFullVols, scope = list(lower= ModelNULLVols, upper=ModelFullVols),
     direction = "backward")

#With AIC test with stepwise regression we chose to keep the following variables for our regression:
#Immigration, Win Lepen, Density, Unemployment, IntensityPoverty

ModelFinalVols <- lm(formula =  Vols.sans.violence.contre.des.personnes ~ 
                        Immig_rate + Win_Lepen
                     +  Density_2019 +  Unemp_2019
                     +  Intensity_povrety)


summary(ModelFinalVols)
vif(ModelFinalVols)









