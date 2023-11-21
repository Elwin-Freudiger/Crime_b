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

Crime_type <- read.csv(here::here("data_end/Crime_per_type_town.csv"))

Crime_nume <- Crime_type[6:21]

#corr plot for the thing
corr_matrix <- cor(Crime_nume)
ggcorrplot(corr_matrix)




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

