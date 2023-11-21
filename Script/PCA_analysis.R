#Lod required packages
library(FactoMineR)
library(readr)
library(dplyr)
library(corrplot)
library(tidyverse)
library(ggplot2)
library(plotly)

#Let's run a PCA analysis

#Load our data first.
PCA_rate <- read_csv(here::here("data_end/PCA_rate.csv"))

colnames(PCA_rate) <- c("Dep_name",
                        "Other assault and battery",
                        "House burglary",
                        "Assault and battery",
                        "Domestic assault and battery",
                        "Destruction and damage",
                        "Drug trafficking",
                        "Drug use",
                        "Sexual violence",
                        "Theft with weapons", 
                        "Theft of vehicle accessories",
                        "Theft from vehicles", 
                        "Vehicle theft",
                        "Non-violent theft against individuals",
                        "Violent theft without weapon")

#exclude row names
PCA_nums <-  PCA_rate |>
  filter(!Dep_name %in% c("75", "92", "93", "94")) |>
  select(-Dep_name)



#Compute the correlation matrix first
cor_matrix <- cor(PCA_nums)
#plot it
cor_plot_pca <- corrplot(cor_matrix, tl.pos='l')


#Run the PCA analysis
PCA1 <- princomp(PCA_nums)
summary(PCA1)

PCA_scale<- prcomp(PCA_nums, scale. = TRUE)
plot(PCA_scale)

biplot(PCA_scale)
summary(PCA(PCA_nums))




