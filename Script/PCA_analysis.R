#Lod required packages
library(FactoMineR)
library(readr)
library(dplyr)
library(corrplot)
library(tidyverse)
library(ggplot2)
library(plotly)
library(ggcorrplot)
library(factoextra)
install.packages("ggfortify")
library(ggfortify)

#Let's run a PCA analysis

#Load our data first.
PCA_rate <- read_csv(here::here("data_end/PCA_rate.csv"))

#colnames(PCA_rate) <- c("Dep_name",
                        #"Other assault and battery",
                        #"House burglary",
                        #"Assault and battery",
                        #"Domestic assault and battery",
                        #"Destruction and damage",
                        #"Drug trafficking",
                        #"Drug use",
                        #"Sexual violence",
                        #"Theft with weapons", 
                        #"Theft of vehicle accessories",
                        #"Theft from vehicles", 
                        #"Vehicle theft",
                        #"Non-violent theft against individuals",
                        #"Violent theft without weapon")

#exclude row names
PCA_nums <-  PCA_rate |>
  filter(!Departement %in% c("75", "93", "92", "93", "94")) |>
  select(-Departement) |>
  select("Cambriolages de logement", 
         "Coups et blessures volontaires", 
         "Destructions et dégradations volontaires", 
         "Usage de stupéfiants",
         "Vols sans violence contre des personnes")




#plot the correlation matrix
corr_matrix <- cor(PCA_normalized)
ggcorrplot(corr_matrix)

PCA_normalized <- scale(PCA_nums)

PCA_prin <- prcomp(PCA_normalized)
PCA_prin

#scree plot
fviz_eig(PCA_prin, addlabels = TRUE)

#biplot
pca_var_plot <- fviz_pca_var(PCA_prin, col.var = "black")
pca_var_plot

autoplot(PCA_prin)

d.factanal <- factanal(PCA_normalized, factors = 2, scores = 'regression', rotation = "varimax")

autoplot(d.factanal, label = TRUE, label.size = 3,
         loadings = TRUE, loadings.label = TRUE, loadings.label.size  = 3)




