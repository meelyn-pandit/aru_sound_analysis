#####################################################
###### Calculate Site-specific Species Diversity ####
#####################################################

library(tidyverse) #data manipulation
library(RColorBrewer) # brewer color palette
library(viridis) # viridis color palette
library(lubridate) #manipulating date and time
library(hms) #manipulate time
library(zoo) #for na.approx to approximate missing values in weather dataset
library(gridExtra) #ggplot multi panels
library(lme4) #lmm and glmm analysis
library(lmerTest) #get p-values for lmm tests
library(reshape2) #???
library(suncalc) #calculate sunrise time and sun altitude 
library(zoo)#approximating rows with NA in the weather data
library(car)#ANOVAs
library(multcomp) #posthoc tests for ANOVA type III effects
library(bbmle) #AIC comparisons
library(performance) #performance
library(emmeans)
library(magrittr)
library(gt)
library(htmltools)
library(webshot2)
library(ggbiplot) # plot pcas
library(broom)
library(docstring)

# ### Install ggbiplot ###
# library(devtools)
# install_github("vqv/ggbiplot")
# 
# ### Install dotwhisker ###
# devtools::install_github("fsolt/dotwhisker")

# Load Functions ----------------------------------------------------------

source("src/functions.R")
source("src/aridity_gradient_functions/aridity_gradient_mas_functions.R")
source("src/aridity_gradient_functions/aridity_gradient_graphs.R")
source("src/water_supp_functions/sswma_water_functions.R")
source("src/water_supp_functions/cbma_water_functions.R")
source("src/ece_functions/ece_functions.R")
source("src/ece_functions/sswma_water_ece_functions.R")
source("src/ece_functions/cbma_ece_functions.R")
source("src/evap_rate_function.R")
source("src/inflection_points.R") # used to determine thresholds for ECE analysis


# Load Birdnet dataset with bird species names --------------------------------------

load("data_clean/birdnet_data/birdnet_data.Rdata")

sites = c("lwma","sswma","cbma","kiowa")

site_species = lapply(sites, function(x){
  df = birdnet_data %>% 
    dplyr::filter(site == x) %>%
    summarise(species = unique(common_name))
  df$site = x
  return(df)
})
# do.call(rbind,site_species)

lwma_species  = site_species[[1]]
sswma_species = site_species[[2]]
cbma_species  = site_species[[3]]
kiowa_species = site_species[[4]]

lwma_only = anti_join(lwma_species,sswma_species, by = "species")
sswma_only = anti_join(sswma_species,cbma_species, by = "species")
cbma_only = anti_join(cbma_species,kiowa_species, by = "species")
kiowa_only = anti_join(kiowa_species, cbma_species, by = "species")
