library(tidyverse)
library(taxize)


# Get common names from scientific names ----------------------------------

metab_birds = read_csv('data/song_beissinger_metabolic_data.csv', col_names = TRUE)

common_name = sci2comm(metab_birds$scientific_name, simplify = FALSE) # output is a list, need to convert to dataframe

common_name2 = enframe(common_name) %>% # creates the 'value' as a `list` column
  mutate(value = map(value, as.character)) %>% # change to single type
  rename(scientific_name = name) %>%
  unnest 

metab_birds2 = full_join(metab_birds, common_name2, by = "scientific_name")

write_csv(metab_birds2, file = 'data/song_beissinger_metabolic_data.csv')


# Combine with beak measurement dataset -----------------------------------

# load song and beissinger dataset with full scientific and common names
bird_data = read_csv('data/song_beissinger_metabolic_data.csv', col_names = TRUE) %>%
  mutate(scientific_name = str_replace(scientific_name, " ", "_"))

# Rombaut, Louie (2021): body mass and beak size data for the world's birds. figshare. Dataset. https://doi.org/10.6084/m9.figshare.16556145.v2
# bird beak size in units of centroid size (mm) and body mass in grams, both log-transformed.
beaks = read_csv('data/macro_allomdat_18_11_2018.csv', col_names = TRUE) %>%
  rename(scientific_name = "species") %>%
  mutate(beak_source = "Rombaut 2021")

bird_data2 = left_join(bird_data, beaks, by = "scientific_name")
which("Cuculus_gularis" %in% beaks$scientific_name) # tells you if bird is in dataset
