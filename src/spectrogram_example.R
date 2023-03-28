library("soundecology") # broad acoustic metrics
library("tuneR") # load sound files


# Load sound file ---------------------------------------------------------

spec_ex = readWave("data/20210518_110000.WAV", # from CBMA ARU03
                   from = 0,
                   to = 3,
                   units = "seconds")

acoustic_complexity(spec_ex,
                    max_freq = 8000, 
                    j = 3) #64.7517
acoustic_diversity(spec_ex,
                   max_freq = 8000) #2.074488
acoustic_evenness(spec_ex,
                  max_freq = 8000) #0.049638
bioacoustic_index(spec_ex,
                  max_freq = 8000) #8.101037

spec_ml = aw4 %>%
  dplyr::filter(site == "cbma") %>%
  dplyr::filter(aru == "aru03") %>%
  dplyr::filter(date_time == "2021-05-18 11:00:00 UTC")
