library(ohun)
library(warbleR)
library(dplyr)
#######################################
########EAME Ohun Autodetection#######
#######################################

# Setting Working Directory -----------------------------------------------

setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data/ml_training/templates/eame/")
dir = "C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data/ml_training/templates/eame/"

wavs <- list.files(pattern = ".wav$", ignore.case = TRUE)
wavs_df = as.data.frame(wavs)
names(wavs_df) = "sound.files"
x = lapply(wavs, readWave)

#Species specific autodetection parameters
eame_ed = energy_detector(files = wavs,
  # files = c("lbh1.wav", "lbh2.wav"),
                          path = dir, 
                          threshold = 10, 
                          smooth = 10,
                          bp = c(2, 6), 
                          hop.size = 6.8,
                          min.duration = 0.09)

eame_ad = autodetec(eame_ed, 
                            bp = c(2, 6), 
                            threshold = 5, 
                            mindur = 0.04, 
                            maxdur = 0.5, 
                            envt="abs", 
                            ssmooth = 300, 
                            ls = TRUE, 
                            res = 100, 
                            flim = c(1, 12), 
                            wl = 300, 
                            set = TRUE, 
                            sxrow = 6, 
                            rows = 15, 
                            redo = FALSE)

write.csv(eame_ed, "eame_selection_table.csv",row.names = FALSE)

# plot spectrogram
label_spectro(wave = x[[1]],
              path = dir,
              f = 48000,
              envelope = TRUE, 
              threshold = 50, 
              detection = eame_ed)

# optimize_energy_detector(reference = eame_ed, 
#                          files = wavs[1], 
#                          threshold = 50,
#                          min.duration = 1, 
#                          path = dir, 
#                          # smooth = c(100, 250, 350))[, c(1, 3:6, 8:13,18:19)]
#                          smooth = c(100, 250, 350))
eame_ed$song = "song"
eame_params = song_analysis(eame_ed[c(1:132),]) #get syllable/element length metrics
write.csv(eame_params,
          "eame_params.csv", row.names = FALSE)
eame_st = selection_table(eame_ed,
                     path = dir,
                     extended = TRUE
                     ) #does not create extended table...
eame_specan = specan(eame_st,
                     bp = c(2,6))

eame_cc = cross_correlation(eame_ed,
                            wl = 300, ovlp = 90, path = dir) #need top and bottom frequency measurements to do cross correlation.

# Template Detection ------------------------------------------------------
eame_st = read.csv("eame_selection_table.csv",header= TRUE)
eame1_reference <- eame_st[eame_st$sound.files == "20210519_070000_eame_ws06_sswma01.wav", ] #only focusing on lbh1 syllables

correlations_eame <- template_correlator(templates = eame1_reference[1, ], 
                                         files = wavs[2],
                                         path = getwd())

# run detection
detection <- template_detector(template.correlations = correlations_eame, 
                               threshold = 0.4)

detection

setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data/ml_training/templates/eame/test_files/")
tests <- list.files(pattern = ".wav$", ignore.case = TRUE)
y = lapply(tests, readWave)

correlations_eame <- template_correlator(templates = eame1_reference[1, ], 
                                         files = tests[1],
                                         path = getwd())

# run detection
detection <- template_detector(template.correlations = correlations_eame, 
                               threshold = 0.4)

# OLD STUFF!!!! -----------------------------------------------------------


specreator(eame_snr,
           wl = 300,
           flim = c(2, 10), #frequency conditions, will need to change depending on bird species
           it = "jpeg",
           res = 150,
           osci = TRUE,
           ovlp = 90)

lspec(eame_snr, wl = 300, flim = c(2, 10), ovlp = 10, sxrow = 6, rows = 15, it = "jpeg")

eame_params = cbind(eame_params,eame_snr$start,eame_snr$end)
eame_params = eame_params %>%
  rename(start = `eame_snr$start`,
         end = `eame_snr$end`)
eame_st = selection_table(eame_params)

eame_st = read.csv("eame_selection_table.csv", header = TRUE)
# print spectrogram
label_spectro(wave = x[[1]], 
              reference = eame_st[eame_st$sound.files == "20210519_070000_eame_ws06_sswma01.wav", ], 
              hop.size = 10, 
              ovlp = 50, 
              flim = c(2, 5)
)

#The function diagnose_detection() evaluates the performance of a detection routine by comparing it to a reference table. For instance, a perfect detection is given by comparing lbh_reference to itself

eame1_reference <- eame_ed[eame_ed$sound.files == "20210519_070000_eame_ws06_sswma01.wav", ] #only focusing on lbh1 syllables

# diagnose
diagnose_detection(reference = eame1_reference, 
                   detection = eame1_reference)[, c(1:3, 7:8)]

# smooth works by merging the amplitude envelope ‘hills’ of the split signals themselves. It smooths envelopes by applying a sliding window averaging of amplitude values. It’s given in ms of the window size. A smooth of 350 ms can merged back split signals from our example:

# detect sounds
detection_final = NULL
for(i in 1:length(wavs)){
  detection_temp <- energy_detector(files = wavs[1], 
                               threshold = 10, 
                               min.duration = 1,
                               path = dir, 
                               hold.time = 30
  )
  detection_final = rbind(detection_temp,
                          detection_final)
}
detection <- energy_detector(files = wavs[1], 
                             threshold = 10, 
                             min.duration = 1,
                             path = tempdir() , 
                             hold.time = 30
                             )

label_spectro(wave = x[[6]], 
              detection = detection)

dir2 = "C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data/ml_training/templates/eame/test_files/"
test1 = readWave(paste0(dir2,"20210519_070000.wav",sep=""))
split_acoustic_data(dir2)
split_wavs <- list.files(path = dir2, pattern = ".wav$", ignore.case = TRUE)
setwd(dir2)
x2 = lapply(split_wavs, readWave)
# plot spectrogram
for(i in 1:length(x2)){
  label_spectro(wave = x2[[i]], 
                detection = detection_final)
}
  
#need to figure out hwo to expand detection table so more elements are found

# Template based detection ------------------------------------------------
####need to get frerquencies for templates in warbleR!!!
# get correlations
correlations_eame <- template_correlator(templates = eame1_reference[1, ], 
                                    files = wavs[2],
                                    path = dir
)

