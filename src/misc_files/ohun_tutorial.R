
# Araya-Salas, M. (2021), ohun: automatic detection of acoustic signals. R package version 0.1.0.
# Installing Ohun ---------------------------------------------------------

devtools::install_github("maRce10/warbleR") #install latest version of warbleR

library(devtools)
devtools::install_github("maRce10/ohun")

#load package
library(ohun)

# Setting Working Directory -----------------------------------------------

setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data/ml_training/ohun_tutorial/")
dir = "C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data/ml_training/ohun_tutorial/"
# load example data
data("lbh1", "lbh2", "lbh_reference")

lbh_reference
# convert to data frame
as.data.frame(lbh_reference)

# save sound file
writeWave(lbh1, file.path(dir, "lbh1.wav"))

# save sound file
writeWave(lbh2, file.path(dir, "lbh2.wav"))

# print spectrogram
label_spectro(wave = lbh1, 
              reference = lbh_reference[lbh_reference$sound.files == "lbh1.wav", ], 
              hop.size = 10, 
              ovlp = 50, 
              flim = c(1, 10)
              )

# print spectrogram
label_spectro(wave = lbh2, 
              reference = lbh_reference[lbh_reference$sound.files == "lbh2.wav", ], 
              hop.size = 10, 
              ovlp = 50, 
              flim = c(1, 10)
              )

#The function diagnose_detection() evaluates the performance of a detection routine by comparing it to a reference table. For instance, a perfect detection is given by comparing lbh_reference to itself

lbh1_reference <- lbh_reference[lbh_reference$sound.files == "lbh1.wav", ] #only focusing on lbh1 syllables

# diagnose
diagnose_detection(reference = lbh1_reference, 
                   detection = lbh1_reference)[, c(1:3, 7:8)]

#We will work mostly with a single sound file for convenience but the functions can work on several sound files at the time. The files should be found in a single working directory. Although the above example is a bit silly, it shows the basic diagnostic indices, which include basic detection theory indices (‘true.positives’, ‘false.positives’, ‘false.negatives’, ‘sensitivity’ and ‘specificity’) mentioned above. We can play around with the reference table to see how these indices can be used to spot imperfect detection routines (and hopefully improve them!). For instance, we can remove some signals to see how this is reflected in the diagnostics. Getting rid of some rows in ‘detection’, simulating a detection with some false negatives, will affect the sensitivity but not the specificity

# create new table
lbh1_detection <- lbh1_reference[3:9, ] #skipping the first two detections and the last detection

# print spectrogram
label_spectro(wave = lbh1, 
              reference = lbh1_reference, 
              detection = lbh1_detection,
              hop.size = 10, 
              ovlp = 50, 
              flim = c(1, 10)
              )

# diagnose
diagnose_detection(reference = lbh1_reference, 
                   detection = lbh1_detection)[, c(1:3,7:8)]
#get three false negatives now (3 background noise when there should have been detections)

#Having some additional signals not in reference will do the opposite, reducing specificity but not sensitivity. We can do this simply by switching the tables:

# print spectrogram
label_spectro(wave = lbh1, 
              detection = lbh1_reference, 
              reference = lbh1_detection,
              hop.size = 10, 
              ovlp = 50, 
              flim = c(1, 10)
              )

# diagnose
diagnose_detection(reference = lbh1_detection, 
                   detection = lbh1_reference)[, c(1:3,7:8)]
#3 false positives (background noise incorrectly identified as signal)

#The function offers three additional diagnose metrics:

# 1) Split positives: target signals overlapped by more than 1 detecion
# 2) Merged positives: number of cases in which 2 or more target signals in ‘reference’ were overlapped by the same detection
# 3) Proportional overlap of true positives: ratio of the time overlap of true positives with its corresponding signal in the reference table

#In a perfect detection routine split and merged positives should be 0 while proportional overlap should be 1. We can shift the start of signals a bit to reflect a detection in which there is some mismatch to the reference table regarding to the time location of signals:

# create new table
lbh1_detection <- lbh1_reference

# add 'noise' to start
set.seed(18)
lbh1_detection$start <- lbh1_detection$start + rnorm(nrow(lbh1_detection),
                                                     mean = 0,
                                                     sd = 0.1)

## print spectrogram
label_spectro(wave = lbh1, 
              reference = lbh1_reference, 
              detection = lbh1_detection,
              hop.size = 10, 
              ovlp = 50, 
              flim = c(1, 10)
              )

# diagnose
diagnose_detection(reference = lbh1_reference, 
                   detection = lbh1_detection)

#In addition, the following diagnostics related to the duration of the signals can also be returned by setting time.diagnostics = TRUE. Here we tweak the reference and detection data just to have some false positives and false negatives:

# diagnose with time diagnostics
diagnose_detection(reference = lbh1_reference[-1, ], 
                   detection = lbh1_detection[-10,], 
time.diagnostics = TRUE)

#These additional metrics can be used to further filter out undesired signals based on their duration (for instance in a energy-based detection as in energy_detector(), explained below).

#Diagnostics can also be detailed by sound file:
# diagnose by sound file
diagnostic <- diagnose_detection(reference = lbh1_reference, 
                                 detection = lbh1_detection,
                                 by.sound.file = TRUE)

diagnostic

#These diagnostics can be summarized (as in the default diagnose_detection() output) with the function summarize_diagnostic():
# summarize
summarize_diagnostic(diagnostic)


# Detecting signals with ohun ---------------------------------------------

#Energy-based detection
# plot spectrogram and envelope
label_spectro(wave = cutw(lbh1, 
                          from = 0, 
                          to = 1.5, 
                          output = "Wave"), 
              ovlp = 90,
              hop.size = 10, 
              flim = c(0, 10), 
              envelope = TRUE)

#This type of detector doesn’t require highly stereotyped signals, although they work better on high quality recordings in which the amplitude of target signals is higher than the background noise (i.e. high signal-to-noise ratio). The function ernergy_detector() performs this type of detection.

#We can understand how to use ernergy_detector() using simulated signals. We will do that using the function simulate_songs() from warbleR. In this example we simulate a recording with 10 sounds with two different frequency ranges and durations:

# install this package first if not installed install.packages('Sim.DiffProc')

# Creating vector for duration
durs <- rep(c(0.3, 1), 5)

# Creating simulated song
set.seed(12)
simulated_1 <- warbleR::simulate_songs(n = 10, 
                                       durs = durs, 
                                       freqs = 5, 
                                       sig2 = 0.01,
                                       gaps = 0.5, 
                                       harms = 1, 
                                       bgn = 0.1, 
                                       path = tempdir(), 
                                       file.name = "simulated_1",
                                       selec.table = TRUE, 
                                       shape = "cos", 
                                       fin = 0.3, 
                                       fout = 0.35, 
                                       samp.rate = 18)$wave

#The function call saves a ‘.wav’ sound file in a temporary directory (tempdir()) and also returns a wave object in the R environment. This outputs will be used to run energy-based detection and creating plots, respectively. This is how the spectrogram and amplitude envelope of the simulated recording look like:

# plot spectrogram and envelope
label_spectro(wave = simulated_1, 
              env = TRUE, 
              fastdisp = TRUE)

#Note that the amplitude envelope shows a high signal-to-noise ratio of the signals, which is ideal for energy-based detection. This can be conducted using ernergy_detector() as follows:

# run detection
detection <- energy_detector(files = "simulated_1.wav", 
                             bp = c(2, 8), 
                             threshold = 50,
                             smooth = 150, 
                             path = tempdir())

# plot spectrogram and envelope
label_spectro(wave = simulated_1, 
              envelope = TRUE, 
              detection = detection, 
              threshold = 50)

detection

#Now we will make use of some additional arguments to filter out specific signals based on their structural features. For instance we can use the argument minimum.duration to provide a time treshold (in ms) to exclude short signals and keep only the longest signals:

# run detection
detection <- energy_detector(files = "simulated_1.wav", 
                             bp = c(1, 8), 
                             threshold = 50,
                             min.duration = 500, 
                             smooth = 150, 
                             path = tempdir())

# plot spectrogram
label_spectro(wave = simulated_1, 
              detection = detection)


#We can use the argument max.duration (also in ms) to exclude long signals and keep the short ones:

# run detection
detection <- energy_detector(files = "simulated_1.wav", 
                             bp = c(1, 8), 
                             threshold = 50,
                             smooth = 150, 
                             max.duration = 500, #in ms
                             path = tempdir())

# plot spectrogram
label_spectro(wave = simulated_1, detection = detection)


# We can also focus the detection on specific frequency ranges using the argument bp (bandpass). By setting bp = c(5, 8) only those signals found within that frequency range (5-8 kHz) will be detected, which excludes signals below 5 kHz:

# Detecting
detection <- energy_detector(files = "simulated_1.wav", 
                             bp = c(5, 8), 
                             threshold = 50,
                             smooth = 150, 
                             path = tempdir())

# plot spectrogram
label_spectro(wave = simulated_1, detection = detection)

# The same logic can be applied to detect those signals found below 5 kHz. We just need to set the upper bound of the band pass filter below the range of the higher frequency signals (for instance bp = (0, 6)):
# Detect
detection <- energy_detector(files = "simulated_1.wav", 
                             bp = c(0, 6), 
                             threshold = 50,
                             min.duration = 1, 
                             smooth = 150, 
                             path = tempdir())

# plot spectrogram
label_spectro(wave = simulated_1, detection = detection)

# Amplitude modulation (variation in amplitude across a signal) can be problematic for detection based on amplitude envelopes. We can also simulate some amplitude modulation using warbleR::simulate_songs():

# Creating simulated song
set.seed(12)
sim_2 <- sim_songs(n = 10, 
                   durs = durs, 
                   freqs = 5, 
                   sig2 = 0.01, 
                   gaps = 0.5, 
                   harms = 1,
                   bgn = 0.1, 
                   path = tempdir(), 
                   file.name = "simulated_2", 
                   selec.table = TRUE, 
                   shape = "cos",
                   fin = 0.3, 
                   fout = 0.35, 
                   samp.rate = 18, 
                   am.amps = c(1, 2, 3, 2, 0.1, 2, 3, 3, 2, 1))

# extract wave object and selection table
simulated_2 <- sim_2$wave
sim2_sel_table <- sim_2$selec.table

# plot spectrogram
label_spectro(wave = simulated_2, envelope = TRUE)

# When signals have strong amplitude modulation they can be split during detection:

# detect sounds
detection <- energy_detector(files = "simulated_2.wav", threshold = 50, path = tempdir())

# plot spectrogram
label_spectro(wave = simulated_2, envelope = TRUE, threshold = 50, detection = detection)

# There are two arguments that can deal with this: holdtime and smooth. hold.time allows to merge split signals that are found within a given time range (in ms). This time range should be high enough to merge things belonging to the same signal but not too high so it merges different signals. For this example a hold.time of 200 ms can do the trick (we know gaps between signals are ~0.5 s long):

# detect sounds
detection <- energy_detector(files = "simulated_2.wav", 
                             threshold = 50, 
                             min.duration = 1,
                             path = tempdir(), 
                             hold.time = 200)

# plot spectrogram
label_spectro(wave = simulated_2, 
              envelope = TRUE, 
              threshold = 50, 
              detection = detection)

# smooth works by merging the amplitude envelope ‘hills’ of the split signals themselves. It smooths envelopes by applying a sliding window averaging of amplitude values. It’s given in ms of the window size. A smooth of 350 ms can merged back split signals from our example:

# detect sounds
detection <- energy_detector(files = "simulated_2.wav", 
                             threshold = 50, 
                             min.duration = 1,
                             path = tempdir(), 
                             smooth = 350)

# plot spectrogram
label_spectro(wave = simulated_2, 
              envelope = TRUE, 
              threshold = 50, 
              detection = detection,
              smooth = 350)


# Optimizing energy-based detection ---------------------------------------

# This last example using smooth can be used to showcase how the tunning parameters can be optimized. As explained above, to do this we need a reference table that contains the time position of the target signals. The function optimize_energy_detector() can be used finding the optimal parameter values. We must provide the range of parameter values that will be evaluated:

optimize_energy_detector(reference = sim2_sel_table, 
                         files = "simulated_2.wav", 
                         threshold = 50,
                         min.duration = 1, 
                         path = tempdir(), 
                         # smooth = c(100, 250, 350))[, c(1, 3:6, 8:13,18:19)]
                         smooth = c(100, 250, 350))

# The output contains the combination of parameters used at each iteration as well as the corresponding diagnose indices. In this case all combinations generate a good detection (sensitivity & specificity = 1). However, only the routine with the highest smooth (last row) has no split signals (‘split.positive’ column). It also shows a better overlap to the reference signals (‘overlap.to.true.positives’ closer to 1).


# Template-based detection ------------------------------------------------

# This detection method is better suited for highly stereotyped signals. As it doesn’t depend on the signal-to-noise ratio it’s more robust to higher levels of background noise. The procedure is divided in two steps:

# 1) Estimating the cross-correlation scores of templates along sound files (template_correlator())
# 2) Detecting signals by applying a correlation threshold (template_detector())

# get correlations
correlations <- template_correlator(templates = lbh1_reference[1, ], 
                                    files = "lbh1.wav",
                                    path = tempdir()
                                    )

# The output is an object of class ‘template_correlations’, with its own printing method:

# print
correlations

# This object can then be used to detect signals using template_detector():

# run detection
detection <- template_detector(template.correlations = correlations, 
                               threshold = 0.4)

detection

# The output can be explored by plotting the spectrogram along with the detection and correlation scores:

# plot spectrogram
label_spectro(wave = lbh1, 
              detection = detection, 
              template.correlation = correlations$`lbh1.wav-10/lbh1.wav`,
              flim = c(0, 10), 
              threshold = 0.4, 
              hop.size = 10, 
              ovlp = 50)
# The perferformance can be evaluated using the diagnose_detection()
#diagnose
diagnose_detection(reference = lbh1_reference, detection = detection)

# Optimizing template-based detection
# The function optimize_template_detector() allows to evaluate the performance under different correlation thresholds:

# run optimization
optimization <- optimize_template_detector(template.correlations = correlations,
                                           reference = lbh1_reference, 
                                           threshold = seq(0.1, 0.5, 0.1))
# print output
optimization

# Additional threshold values can be evaluated without having to run it all over again. We just need to supplied the output from the previous run with the argument previous.output (the same trick can be done when optimizing an energy-based detection):

# run optimization
optimize_template_detector(template.correlations = correlations, 
                           reference = lbh1_reference,
                           threshold = c(0.6, 0.7), 
                           previous.output = optimization)

# In this case several threshold values can achieved an optimal detection.

# Detecting several templates
# Several templates can be used within the same call. Here we correlate two templates on the two example sound files, taking one template from each sound file:

# get correlations
correlations <- template_correlator(templates = lbh_reference[c(1, 10), ], 
                                    files = c("lbh1.wav","lbh2.wav"),
                                    path = tempdir())

# run detection
detection <- template_detector(template.correlations = correlations, 
                               threshold = 0.5)


# Note that in these cases we can get the same signal detected several times (duplicates), one by each template. We can check if that is the case just by diagnosing the detection:

# diagnose
diagnose_detection(reference = lbh_reference, 
                   detection = detection)

# Duplicates are shown as split positives. Fortunately, we can leave a single detected signal by leaving only those with the highest correlation. To do this we first need to label each row in the detection using label_detection() and then remove duplicates using filter_detection():

# labeling detection
labeled <- label_detection(reference = lbh_reference, 
                           detection = detection)

# This function adds a column (‘detection.class’) with the class label for each row:

table(labeled$detection.class)

# Now we can filter out duplicates and diagnose the detection again, telling the function to select a single row per duplicate using the correlation score as a criterium (by = "scores", this column is part of the template_detector() output):

# filter
filtered <- filter_detection(detection = labeled, by = "scores")

# diagnose
diagnose_detection(reference = lbh_reference, detection = filtered)

# We successfully get rid of duplicates and detected every single target signal.



# Improving Detection Speed -----------------------------------------------

# Detection routines can take a long time when working with large amounts of acoustic data (e.g. large sound files and/or many sound files). These are some useful points to keep in mine when trying to make a routine more time-efficient:

# 1) Always test procedures on small data subsets
# 2) template_detector() is faster than energy_detector()
# 3) Parallelization (see parallel argument in most functions) can significantly speed-up routines, but works better on Unix-based operating systems (linux and mac OS)
# 4) Sampling rate matters: detecting signals on low sampling rate files goes faster, so we should avoid having nyquist frequencies (sampling rate / 2) way higher than the highest frequency of the target signals (sound files can be downsampled using warbleR’s fix_sound_files())
# 5) Large sound files can make the routine crash, use split_acoustic_data() to split both reference tables and files into shorter clips.
# 6) Think about using a computer with lots of RAM memory or a computer cluster for working on large amounts of data
# 7) thinning argument (which reduces the size of the amplitude envelope) can also speed-up energy_detector()

