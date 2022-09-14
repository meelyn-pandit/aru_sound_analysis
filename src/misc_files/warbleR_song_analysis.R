################################################################
### Song Variation Across American southwest############
################################################################

setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data/ml_training/templates/pabu") #Set working directory, yours will be differernt than what is here. You can set it using RStudio under the Files tab in the lower right window.

library(warbleR)
library(Rraven)



###Change your Directory to whichever folder has the .wav song files.
wavs <- list.files(pattern = ".wav$", ignore.case = TRUE)

# Set a seed so we all have the same results
set.seed(1)
sub <- wavs[sample(1:length(wavs), 3)]

# Autodetect Syllables ----------------------------------------------------

# Run autodetec() on subset of recordings
# The data frame object output is printed to the console, we are not saving this in an object yet, since we are just playing around with argument settings
# you can run this in parallel to speed up computation time
###This will give us syllable diversity rather than number of song bouts
autodetec(flist = sub, 
          bp = c(2, 10), #frequency bandwidth
          threshold = 3, 
          mindur = 0.03, #minimum syllable duration (s)
          maxdur = 4, #maximum syllable duration
          envt="abs", 
          ssmooth = 300, 
          ls = TRUE, 
          res = 100, 
          flim = c(1, 12), 
          wl = 300, 
          set = TRUE, 
          sxrow = 6, 
          rows = 15, 
          redo = FALSE,
          fast.spec = TRUE)

#Species specific autodetection parameters
pabu.ad = autodetec(flist = wavs, 
          bp = c(1, 9), 
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
#Best detection parameters based on warbleR

  # autodetec(flist = sub,
  #                   bp = c(2, 10), 
  #                   threshold = 3, 
  #                   mindur = 0.03, 
  #                   maxdur = 4, 
  #                   envt = "abs", 
  #                   ssmooth = 900, 
  #                   ls = TRUE, 
  #                   res = 100, 
  #                   flim= c(1, 12), 
  #                   wl = 300, 
  #                   set =TRUE, 
  #                   sxrow = 6, 
  #                   rows = 15, 
  #                   redo = TRUE, 
  #                   it = "tiff", 
  #                   img = TRUE, 
  #                   smadj = "end",
  #                   fast.spec = TRUE)

#Take a look at number of selections per sound file:
table(pabu.ad$sound.files)

#Exclude any rows with NAs in them
pabu.ad = na.exclude(pabu.ad)

pabu_st = selection_table(X=pabu.ad, pb = FALSE)

# Use Signal Noise Ratio to filter automatically selected signals ---------
#signal to noise ratio (SNR) can be a useful filter for automated signal deteection. When background noise is detected as a signal it will have a low SNR. SNR = 1 means the signal and background noise have the same amplitude.
# A margin that's too large causes other signals to be included in the noise measurement
# Re-initialize X as needed, for either autodetec or manualoc output

# Try this with 10% of the selections first
# Set a seed first, so we all have the same results
set.seed(5)

X <- pabu.ad[sample(1:nrow(pabu.ad),(nrow(pabu.ad)*0.05)), ]
nrow(X)

snrspecs(X = X, 
         flim = c(2, 10), 
         snrmar = 0.04, 
         mar = 0.7, 
         it = "jpeg")


# Calculate SNR for automatically selected signals ------------------------
#Once we've picked up an SNR margin we can move forward withthe SNR calculation. We will measure SNR on everyother selection to speed up the process.

# pabu.snr <- sig2noise(X = pabu.ad[seq(1, nrow(pabu.ad), 2), ], mar = 0.04) #takes every other signal from the pabu_ad dataframe and creates a separate one from it
# 
# pabu.snr <- sig2noise(X = pabu.ad[seq(length.out = 10), ], mar = 0.04) #takes 10 rows from the pabu_ad dataframe and creates a separate one from it

pabu.snr <- sig2noise(X = pabu.ad, mar = 0.04) #takes signal-to-noise ratio for the entire pabu dataset.


##We just need a few songs to characterize individuals (here sound files are equivalent to different individuals), we can choose selections with the highest SNR per sound file. In this example, we will choose 5 selections per recording with the highest SNRs.

pabu.hisnr <- pabu.snr[ave(-pabu.snr$SNR, pabu.snr$sound.files, FUN = rank) <= 5, ] #Creates a subset of snr dataframe with only syllables that have an SNR ratio of 5 or higher

#pabu.hisnr = pabu.snr #renames pabu.snr dataframe to pabu.hisnr dataframe

#save the selections as a physical file
write.csv(pabu.hisnr, "pabu_hisnr.csv", row.names = FALSE)


# Manually select signals with manualoc -----------------------------------
# #manualoc is a function that provides an interactive interface to select signals. This function makes a selection table as a .csv file in your working directory.
# 
# #manualoc now opens an external graphics device for selecting signals. You can stop the function at anypoint by clicking twice on the stop button.
# 
# # Run manualoc() with frequency range set for desired bird (flim = c(2,10))
# # Recording comments are enabled to mark recording quality
# # Selection comments enabled to include visual classifications
# manualoc(flim = c(1, 6), reccomm = TRUE, selcomm = TRUE, osci = TRUE, seltime = 2)
# 
# # Read manualoc() output back into R as an object
# # This data frame object can be used as input for later functions
# manualoc_out <- read.csv("manualoc_output.csv", header = TRUE)

###########################################################################
# Visual Inspection and Signal Classification -----------------------------
###########################################################################


# Find Overlapping Selections ---------------------------------------------

#Overlapping selections can sometimes arise after selecting signals using other functions or software. The function below helps you detect overlapping signals in your selection table, and has arguments that you can play around with for overlap detection, renaming or deleting overlapping selections.

pabu.hisnr = read.csv("pabu_hisnr.csv",header = TRUE)
str(pabu.hisnr)
head(pabu.hisnr, n = 15)

# yields a data frame with an additional column (ovlp_sels) that indicates which selections overlap
pabu.hisnr <- ovlp_sels(X = pabu.hisnr, max.ovlp = 0)

# run the function again but this time retain only the signals that don't overlap
pabu.hisnr <- ovlp_sels(X = pabu.hisnr, max.ovlp = 0, drop = TRUE)


# Make Spectrograms of selections -----------------------------------------

#"specreator" generates spectrograms of individual selected signals. These image files can be used to filter out selections that were poorly made or represent signals that are not relevant to your analysis. This quality control step is important for visualizing your selected signals after any selection method, even if you imported your selections from Raven or Syrinx.

#This will create new images in your working directory that you will need to open individually to make sure the spectrograms are there/correct.

specreator(pabu.hisnr,
           wl = 300,
           flim = c(2, 10), #frequency conditions, will need to change depending on bird species
           it = "jpeg",
           res = 150,
           osci = TRUE,
           ovlp = 90)

#Inspect spectrograms and throw away image files that are poor quality to prepare for later steps. 
#Make sure you are working in a directory that only has image files associateed with this vignette.
#Delete the imate files corresponding to recording 154070 selection 8 and 154070 selection 12, as the start coordinates for these selections are not accurate (no idea what this means).


# Remove selections with missing image files ------------------------------

# remove selections after deleting corresponding image files
pabu.hisnr2 <- filtersels(pabu.hisnr, it = "jpeg", incl.wav = TRUE)
nrow(pabu.hisnr2)


# Check Selections --------------------------------------------------------

# if selections can be read, "OK" will be printed to check.res column
checksels(pabu.hisnr2, check.header = FALSE)

#if selections cannot be read, it is possible the sound files are corrupt. If so use the "fixwavs" function to repair wav files.


# Tailor temporal coordinates of selections -------------------------------

#Sometimes the start and end times of selected signals need fine-tuned adjustments. This is particularly true when signals are found within bouts of closely delivered sounds that may be hard to pull apart.
#"seltailor" provides an interactive interface similar to manualoc for tailoring the temporal coordinates of selections.

seltailor(pabu.hisnr2, wl = 300, flim = c(2,10), wn = "hanning", mar = 0.1, osci = TRUE, title = c("sound.files", "selec"), auto.next = TRUE)

# Read in seltailor output after renaming the csv file
pabu.hisnrt <- read.csv("seltailor_output.csv", header = TRUE)
str(pabu.hisnrt)

###########################################################################
# Visual Classification of selected signals -------------------------------
###########################################################################


# Print long spectrograms with lspec --------------------------------------
#The function "lspec" can be a tool for visually classifying signals. 
#Long spectrograms can be printed to classify signals by hand or comments accompanying the selections can be printed over selected signals.

#Here we print the start and end of signals with a red dotted line, and the selection number printed over the signal.
#If a selection data frame contains a comments column, these will be printed with the selection number

# highlight selected signals
lspec(pabu.hisnrt, wl = 300, flim = c(2, 10), ovlp = 10, sxrow = 6, rows = 15, it = "jpeg")

# concatenate lspec image files into a single PDF per recording
# lspec images must be jpegs 
lspec2pdf(keep.img = FALSE, overwrite = TRUE) #does not seem to work, keep getting "Error in dir.exists(path) : invalid filename argument"


# Highlight spectrogram regions with color.spectro ------------------------
#color.spectro allows you to highlight selections you've made within a short region of a spectrogram. 

# we will use Phaethornis songs and selections from the warbleR package
data(list = c("20210518_070000_pabu_ws02_sswma01", "pabu.hisnrt"))
writeWave(20210518_070000_pabu_ws02_sswma01, "20210518_070000_pabu_ws02_sswma01_2.wav") #save sound files 

# subset selection table
# already contains the frequency range for these signals
st <- selec.table[selec.table$sound.files == "Phae.long1.wav",]
st = pabu.hisnrt[pabu.hisnrt$sound.files == "20210518_070000_pabu_ws02_sswma01.wav"]
st = pabu.hisnrt %>%
  filter(sound.files == "20210518_070000_pabu_ws02_sswma01.wav")

# read wave file as an R object
sgnl <- tuneR::readWave(as.character(st$sound.files[1]))

# create color column
st$colors <- c("red", "orange","yellow", "blue", "green")

# highlight selections
color.spectro(wave = sgnl, wl = 300, ovlp = 90, flim = c(1, 8.6), collevels = seq(-90, 0, 5), dB = "B", X = st, col.clm = "colors", base.col = "skyblue",  t.mar = 0.07, f.mar = 0.1)


# Make Lexicons of Signals ------------------------------------------------

# create a column of recording IDs for friendlier catalog labels
rec_ID <- sapply(1:nrow(pabu.hisnrt), function(x){
  # gsub(x = strsplit(as.character(pabu.hisnrt$sound.files[x]), split = "-")[[1]][3], pattern = ".wav$", replacement = "")
  paste0(substr(as.character(pabu.hisnrt$sound.files[x]),32,33),"_",pabu.hisnrt$selec[x],sep="")
  }
)
rec_ID

pabu.hisnrt$rec_ID <- rec_ID
str(pabu.hisnrt)

# set color palette
# alpha controls transparency for softer colors
cmc <- function(n) cm.colors(n, alpha = 0.8)

catalog(X = pabu.hisnrt, 
        flim = c(2, 10), 
        nrow = 5, 
        ncol = 3,
        height = 10, 
        width = 10, 
        tag.pal = list(cmc), 
        cex = 0.8, 
        same.time.scale = TRUE, 
        mar = 0.01, 
        wl = 300, 
        gr = FALSE, 
        labels = "rec_ID", 
        tags = "rec_ID", 
        hatching = 1, 
        group.tag = "rec_ID", 
        spec.mar = 0.4, 
        lab.mar = 0.8, 
        max.group.cols = 5)

catalog2pdf(keep.img = FALSE, overwrite = TRUE)

# assuming we are working from the warbleR_example directory
# the ~/ format does not apply to Windows
# make sure you have already moved or deleted all other pdf files
move.imgs(from = ".", it = "pdf", create.folder = TRUE, folder.name = "Catalog_image_files")

# Obtain Song Parameters --------------------------------------------------

# measure acoustic parameters
params <- specan(pabu.hisnrt, bp = c(2, 10), threshold = 3)
write.csv(params, "acoustic_parameters.csv", row.names = FALSE)

#sp_param <- specan(X = pabu.ad, bp = c(1,7), path = tempdir())
sp_param <- specan(X = pabu.hisnrt, bp = c(1,7))



# measuring peakf
sp_param <- specan(X = pabu.hisnrt, bp = c(2,10), fast = FALSE)


# measuring harmonic-related parameters using progress bar
sp_param <- specan(X = pabu.hisnrt, harmonicity = TRUE, path = "C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data/ml_training/templates/pabu")


# Acoustic Similarity -----------------------------------------------------
# We can evaluate whether or not the observed variation in song structure is reflected by the meters we just measured. For this we will conduct a Principal Component Analysis on scaled (z-transformed) meters and look at the grouping of songs (data points) in the scatter plot.
# Run the PCA with only numeric variables of params
pca <- prcomp(x = params[, sapply(params, is.numeric)], scale. = TRUE)

# Check loadings
summary(pca)

# Extract PCA scores
pcascor <- as.data.frame(pca[[5]])

params$selec.id = paste0(substr(as.character(params$sound.files),32,33),"_",params$selec,sep="")
# Plot the 2 first PCs
plot(pcascor[, 1], pcascor[, 2], col = as.numeric(as.factor(params$sound.files)), pch = 20, 
     cex = 1, xlab = "PC1", ylab = "PC2", )
legend("bottomright",
       params$selec.id)

# Add recordings/individuals labels 
x <- tapply(pcascor[, 1], params$sound.files, mean)
y <- tapply(pcascor[, 2], params$sound.files, mean)

labs <- gsub(".wav", "", unique(sapply(as.character(params$sound.files), function(x){
  # strsplit(x, split = "-", fixed = TRUE)[[1]][3]
  paste0(substr(as.character(params$sound.files),32,33),"_",params$selec,sep="")
}, USE.NAMES = FALSE)))
# labs = params$selec
  # paste0(substr(as.character(params$sound.files),32,33),"_",params$selec,sep="")

text(x, y, labs, cex = 0.75)

# Songs are grouped by sound file. As each sound file represents a single individual, this suggests that songs have individual signatures. Let’s look at the song type level. First, we need to classify the songs by song type. We can check the spectrograms we previously created to do this.

# Create a song type variable

# First, extract recording ID
songtype <- gsub(".wav", "", sapply(as.character(params$sound.files), function(x){
  # strsplit(x, split = "-", fixed = TRUE)[[1]][3]
  paste0(substr(as.character(params$sound.files),32,33),"_",params$selec,sep="")
}, USE.NAMES = FALSE))

# Now change IDs for letters representing song types
songtype <- gsub("01_2|02_3|03_2", "A", songtype)
songtype <- gsub("01_3|03_3|04_4|05_4", "B", songtype)
songtype <- gsub("01_4|03_4|04_5", "C", songtype)
songtype <- gsub("01_6|01_7|03_7|05_8|04_8", "C", songtype)

# Add song type as a variable representing symbol type
plot(pcascor[, 1], pcascor[, 2], col = as.numeric(as.factor(params$sound.files)), 
     pch = as.numeric(as.factor(songtype)), 
     cex = 1, xlab = "PC1", ylab = "PC2")

# Add song type labels 
x <- tapply(pcascor[, 1], songtype, mean)
y <- tapply(pcascor[, 2], songtype, mean)

text(x, y, unique(songtype), cex = 1)