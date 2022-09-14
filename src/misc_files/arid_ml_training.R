# For template matching using spectrogram cross-correlation one would typically use the following functions in the order given:
# 1. makeCorTemplate to make the template(s)
# 2. combineCorTemplates to combine templates together in a single template list
# 3. corMatch to calculate scores
# 4. findPeaks to and peaks and identify detections
# 5. plot to see the scores and detections
# 6. getDetections to get the (numeric) detection results
# 7. templateCutoff to change template cutoffs in the detection list object (iteratively with plot and getDetections)

library(seewave)
library(tuneR)
library(monitoR)

#Renaming field recordings so monitoR can read time stamp
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data/ml_training/ws06/")

old_files = list.files(
  # "data/ml_training/ws06/",
  pattern = "*.WAV",
  full.names = TRUE)

new_files = NULL
for(i in (old_files)){
  year = substr(i,3,6)
  month = substr(i,7,8)
  day = substr(i,9,10)
  hour = substr(i,12,13)
  if(substr(i,14,15)=="00"){
    min = "02"
  } else {
    min = as.numeric(substr(i,14,16))+2
  }
  sec = substr(i,16,17)
  fn = paste0("survey",
              year,
              "-",
              month,
              "-",
              day,
              "_",
              hour,
              min,
              sec,
              "_CST.wav",
              sep = "")
  print(fn)
  new_files = rbind(new_files,fn)
}

file.copy(from = old_files, to = new_files)


# Sound Templates and Analysis --------------------------------------------
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis")

eame = readWave("data/ml_training/templates/eame.wav")
eame2 = readWave("data/ml_training/templates/eame2.wav")

# Saving WAV files --------------------------------------------------------

# eame.fp = file.path(tempdir(), "eame.wav")
# survey.fp <- file.path(tempdir(), "survey2021-05 -19_070000_CST.wav") #fake datetime for the sake of the exercise


eame.fp = file.path("data/ml_training/templates/", "eame.wav")
eame2.fp = file.path("data/ml_training/templates/", "eame2.wav")
eame3.fp = file.path("data/ml_training/templates/", "eame3.wav")


survey.fp = file.path("data/ml_training/ws06","survey2021-05-19_070200_CST.wav")
survey2.fp = file.path("data/ml_training/ws06","survey2021-05-19_080200_CST.wav")
survey3.fp = file.path("data/ml_training/ws06","survey2021-05-19_090200_CST.wav")

emt1 = makeCorTemplate(eame.fp,
                       frq.lim = c(2, 6),
                       t.lim = c(0.1,1.5),
                       name = "e1")
emt2 = makeCorTemplate(eame2.fp, 
                       frq.lim = c(2, 6),
                       name = "e2")
emt3 = makeCorTemplate(eame3.fp, 
                       frq.lim = c(2, 6),
                       name = "e3")
ctemps <- combineCorTemplates(emt1,emt2,emt3)
plot(ctemps)

templateCutoff(ctemps) = c(e1 = 0.2,
                           e2 = 0.2,
                           e3 = 0.2)



cscores <- corMatch(survey.fp, ctemps)
cscores
cdetects <- findPeaks(cscores)
cdetects
getDetections(cdetects)
dev.new()
plot(cdetects)


# Binary Point Matching ---------------------------------------------------
ebt1 <- makeBinTemplate(eame.fp, 
                        amp.cutoff = -30, #setting amplitude cutoff to -40
                        frq.lim = c(2,5),
                        buffer = 1,
                        name = "e1")
ebt2 <- makeBinTemplate(eame2.fp, 
                        amp.cutoff = -30, 
                        # t.lim = c(0, 1.5),
                        frq.lim = c(2, 5), 
                        buffer = 2, #buffer is...?
                        name = "e2")

#combine templates
btemps <- combineBinTemplates(ebt1, ebt2)
btemps #templates have "on.points" and "off.points"
templateCutoff(btemps) <- c(e1 = 6, 
                            default = 4) #change wtemplate to 7, and keep the default cutoff to 4

bscores <- binMatch(survey2.fp, btemps) #calculate scores with binMatch
bscores

bdetects <- findPeaks(bscores) #find detections using bscores
bdetects
dev.new()
plot(bdetects)
