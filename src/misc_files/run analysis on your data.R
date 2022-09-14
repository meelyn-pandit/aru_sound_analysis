# Instructions for running the sound analysis pipeline described in 
# "A machine learning approach for classifying and quantifying acoustic diversity"


# IMPORTANT: Comments that begin with "TODO" indicate lines of code that 
# the user must change to analyze sounds on their own machine

# This code will analyze variation among a collection of sound clips (wav files). 
# Each clip should ideally contain a single note, but this code could also be applied to short sound files containing multiple notes


# clear the R workspace
rm(list = ls())

# put any packages we'll need in here
x <- gc()
x <- c( "parallel","vegan", "bioacoustics", "warbleR" , "ggplot2", "Rraven", "cluster","Rtsne","randomForest", "MASS","fossil","pbapply", "adehabitatHR","caret", "data.table","dplyr")

# load packages
aa <- lapply(x, function(y) {
  if(!y %in% installed.packages()[,"Package"])  {
    install.packages(y) 
  }
  try(require(y, character.only = T), silent = T)
})



# TODO: set this to your working directory
### for example: 
curr_dir<-"C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data/ml_training/templates/pabu/"

# Set sound analysis parameters for the warbleR package
# TODO: Change the wavpath to the directory containing your audio clips
# TODO: Change spectrogram parameters(wl, flim, ovlp, and bp) to suit your own sounds
warbleR_options(wav.path = "C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data/ml_training/templates/pabu/",
                wl = 600, 
                flim = c(1.5, 9.5), 
                ovlp = 90, 
                bp = c(3, 7), 
                parallel = parallel::detectCores() - 1)

# Now we'll put the wav clips into an extended selection table, the format the warbleR package needs
# You can also just import a raven selection table if you have one
# Assuming you don't have a Raven selection table, let's load in the sound clips you want to analyze
# TODO: Change "est.file.name" to whatever you'd like to name the table with all wav clips
# TODO: Change "..." to the local path containing your sounds
pabu_est_fn<-file.path(curr_dir,"Extended selection table.RDS")

# get wave file info 
wi <- wav_info(pabu_est_fn)
summary(wi)

# create extended selection table by reading in wav files
est <- selection_table(whole.recs = T, extended = T, confirm.extended = F)

#  save  extended selection table locally
save(est,file=pabu_est_fn)

# Uncomment this line just load the extended selection table if you previously ran the steps above - handy because those steps can be slow!
# est <- readRDS(est.file.name)


# Create image files of spectrograms if you want them.
if (TRUE){
  specreator(est, propwidth = TRUE, parallel = 1,dest.path = curr_dir)
}


# Now let's get feature measurements for each wav file
# You can run most of these functions in parallel, 
# but here we only use 1 core 
# These functions inherit the parameters set in warbleR_options above
# If you don't set est$bottom.freq and est$top.freq to the desired  upper/lower frequency bounds,
# the functions use 0 - Nyquist frequency as a default
# You can also set different frequency bounds for each row in the table if you want (as in a Raven selection table, for example)


# Note that if the sound files are large or if there are lots of them this might be slow
# Measure spectral parameters
sp <- specan(est,parallel=1)
# Spectrogram cross-correlation
xc <- xcorr(est,bp=c(1,5))
# MDS for cross-correlation 
xc.mds <- cmdscale(1 - xc, k = 4) #k must be n-1
# Translate MDS output into 5-D coordinates that we'll use as features
colnames(xc.mds) <- paste0("xc.dim.", 1:4) #changed from 1:5 to 1:4
# Dynamic time warping of frequency contours
dtw.dist <- dfDTW(est, pb=TRUE, parallel = 1)
# MDS on DTW distance 
dtw.mds <- cmdscale(dtw.dist, k = 4)
# Translate DTW MDS into 5-D coordinates we'll use as features
colnames(dtw.mds) <- paste0("dtw.dim.", 1:4)
# Get cepstral coefficients and descriptive statistics
cps.cf <- mfcc_stats(est)

# put all features together in a single matrix
# keep the file names of each sound (here this is just column 1)
prms <- data.frame(est[, c("sound.files")], sp[, -c(1:4)], xc.mds, dtw.mds, cps.cf[, -c(1:2)])

## save feature measurements so we can just load these later and skip feature extraction
write.csv(prms,file.path(curr_dir,"acoustic parameters.csv"),row.names = FALSE)
prms <- read.csv(file.path(curr_dir,"acoustic parameters.csv"), stringsAsFactors = FALSE)

# edit columns names for clarity
old_names <- names(prms)
to_change <-which(old_names %like% "sound_files")
new_names <- old_names
new_names[to_change] <- "sound_files"
colnames(prms) <- new_names
prms = rename(prms, sound.files = est...c..sound.files...)

# ideally you should be able to transform and scale and center variables using the code below
# unfortunately one test user said that these steps threw errors
# skip if necessary and instead run code after next if statement
if (TRUE){
  preprms <- preProcess(prms[, -c(1)], method=c("center", "scale", "BoxCox", "corr"))
  
  # these columns will be removed because they are highly correlated
  corr_cols <-findCorrelation(as.matrix(prms[, -c(1)]), cutoff = 0.9, verbose = FALSE)
  trns_prms <- predict(preprms, est)
  # Remove colinear variables
  nums <- unlist(lapply(trns_prms, is.numeric))  
  trns_prms <- trns_prms[ , nums]
  
  # Remove highly correlated vars
  cm <- cor(trns_prms, use="pairwise.complete.obs")
  high_corr <- findCorrelation(cm, cutoff = .95)
  print("Removed colinear parameters (r > 0.95)")
  names(trns_prms)[high_corr]
  trns_prms <- trns_prms[, !names(trns_prms) %in% names(trns_prms)[high_corr]]
  
  # Save this file so we can skip these steps later
  new_file_name <- file.path(curr_dir,"Transformed non-colinear acous meas.csv")
  write.csv(ap_trans, new_file_name, row.names = FALSE)
  trns_prms <- read.csv( new_file_name, stringsAsFactors = FALSE)
  
  acous_meas <- trns_prms
}

# do this if the above steps give errors. Change FALSE to TRUE to run this line
if (TRUE){
  acous_meas <- prms
}


# Skip this section if you don't have truth labels for each sound file (truth labels might be call types, or some other manual classification)
# If you don't have truth labels but want to add them, uncomment the two lines below and
# manually assign labels.
# trns_prms$Call.Type <- c(1,2,2,3,2)
# trns_prms$elm.type <- as.factor(trns.prms$Call.Type)
acous_meas$Call.Type <- c(1,2,2,3,2)
acous_meas$elm.type <- as.factor(acous_meas$Call.Type)


# Run unsupervised random forest to evaluate pairwise differences between all sound files
# TODO: Optional - you can remove particular feature measurements before running this if you know they're unhelpful
ap_trans_urf = randomForest(acous_meas[,-c(1)], proximity=T, ntree = 10000)


# Save random forest so we can easily load it later
new_file_name <- file.path(curr_dir,"ap_trans_urf.rda")
save(ap_trans_urf, file = new_file_name)
load(new_file_name)

# Make distance matrix from URF output
ap_trans_urf_dist = 1 - ap_trans_urf$proximity

# Take a look at the fist few rows 
ap_trans_urf_dist[1:5, 1:5]
range(ap_trans_urf_dist)

# See which variables are most useful for finding structure in the data
measurement_ranks <- order(ap_trans_urf$importance, decreasing = TRUE)
# Look at top ten most useful variables
names(acous_meas[measurement_ranks[1:10]])

# OOB error rate - gives an idea of how well data separates into discrete classes
sum(ap_trans_urf$votes[,1] < ap_trans_urf$votes[,2]) / length(ap_trans_urf$votes[,1])

# Get distance matrix between sound files
RFdist = 1 - ap_trans_urf$proximity
dist_data <- as.dist(RFdist)


# Cluster the distance matrix to group similar sounds together
# Many ways to do this, here we use PAM
sil_width <- c(NA)
# TODO: Change this from 10 to the max number of cluster you want to allow (e.g. maximum expected repertoire size)
sil_max <- 3 
for(i in 2:sil_max){
  print(i)
  pam_fit <- pam(RFdist,diss = TRUE,k = i)
  sil_width[i] <- pam_fit$silinfo$avg.width
}

# Find the optimal number of clusters
num_clusts<- which.max(sil_width)

# Take a look at how well the data clusters
plot(1:sil_max, sil_width, pch=19,
     xlab = "Number of clusters",
     ylab = "Silhouette Width",
     main=sprintf("optimal number clusters = %i",num_clusts))
lines(1:sil_max, sil_width)


# Visualize clustered  sound files using t-SNE
# We use 2-D t-SNE here
clusters_pam <- pam(1-ap_trans_urf$proximity, k=num_clusts, diss = TRUE)
acous_meas$cluster <- clusters_pam$clustering
pam_fit <- pam(dist_data,diss = TRUE,k = num_clusts)
# note with very small datasets you might have to change the perplexity
tsne_obj <- Rtsne(dist_data, is_distance = TRUE,perplexity = 1)
tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(acous_meas$cluster),
         name = acous_meas$sound.files)

# Note that tsne distorts distances!
ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(size = 2, pch=19,aes(color = as.factor(tsne_data$cluster))) +
  theme_classic() +
  ggtitle(sprintf("assigned num clusts = %i",num_clusts)) +
  xlab("acoustic space") + 
  ylab(" ") +
  theme(legend.position="none") 


# optional - make same plot with points labeled
if (FALSE){
  ggplot(aes(x = X, y = Y), data = tsne_data) +
    geom_point(size = 2, pch=19,aes(color = as.factor(tsne_data$cluster))) +
    theme_classic() +
    ggtitle(sprintf("assigned num clusts = %i",num_clusts)) +
    xlab("acoustic space") + 
    ylab(" ") +
    theme(legend.position="none") + 
    geom_text(aes(label = acous.meas$sound.files), hjust = 0.5,  vjust = -1)
}
  
  
# Visualize non-distored distribution in 2-D 
# This looks messier but remember we are reducing dimensionality
mds_2d <- cmdscale(RFdist, k = 2)
tsne_data$mds1 <- mds_2d[,1]
tsne_data$mds2 <- mds_2d[,2]

ggplot(aes(x = mds1, y = mds2), data = tsne_data) +
  geom_point(size = 2, pch=19,aes(color = as.factor(tsne_data$cluster))) +
  theme_classic() +
  ggtitle(sprintf("assigned num clusts = %i",num_clusts)) +
  xlab("acoustic space") + 
  ylab(" ") +
  theme(legend.position="none")


# Calculate acoustic space occupancy
# We use 2-D MDS here
mds <- isoMDS(RFdist, y = cmdscale(RFdist, k = 2), k = 2, maxit = 5, trace = FALSE, tol = 1e-3, p = 2)

# Standardize to 0-1 range
mds$points[, 1] <- mds$points[, 1] / max(mds$points[, 1])
mds$points[, 2] <- mds$points[, 2] / max(mds$points[, 2])

# Put truth labels and MDS vectors together
# note that you may want to esit the variable assigned as the label
acous_meas = setNames(acous_meas, replace(names(acous_meas), names(acous_meas) == 'est...c..sound.files...', 'sound.files'))
labels <- acous_meas$sound.files
Y <- data.frame(id = 1:nrow(mds$points), labels, X1 = mds$points[, 1], X2 = mds$points[, 2])
coords <- SpatialPoints(Y[, c("X1", "X2")])

# This produces the acoustic area estimate
area <- mcp(coords)



