# Run analysis on field recodings of long-billed hummingbird songs

# clear workspace and load packages
rm(list = ls())

x <- gc()

x <- c( "sound", "parallel", "MASS", "pbapply", "pbmcapply", "ggplot2", "corrplot", "caret", "AppliedPredictiveModeling", "e1071", "randomForest","vegan", "compiler", "smacof", "warbleR", "RColorBrewer", "sna", "network", "GGally", "mclust", "Rraven", "cluster","Rtsne","magrittr","dplyr","clusteval","ade4","colorspace","stringr","fpc")

aa <- lapply(x, function(y) {
  if(!y %in% installed.packages()[,"Package"])  {if(y != "warbleR") install.packages(y) else devtools::install_github("maRce10/warbleR")
  }
  try(require(y, character.only = T), silent = T)
})

## TODO: ADD YOUR LOCAL PATH HERE (wherever you locally saved the data that downloaded from the online repository)
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data/ml_training")

# set warbleR settings
warbleR_options(wl = 300, parallel = 1, ovlp = 90, pb = FALSE, bp = c(0,22), dens = 0.7, img = FALSE)

# load extended selection table with LBH songs
est <- readRDS(file.path(getwd(),"LBH field EST.RDS"))

# Make table with number of copies per song types: 50 songs, 10 of each
tab <- table(est$song.type)
tab

# save original selection table in separate variable so we don't overwrite anything
Y <-est


## ACOUSTIC FEATURE MEASUREMENTS

# spectral parameters
sp <- specan(Y)
# cross-correlation
xc <- xcorr(Y,pb=TRUE,parallel=3)
# MDS cross-correlation
xc.mds <- cmdscale(1 - xc, k = 5)
#rename columns
colnames(xc.mds) <- paste0("xc.dim.", 1:5)
# DTW distance
dtw.dist <- dfDTW(Y,pb=TRUE)
# MDS on DTW distance
dtw.mds <- cmdscale(dtw.dist, k = 5)
#rename columns
colnames(dtw.mds) <- paste0("dtw.dim.", 1:5)
# cepstral coefficients
cps.cf <- mfcc_stats(Y)

# put all acoustic feature measurements together, don't include sound file names
prms <- data.frame(Y[, c("sound.files", "song.type")], sp[, -c(1:4)], xc.mds, dtw.mds, cps.cf[, -c(1:2)])

# remove colinear, boxcox transform and scale/center
preprms <- preProcess(prms[, -c(1:2)], method=c("center", "scale", "BoxCox", "corr"))
# save the transformed acoustic measurments here
trns.prms <- predict(preprms, prms)
# make classification column a factor
trns.prms$elm.type <- as.factor(trns.prms$song.type)

## save acoustic measurements
write.csv(trns.prms, file.path(getwd(),"/Acoustic parameters LBH field data.csv"), row.names = FALSE)
# can just load them here for time savings
trns.prms<-read.csv(file.path(getwd(),"/Acoustic parameters LBH field data.csv"), stringsAsFactors = FALSE)



## SUPERVISED RANDOM FOREST

# remove sound files and elm type columns
trns.prms.only<-trns.prms
trns.prms.only$sound.files <- NULL
trns.prms.only$song.type <- NULL

# set seed to get always the same results
set.seed(27)
# run RF (must be 1000 trees)
rf.elm.type <- randomForest(as.factor(elm.type) ~ ., data = trns.prms.only, ntree = 1000)

# save supervised RF results
saveRDS(rf.elm.type, file.path(getwd(),"/SRF field LBH dataset.RDS"))
rf.elm.type<-readRDS(file.path(getwd(),"/SRF field LBH dataset.RDS"))

# get performance stats on supervised RF
# mean out-of-bag accuracy
m.oob.err <- mean(rf.elm.type$err.rate[,1])
m.oob.err
# 0.036658


# get accuracy
cm <- confusionMatrix(predict(rf.elm.type, trns.prms), as.factor(trns.prms$elm.type))
cm$overall

# feature rankings saved here in case we want to look
imp <- importance(rf.elm.type)


## UNSUPERVISED RANDOM FOREST

# reload acoustic measures
acous.meas<-read.csv(file.path(getwd(),"/Acoustic parameters LBH field data.csv"), stringsAsFactors = FALSE)

# make data frame
ap.df<-acous.meas
ap.df$song <- NULL

# remove label columns and apply transform
###Try to edit this space here and see if you can upload raw sound files
##would need to get sound parameters on raw files first
ap.df.tt<-ap.df[, !names(ap.df) %in% c("sound.files", "song.type", "elm.type")]
pre.ap.trans <- preProcess(ap.df.tt, method = c("BoxCox", "center", "scale"))
ap.trans <- predict(pre.ap.trans, ap.df.tt)

# remove highly correlated vars
corvars <- cor(ap.trans,use="pairwise.complete.obs")
high.corr <- findCorrelation(corvars, cutoff = .95)
print("Removed colinear parameters (r > 0.95)")
names(ap.trans)[high.corr]
ap.trans <- ap.trans[, !names(ap.trans) %in% names(ap.trans)[high.corr]]

# make sure no NAs in array of feature measurements
which(is.na(ap.trans[,1]))
# if no NAs this should output integer(0)

# run RF
ap.trans.urf = randomForest(ap.trans, proximity=T, ntree = 10000)
  
 
# save files to your computer so you don't have to run the RF again 
new.file.name<-file.path(getwd(),"URF field LBH data.rda")
save(ap.trans.urf, file=new.file.name)
load(new.file.name)

# make distance matrix
ap.trans.urf.dist = 1 - ap.trans.urf$proximity

# take a look at the upper left corner and range of values to make sure it looks good
ap.trans.urf.dist[1:5, 1:5]
range(ap.trans.urf.dist)

# see which RF values most important
measurement_ranks<-order(ap.trans.urf$importance, decreasing = TRUE)
# take a look at the top 10
names(ap.trans[measurement_ranks[1:10]])
# note these can change because acoustic features are selected randomly each time the RF model is created

# OOB error rate 
sum(ap.trans.urf$votes[,1] < ap.trans.urf$votes[,2]) / length(ap.trans.urf$votes[,1])



## APPLY CLUSTERING TO DETERMINE SONG TYPES
RFdist = 1 - ap.trans.urf$proximity
dist_data<-as.dist(RFdist)

#how many cluster to have? Allow up to 75
sil_width <- c(NA)
sil_max<-75
for(i in 2:sil_max){
  print(i)
  pam_fit <- pam(RFdist,diss = TRUE,k = i)
  sil_width[i] <- pam_fit$silinfo$avg.width
}
# we use whatever number of clusters gives the highest silhouette width
num_clusts<- which.max(sil_width)

# Plot sihouette width (higher is better)
plot(1:sil_max, sil_width, pch=19,
     xlab = "Number of clusters",
     ylab = "Silhouette Width",
     main=sprintf("real LBH songs\nnum clusts = %i",num_clusts))
lines(1:sil_max, sil_width)

# add line at number of clusters selected
abline(v = num_clusts,col="red",lwd=2, lty=2)


# Use partitioning around medioids to assign songs to clusters
clusters_pam <- pam(1-ap.trans.urf$proximity, k=num_clusts, diss = TRUE)
acous.meas$cluster <- clusters_pam$clustering

# see how many songs are in each cluster
table(acous.meas$cluster)

# compare clustering to truth classes 
# assign numbers to represent the 50 different song types to make it easir to measure accuracy and ARI
acous.meas$truth.class <- rep(1:50, each=10)

# Compute cluster statistics
clust_stats <- fpc::cluster.stats(d = RFdist, as.numeric(acous.meas$truth.class), as.numeric(acous.meas$cluster))

# Figure out which songs of the same type have been placed in the same cluster
tab.out<-table( acous.meas$truth.class, acous.meas$cluster)
corr.assnd<-0
incorr.assnd<-0
for (k in 1:nrow(tab.out)){
  curr.clust<- as.numeric(which.max(tab.out[k,]))
  corr.assnd <-corr.assnd + tab.out[k,curr.clust]
  incorr.assnd<-  incorr.assnd + tab.out[k,] - tab.out[k,curr.clust]
}
# Accuracy
acc <- corr.assnd / nrow(trns.prms)
print(acc)  # 76.4%

# Corrected Rand index
adj.rand.ix<- clust_stats$corrected.rand
print(adj.rand.ix) # 0.7295

# note that these values might vary slightly with each run but will always be approximately the same

# apply tsne to visualize
pam_fit <- pam(dist_data,diss = TRUE,k = num_clusts)
tsne_obj <- Rtsne(dist_data, is_distance = TRUE,perplexity = 10)
tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(acous.meas$cluster),
         name = acous.meas$sound.files)

tsne_data$truth.class <- rep(1:50, each=10)
tsne_data.sorted<-tsne_data[order(tsne_data$truth.class),]

# assign unique colors and shapes for the 50 song types
pch.list<-rep(c(21,22,23,24,25),times=10, each = 10)
col.list.20 <- c("dodgerblue2", "green4",  "#6A3D9A", "gold1", "deeppink1",  
                 "darkturquoise","white", "black", "orchid1", "khaki2", 
                 "gray70", "brown","steelblue4", "maroon","#FF7F00", 
                 "skyblue2", "#FB9A99",  "palegreen2", "#CAB2D6","#FDBF6F")
cols.to.use<- rep(c(rep(col.list.20,times=2),col.list.20[1:10]),each=10)


#  scatter plot showing distribution od song types in 2-D acoustic space
p<- ggplot(aes(x = X, y = Y), data = tsne_data.sorted) +
  geom_point(aes(fill = cols.to.use), size = 3,alpha=0.8, pch=pch.list)+ 
  theme(      axis.line = element_line("grey35"),panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.position="none")+
  theme(plot.background=element_rect(fill = "white"),
        panel.background = element_rect(fill = 'white'),text = element_text(color ="black"))+
  xlab("acoustic space") + ylab(" ") +
     ggtitle(sprintf("Field long-billed hermit songs")) 

p

# scatter plot with ellipses overlaid showing assigned clusters
p + stat_ellipse(aes(x=X, y=Y, group=cluster),color="grey10") 


