# Run analysis on synthetic budgie dataset with 5-15 unique elements

# clear workspace and load packages
rm(list = ls())
x <- gc()

x <- c( "sound", "parallel", "MASS", "pbapply", "pbmcapply", "ggplot2", "corrplot", "caret", "AppliedPredictiveModeling", "e1071", "randomForest","vegan", "compiler", "smacof", "warbleR", "RColorBrewer", "sna", "network", "GGally", "mclust", "Rraven", "cluster","Rtsne","magrittr","dplyr","clusteval","ade4","colorspace","stringr", "fpc")

aa <- lapply(x, function(y) {
  if(!y %in% installed.packages()[,"Package"])  {if(y != "warbleR") install.packages(y) else devtools::install_github("maRce10/warbleR")
  }
  try(require(y, character.only = T), silent = T)
})

## TODO: ADD YOUR LOCAL PATH HERE (wherever you locally saved the data that downloaded from the online repository)
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data/ml_training")

# set warbleR settings
warbleR_options(wl = 300, 
                parallel = 20, 
                ovlp = 90, 
                pb = FALSE, 
                bp = c(0,22), #changed it from "frange" to c(0,22) 
                dens = 0.7, 
                img = FALSE)


# load extended selection tables with synthetic calls. This file is big.set
sim.ests <- readRDS(file.path(getwd(), "Synth budgie small repertoire 24 ESTs.RDS") )


# intialize variables to save results as we go through loop
clust_est<-NULL
plot_list<-list()
m.oob.unsup<-NULL
m.oob.sup<-NULL
clust_est<-NULL
true_rep_size_all<-NULL
accuracy_all<-NULL
adj_rand_all<-NULL

# now we loop through the synthetic datasets and run the analysis on each
# this takes a while
for (i in 1:length(sim.ests)){
  
  print(i)
  #  curr.folder<-file.path(getwd())
  
  # load current dataset
  Y <- sim.ests[[i]]
  Y$sound.files<-as.character(Y$sound.files)
  
  # GET ACOUSTIC FEATURES
  
  # measure spectral parameters
  sp <- specan(Y,parallel = 20)
  # cross-correlation
  xc <- xcorr(Y,pb=TRUE,parallel=20)
  # MDS cross-correlation
  xc.mds <- cmdscale(1 - xc, k = 5)
  #rename columns
  colnames(xc.mds) <- paste0("xc.dim.", 1:5)
  # DTW distanc
  dtw.dist <- dfDTW(Y,pb=TRUE,parallel=20)
  # MDS on DTW distance
  dtw.mds <- cmdscale(dtw.dist, k = 5)
  #rename columns
  colnames(dtw.mds) <- paste0("dtw.dim.", 1:5)
  # cepstral coefficients
  cps.cf <- mfcc_stats(Y,parallel  = 20)
  # put data together
  prms <- data.frame(Y[, c("sound.files", "elm.type")], sp[, -c(1:4)], xc.mds, dtw.mds, cps.cf[, -c(1:2)])
  # remove colinear, boxcox transform and scale/center
  preprms <- preProcess(prms[, -c(1:2)], method=c("center", "scale", "BoxCox", "corr"))
  trns.prms <- predict(preprms, prms)
  # make classification column a factor
  trns.prms$elm.type <- as.factor(trns.prms$elm.type)
  # remove sound files column
  trns.prms$sound.files <- NULL
  
  # RUN SUPERVISED RF
  set.seed(27)
  #  (use 1000 trees)
  trns.prms$elm.type<-as.factor(trns.prms$elm.type)
  rf.elm.type <- randomForest(elm.type ~ ., data = trns.prms, ntree = 1000)
  # mean out-of-bag accuracy
  m.oob.err <- mean(rf.elm.type$err.rate[,1])
  m.oob.sup[i]<-m.oob.err
  # get accuracy
  cm <- confusionMatrix(predict(rf.elm.type, trns.prms), trns.prms$elm.type) 
  cm$overall
  # importance
  imp <- importance(rf.elm.type)
  # save out of bag error
  m.oob.err <- mean(rf.elm.type$err.rate[,1])
  m.oob.sup[i]<-m.oob.err
  
  # RUN UNSUPERVISED RF
  
  # reload feature measurements
  acous.parms <- prms
  # remove sound file names and element types
  acous.parms <- acous.parms[, -c(1:2)]
  # remove highly correlated vars 
  cm <- cor(acous.parms,use="pairwise.complete.obs")
  high.corr <- findCorrelation(cm, cutoff = .95)
  print("Removed colinear parameters (r > 0.95)")
  names(acous.parms)[high.corr]
  acous.parms <- acous.parms[, !names(acous.parms) %in% names(acous.parms)[high.corr]]
  
  # make sure this didn't put NAs in our data
  which(is.na(acous.parms[,1]))
  
  # run RF
  ap.trans.urf = randomForest(acous.parms, proximity=T, ntree = 10000)
  
  # make distance matrix
  ap.trans.urf.dist = 1 - ap.trans.urf$proximity
  ap.trans.urf.dist[1:5, 1:5]
  range(ap.trans.urf.dist)
  
  # see which RF values most important
  measurement_ranks<-order(ap.trans.urf$importance, decreasing = TRUE)
  # take a look if you'd like, commented out for now
  # names(acous.parms[measurement_ranks[1:10]])
  
  # OOB error rate 
  oob.unsup<-sum(ap.trans.urf$votes[,1] < ap.trans.urf$votes[,2]) / length(ap.trans.urf$votes[,1])
  # save out of bag error 
  m.oob.unsup[i]<-oob.unsup
  
  # estimate repertoire size
  RFdist = 1 - ap.trans.urf$proximity
  dist_data<-as.dist(RFdist)
  
  # get info about true repertoire size from dataset name
  true_rep_size <- suppressWarnings(as.numeric(str_sub(names(sim.ests[i]),-2,-1)))
  if (is.na(true_rep_size)){
    true_rep_size<-5
  }
  true_rep_size_all[i]<-true_rep_size
  
  # apply limits to the range of repertoire sizes we will test for based on the true repertoire size
  sil_min<-true_rep_size - 5
  sil_max<-true_rep_size + 5
  sil_min<-max(sil_min,3)
  sil_width <- c(NA)
  
  # Caluclate silhouette widtths to determine optimal # of clusters
  for(j in sil_min:sil_max){
    print(j)
    pam_fit <- pam(RFdist,diss = TRUE,k = j)
    sil_width[j] <- pam_fit$silinfo$avg.width
  }
  
  # save optimal number of clusters as estimated repertoire size
  num_clusts<- which.max(sil_width)
  clust_est[i]<-num_clusts
  
  # Plot sihouette width 
  # commented for now but change to TRUE if you want to make plots
  if (FALSE){
    new.file.name<-sprintf("%s/synth BUDGIE small dataset %i silhouette width.jpg", getwd(), i)
    jpeg(new.file.name, width = 350, height = 350)
    plot.lim <- sil_max-length(sil_width)+1
    plot(plot.lim:sil_max, sil_width, pch=19,
         xlab = "Number of clusters",
         ylab = "Silhouette Width",
         main= sprintf("BUDGIE small syth dataset %i\n%s\nnum clusts = %i", i,names(sim.ests[i]),num_clusts))
    lines(plot.lim:sil_max, sil_width)
    dev.off()
  }
  
  # assign similar signals to clusters
  clusters_pam <- pam(1-ap.trans.urf$proximity, k=num_clusts, diss = TRUE)
  
  # reload feature measurements
  acous.parms <- prms
  # add assigned cluster values as column
  acous.parms$cluster <- clusters_pam$clustering
  
  # apply tsne to visualize
  pam_fit <- pam(dist_data,diss = TRUE,k = num_clusts)
  tsne_obj <- Rtsne(dist_data, is_distance = TRUE,perplexity = 15)
  tsne_data <- tsne_obj$Y %>%
    data.frame() %>%
    setNames(c("X", "Y")) %>%
    mutate(cluster = factor(acous.parms$cluster),
           name = acous.parms$sound.files)
  
  # save  scatter plot in case we want to take a look later
  plot_list[[i]] <-  ggplot(aes(x = X, y = Y), data = tsne_data) +
    geom_point(aes(color = cluster), size = 2, pch=19)+ 
    theme(      axis.line = element_line("grey35"),panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    theme(legend.position="none")+
    theme(plot.background=element_rect(fill = "black"),
          panel.background = element_rect(fill = 'black'),text = element_text(color ="white"))+
    ggtitle(sprintf("BUDGIE small syth dataset %i\n%s\nnum clusts = %i", i,names(sim.ests[i]),num_clusts)) +
    xlab("acoustic space") + ylab(" ")
  
  
  # GET ACCURACY AND ADJUSTED RAND INDEX
  
  # compare clustering to truth classes 
  # assign numbers to represent the  different song types to make it easier to measure accuracy and ARI
  acous.parms$truth.class <- acous.parms$elm.type
  
  # Compute cluster statistics
  clust_stats <- fpc::cluster.stats(d = RFdist, as.numeric(acous.parms$truth.class), as.numeric(acous.parms$cluster))
  
  # Figure out which songs of the same type have been placed in the same cluster
  tab.out<-table( acous.parms$truth.class, acous.parms$cluster)
  corr.assnd<-0
  incorr.assnd<-0
  for (k in 1:nrow(tab.out)){
    curr.clust<- as.numeric(which.max(tab.out[k,]))
    corr.assnd <-corr.assnd + tab.out[k,curr.clust]
    incorr.assnd<-  incorr.assnd + tab.out[k,] - tab.out[k,curr.clust]
  }
  # Accuracy
  acc <- corr.assnd / nrow(trns.prms)
  accuracy_all[i]<-acc
  
  
  # Corrected Rand index
  adj.rand.ix<- clust_stats$corrected.rand
  adj_rand_all[i]<- adj.rand.ix
  
}


# save results
datalist = list(m.oob.sup = m.oob.sup, m.oob.unsup=m.oob.unsup, 
                plot_list=plot_list, clust_est=clust_est,
                true_rep_size_all=true_rep_size_all,  
                accuracy_all=accuracy_all, adj_rand_all=adj_rand_all)
saveRDS(datalist, "small.budgie.results.RDS")


