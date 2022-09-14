# plot results of analyses of all synthetic data
# we can do this by loading the files saved in the repository 
# can can also run the code to produce the data files yourself (see the README file)

rm(list = ls())



# TODO: ADD YOUR LOCAL PATH HERE (wherever you locally saved the data that downloaded from the online repository)
setwd("C:/Users/meely/OneDrive - University of Oklahoma/University of Oklahoma/Ross Lab/Aridity and Song Attenuation/Sound Analysis/data/ml_training")


# load data from all synthetic dataset analyses
small.budgie.results <- readRDS("small.budgie.results.RDS")
# large.budgie.results <- readRDS("large.budgie.results.RDS")
# small.lbh.results <- readRDS("small.lbh.results.RDS")
# large.lbh.results <- readRDS("large.lbh.results.RDS")
synth.data.specs <- readRDS("synth.data.specs.RDS")


# combine into single variables
m.oob.sup.all <- c(small.budgie.results$m.oob.sup
                   # , large.budgie.results$m.oob.sup, small.lbh.results$m.oob.sup, large.lbh.results$m.oob.sup
                   )

m.oob.unsup.all <- c(small.budgie.results$m.oob.unsup
                     # , large.budgie.results$m.oob.unsup, small.lbh.results$m.oob.unsup, large.lbh.results$m.oob.unsup
                     )

clust.est.all <- c(small.budgie.results$clust_est
                   # , large.budgie.results$clust_est, small.lbh.results$clust_est, large.lbh.results$clust_est
                   )

true_rep_size_all <- c(small.budgie.results$true_rep_size_all
                       # , large.budgie.results$true_rep_size_all, small.lbh.results$true_rep_size_all, large.lbh.results$true_rep_size_all
                       )

adj_rand_all <- c(small.budgie.results$adj_rand_all
                  # , large.budgie.results$adj_rand_all, small.lbh.results$adj_rand_all, large.lbh.results$adj_rand_all
                  )

accuracy_all <- c(small.budgie.results$accuracy_all
                  # , large.budgie.results$accuracy_all, small.lbh.results$accuracy_all, large.lbh.results$accuracy_all
                  )

spp.all <- c(rep("budgie",48),rep("lbh",48))


# put into dataframe for easy plotting
comb.df <-data.frame("m.oob.sup" = m.oob.sup.all, "m.oob.unsup" = m.oob.unsup.all,
                     "true.rep"= true_rep_size_all,"est.rep"=clust.est.all,
                     "accuracry" = accuracy_all, "adj.rand.index" = adj_rand_all,
                     "species" = spp.all, duration = synth.data.specs$duration,
                     "harmonics" = synth.data.specs$harm, "noise" = synth.data.specs$bgn.noise )


comb.df$true.obs.diff<- comb.df$est.rep-comb.df$true.rep

require(gridExtra)
require(ggplot2)


# PRODUCE PLOTS FROM FIGURE 3

# violin plot
p3a<-ggplot(comb.df,aes(y=m.oob.sup,x=as.factor(duration), color=species))+
  geom_violin(aes(fill=species)) +
  theme_bw(base_size = 15)  +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        axis.title=element_text(size=13)) +
  ylab("SRF out of bag error") + 
  xlab("duration") +
  ylim(c(0,0.2)) +
  scale_color_manual(values = c("black","darkgray")) +
  theme(legend.position="none") +
  scale_fill_manual(values = c("black","darkgray")) 


p3b<-ggplot(comb.df,aes(y=m.oob.sup,x=as.factor(harmonics), color=species))+
  geom_violin(aes(fill=species)) +
  theme_bw(base_size = 15)  +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        axis.title=element_text(size=13)) +
  ylab("SRF out of bag error") + 
  xlab("harmonic content") +
  ylim(c(0,0.2)) +
  scale_color_manual(values = c("black","darkgray")) +
  theme(legend.position="none") +
  scale_fill_manual(values = c("black","darkgray")) 

p3c<-ggplot(comb.df,aes(y=m.oob.sup,x=as.factor(noise), color=species))+
  geom_violin(aes(fill=species)) +
  theme_bw(base_size = 15)  +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        axis.title=element_text(size=13)) +
  ylab("SRF out of bag error") + 
  xlab("background noise") +
  ylim(c(0,0.2)) +
  scale_color_manual(values = c("black","darkgray")) +
  theme(legend.position="none") +
  scale_fill_manual(values = c("black","darkgray")) 



p3d<-ggplot(comb.df,aes(y=m.oob.sup,x=as.factor(true.rep), color=species))+
  geom_boxplot(aes(fill=species)) +
  theme_bw(base_size = 15)  +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        axis.title=element_text(size=13)) +
  ylab("SRF out of bag error") + 
  xlab("repertoire size") +
  ylim(c(0,0.2)) +
  scale_color_manual(values = c("black","black")) +
  theme(legend.position="none") +
  scale_fill_manual(values = c("white","darkgray")) 


grid.arrange(p3a, p3b, p3c, p3d, ncol=2)




# PRODUCE PLOTS FROM FIGURE 4
p4a<-ggplot(comb.df,aes(y=true.obs.diff,x=as.factor(true.rep), fill=species))+
  geom_hline(yintercept =0, color="gray")+
  geom_boxplot()  +
  geom_point(size= 3,  position =  position_dodge(width = .75), pch = 20) +
  theme_bw(base_size = 20)  +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        axis.text=element_text(size=13),
        axis.title=element_text(size=15)) +
  ylab("repertoire estimate error") + 
  xlab("true repertoire size") +
  theme(legend.position="none") +
  scale_fill_manual(values = c("white","darkgray")) 


p4b<-ggplot(comb.df,aes(y=accuracry,x=as.factor(true.rep), fill=species))+
  geom_hline(yintercept =0, color="gray")+
  geom_boxplot()  +
  theme_bw(base_size = 20)  +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        axis.text=element_text(size=13),
        axis.title=element_text(size=15)) +
  ylab("accuracy") + 
  xlab("true repertoire size") +
  theme(legend.position="none") +
  scale_fill_manual(values = c("white","darkgray")) 


p4c<-ggplot(comb.df,aes(y=adj.rand.index,x=as.factor(true.rep), fill=species))+
  geom_hline(yintercept =0, color="gray")+
  geom_boxplot()  +
  theme_bw(base_size = 20)  +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        axis.text=element_text(size=13),
        axis.title=element_text(size=15)) +
  ylab("adjusted rand index") + 
  xlab("true repertoire size") +
  theme(legend.position="none") +
  scale_fill_manual(values = c("white","darkgray"))


grid.arrange(p4a, p4b, p4c, ncol=1)



