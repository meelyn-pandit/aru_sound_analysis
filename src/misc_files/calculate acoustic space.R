# code for acoustic space analyses 
# 
# Here, we evaluate the acoustic area of datasets of synthetic budgie and long-billed hermit data
# and reproduce figure 6

# The comments also explain how to caculate acoustic space for your own data

# clear workspace
rm(list = ls())

require(gridExtra)
require(ggplot2)



# TODO: ADD YOUR LOCAL PATH HERE (wherever you locally saved the data that downloaded from the online repository)
setwd("YOUR LOCAL PATH HERE")
source("acoustic_spaces.R")


# To caculate acoustic area (i.e. acoustic space) that signals occupy in the feature space, 
# we use the output of the unsupervised random forest (URF) model 
# The URF output includes the variable "proximity", which has pairwise similarity values for all datapoints

# In this example, we use URFs calculated from synthetic budgerigar and long-billed hermit datasets 
# with 100 discrete element types.
# To estimate the acoustic space occupied by datasets with different repertoire sizes, we call 
# the function acoustic_spaces.R to caculate acoustic space for dataset with 5, 10, 15....100 unique elements.
# acoustic_spaces.R will randomly pull out subsets of the larger 100-element dataset to do these calculations

# NOTE: To compare acoustic space sizes, it is crucial that all signals are analyzed in a single URF model
# For example, if you want to complare acoustic space occupied by two different subsets of signals,
# you must analyze all signals together within a single URF model, and then apply acoustic_spaces.R to the
# subsets of your data that include the different signal types.



# ACOUSTIC SPACE OCCUPIED BY BUDGERIGAR CALLS
# first let's look at the synthetic budgie data with 100 unique elements (i.e. repertoire size = 100)
load("budgie synth data ap.trans.urf 21.rda")
budgie.urf <- ap.trans.urf

elm.types <- rep(1:100, each = 10)

# set seed to get same results if you want to replicate exact results each time
# set.seed(5)  

# calculate acoustic space occupied for repertoires of various sizes. Broken up into steps for less intensive computing 
# repertoires with 5, 10, 15, 20, 25, 30 unique elements
aps1a <- acoustic_spaces(X = budgie.urf$proximity, n.elements = seq(5, 30, 5), labels = elm.types, cl = 3, pb = FALSE)
# repertoires with 35, 40, 45, 50, 55, 60, 65, 70 unique elements
aps1b <- acoustic_spaces(X = budgie.urf$proximity, n.elements = seq(35, 70, 5), labels = elm.types, cl = 3, pb = FALSE)
# repertoires with 75, 80, 85, 90 unique elements
aps1c <- acoustic_spaces(X = budgie.urf$proximity, n.elements = seq(75, 90, 1), labels = elm.types, cl = 3, pb = FALSE)
# repertoires with 95 or 100 unique elements
aps1d <- acoustic_spaces(X = budgie.urf$proximity, n.elements = seq(90, 100, 1), labels = elm.types, cl = 3, pb = FALSE)

# combine acoustic space estimates and true repertoire size values
aps1.area<-c(aps1a$area,aps1b$area,aps1c$area,aps1d$area)
aps1.rep<-c(aps1a$repertoire,aps1b$repertoire,aps1c$repertoire, aps1d$repertoire)
aps1 <- data.frame("area"= aps1.area, "repertoire" = aps1.rep, "area.sq" =  (aps1.area)^2)

# calculate stats if you want
# cor(aps1.area, aps1.rep, method = "spearman")

plot6a <- ggplot(aps1, aes(y=area.sq,x=repertoire))+
  geom_point(size= 4, pch = 21,fill="black",alpha=.6)  +
  geom_smooth(method='lm', formula= y~x,colour="black") +
  theme_bw(base_size = )  +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        axis.title=element_text(size=13),
        axis.text=element_text(size=13)) +
  ylab("acoustic space") + 
  xlab("true repertoire size") +
  theme(legend.position="none")




# ACOUSTIC SPACE OCCUPIED BY BUDGERIGAR CALLS
# first let's look at the synthetic budgie data with 100 unique elements (i.e. repertoire size = 100)

load("LBH URF data.rda")
lbh.urf <- ap.trans.urf

# repertoires with 5, 10, 15, 20, 25, 30 unique elements
aps2a <- acoustic_spaces(X = lbh.urf$proximity, n.elements = seq(5, 30, 5), labels = elm.types, cl = 3, pb = FALSE)
# repertoires with 35, 40, 45, 50, 55, 60, 65, 70 unique elements
aps2b <- acoustic_spaces(X = lbh.urf$proximity, n.elements = seq(35, 70, 5), labels = elm.types, cl = 3, pb = FALSE)
# repertoires with 75, 80, 85, 90 unique elements
aps2c <- acoustic_spaces(X = lbh.urf$proximity, n.elements = seq(75, 90, 1), labels = elm.types, cl = 3, pb = FALSE)
# repertoires with 95 or 100 unique elements
aps2d <- acoustic_spaces(X = lbh.urf$proximity, n.elements = seq(90, 100, 1), labels = elm.types, cl = 3, pb = FALSE)

aps2.rep<-c(aps2a$repertoire,aps2b$repertoire,aps2c$repertoire, aps2d$repertoire)
aps2.area<-c(aps2a$area,aps2b$area,aps2c$area,aps2d$area)
aps2 <- data.frame("area"= aps2.area, "repertoire" = aps2.rep, "area.sq" =  (aps2.area)^2)

# cor(aps2.area, aps2.rep, method = "spearman")


plot6b<-ggplot(aps2,aes(y=area.sq,x=repertoire))+
  geom_point(size= 4, pch = 21,fill="black",alpha=.6)  +
  geom_smooth(method='lm', formula= y~x,colour="black") +
  theme_bw(base_size = )  +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        axis.title=element_text(size=13),
        axis.text=element_text(size=13)) +
  ylab("acoustic space") + 
  xlab("true repertoire size") +
  theme(legend.position="none")


# Make plot
grid.arrange(plot6a, plot6b, ncol = 1)


