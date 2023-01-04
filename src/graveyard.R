# Water Supp - SSWMA - Datetime - Statistical Analyses  - Pairwise -----------------

sswma_watermas = sswma_water %>%
  mutate(date = date(date_time)) %>%
  group_by(site, ws_site, water, arid_withinf, date, mas_bin) %>%
  # summarise_at(c("pc1","pc2","pc3"), mean) 
  summarise_at(vars(gh, 
                    arid_within,
                    sound_atten04:sound_atten12,
                    pc1:pc3), ~ mean(.x, na.rm = TRUE))


# PC1: ADI, AEI, positive  values more likely to have higher ADI
m1 = lmer(pc1 ~ ws_site*water*arid_within + date + (1|ws_site), data = sswma_watermas)
summary(m1)
assump(m1)

emmeans(m1, pairwise ~ ws_site:water|arid_within)
emm_options(pbkrtest.limit = 3000) # run this R will crash
emm_options(lmerTest.limit = 11778) # set lmerTest limit so you can do the within site comparisons


# PC2: Num vocals and species diversity
m2 = lmer(pc2 ~ ws_site*water*arid_within + scale(date_time) + (1|ws_site), data = sswma_water)
summary(m2)
assump(m2)
emmeans(m2, pairwise ~ ws_site:water|arid_within)

emm_options(lmerTest.limit = 54931) # set lmerTest limit so you can do the within site comparisons


# PC3: ACI and BIO
m3 = lmer(pc3 ~ ws_site*water*arid_within + scale(date_time) + (1|ws_site), data = sswma_water)
summary(m3)
assump(m3)
emmeans(m3, pairwise ~ ws_site:water|arid_within)

emm_options(lmerTest.limit = 54931) # set lmerTest limit so you can do the within site comparisons


# Water Supp - SSWMA - Datetime - Stats Analysis - Lag -----------------

sswma_dtlag = sswmawl %>%
  mutate(ws_site = as.factor(ws_site),
         water = as.factor(water)) %>%
  group_by(site, ws_site, water, arid_within, date_time) %>%
  # summarise_at(c("pc1","pc2","pc3"), mean) 
  summarise_at(vars(gh, 
                    arid_within,
                    sound_atten04:sound_atten12,
                    pc1:pc3), ~ mean(.x, na.rm = TRUE))

# PC1: ADI, AEI, positive  values more likely to have higher ADI
m1 = lm(pc1 ~ ws_site*water*arid_within + scale(date_time), data = sswma_dtlag)
summary(m1)
assump(m1)
###stick with lms over lmer
emmeans(m1, pairwise ~ ws_site*water|arid_within)
# emm_options(pbkrtest.limit = 3000) # run this R will crash
# emm_options(lmerTest.limit = 11778) # set lmerTest limit so you can do the within site comparisons

# PC2: Num vocals and species diversity
m2 = lm(pc2 ~ ws_site*water*arid_within + date_time, data = sswma_dtlag)
summary(m2)
assump(m2)
emmeans(m2, pairwise ~ ws_site:water|arid_within)

# PC3: ACI and BIO
m3 = lm(pc3 ~ ws_site*water*arid_within + date_time, data = sswma_dtlag)
summary(m3)
assump(m3)
emmeans(m3, pairwise ~ ws_site*water|arid_within)


# ECE - Threshold - Multi-piecewise linear regression ---------------------

### Estimating continuous piecewise linear regression ###
# https://www.r-bloggers.com/2013/04/estimating-continuous-piecewise-linear-regression/
# N <- 325 # number of sampled points, lwma = 325, sswma = 354, cbma = 368, kiowa = 367

# Fixing Within-normalized aridity
aw4 = aw4 %>%
  group_by(site) %>%
  dplyr::mutate(gh_within = scale_this(gh))

# checking it against separating sites and scaling aridity 

aw4lwma = aw4 %>%
  dplyr::filter(site == "lwma") %>%
  dplyr::mutate(gh_within2 = scale_this(gh))

aw4sswma = aw4 %>%
  dplyr::filter(site == "sswma") %>%
  dplyr::mutate(gh_within2 = scale_this(gh))

aw4cbma = aw4 %>%
  dplyr::filter(site == "cbma") %>%
  dplyr::mutate(gh_within2 = scale_this(gh))

aw4kiowa = aw4 %>%
  dplyr::filter(site == "kiowa") %>%
  dplyr::mutate(gh_within2 = scale_this(gh))

aw4 = rbind(aw4lwma,aw4sswma,aw4cbma,aw4kiowa)

# Checking to see if they scaled within aridity is different or not
arid_df = aw4 %>%
  dplyr::select(site,gh,gh_within, gh_within2) %>%
  arrange(gh_within,site)
# gh_wihtin and gh_within2 are the same so within scaling seems to have worked

# plotting to see changepoints - full dataset

ggplot(data = aw4, aes(x = gh_within,
                       y = pc1,
                       # color = site
)
)+
  geom_smooth(method = loess, se = FALSE) 

# plotting to see if where changepoints are - MAS dataset

ggplot(data = aw6, aes(x = gh_within,
                       y = pc2,
                       # color = site
)
)+
  geom_smooth(method = loess, se = TRUE) 
# aw4site = aw4 %>% dplyr::filter(site == "kiowa") %>% dplyr::select(gh_within, pc2)
# x = aw4site$gh_within
# y = aw4site$pc2

# x <- seq(-1, 1, len = N)
# y <- f(x) + rnorm(length(x))

knot_model = function(num_knots,
                      x_var,
                      y_var) {
  
  # Setting up piecewise regression with multiple breaks or knots
  
  piece.formula <- function(var.name, knots) {
    formula.sign <- rep(" - ", length(knots))
    formula.sign[knots < 0] <- " + "
    paste(var.name, "+",
          paste("I(pmax(", var.name, formula.sign, abs(knots), ", 0))",
                collapse = " + ", sep=""))
  }
  
  f <- function(x) {
    2 * sin(6 * x)
  }
  
  x = x_var
  y = y_var
  
  set.seed(1)
  K <- num_knots  # number of knots
  knots <- seq(min(x), max(x), len = K + 2)[-c(1, K + 2)]
  model <- lm(formula(paste("y ~", piece.formula("x", knots))))
  
  par(mar = c(4, 4, 1, 1))
  plot(x, y)
  
  # ggplot(data = aw4, aes(gh_within,pc2))+
  #   geom_smooth(method = loess, se = FALSE)
  
  lines(x, f(x))
  new.x <- seq(min(x), max(x) ,len = 10000)
  points(new.x, predict(model, newdata = data.frame(x = new.x)),
         col = "red", pch = ".")
  points(knots, predict(model, newdata = data.frame(x = knots)),
         col = "red", pch = 18)
  summary(model) # for kiowa at least, threshold for gh_wihtin is 2.26
  return(model)
}


# Checking which piecewise regression model has best fit with AIC
knotms = lapply(c(1:10), FUN = function(x) knot_model(num_knots = x,
                                                      x_var = aw4$gh_within, 
                                                      y_var = aw4$pc3))

AICctab(knotms[[1]],knotms[[2]],knotms[[3]],knotms[[4]],knotms[[5]],
        knotms[[6]],knotms[[7]],knotms[[8]],knotms[[9]],knotms[[10]],
        nobs = 1411, base=T, weights=T, delta=T, logLik=T)

summary(knotms[[1]])


# ECE - Threshold - Loess Regression --------------------------------------

ggplot(data = aw4, aes(x = gh_within,
                       y = pc2,
                       # color = site
))+
  geom_smooth(method = loess, se = FALSE) +
  geom_vline(xintercept = 0.5520, color = "red")
geom_vline(xintercept = -1.162, color = "red") +
  geom_vline(xintercept = -0.057, color = "blue") +
  geom_vline(xintercept = 1.166, color = 'green')

abline(v = -0.057, col = "blue")
abline(v = 1.166, col = "green")

loess_m10 = loess(pc2 ~ gh_within, data = aw4, span = 0.10)
loess_m25 = loess(pc2 ~ gh_within, data = aw4, span = 0.25)
loess_m50 = loess(pc2 ~ gh_within, data = aw4, span = 0.50)
summary(loess_m50)

# Get smooth output
smoothed10 <- predict(loess_m10) 
smoothed25 <- predict(loess_m25) 
smoothed50 <- predict(loess_m50) 

plot(y = aw4$pc2, aw4$gh_within, type = "p", main = "Loess Smoothing and Prediction")
lines(smoothed10, x=aw4$gh_within, col="red")
lines(smoothed25, x=aw4$gh_within, col="green")
lines(smoothed50, x=aw4$gh_within, col="blue")

# find changepoints in loess fit for full dataset, but manually segmenting data to where there is a dropoff in pc
# pc1: 1-2
# pc2: 1-2
# pc3: 1-2

loess_max = function(data,
                     x_var,
                     y_var,
                     arid_min,
                     arid_max) {
  # loess_m = loess(y_var ~ gh_within, data = aw4)
  
  # segmenting data from gh_within 1 to 2 since that is where we see the drop in the loess model
  data_filtered = data %>% 
    dplyr::filter(gh_within >= arid_min) %>% 
    dplyr::filter(gh_within <= arid_max)
  loess_mex = loess(y_var ~ x_var, data = data_filtered)
  
  x <- x_var[x_var >= 1]
  x = x[x <= 2]
  # x = seq(-2,3, by = 0.1)
  px <- predict(loess_mex, newdata=x)
  px1 <- diff(px)
  # px1_ex = px1[px1 >= 1] 
  
  max_gh = which.max(px1) # max is 4 but start value of x is -2, means the curve is flat at position -2+4 = 2
  px1[max_gh] # 0.01929 value with max
  loess_predict = cbind(x = x[-1], y = px1)
  max_arid = loess_predict[max_gh,] # max x is 1.4769
  
  par(mfrow=c(1, 2))
  plot(x, px, main="loess model")
  abline(v=max_arid, col="red")
  
  # loess_predict[7488,] # x = 1.136
  
  plot(x[-1], px1, main="diff(loess model)")
  abline(v=max_arid, col="red")
  return(max_arid)
}

loess_max(aw4, aw4$gh_within, aw4$pc1,-1,1.5) # 1
loess_max(aw4, aw4$gh_within, aw4$pc2,1,2) # 1.136
loess_max(aw4, aw4$gh_within, aw4$pc3,1,2) # 1.597

# Piecewise Linear Regression model on loess predicted data

loess_m = loess(pc1 ~ gh_within, data = aw4)
set.seed(124)
# x = rnorm(1000, 0,1.2)
x = seq(-2,3, by = 0.1)
px <- predict(loess_m, newdata=x)

loess_predict = data.frame(x = x,
                           y = px)

# par(mfrow=c(1, 1))
plot(x, px, main="loess model")
# abline(v = -1.162, col = "red")
# abline(v = -0.057, col = "blue")
# abline(v = 1.166, col = "green")

# map piecewise regression to loess model. 3 breakpoints for pc1, 4 for pc2, 3 for pc3

mcp_loess = list(y ~ 1, # intercept
                 ~ 0 + x, #linear segment1 (int_1)
                 ~ 0 + x, #linear segment2 slope (time_2) at cp_1
                 ~ 0 + x # disjoined slope (int_3, time_3) at cp_2
                 # ~ 1+x
)
fit_loess = mcp(mcp_loess, data = loess_predict, sample = 'prior')
summary(fit_loess)
summaryfl = summary(fit_loess)

# mcp predicted breakpoints
plot(x, px, main="loess model")
abline(v = summaryfl$mean[1], col = "red")
abline(v = summaryfl$mean[2], col = "blue")
abline(v = summaryfl$mean[3], col = "green")
# abline(v = 2.0423, col = "green")





# Threshold - ECE - Threshold Modelling - Piece wise -----------------------------------------------------

library(segmented)
library(nlme)

# Full Dataset - Site affects slope, aridity (z) affects changepoint
## U = random effects in the slope-difference parameter
## G0 = random effects in the breakpoints
# does not give breakpoints for each individual site, have to analyze each site separately
# MAS

thres_seg(site,ps1,ps2){
  m1 = lm(pc ~ gh, data = data %>%
            dplyr::filter(site == site))
  m1s = segmented(m1, seg.Z=~gh,
                  # npsi = 2
                  # psi = psi1
                  psi = list(gh = c(psi1,psi2))
  )
}

m1s_summary = summary(m1s) 
m1s_slope = slope(m1s)
# get the fitted data
my.fitted <- fitted(m1s)
my.model <- data.frame(data %>% 
                         dplyr::filter(site == site) %>%
                         dplyr::select(gh), pc = my.fitted)

thres_lwma = thres_seg(data = aw6,
                       pc = aw6$pc2,
                       site = "lwma",
                       psi1 = -24,
                       psi2 = -14
); thres_lwma[[2]];thres_lwma[[3]]

thres_sswma = thres_seg(data = aw6,
                        pc = aw6$pc2,
                        site = "sswma",
                        psi1 = -18,
                        psi2 = -10
); thres_sswma[[2]];thres_sswma[[3]]

thres_cbma = thres_seg(data = aw6,
                       pc = aw6$pc2,
                       site = "cbma",
                       psi1 = -18,
                       psi2 = -10
); thres_cbma[[1]];thres_cbma[[2]];thres_cbma[[3]]

thres_kiowa = thres_seg(data = aw6,
                        pc = aw6$pc2,
                        site = "kiowa",
                        psi1 = -15,
                        psi2 = -10
); thres_kiowa[[1]];thres_kiowa[[2]];thres_kiowa[[3]]



# Threshold - ECE - Hyperbolic Model --------------------------------------------------------

thres1 <- drm(ea_mas$pc1 ~ ea_mas$gh_within*ea_mas$site, fct = GRT.YL())
summary(thres1)
plot(thres1, log="", 
     legendPos = c(5, 1.0), xlab = "Aridity")

# Threshold - ECE - Exponential switch-off model --------------------------------------------

modExb <- drm(ea_mas$pc1 ~ ea_mas$gh_within*ea_mas$site, fct = GRT.Exb())
summary(modExb)
plot(modExb, log="", 
     legendPos = c(5, 1.0), xlab = "Temperature (°C)")


# Threshold - ECE - Broken-curvilinear model ------------------------------------------------

modExb <- drm(ea_mas$pc1 ~ ea_mas$gh_within*ea_mas$site, fct = GRT.RFb())
summary(modExb)
plot(modExb, log="", 
     legendPos = c(5, 1.0), xlab = "Temperature (°C)")



# Threshold - ECE - findthresh --------------------------------------------

library(evir)

findthresh(aw6, 100)


# ECE - Threshold - EnvCpt ------------------------------------------

library(EnvCpt)
aw6 = aw6 %>% 
  # dplyr::filter(site == "lwma") %>%
  # dplyr::select(gh_within, pc1:pc3) %>%
  arrange(pc2)
fit_envcpt = envcpt(aw6$pc2)  # Fit all models at once
fit_envcpt$summary  # Show log-likelihoods
out=fit_envcpt # run all models with default values
out[[1]] # first row is twice the negative log-likelihood for each model
# second row is the number of parameters
AIC(out) # returns AIC for each model.
which.min(AIC(out)) # gives trendar2cpt (model 12) as the best model fit.
out$trendar1cpt # gives the model fit for the meancpt model.
AICweights(out) # gives the AIC weights for each model
BIC(out) # returns the BIC for each model.
which.min(BIC(out)) # gives meancpt (model 2) as the best model fit too.
plot(out,type='fit') # plots the fits
plot(out,type="aic") # plots the aic values
plot(out,type="bic") # plots the bic values # run all models with default values

out$meancpt@cpts # example code
out$meancpt@param.est #example code

fit_envcpt$trendar1cpt@cpts # example code
fit_envcpt$trendar2cpt@param.est #example code


