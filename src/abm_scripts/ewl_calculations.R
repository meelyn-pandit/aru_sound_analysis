#####################################################
######### EWL Equations from digitized data ##########
##### from Albright et al. 2017, Gravilov 2017, Mckenchie et al. 2020

# Albright et al. 2017 ----------------------------------------------------
#Data digitized from scatter plots of abert's towhee, cactus wren, curve-billed thrasher, 
#house finch, and lesser goldfinch
#Equation derived from ANCOVA of digitized data, rsquare = 0.849

ewl_albright = function(mass, temp){
  #log(ewl) = (0.1181515*temp) + (0.0224677*mass)-3.8895978
  ewl_log = (0.1181515*temp) + (0.0224677*mass) - 3.8895978
  EWL = (exp(ewl_log))/12 #ewl is in g/5min
  
  return(EWL)
  # if(wdavg$bin1[1]==730){
  #   wdavg$TEWL = 0 + wdavg$EWL
  # } else {
  #   wdavg$TEWL = lag(wdavg$TEWL)+wdavg$EWL
  # }
  
  # if(wdavg$bin1 == 0 & wdavg$bin2 == 5){
  #   wdavg$TEWL = 0
  # } else if(wdavg$bin1 == 0 && lag(wdavg$bin1) == 1435){
  #   wdavg$TEWL = 0
  # } else if(wdavg$bin1 == 5 && lag(wdavg$bin1) != 0){
  #   wdavg$TEWL = 0
  # } else {
  #   
  # }
}

# #Mckechnie et al. 2020 equations ---------------------------------------

ewl_mckechnie = function(mass, tuc, temp){

if(temp<tuc){
    ewl = (0.808*log10(mass))-1.893 #min EWL below the TUC for a 16g bird
    ewl = (10^(ewl)/12) #have to raise to the power of 10 since the y variable is logged, dividing by 60 since the equation is per hour and we have and we have 5 min bins
  } else if(temp>tuc){
    ewl = (0.701*log10(mass))-0.813 #max EWL above the TUC for a 16g bird
    ewl = (10^(ewl)/12)
  }
}

# Gravilov 2017 Equations -------------------------------------------------
ewl_gravilov = function(mass, tuc, temp){

  if(temp<tuc){
    ewl = (0.245*(mass^0.716))/12 #TEWL below the upper critical limit, divided by 12 for the 5 min bins
  } else if(temp>tuc){
    ewl = (0.560*(mass^0.780))/12
  }
}
