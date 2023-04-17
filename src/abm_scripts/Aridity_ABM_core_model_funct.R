#Agent based model of territory defense interactions among birds
#The arena is a hexagonal grid representing an array of territories
#Birds sing, move, and rest within their grid cells according to 
#pre-assigned probabilities
#The "goal" of each bird is to sing within the hearing range of each 
#of its neighbors.
#The model ends when all of the birds with interior territories have
#met this goal.

aridityABM <- function(HexSize=X, #diameter of territory
                       Song_volume = 85, #volume of song/call in dB
                       Song_detection = 30, #threshold for detecting a song
                       SProb = 0.33, #Singing probability
                       MProb = 0.33, #moving probability
                       Song_freq = 2000, #frequency or pitch of song/call in Hz
                       mass = 16, #mass of bird in grams
                       tuc = 35.7, #thermal upper critical limit
                       tlc = 22, #thermal lower critical limit
                       wdata, #weather data
                       ewl = T, #evaporative water loss, water lost in grams
                       runTime = 360, #duration of the model in min (6 hrs)
                       iter = 1, #number of times the model is run for entire data
                       plot = F,
                       site_lab = site) { #plot the grid for each time step, default is F for false. Set to T to plot the grid
  
  #-----NOTES------
  #HexSize is the approximate DIAMETER or the grid cells (Not radius!!)
  #tuc = upper critical temperature
  #tlc = lower critical temperatre
  #-----NOTES------
  
  #----Libraries----
  library(tidyverse) # data science
  library(suncalc) # calculating sunrise and sunset times
  library(lubridate) # date and time parsing
  library(rgeos) # geo data
  library(sp) # geo data
  library(spdep) # geo data
  library(FNN)
  library(plotrix)
  #----Libraries----
  
  #make sure working directory is correct....
  source("src/abm_scripts/Atmospheric_sound_attenuation.R")
  image_count = 1  
  #------Set up Arena-----
  HexSize = as.numeric(HexSize)
  Song_freq = as.numeric(Song_freq)
  
  Arena = HexSize*10.1        #rough size of entire square arena in meters
  set.seed(12) #set random seed so you always get the same arrangement of hexagons
  r1 = matrix(data = c(0,Arena,Arena,0,0,0,Arena,Arena), nrow = 4, ncol = 2) #start with square
  Ps1 = Polygons(list(Polygon(r1)), ID = "a")
  SPs = SpatialPolygons(list(Ps1))
  HexPts <-spsample(SPs, type="hexagonal", cellsize=HexSize) # Fill arena with hexigons
  HexPols <- HexPoints2SpatialPolygons(HexPts)
  #plot(HexPols)
  
  pid <- sapply(slot(HexPols, "polygons"), function(x) slot(x, "ID"))
  Hx = SpatialPolygonsDataFrame(HexPols, data.frame(N = c(1:length(HexPols)), row.names = pid)) # Make hexigons a spatial data frame, include hexigon ID
  # Add to df the number of neighbors
  Prox = poly2nb(HexPols)
  Hx@data$nProx <- unlist(lapply(Prox, FUN = length)) 
  #initialize a column that designates current activity and previous action
  Hx@data$Action <- 0  #1 = sing, 2 = move, 0 = rest
  Hx@data$PrevAction = 0
  #make six columns that record neighbor contacts
  Hx@data[c("NC1","NC2","NC3","NC4","NC5","NC6")] <- 0
  Hx$done <- 0 #Column to indicate if all interactions are complete
  Hx@data[c("SingCnt","MoveCnt","RestCnt")] <- 0 #make three columns for activity tracking
  #Generate a random location in each grid cell
  Locs <- lapply(Hx@polygons, FUN = function(x) spsample(x, n = 1, "random"))
  Locs = SpatialPoints(Locs)
  Locs <- as.numeric(Locs@coords) #was originally as.numeric
  Locs <- t(matrix(Locs, nrow = 2, ncol = length(Locs)/2))
  #Add currenta and previous locations to the data frame
  Hx@data$LocX = Locs[,1]
  Hx@data$LocY = Locs[,2]
  Hx@data$prevLocX = 0
  Hx@data$prevLocY = 0
  
  #make six columns that list neighbor IDs.
  Hx@data[c("N1","N2","N3","N4","N5","N6")] <- NA
  #loop to fill in neighbor IDs
  for(nb in 1:nrow(Hx@data)){
    numNb <- length(Prox[[nb]])
    Hx@data$N1[nb] = Prox[[nb]][1]
    Hx@data$N2[nb] = Prox[[nb]][2]
    if(numNb > 2) {Hx@data$N3[nb] = Prox[[nb]][3]}
    if(numNb > 3) {Hx@data$N4[nb] = Prox[[nb]][4]}
    if(numNb > 4) {Hx@data$N5[nb] = Prox[[nb]][5]}
    if(numNb > 5) {Hx@data$N6[nb] = Prox[[nb]][6]}
  }
  
  #make six columns that note neighbor contacts for a particular time step
  Hx@data[c("TNC1","TNC2","TNC3","TNC4","TNC5","TNC6")] <- 0
  #make six columns that define all possible contacts 
  neibDat <- Hx@data[,c("N","N1","N2","N3","N4","N5","N6")]
  colnames(neibDat) <- c("N","CN1","CN2","CN3","CN4","CN5","CN6")
  conList <- pivot_longer(data=neibDat, cols=!N, 
                          names_to = c(".value", "set"), 
                          names_pattern = "(.)(.)")
  conList$loNum <- pmin(conList$N, conList$C)
  conList$hiNum <- pmax(conList$N, conList$C)
  conList$comb <- paste(conList$loNum, conList$hiNum, sep="_")
  conMat <- matrix(data=conList$comb, ncol=6, byrow = T)
  Hx@data[,c("NM1","NM2","NM3","NM4","NM5","NM6")] <- conMat
  
  #-----Load and prep weather data ------
  # Load data, filter by site, unique date_times, and only select filename, site, date_time, mas_num, temp, relh, pres
  wdata = wdata %>%
    dplyr::filter(site == site_lab) %>%
    dplyr::filter(is.na(mas)==FALSE) %>%
    dplyr::filter(as.numeric(as.character(mas)) >= 0) %>%
    dplyr::select(site,date_time,mas,temp,relh,pres,dew) %>%
    dplyr::arrange(date_time)
  
  wdata$date = as_date(wdata$date_time)
  wdata$CallRad <- mapply(aud_range, Song_volume, Song_detection, Song_freq,
                          wdata$temp, wdata$relh, wdata$pres)
  wdata$mas_num1 = as.numeric(as.character(wdata$mas))
  wdata$mas_num2 = wdata$mas_num1 + 5
  
  #----Calculate evaporative water loss (EWL) and total EWL (TEWL) for aridity gradient dataset (aw4)
  
  source("src/abm_scripts/ewl_calculations.R")
  
  #Calculate EWL for each 5 min interval
  wdata$EWL = ewl_albright(mass,wdata$temp) #albright equation
  
  for(j in 1:length(wdata$mas_num1)){ #moved EWL equations after summarising data to see if the average EWL differs at all.
    if(j == 1){
      wdata$TEWL[j] = 0 + wdata$EWL[j]
    } else if(wdata$mas_num1[j] == 0 && wdata$mas_num2[j] == 5){
      wdata$TEWL[j] = 0
    } else if(wdata$mas_num1[j] == 0 && wdata$mas_num1[j-1] == 1435){
      wdata$TEWL[j] = 0
    } else if(wdata$mas_num1[j] == 5 && wdata$mas_num1[j-1] != 0){
      wdata$TEWL[j] = 0
    } else if(is.na(wdata$temp[j])==TRUE){
      wdata$EWL[j] = 0
    } else {
      wdata$TEWL[j] = wdata$TEWL[j-1]+wdata$EWL[j]
    }
  }
  # wdavg = wdavg %>%
    # filter(dayMonthLocal == "06-19") #ONLY FOR SAVING IMAGES!!! COMMENT OUT WHEN DOING NORMAL MODEL RUNS!
  
  #------Define dates for Model
  # dates = as.data.frame(table(wdavg$dayMonthLocal))
  dates = as.data.frame(table(wdata$date))
  dates = as.vector(dates$Var1)
  
  #------Prepare output data frame
  outSize <- runTime * length(dates) * iter #calculate output rows
  outData = data.frame(site = site_lab,
                       iter = rep(0, outSize), 
                       date = NA, 
                       timeCnt = 0, 
                       Terrs = nrow(Hx@data), 
                       FullTerrs = nrow(Hx@data)-sum(is.na(Hx@data$N6)),
                       HexSize = HexSize,
                       Frequency = Song_freq,
                       contacts=0, 
                       Singers=0, 
                       Movers=0, 
                       Resters=0, 
                       Done = 0, 
                       Dew=0, 
                       Tair=0, 
                       RelH=0, 
                       Pres=0, 
                       TEWL=0, 
                       CallRad=0,
                       tewl_present = ewl
                       ) #df for output
  outCnt = 1
  set.seed(second(Sys.time())) #set seed by system time to get "real" randomness

#ITERATION LOOP HERE use variable 'iteration' not iter
  for (iteration in 1:iter) {
    for(d in 1:length(dates)) { #dates loop
      Date = dates[d]
      # wdavgDay = wdavg[wdavg$dayMonthLocal == Date,]
      wdavgDay = wdata[wdata$date == Date, ]
      # ifelse(wdavgDay$dayMonthLocal == "06-13", wdavgDay$bin2 = wdavgDay$bin2 + 5, wdavgDay$bin2 = wdavgDay$bin2 + 0)
      # if(wdavgDay$dayMonthLocal == "06-13"){ #added condition because 06/13/2011 bin1 differed by 10 instead of 5, would halt timeCnt because after timeCnt = 5, the code would not pull CallRad or TEWL
      #   wdavgDay$bin2 = wdavgDay$bin2 + 5
      # }
    
      #Reset columns in spatial data frame
      Hx@data[c("NC1","NC2","NC3","NC4","NC5","NC6")] <- 0
      Hx@data[c("Action", "PrevAction", "done", "SingCnt", "MoveCnt", "RestCnt")] <- 0 #make three columns for activity tracking
      
      timeCnt = 0  #Initialize counter
        while(timeCnt < runTime) {
          
          if(wdavgDay$TEWL[d] == 0 && wdavgDay$mas_num1[d] == 5){
            timeCnt = timeCnt + 5 #advancing timecnt so the model continues past missing first row in a day
            
            # } else if(wdavgDay$bin1[2]-wdavgDay$bin1[1] == 10){
            #   wdavgDay$bin2=wdavgDay$bin2+5  #dealing with 06/13/2011 data in which bin1 differs by 10 instead of 5
           
            } else {
              timeCnt = timeCnt + 1
            }
          
          # timeCnt = timeCnt + 1 #advancing timeCnt when Mesonet did not get weather readings every 5 min and got them every 10 min
        
        if(timeCnt %% 10 == 0) {
          print(paste0(iteration, "_", Date, "_", timeCnt, "_", HexSize, "_", Song_freq,"_",ewl,"_",site_lab))
        }
        
        #Extract relevant call radius and TEWL for this iteration
        CallRad <- wdavgDay$CallRad[wdavgDay$mas_num1<=timeCnt & wdavgDay$mas_num2>timeCnt]
        TEWL <- wdavgDay$TEWL[wdavgDay$mas_num1<=timeCnt & wdavgDay$mas_num2>timeCnt]
    
        #add counter and environmental data to output
        
        outData[outCnt, c("iter", "date", "timeCnt", "TEWL", "CallRad")] <- 
          c(iteration, Date, timeCnt, TEWL, CallRad)
        outData[outCnt, c("Dew", "Tair", "RelH", "Pres")] <-
          wdavgDay[wdavgDay$mas_num1<=timeCnt & wdavgDay$mas_num2>timeCnt, c("dew", "temp", "relh", "pres")]
        # outLoc[outCnt,c("locx","locy")] = c(Hx@data$LocX, Hx@data$LocY)
        if(ewl == F){
          SingProb = SProb
          MoveProb = MProb
          RestProb = 1-(SingProb+MoveProb)
        } else {
          ###EWL Equations from Wolf et al. 2020
          if(TEWL <=(0.15*mass)){ #15% of Body Mass of painted bunting
            SingProb = SProb-(SProb*((TEWL/2)/(0.15*mass)/2))     #probability of singing on a given turn, decreases the total singing probability (0.33) multiplied by half of the EWL at that time step, product is then divided by half the total amount of water the bird can lose to EWL based on its mass
            #basically converting the EWL lost at each time step into a percentange relative to the singing probability, with each time step, EWL increases, and the total percent from singing probability decreases
            MoveProb = MProb-(MProb*((TEWL/2)/(0.15*mass)/2))      #probability of moving on a given turn, decreases the total singing probability (0.33) multiplied by the EWL at that time step, product is then divided by the total amount of water the bird can lose to EWL based on its mass, divided by 2 to account for singing probability, multiplied by the time step
            RestProb = 1-(SingProb+MoveProb)
          } 
          if (TEWL>(0.15*mass)){
            SingProb = 0.01 #can't be zero or the LocX won't be filled with data
            MoveProb = 0.01
            RestProb = 1-(SingProb+MoveProb)
          }
        }
         
        Hx@data$Action <- 0 #reinitialize action column
        Hx@data[c("TNC1","TNC2","TNC3","TNC4","TNC5","TNC6")] <- 0 #Initialize contact matrix - set all to zero.
    
        if(timeCnt == 1){
          #Give each bird a random number to determine activity
          Rd = runif(n=nrow(Hx@data))
        }  else {
          # If not first time step random numbers asigned wihtin limits defined by previous time step and action probabiliities
          Rd = ifelse(Hx@data$PrevAction == 2, runif(n=nrow(Hx@data), min = 0, max = (SingProb+RestProb)), runif(n=nrow(Hx@data)))
        }
        
        #Birds with a random value below SingProb will sing
        Hx@data$Action = ifelse(Rd <= SingProb, 1, 0)  #1 = sing, 2 = move, 0 = rest
        sing = which(Hx@data$Action==1)                #which birds sing
        
        if(length(sing) > 0) { #NEED if statement in case there is no singing.
        
          dfSing <- Hx@data[sing,]
          #Get all X and Y Coordinate for neighbors
          nv <- as.vector(as.matrix(dfSing[,c("N1","N2","N3","N4","N5","N6")]))  #Get a list of all coordinates
          dfSing[,c("N1X","N2X","N3X","N4X","N5X","N6X")] <- Hx@data$LocX[nv]    #Put current X coordinates into the data frame
          dfSing[,c("N1Y","N2Y","N3Y","N4Y","N5Y","N6Y")] <- Hx@data$LocY[nv]    #Put current Y coordinates into the data frame
          dfSing$DN1 = sqrt((dfSing$LocX-dfSing$N1X)^2 + (dfSing$LocY-dfSing$N1Y)^2)
          dfSing$DN2 = sqrt((dfSing$LocX-dfSing$N2X)^2 + (dfSing$LocY-dfSing$N2Y)^2)
          dfSing$DN3 = sqrt((dfSing$LocX-dfSing$N3X)^2 + (dfSing$LocY-dfSing$N3Y)^2)
          dfSing$DN4 = sqrt((dfSing$LocX-dfSing$N4X)^2 + (dfSing$LocY-dfSing$N4Y)^2)
          dfSing$DN5 = sqrt((dfSing$LocX-dfSing$N5X)^2 + (dfSing$LocY-dfSing$N5Y)^2)
          dfSing$DN6 = sqrt((dfSing$LocX-dfSing$N6X)^2 + (dfSing$LocY-dfSing$N6Y)^2)
          #Populate data frame with index numbers of contacted neighbors
          dfSing[,c("C1","C2","C3","C4","C5","C6")] <- ifelse(dfSing[,c("DN1", "DN2", "DN3", "DN4", "DN5", "DN6")] < CallRad, 1, 0) * dfSing[,c("N1","N2","N3","N4","N5","N6")]
          
          if(sum(dfSing[,c("C1","C2","C3","C4","C5","C6")], na.rm=T) > 0) { #!!!!! IF statement in case no contacts !!!!!
            subSing <- dfSing[,c("N","C1","C2","C3","C4","C5","C6")]
            contacts <- pivot_longer(data=subSing, cols=!N, 
                     names_to = c(".value", "set"), 
                     names_pattern = "(.)(.)")
        
            contacts <- contacts[!is.na(contacts$C),c("N", "C")]
            contacts <- contacts[contacts$C>0,]
            contacts$lowN <- pmin(contacts$N, contacts$C)
            contacts$hiN <- pmax(contacts$N, contacts$C)
            contacts$cons <- paste(contacts$lowN, contacts$hiN, sep="_")
            outData$contacts[outCnt] <- nrow(contacts)
            
            #genearate a 1 for temporary neighbor tracking for all pairings that occurred
            Hx@data[, c("TNC1", "TNC2", "TNC3", "TNC4", "TNC5", "TNC6")] <- 
                    as.numeric(as.matrix(Hx@data[1:nrow(Hx@data),c("NM1","NM2","NM3","NM4","NM5","NM6")]) %in% contacts$cons)
            
            #Add contacts to NC (Neighbor Contacts) columns
            Hx@data[, c("NC1", "NC2", "NC3", "NC4", "NC5", "NC6")] <-
              Hx@data[, c("NC1", "NC2", "NC3", "NC4", "NC5", "NC6")] +
              Hx@data[, c("TNC1", "TNC2", "TNC3", "TNC4", "TNC5", "TNC6")]
            
            sums <- rowSums(Hx@data[, c("TNC1", "TNC2", "TNC3", "TNC4", "TNC5", "TNC6")]) > 0
            Hx@data$Action[sums] <- 1  #set action to 1 for all singers
            Rd[sums] <- 0  #change random number to zero to ensure it counts as a singing action
          }
        }
           
        #Store some activity data here
        sing = which(Hx@data$Action==1 & !is.na(Hx@data$N6))  #which birds sing and respond - only count central territories
        outData$Singers[outCnt] <- length(sing)
        doneProd <- Hx@data$NC1 * Hx@data$NC2 * Hx@data$NC3 * Hx@data$NC4 * Hx@data$NC5 * Hx@data$NC6
        doneProd <- as.numeric(doneProd > 0)  #territories with less than 6 neighbors will never be done.
        Hx@data$done <- doneProd
        outData$Done[outCnt] <- sum(Hx@data$done)
        
        #Now deal with the movers
        Hx@data$Action[Rd >= 1-MoveProb] = 2  #1 = sing, 2 = move, 0 = rest
        Mvrs <- which(Hx$Action == 2 & !is.na(Hx@data$N6)) #get rows for movers - only count central territories
        outData$Movers[outCnt] <- length(Mvrs)
        Mvrs <- which(Hx$Action == 2) #get rows for movers - now include edge territories
        
        
        if(length(Mvrs) > 0){ #if there are movers in the latest timestep
          #Generate some new locations for the movers
          Hx@data$prevLocX <- Hx@data$LocX
          Hx@data$prevLocY <- Hx@data$LocY
          Locs <- lapply(Hx@polygons[Mvrs], FUN = function(x) spsample(x, n = 1, "random"))
          Locs = SpatialPoints(Locs)
          Locs <- as.numeric(Locs@coords) #changed from "as.numeric" to as.integer
          Locs <- t(matrix(Locs, nrow = 2, ncol = length(Locs)/2))
          #Add locations to the data frame
          Hx@data$LocX[Mvrs] = Locs[,1]
          Hx@data$LocY[Mvrs] = Locs[,2]
          Hx$MoveCnt[Mvrs] <- Hx$MoveCnt[Mvrs] + 1
        } else { 
          Hx@data$prevLocX <- Hx@data$LocX
          Hx@data$prevLocY <- Hx@data$LocY
        }
          
        #Resting birds just add to the restCnt column
        rest <- which(Hx$Action == 0 & !is.na(Hx@data$N6)) #get rows for resters - only count central territories
        outData$Resters[outCnt] <- length(rest)
        rest <- which(Hx$Action == 0) #get rows for movers - now count edge territories
        Hx$RestCnt[rest] <- Hx$RestCnt[rest] + 1
        outData$Resters[outCnt] = length(rest)
        
        outCnt = outCnt + 1 #advance data counter
        
        if(timeCnt < runTime){
          Hx@data$PrevAction = Hx@data$Action #advance runTime counter
        } 
    
    
        # ################################################
        # ########plot outcome############################
        # ################################################
        if(plot){
          if(HexSize == 1000 & weather_data == "dir/ERIC_climate_change_extreme.Rdata"){
            dir = "med_case_images/"
          } else if(HexSize == 1500 & weather_data == "dir/ERIC_climate_change_extreme.Rdata"){
            dir = "bad_case_images/"
          } else if(HexSize == 3000 & weather_data == "dir/ERIC_climate_change_extreme.Rdata"){
            dir = "worst_case_images/"
          } else {
            dir = "contemporary_case_images/"
          }
          
          tiff(paste0("terrgrid_",Date,"_",timeCnt,".tif",sep=""), width = 960, height = 1280, units = "px", res = 250)
    
          par(mar=c(0,0,0,0))
          # Hcol = ifelse(Hx$done==0, "#FFFFCC30", ifelse(Hx$done==1, "#CCCCCC90", "#99FF6680"))
          Hcol = ifelse(Hx$done==0 & is.na(Hx@data$N6)==TRUE, "#CCCCCC90", ifelse(Hx$done==0,"#FFFFCC30",ifelse(Hx$done==1, "#CCCCCC90", "#99FF6680"))) #trying to get edge hexagons to be grayed out
          plot(HexPols, col=Hcol)
          points(x=Hx@data$prevLocX, y=Hx@data$prevLocY, pch = 20, cex = 0.5, col = "pink") #use LocX - unmodified by movement on this, turn, pink points represent previous locations
          points(x=Hx@data$LocX, y=Hx@data$LocY, pch = 20, cex = 0.5, col = "red") #use LocX - unmodified by movement on this turn
          
          sing <- which(Hx@data$Action==1)
          cCol <- "#CCFF6630"
          for(cir in sing) {
            draw.circle(x=Hx@data$LocX[cir], y=Hx@data$LocY[cir], radius=CallRad, col = cCol)
          }
           # arrows(x0=LocX[Mvrs,1], y0=LocX[Mvrs,2], x1=Hx$Loc[Mvrs,1], y1=Hx$Loc[Mvrs,2], length = 0.05)
           text(x=min(HexPols@polygons[[1]]@Polygons[[1]]@coords[,1]), y = -50, labels = paste0("Time ", timeCnt, "   Call radius = ", round(CallRad)), pos = 4)
          
           Sys.sleep(0.25) #pause a bit to see the plot
        } #plot if statement
        # dev.off() #uncomment when you are printing images
       } #end of timecount while loop
      } #end of date for loop
  } #end of iteration loop

  return(outData)
  gc(reset = TRUE)
} #end function
