# need to load these packages first
library(MASS)  
library(adehabitatHR)  
library(pbapply)  

acoustic_spaces <- function(X, labels, n.elements = seq(5, 40, 5), cl = 1, kernel = FALSE, pb = TRUE) {
  
  # reset progress bar when exiting
  on.exit(pbapply::pboptions(type = .Options$pboptions$type))
  
  # set progress bar
  pbapply::pboptions(type = ifelse(pb, "timer", "none"))
  
  # convert to distance
  dst_mt <- sqrt(1- X)
  
  # shuflle data position
  shff <- sample(1:nrow(dst_mt))
  dst_mt <- dst_mt[shff, shff] 
  labels <- labels[shff]
  
  # mds for 2 dimensions
  mds <- isoMDS(dst_mt, y = cmdscale(dst_mt, k = 2), k = 2, maxit = 5, trace = FALSE, tol = 1e-3, p = 2)
  
  # repertoires
  rps <- rep(n.elements, 10000)
  cs <- cumsum(rps)
  rps <- rps[cs < nrow(X)]
  
  rps <- sort(rps, decreasing = TRUE)
  
  df <- data.frame(labels, id = NA, mds$points)
  
  df$row <- 1:nrow(df)
  
  for(i in 1:length(rps)){
    
    # number of element types for each individual
    n <- rps[i]
    
    # element replicates that are available
    to.fill <- df$row[is.na(df$id)]
    df2 <- df[df$row %in% to.fill, ]
    to.fill <- df2$row[!duplicated(df2$labels)]
    
    # select the ones needed for this individual
    to.fill <- to.fill[1:n]  
    
    # add id of individual to data frame
    df$id[df$row %in% to.fill] <- i     
  }  
  
  # put indiv id, element label and MDS vectors together
  Y <- data.frame(id = df$id, labels, X1 = df$X1, X2 = df$X2)
  
  # remove not assigned rows
  Y <- Y[!is.na(Y$id), ]
  
  # if kernel area was selected
  if (kernel){
    raref_fun <- function(min.n, W) {
      Z <- W[sample(1:nrow(W), min.n), ]
      coordinates(Z) <-  ~ X1 + X2
      
      kernel.area(kernelUD(Z, extent = 1.5), percent = 95)[[1]]
      
    }
    
    # calculate acoustic area for each individual 
    krnl.area <- pblapply(unique(Y$id), cl = cl, function(y) {
      
      # subset for each individual 
      W <- Y[Y$id == y, c("X1", "X2")]
      
      ka <- replicate(30, raref_fun(min.n = min(n.elements), W))
      
      
      return(data.frame(id = y, repertoire = nrow(W), area = mean(ka), stress = mds$stress))
      
    })
    
    acous.area <- do.call(rbind, krnl.area)
  } else  { # else use minimum convex polygon
    mcp.area <- pblapply(unique(Y$id), cl = cl, function(y) {
      
      # subset for each individual 
      W <- Y[Y$id == y, c("X1", "X2")]
      coordinates(W) <-  ~ X1 + X2
      
      return(data.frame(id = y, repertoire = nrow(W@coords), area = mcp(xy = W)$area, stress = mds$stress))
    })
    acous.area <- do.call(rbind, mcp.area)
  }
  
  print(paste(nrow(acous.area), "individuals simulated"))
  
  return(acous.area)
}

