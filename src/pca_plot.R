#### PCA 3D Plot Functions

library(plot3D)
library(rgl)


# Create PCA --------------------------------------------------------------

audio_pca = prcomp(aw4[,c("aci","bio","adi","aei","num_vocals","species_diversity")], center = TRUE, scale. = TRUE)

apca = princomp(aw4[,c("aci","bio","adi","aei","num_vocals","species_diversity")], 
                cor = TRUE, 
                scores = TRUE)
scores = apca$scores
# Isolate PC Scores for each variable -------------------------------------


scores = as.data.frame(audio_pca$x)

head(scores[1:4])

plot3d(scores[,1:3], 
       size=5,
       col = seq(nrow(scores)))

text3d(scores[,1:3],
       texts=c(rownames(scores)), 
       cex= 0.7, pos=3)

coords <- NULL
for (i in 1:nrow(apca$loadings)) {
  coords <- rbind(coords, 
                  rbind(c(0,0,0),
                        apca$loadings[i,1:3]))
}
text3d(apca[,1:3], 
       texts=rownames(apca$loadings), 
       col="red")

lines3d(coords, 
        col="red", 
        lwd=1)


# PCA3D method ------------------------------------------------------------

ag_pca = pca3d(audio_pca,
               biplot = TRUE,
               components = 1:3)
makeMoviePCA()
