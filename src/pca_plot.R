#### PCA 3D Plot Functions

library(plot3D)
library(rgl)


# Create PCA --------------------------------------------------------------

audio_pca = prcomp(aw4[,c("aci","bio","adi","aei","num_vocals","species_diversity")], center = TRUE, scale. = TRUE)

audio_pca$x[,1] = audio_pca$x[,1]*-1
audio_pca$x[,2] = audio_pca$x[,2]*-1

apca = princomp(aw4[,c("aci","bio","adi","aei","num_vocals","species_diversity")], 
                cor = TRUE, 
                scores = TRUE)
apca = as.data.frame(apca)
scores = as.data.frame(apca$scores)
names(scores) = c("PC1", "PC2", "PC3")

apca$loadings[,1] = apca$loadings[,1]*-1 # change so adi is positive
apca$loadings[,2] = apca$loadings[,2]*-1 # change so higher num vocals & species diversity is positive COMMENT OUT IF ON LINUX!!!

# scores[,1] = scores[,1]*-1
# scores[,2] = scores[,2]*-1

# Isolate PC Scores for each variable -------------------------------------
#https://planspace.org/2013/02/03/pca-3d-visualization-and-clustering-in-r/
plot3d(scores[,1:3], 
       alpha = 1, 
       size=1,
       xlim = c(-0.7,0.7),
       ylim = c(-0.4, 0.7),
       zlim = c(-0.8, 0.8))
# text3d(apca$scores[,1:3],texts=rownames(aw4))
text3d(apca$loadings[,1:3], 
       texts=rownames(apca$loadings),
       col="red",
       size = 10)
coords <- NULL
for (i in 1:nrow(apca$loadings)) {
  coords <- rbind(coords, rbind(c(0,0,0),apca$loadings[i,1:3]))
}
lines3d(coords, col="red", lwd=4)
snapshot3d("results/ag_pca_full.png")

# Making it so Loading lines are longer
plot3d(scores[,1:3], 
       alpha = 1, 
       size=1)
# text3d(apca$scores[,1:3],texts=rownames(aw4))
# text3d(apca$loadings[,1:3]*10, 
#        texts=rownames(apca$loadings),
#        col="red",
#        size = 10)
coords <- NULL
for (i in 1:nrow(apca$loadings)) {
  coords <- rbind(coords, rbind(c(0,0,0),apca$loadings[i,1:3]))
}
lines3d(coords*10, col="red", lwd=4)
snapshot3d("results/ag_pca_full_no_text.png")

# PCA3D method ------------------------------------------------------------
library(pca3d)
ag_pca = pca3d(audio_pca,
               biplot = TRUE,
               components = 1:3)
snapshotPCA3d("ag_pca.png")