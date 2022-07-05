# ordination of top tracks of CSSS 22

# load libraries
library(ggplot2)
library(ggradar)
library(scales)
library(ggplot2)
library(ggiraphExtra)
library(ggRadar)
library(ggrepel)
library(ggridges)

# load data
tracks <- read.table("/Volumes/BunnyBike/dataviz/csss-spotify/tracks.tsv", 
                     sep = "\t", header = T)
tracks <- tracks[complete.cases(tracks ), ]
scaled_tracks <- as.data.frame(scale(tracks[,c(5:16)]))

tracks_pca <- prcomp(tracks[,c(5:16)], center = TRUE, scale = TRUE)

library(ggbiplot)
ggbiplot(tracks_pca)

library(reshape)


scaled_tracks_melted <- melt(scaled_tracks)
scaled_tracks_melted <- scaled_tracks_melted[!scaled_tracks_melted$variable=="duration_ms",]
scaled_tracks_melted <- scaled_tracks_melted[!scaled_tracks_melted$variable %in% c("duration_ms", "key","mode","tempo", "valence"),]

palette <- c('royalblue1', 'orange', 'seagreen4', 'gold', 'pink', 'purple', 
             'orangered2')

ggplot(scaled_tracks_melted, aes(x = value, y = variable, fill = variable)) +  
  geom_density_ridges()+ 
  #scale_fill_manual(values = c()) +
  scale_fill_manual(values = palette) +
  xlim(-5,5)+
  theme_classic()
#geom_histogram(position = "identity", alpha = 0.2, bins = 50)



ggplot(tracks$popularity) +  
  geom_histogram(position = "identity", alpha = 0.2, bins = 50)


hist(tracks$popularity)
