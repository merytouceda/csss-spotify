# Who is the most basic
# CSSS 2022

# load libraries
library(ggplot2)
library(ggrepel)
library(peRReo)
library(dplyr)

# palettes
pal1=latin_palette("badbunny1",50,type="continuous")

# load data
topartists <- read.table("/Volumes/BunnyBike/dataviz/csss-spotify/user_top_artists.tsv",sep = '\t', 
                         header = T)
toptracks <- read.table("/Volumes/BunnyBike/dataviz/csss-spotify/user_top_tracks.tsv",sep = '\t', 
                         header = T)
metadata <- read.csv("/Volumes/BunnyBike/dataviz/csss-spotify/csss_spoty_metadata.csv")


# add name to top sets
#topartists$username <- metadata$Name


# devide table by user
topartists %>%
  group_by(email)

split_list <- split(topartists,topartists$email)

benhosken <- as.data.frame(split_list[[1]])
johnmalloy <- as.data.frame(split_list[[2]])
simonepoetto <- as.data.frame(split_list[[3]])
keikonamuraa <- as.data.frame(split_list[[4]])
lenamangold <- as.data.frame(split_list[[5]])
loudifelice<- as.data.frame(split_list[[6]])
debankurribu <- as.data.frame(split_list[[7]])
thibautprouteau <- as.data.frame(split_list[[8]])
violetacalleja <- as.data.frame(split_list[[9]])


# plot galore

# - Ben
benhosken_basic <- 
  ggplot(benhosken, aes(y = top_position, x = artist_popularity, label = name, 
                       color = top_position, group = email))+
  geom_point(aes(size = artist_popularity),show.legend = FALSE)+ 
  #geom_text(aes(label=name), size=3)+
  #geom_label_repel()+
  scale_y_reverse()+ 
  xlab("How popular the artist is")+
  ylab("How high is this artist on my list")+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, colour="black", size=0.8)+
  scale_color_gradientn(colors=rev(pal1))+
  theme(legend.position = "none", axis.title=element_text(size=14), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# stats
cor.test(benhosken$top_position, benhosken$artist_popularity)



# - John
johnmalloy_basic  <- 
  ggplot(johnmalloy , aes(y = top_position, x = artist_popularity, label = name, 
                        color = top_position, group = email))+
  geom_point(aes(size = artist_popularity),show.legend = FALSE)+ 
  #geom_text(aes(label=name), size=3)+
  #geom_label_repel()+
  scale_y_reverse()+ 
  xlab("How popular the artist is")+
  ylab("How high is this artist on my list")+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, colour="black", size=0.8)+
  scale_color_gradientn(colors=rev(pal1))+
  theme(legend.position = "none", axis.title=element_text(size=14), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# stats
cor.test(johnmalloy$top_position, johnmalloy$artist_popularity)


# - Simone
simonepoetto_basic  <- 
  ggplot(simonepoetto , aes(y = top_position, x = artist_popularity, label = name, 
                          color = top_position, group = email))+
  geom_point(aes(size = artist_popularity),show.legend = FALSE)+ 
  #geom_text(aes(label=name), size=3)+
  #geom_label_repel()+
  scale_y_reverse()+ 
  xlab("How popular the artist is")+
  ylab("How high is this artist on my list")+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, colour="black", size=0.8)+
  scale_color_gradientn(colors=rev(pal1))+
  theme(legend.position = "none", axis.title=element_text(size=14), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# stats
cor.test(simonepoetto$top_position, simonepoetto$artist_popularity)


# - Keiko
keikonamuraa_basic <- 
  ggplot(keikonamuraa, aes(y = top_position, x = artist_popularity, label = name, 
                        color = top_position, group = email))+
  geom_point(aes(size = artist_popularity),show.legend = FALSE)+ 
  #geom_text(aes(label=name), size=3)+
  #geom_label_repel()+
  scale_y_reverse()+ 
  xlab("How popular the artist is")+
  ylab("How high is this artist on my list")+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, colour="black", size=0.8)+
  scale_color_gradientn(colors=rev(pal1))+
  theme(legend.position = "none", axis.title=element_text(size=14), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# stats
cor.test(keikonamuraa$top_position, keikonamuraa$artist_popularity)


# - Lena
lenamangold_basic <- 
  ggplot(lenamangold, aes(y = top_position, x = artist_popularity, label = name, 
                        color = top_position, group = email))+
  geom_point(aes(size = artist_popularity),show.legend = FALSE)+ 
  #geom_text(aes(label=name), size=3)+
  #geom_label_repel()+
  scale_y_reverse()+ 
  xlab("How popular the artist is")+
  ylab("How high is this artist on my list")+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, colour="black", size=0.8)+
  scale_color_gradientn(colors=rev(pal1))+
  theme(legend.position = "none", axis.title=element_text(size=14), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# stats
cor.test(lenamangold$top_position, lenamangold$artist_popularity)

# - Lou
loudifelice_basic <- 
  ggplot(loudifelice, aes(y = top_position, x = artist_popularity, label = name, 
                          color = top_position, group = email))+
  geom_point(aes(size = artist_popularity),show.legend = FALSE)+ 
  #geom_text(aes(label=name), size=3)+
  #geom_label_repel()+
  scale_y_reverse()+ 
  xlab("How popular the artist is")+
  ylab("How high is this artist on my list")+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, colour="black", size=0.8)+
  scale_color_gradientn(colors=rev(pal1))+
  theme(legend.position = "none", axis.title=element_text(size=14), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# stats
cor.test(loudifelice$top_position, loudifelice$artist_popularity)


