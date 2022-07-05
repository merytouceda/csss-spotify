# Who is the most basic
# CSSS 2022

# load libraries
library(ggplot2)
library(ggrepel)
library(peRReo)
library(dplyr)
library(gridExtra)
library(grid)
library(gridtext)
library(tidyverse)
library(ggpubr)



# palettes
pal1=latin_palette("badbunny1",50,type="continuous")

# load data
topartists <- read.table("/Volumes/BunnyBike/dataviz/csss-spotify/user_top_artists.tsv",sep = '\t', 
                         header = T)
toptracks <- read.table("/Volumes/BunnyBike/dataviz/csss-spotify/user_top_tracks.tsv",sep = '\t', 
                         header = T)
metadata <- read.csv("/Volumes/BunnyBike/dataviz/csss-spotify/csss_spoty_metadata.csv")


# split dataset
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
  ggtitle("Ben Hosken")+
  scale_y_reverse()+
  xlab("How popular the artist is")+
  ylab("How high is this artist on my list")+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, colour="black", size=0.8)+
  scale_color_gradientn(colors=rev(pal1))+
  #stat_cor(method = "pearson", label.x = -5, label.y = 30, size = 2)+
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
  ggtitle("John Malloy")+
  xlab("How popular the artist is")+
  ylab("How high is this artist on my list")+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, colour="black", size=0.8)+
  scale_color_gradientn(colors=rev(pal1))+
  #stat_cor(method = "pearson", label.x = -5, label.y = 30, size = 2)+
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
  ggtitle("Simone Poetto")+
  xlab("How popular the artist is")+
  ylab("How high is this artist on my list")+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, colour="black", size=0.8)+
  scale_color_gradientn(colors=rev(pal1))+
  #stat_cor(method = "pearson", label.x = -5, label.y = 30, size = 2)+
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
  ggtitle("Keiko Namuraa")+
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
  ggtitle("Lena Mangold")+
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
  ggtitle("Lou di Felice")+
  xlab("How popular the artist is")+
  ylab("How high is this artist on my list")+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, colour="black", size=0.8)+
  scale_color_gradientn(colors=rev(pal1))+
  theme(legend.position = "none", axis.title=element_text(size=14), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# stats
cor.test(loudifelice$top_position, loudifelice$artist_popularity)


# - Debankur
debankurribu_basic <- 
  ggplot(debankurribu, aes(y = top_position, x = artist_popularity, label = name, 
                          color = top_position, group = email))+
  geom_point(aes(size = artist_popularity),show.legend = FALSE)+ 
  #geom_text(aes(label=name), size=3)+
  #geom_label_repel()+
  scale_y_reverse()+ 
  ggtitle("Debankur Ribu")+
  xlab("How popular the artist is")+
  ylab("How high is this artist on my list")+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, colour="black", size=0.8)+
  scale_color_gradientn(colors=rev(pal1))+
  theme(legend.position = "none", axis.title=element_text(size=14), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# stats
cor.test(debankurribu$top_position, debankurribu$artist_popularity)


# - Thithi
thibautprouteau_basic <- 
  ggplot(thibautprouteau, aes(y = top_position, x = artist_popularity, label = name, 
                           color = top_position, group = email))+
  geom_point(aes(size = artist_popularity),show.legend = FALSE)+ 
  #geom_text(aes(label=name), size=3)+
  #geom_label_repel()+
  scale_y_reverse()+ 
  ggtitle("Thibault Prouteau")+
  xlab("How popular the artist is")+
  ylab("How high is this artist on my list")+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, colour="black", size=0.8)+
  scale_color_gradientn(colors=rev(pal1))+
  theme(legend.position = "none", axis.title=element_text(size=14), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# stats
cor.test(thibautprouteau$top_position, thibautprouteau$artist_popularity)


# - Thithi
violetacalleja_basic <- 
  ggplot(violetacalleja, aes(y = top_position, x = artist_popularity, label = name, 
                              color = top_position, group = email))+
  geom_point(aes(size = artist_popularity),show.legend = FALSE)+ 
  #geom_text(aes(label=name), size=3)+
  #geom_label_repel()+
  scale_y_reverse()+
  ggtitle("Violeta Calleja")+
  xlab("How popular the artist is")+
  ylab("How high is this artist on my list")+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE, colour="black", size=0.8)+
  scale_color_gradientn(colors=rev(pal1))+
  theme(legend.position = "none", axis.title=element_text(size=14), panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# stats
cor.test(violetacalleja$top_position, violetacalleja$artist_popularity)




yright = richtext_grob("How high is this artist on list", rot=90, gp = gpar(fontsize = 20))
bottom = richtext_grob(text = 'How popular the artist is', gp = gpar(fontsize = 20))

p <- list(benhosken_basic, johnmalloy_basic, simonepoetto_basic,
          keikonamuraa_basic, lenamangold_basic,loudifelice_basic,
          debankurribu_basic,thibautprouteau_basic,violetacalleja_basic) %>% map(~.x + labs(x=NULL, y=NULL))

g <- grid.arrange(grobs=p,nrow = 3, left = yright, bottom = bottom)
ggsave("/Volumes/BunnyBike/dataviz/csss-spotify/whoisbasic.png", g)









