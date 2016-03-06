#map for vecchi

library(dplyr)
library(ggplot2)
library(choroplethrAdmin1)

wd <- "~/Dropbox/ad_hoc_R/italyMap"
setwd(wd)



it <- get_admin1_map("italy")


df <- read.csv(file = paste0(wd, "/data/terremoto.csv"),
               stringsAsFactors = FALSE)  


df <- filter(df, Year >= 1861)
df <- filter(df, !is.na(Londef))
df <- filter(df, !is.na(Latdef))

##- create class
df$magnitude[df$Mwdef < 4] <- "< 4"
df$magnitude[df$Mwdef >= 4 & df$Mwdef < 4.5] <- ">= 4 and < 4.5"
df$magnitude[df$Mwdef >= 4.5 & df$Mwdef < 5] <-  ">= 4.5 and < 5"
df$magnitude[df$Mwdef >= 5 & df$Mwdef < 5.5] <-  ">= 5 and < 5.5"
df$magnitude[df$Mwdef >= 5.5 & df$Mwdef < 6] <-  ">= 5.5 and < 6"
df$magnitude[df$Mwdef >= 6 & df$Mwdef < 6.5] <-  ">= 6 and < 6.5"
df$magnitude[df$Mwdef >= 6.5] <-  ">= 6.5"

df$shape[df$Mwdef >= 6.5] <- "strong"
df$shape[is.na(df$shape)] <- "not strong"


df.sm <- filter(df, df$Mwdef < 6.5)
df.large <- filter(df, df$Mwdef >= 6.5)

##ATTEMPT TO COMBINE IN ONE LAYER MANUALLY SET SIZE, GIVES ALL 3 LEGENDS
df$size[df$shape == "strong"] <- 15
df$size[df$shape == "not strong"] <- 1


##FINAL MAP
p <- ggplot()
p <- p + geom_polygon(data = it, aes(x = long, y = lat, group = group), colour="darkgrey", fill = "white") 
p <- p +  geom_polygon(color = "grey", fill = "grey") 
p <- p + geom_point(data = df, aes(x=Londef, y=Latdef, color = magnitude), alpha = .4)
p <- p + scale_colour_manual(name = "   Magnitude", 
                             values = c("grey75","grey43","grey15","darksalmon","red", "red4", "red4"))
p <- p + geom_point(data = df.large, aes(x=Londef, y=Latdef), size = 6, shape = 18, color = "red4")
p <- p + geom_text(data=df.large, aes(x=Londef, y=Latdef,label=Year), size = 5, vjust = -.25, hjust = -.25,
                   fontface="bold")
p <- p + theme_classic() 
p <- p + theme(axis.line = element_blank(), 
                 axis.text.x = element_blank(), 
                 axis.text.y = element_blank(),
                 axis.ticks = element_blank(), 
                 axis.title.x = element_blank(), 
                 axis.title.y = element_blank())
p





