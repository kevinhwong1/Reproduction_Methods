#Title:Coral reproduction methods
#Author: KH Wong
#Date Last Modified: 20190102
#See Readme file for details 

### Larval Fixation ###

#Load Libaries
library(dbplyr)
library(tidyverse)
library(readr)
library(stringr)

setwd("~/MyProjects/Reproduction_Methods/Data/") 

#Load Data
Prefix <- read_csv("~/MyProjects/Reproduction_Methods/Data/PreFixationSizing.csv")
Postfix <- read_csv("~/MyProjects/Reproduction_Methods/Data/PostFixationSizing.csv")


#Making a unique column
Prefix$coral.date <- paste(Prefix$coral.id, Prefix$Date)
Postfix$coral.date <- paste(Postfix$coral.id, Postfix$Date)

#Calculating means
prefix.mean <- summarySE(Prefix, measurevar="Volume", groupvars="coral.date")
postfix.mean <- summarySE(Postfix, measurevar="Volume", groupvars="coral.date")

#Merging datasets
pre.post.size <- merge(prefix.mean, postfix.mean, by = "coral.date")

#Scatterplot
fixation.plot<- ggplot(data = pre.post.size, aes(x=Volume.x, y=Volume.y))+
  ylab("Pre-fixation Volume")+ xlab("Post-fixation Volume") + 
  geom_point()+
  geom_smooth(method = "lm") +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#creating a linear model
lm.fix <- lm (Volume.x ~ Volume.y, data = pre.post.size) #creating a linear model off of standard curve
lm.fix.summary <- summary(lm.fix) #creating a summary outfut for the linear model
lm.fix.summary 

#calculating percent change 
pre.post.size$p.change <- ((pre.post.size$Volume.y - pre.post.size$Volume.x)/pre.post.size$Volume.x) * 100 

#adding metadata
pre.post.size$coral.id <- str_split_fixed(pre.post.size$coral.date, " ", 2) #creating colony id column
colony.metadata <- read_csv("~/MyProjects/Reproduction_Methods/Data/Colony.Metadata.csv")

pre.post.size.data <- merge(pre.post.size, colony.metadata, by = "coral.id")

pchange.mean <- summarySE(pre.post.size.data, measurevar="p.change", groupvars="reef.zone")

pd <- position_dodge(0.1) # moves object .05 to the left and right
pchange.plot<- ggplot(data = pchange.mean, aes(x=reef.zone, y=p.change))+
  ylab("Percent change in Volume")+ xlab("Reef zone") + 
  geom_point()+
  geom_errorbar(aes(ymin=p.change-se, ymax=p.change+se), width=.1, position=pd, color="black") + #Error bars
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) 


