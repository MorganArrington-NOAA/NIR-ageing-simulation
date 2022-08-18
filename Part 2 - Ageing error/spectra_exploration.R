# explore spectra simulation based on PCA
library(tidyverse)
library(plotly)
library(stringr)
library(mdatools)

# first I want to explore a few different ways of adding error and how it looks via PCA
# load packages

# Load in data
raw_dat <- read.csv("~/AFSC A&G Contract/Simulation Project/Data/Spectra_2010-2018_n9428.csv")

sim_age_data <- read.csv("~/AFSC A&G Contract/Simulation Project/Data/simulated age data mat.csv") #simulated age data

#####################   
# First look at number of specimens at each age
# Need to average spectra by age for simulation

counts <- raw_dat%>%
  filter(Year == 2013)%>%
  group_by(Age)%>%
  summarize(n())

rm(counts)

## First need to pivot_longer to get column of absorbance
raw_dat_L<-raw_dat%>%tidyr::pivot_longer(.,cols=c(6:ncol(raw_dat)),names_to="wavenumber",values_to="absorbance", names_prefix = "x") 

# Clean up data
raw_dat_L$Age <- as.factor(raw_dat_L$Age)
raw_dat_L$wavenumber <- as.numeric(raw_dat_L$wavenumber)

# Look at raw spectra
ggplotly(ggplot(filter(raw_dat_L, Year %in% c(2013)))+
  geom_line(aes(x = wavenumber, y = absorbance, group_by = Code, color = Age))+
  scale_x_reverse()+
  theme_classic())

# PLS model age vs. spectra
raw_dat$Age <- as.numeric(raw_dat$Age)
m2 <- pls(raw_dat[,7:505], raw_dat$Age, ncomp = 10, scale = T)

# Plot scores
plotScores(m2$res$cal$xdecomp, show.labels = FALSE, cgroup = raw_dat$Age)

plotPredictions(m2$res$cal, show.stat = T)

# PCA of raw spectra
raw_dat_filt <- filter(raw_dat, Year %in% c(2010))
m <- pca(raw_dat_filt[,7:505], ncomp = 7, scale= F, center = F)

# Plot scores
plotScores(m$res$cal, show.labels = FALSE, cgroup = raw_dat_filt$Age)

ggplotly(ggplot(raw_dat_filt)+
           geom_point(aes(m$res$cal$scores[,1], Age, color = as.factor(Age)))+
  scale_y_continuous(lim=c(1,10), breaks = seq(1,10,1))+
  labs(x = "PC1 Scores")+
  theme_classic())

ggplotly(ggplot(raw_dat_filt)+
           geom_boxplot(aes(m$res$cal$scores[,1], middle = mean(m$res$cal$scores[,1]), color = as.factor(Age), fatten = NULL))+
           scale_y_continuous(lim=c(1,10), breaks = seq(1,10,1))+
           labs(x = "PC1 Scores")+
           theme_classic())

# See if we can recompose spectra from scores and loadings
Z1 <- as.matrix(m$loadings[,1], ncols = 1, byrow = F) #loading matrix
Z1_T <- t(Z1) #transposed loading matrix

Scores1 <- as.matrix(m$res$cal$scores[,1], ncols = 1, byrow = F) # 

Spec <- Scores1%*%Z1_T

Spec<- as.data.frame(Spec)
Spec$spec_num <- seq(1:941)
Spec$Age <- raw_dat_filt$Age

# Pivot for plotting
Spec_L<-as.data.frame(Spec)%>%tidyr::pivot_longer(.,cols=c(1:499),names_to="wavenumber",values_to="absorbance", names_prefix = "x") 

# Plot to see what recomposed spectra from PC1 look like
ggplot(filter(Spec_L, spec_num == 1))+
  geom_line(aes(as.numeric(wavenumber), absorbance, group = spec_num), linetype = 3)+
  geom_line(data = filter(raw_dat_L, Year == 2013, Code == "2010_89_1"), aes(wavenumber, absorbance, group = Code))+
  scale_y_continuous(lim = c(0,2), breaks = c(0, 0.5, 1, 1.5, 2))+
  scale_x_reverse()+
  theme_classic()

# One thing I want to try: reconstruct spectra for age 10 using PC1, PC2, and PC3 and plot all on same figure. Want to see what PC3 is modeling in the spectra.

