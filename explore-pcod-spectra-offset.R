# explore spectra simulation based on PCA
library(tidyverse)
library(plotly)
library(stringr)
library(mdatools)
library(prospectr)

# first I want to explore a few different ways of adding error and how it looks via PCA
# load packages

# Load in data
raw_dat <- read.csv("~/AFSC A&G Contract/Simulation Project/NIR-ageing-simulation/Data/Spectra_2010-2018_n9427.csv")

#####################   
# First look at number of specimens at each age
# Need to average spectra by age for simulation

counts <- raw_dat%>%
  #filter(Year == 2013)%>%
  group_by(Age)%>%
  summarize(n())

rm(counts)

## First need to pivot_longer to get column of absorbance
raw_dat_L<-raw_dat%>%tidyr::pivot_longer(.,cols=c(7:ncol(raw_dat)),names_to="wavenumber",values_to="absorbance", names_prefix = "x") 

# Clean up data
raw_dat_L$Age <- as.factor(raw_dat_L$Age)
raw_dat_L$Year <- as.factor(raw_dat_L$Year)
raw_dat_L$wavenumber <- as.numeric(raw_dat_L$wavenumber)

# Look at raw spectra
ggplot(raw_dat_L)+
           geom_line(aes(x = wavenumber, y = absorbance, group_by = Code, color = Year))+ 
           scale_x_reverse()+
           theme_classic()

# PCA of raw spectra

m <- pca(raw_dat[,7:506], ncomp = 7, scale= F, center = F)

# Plot scores
plotScores(m$res$cal, show.labels = FALSE, cgroup = raw_dat$Year)

# Try some preprocessing
nir_mat <- as.matrix(raw_dat[,-c(1:6)])
class(nir_mat) <- "numeric"

nir_sg <- savitzkyGolay(
  X = nir_mat,
  m = 1,  #differentiation order
  p = 2,  #polynomial order
  w = 25 #window size, odd
)

# Add "filename" back
dat_sg <- as.data.frame(cbind(raw_dat[,c(1:6)],nir_sg))

# PCA of raw spectra
m1 <- pca(dat_sg[,7:482], ncomp = 7, scale= F, center = F)

# Plot scores
plotScores(m1$res$cal, show.labels = FALSE, cgroup = dat_sg$Year)

# Plot scores ggplotly
r <- ggplot(raw_dat, aes(label = Code))+
  geom_point(aes(m$res$cal$scores[,1], m$res$cal$scores[,2], color = as.factor(Year)))+
  #stat_ellipse(aes(m$res$cal$scores[,1], m$res$cal$scores[,2], color = as.factor(Year)))+
  labs(title="Scores",
       caption="Source: P cod 2010-2018",
       x="Comp 1",
       y="Comp 2",
       color = "Year")+
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50")+
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50")+
  theme(panel.border = element_rect(fill = NA, color = "grey50"), panel.background = element_blank(), legend.key = element_rect(fill = "white"))

ggplotly(r)

s <- ggplot(dat_sg, aes(label = Code))+
  geom_point(aes(m1$res$cal$scores[,1], m1$res$cal$scores[,2], color = as.factor(Year)))+
  #stat_ellipse(aes(m1$res$cal$scores[,1], m1$res$cal$scores[,2], color = Year))+
  labs(title="Scores",
       caption="Source: P cod 2010-2018",
       x="Comp 1",
       y="Comp 2",
       color = "Year")+
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50")+
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50")+
  theme(panel.border = element_rect(fill = NA, color = "grey50"), panel.background = element_blank(), legend.key = element_rect(fill = "white"))

ggplotly(s)
