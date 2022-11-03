# Okay, starting out new analysis
# What I want to end up with is samp that has a list of different datasets, each dataset has spectra, OG age estimate, new drawn age estimate based on ageing error matrix.

# Packages
library(tidyverse)
library(plotly)
library(stringr)
library(mdatools)
library(ggpubr)

# First, let's use data just from 2013
# No errors, and no instrument off-set issues

#######################
# Load in 2017 spectral data
spec_dat <- read.csv("~/AFSC A&G Contract/Simulation Project/Data/Spectra_2010-2018_n9428.csv")

all_dat <- spec_dat #FOR NOW so that I don't have to rename everything below, eventually all_dat will be merged spec_dat and meta_dat

###################
# figure out how to join meta_data later if I am interested in any other data types
# meta_dat <- read.csv("~/AFSC A&G Contract/Simulation Project/Data/MetaData_2010-2019_n14579.csv")
# 
# meta_dat$Code <- as.factor(meta_dat$Code)
# 
# all_dat <- left_join(raw_dat, meta_dat, by = "Code")
###################

counts <- all_dat%>% #see how many specimens per age
  group_by(Age)%>%
  summarize(n())


#######################
# Plot raw spectra - 
## First need to pivot_longer to get column of absorbance
all_dat_L<-all_dat%>%tidyr::pivot_longer(.,cols=c(6:505),names_to="wavenumber",values_to="absorbance", names_prefix = "x") 

# Clean up data
all_dat_L$Age <- as.factor(all_dat_L$Age)
all_dat_L$wavenumber <- as.numeric(all_dat_L$wavenumber)

# Look at raw spectra
ggplotly(ggplot(all_dat_L)+
           geom_line(aes(x = wavenumber, y = absorbance, group_by = Code, color = Age))+
           scale_x_reverse()+
           theme_classic())

# Clear environment to speed up processing
rm(all_dat_L)
rm(spec_dat)
# Load in ageing error matrix
agemat <- read.csv("~/AFSC A&G Contract/Simulation Project/Puntilizer/B0_S3/ageing error matrix.csv", row.names=1) # load in ageing error matrix 

# Keep only rows that match ages in all_dat dataset (age 1-13)

agemat_match <- agemat[-c(1,15:21),] #remove age 0 and ages 14-20

Iter <- 1000
samp <- matrix(, nrows = nrow(all_dat), ncols = Iter, byrow = F)
age_jit <- vector()


set.seed(13)
for (k in 1:Iter) { #each loop iterates through agemat rows for ages 0:20 and samples from numbers 0-20 based on probabilities
  for (i in 1:nrow(all_dat)) {
    age_jit[i]<- sample(x = c(0:20), #drawing from numbers 0-20
                        size = 1, 
                        prob = agemat_match[all_dat[i,]$Age,], #select row == to all_dat$Age of probabilities for age 0 - 20
                        replace = TRUE)
  }
  samp[,1]<-age_jit
}

test <- data.frame(samp[[1]],all_dat)
colnames(test)[1] <- "jittered_age"

# write function to merge age_jit with spectra
# might not actually need to do this step
merge_dat <- function(df){
  sim_df <- data.frame(df, all_dat)
  colnames(sim_df)[1] <- "jittered_age"
  sim_df
}

jitter_samp <- map(samp, merge_dat)

test_df <- jitter_samp[[1]]

rm(test)
rm(test_df)
rm(samp)
rm(all_dat)
rm(agemat)
rm(agemat_match)

#######################
# Now ready to pass these through code for model fitting!

