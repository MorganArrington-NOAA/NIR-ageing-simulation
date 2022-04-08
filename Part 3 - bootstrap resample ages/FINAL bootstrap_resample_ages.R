# Okay, starting out new analysis
# What I want to end up with is samp that has a list of different datasets, each dataset has spectra, OG age estimate, new drawn age estimate based on ageing error matrix.

# Packages
library(dplyr)
library(purrr)
library(ggpubr)
library(plotly)
library(doParallel)
# library(stringr)
# library(mdatools)
# First, let's use data just from 2013
# No errors, and no instrument off-set issues

#######################
# Load in 2017 spectral data
spec_dat <- read.csv("Z:/NIR-ageing-simulation/Data/Spectra_2010-2018_n9427.csv")

###################
# join meta_data so we can filter by reader index
meta_dat <- read.csv("Z:/NIR-ageing-simulation/Data/MetaData_2010-2019_n14579.csv")

spec_dat <- read.csv("~/AFSC A&G Contract/Simulation Project/NIR-ageing-simulation/Data/Spectra_2010-2018_n9427.csv")

###################
# join meta_data so we can filter by reader index
meta_dat <- read.csv("~/AFSC A&G Contract/Simulation Project/NIR-ageing-simulation/Data/MetaData_2010-2019_n14579.csv")

meta_dat$Code <- as.factor(meta_dat$Code)

all_dat <- left_join(spec_dat, meta_dat, by = "Code")
###################

# all_dat <- filter(all_dat, Year == "2013")
all_dat <- filter(all_dat, reader_index!= "6")
all_dat <- filter(all_dat, reader_index != "68")

counts <- all_dat%>% #see how many specimens per age
  group_by(Age)%>%
  summarize(n())

save(all_dat, file = "Z:/NIR-ageing-simulation/Data/all_dat_filt.rda")
#######################
# # Plot raw spectra - 
# ## First need to pivot_longer to get column of absorbance
# all_dat_L<-all_dat%>%tidyr::pivot_longer(.,cols=c(6:505),names_to="wavenumber",values_to="absorbance", names_prefix = "x") 
# 
# # Clean up data
# all_dat_L$read_age <- as.factor(all_dat_L$read_age)
# all_dat_L$wavenumber <- as.numeric(all_dat_L$wavenumber)
# 
# # Look at raw spectra
# ggplotly(ggplot(all_dat_L)+
#            geom_line(aes(x = wavenumber, y = absorbance, group_by = Code, color = Age))+
#            scale_x_reverse()+
#            theme_classic())

# Clear environment to speed up processing
rm(all_dat_L)
rm(spec_dat)
rm(meta_dat)

# Load in ageing error matrices for each reader in 2013
# Reader 3 - B1_S2

agemat_3 <- read.csv("Z:/NIR-ageing-simulation/Puntilizer/Reader 3 tester 6 2010-2019/B1_S2/ageing error matrix Reader 1.csv", row.names=1) # load in ageing error matrix 

# Reader 8 - B2_S2
agemat_8 <- read.csv("Z:/NIR-ageing-simulation/Puntilizer/Reader 8 tester 6 2010-2019/B2_S2/ageing error matrix Reader 1.csv", row.names=1) # load in ageing error matrix 

# Reader 21 - B2_S1_S3
agemat_21 <- read.csv("Z:/NIR-ageing-simulation/Puntilizer/Reader 21 tester 6 2010-2019/B2_S1_S3/ageing error matrix Reader 1.csv", row.names=1) # load in ageing error matrix 

# Reader 59 - B0_S1_S3
agemat_59 <- read.csv("Z:/NIR-ageing-simulation/Puntilizer/Reader 59 tester 6 2010-2019/B0_S1_S3/ageing error matrix Reader 1.csv", row.names=1) # load in ageing error matrix 

# Reader 71 - B2_S1 
agemat_71 <- read.csv("Z:/NIR-ageing-simulation/Puntilizer/Reader 71 tester 6 2010-2019/B2_S1/ageing error matrix Reader 1.csv", row.names=1) # load in ageing error matrix 

agemat_3 <- read.csv("~/AFSC A&G Contract/Simulation Project/NIR-ageing-simulation/Puntilizer/Reader 3 tester 6 2010-2019/B1_S2/ageing error matrix Reader 1.csv", row.names=1) # load in ageing error matrix 

# Reader 8 - B2_S2
agemat_8 <- read.csv("~/AFSC A&G Contract/Simulation Project/NIR-ageing-simulation/Puntilizer/Reader 8 tester 6 2010-2019/B2_S2/ageing error matrix Reader 1.csv", row.names=1) # load in ageing error matrix 

# Reader 21 - B2_S1_S3
agemat_21 <- read.csv("~/AFSC A&G Contract/Simulation Project/NIR-ageing-simulation/Puntilizer/Reader 21 tester 6 2010-2019/B2_S1_S3/ageing error matrix Reader 1.csv", row.names=1) # load in ageing error matrix 

# Reader 59 - B0_S1_S3
agemat_59 <- read.csv("~/AFSC A&G Contract/Simulation Project/NIR-ageing-simulation/Puntilizer/Reader 59 tester 6 2010-2019/B0_S1_S3/ageing error matrix Reader 1.csv", row.names=1) # load in ageing error matrix 

# Reader 71 - B2_S1 
agemat_71 <- read.csv("~/AFSC A&G Contract/Simulation Project/NIR-ageing-simulation/Puntilizer/Reader 71 tester 6 2010-2019/B2_S1/ageing error matrix Reader 1.csv", row.names=1) # load in ageing error matrix 


# Keep only rows that match ages in all_dat dataset (age 1-13)
#### IDEA FOR IMPROVEMENT:  maybe try to correct specific records based on reader-specific age-reading error matrices

agemat_match_3 <- agemat_3[-1, c(-1,-15)] #remove age 0 and age 14
agemat_match_8 <- agemat_8[-1, c(-1,-15)] #remove age 0 and age 14
agemat_match_21 <- agemat_21[-1, c(-1,-15)] #remove age 0 and age 14
agemat_match_59 <- agemat_59[-1, c(-1,-15)] #remove age 0 and age 14
agemat_match_71 <- agemat_71[-1, c(-1,-15)] #remove age 0 and age 14

agemat3_use <- t(agemat_match_3) #transpose so that estimated ages is along vertical, gives us probability of each true age given an age estimate.
agemat8_use <- t(agemat_match_8) #transpose so that estimated ages is along vertical, gives us probability of each true age given an age estimate.
agemat21_use <- t(agemat_match_21) #transpose so that estimated ages is along vertical, gives us probability of each true age given an age estimate.
agemat59_use <- t(agemat_match_59) #transpose so that estimated ages is along vertical, gives us probability of each true age given an age estimate.
agemat71_use <- t(agemat_match_71) #transpose so that estimated ages is along vertical, gives us probability of each true age given an age estimate.

# test loop concept

# test <- if (all_dat[1,]$reader_index == 3){agemat3_use[all_dat[1,]$Age,]} else {
#   if (all_dat[1,]$reader_index == 8) {agemat8_use[all_dat[1,]$Age,]} else {
#     if(all_dat[1,]$reader_index == 21) {agemat21_use[all_dat[1,]$Age,]} else {
#       if(all_dat[1,]$reader_index == 59) {agemat59_use[all_dat[1,]$Age,]} else{
#         if(all_dat[1,]$reader_index == 71){agemat71_use[all_dat[1,]$Age,]}
#       }
#     }
#   }
# }

# Full sample loop
samp <- list()
age_jit <- vector()
Iter <- 10000

# set.seed(13)
# for (k in 1:Iter) { #each loop iterates through agemat rows for ages 1:13 and samples from numbers 1-13 based on probabilities
#   for (i in 1:nrow(all_dat)) {
#   age_jit[i]<- sample(x = c(1:14), #drawing from numbers 0-14
#                     size = 1,
#                     prob = if (all_dat[i,]$reader_index == 3){agemat3_use[all_dat[i,]$Age,]} else {if (all_dat[i,]$reader_index == 8) {agemat8_use[all_dat[i,]$Age,]} else {if(all_dat[i,]$reader_index == 21) {agemat21_use[all_dat[i,]$Age,]} else {if(all_dat[i,]$reader_index == 59) {agemat59_use[all_dat[i,]$Age,]} else{if(all_dat[i,]$reader_index == 71){agemat71_use[all_dat[i,]$Age,]}}}}},replace = TRUE) #select row == to all_dat$Age of probabilities for age 1 - 13
#   }
#   samp[[k]] <- age_jit
# }

# Can I do this in parallel?
n_cores <- detectCores()-2
cl <- makeCluster(n_cores)
registerDoParallel(cl)

system.time({
set.seed(13)
samp <- foreach(k=1:Iter) %dopar% {#each loop iterates through agemat rows for ages 1:13 and samples from numbers 1-13 based on probabilities
  for (i in 1:nrow(all_dat)) {
    age_jit[i]<- sample(x = c(1:14), #drawing from numbers 0-14
                        size = 1, 
                        prob = if (all_dat[i,]$reader_index == 3){agemat3_use[all_dat[i,]$Age,]} else {if (all_dat[i,]$reader_index == 8) {agemat8_use[all_dat[i,]$Age,]} else {if(all_dat[i,]$reader_index == 21) {agemat21_use[all_dat[i,]$Age,]} else {if(all_dat[i,]$reader_index == 59) {agemat59_use[all_dat[i,]$Age,]} else{if(all_dat[i,]$reader_index == 71){agemat71_use[all_dat[i,]$Age,]}}}}},replace = TRUE) #select row == to all_dat$Age of probabilities for age 1 - 13
  }
  age_jit
}
})

save(samp, file = "Z:/NIR-ageing-simulation/Data/boot_dat_10000.rda")

# 
rm(test)
rm(test_df)
# rm(samp)
# rm(all_dat)
rm(agemat)
rm(agemat_match)
rm(agemat_use)
# 
# #######################
# # Now ready to pass these through code for model fitting!
# 
plot(all_dat$Age, samp[[2]])
