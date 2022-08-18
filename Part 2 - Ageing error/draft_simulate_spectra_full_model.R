# load packages
library(tidyverse)
library(plotly)
library(stringr)
library(mdatools)

# Load in data
raw_dat <- read.csv("~/AFSC A&G Contract/Simulation Project/Data/Spectra_2010-2018_n9428.csv")

sim_age_data <- read.csv("~/AFSC A&G Contract/Simulation Project/Data/simulated age data mat.csv") #simulated age data

#####################   
# First look at number of specimens at each age
# Need to average spectra by age for simulation
counts <- raw_dat%>%
  group_by(Age)%>%
  summarize(n())

rm(counts)

## First need to pivot_longer to get column of absorbance
raw_dat_L<-raw_dat%>%tidyr::pivot_longer(.,cols=c(6:ncol(raw_dat)),names_to="wavenumber",values_to="absorbance", names_prefix = "x") 

# Clean up data
raw_dat_L$Age <- as.factor(raw_dat_L$Age)
raw_dat_L$wavenumber <- as.numeric(raw_dat_L$wavenumber)

# Take mean and SE by final_age
summ <- raw_dat_L %>% 
  group_by(wavenumber, Age) %>%
  summarize(mean_spec = mean(absorbance), se_spec = sd(absorbance)/sqrt(n()))

rm(raw_dat_L)

# Plot it, how much separation is there? Good enough as basis for simulation?
f <- ggplot(summ)+
  geom_line(aes(x = wavenumber, y = mean_spec, color = Age))+
  geom_ribbon(aes(x = wavenumber, ymin = mean_spec - se_spec, ymax = mean_spec + se_spec, fill = Age), alpha = 0.3)+
  # scale_color_manual(values = oo_colors)+
  # scale_fill_manual(values = oo_colors)+
  # scale_x_continuous(breaks=seq(2600,3250,250))+
  scale_x_reverse()+
  theme_classic()

ggplotly(f)

# Need to mess with these to make them more clean cut and obvious. I like the idea of adding SE or something to 7,8,9,10 because it'll keep the shape of the curve realistic.

#########################################
# add SE to 7 and 10 to push them to upper end of their range to space spectra out for simulation
summ$Age <- as.numeric(summ$Age)

summ <- summ%>%
  mutate(new_mean = case_when(Age == 7 ~ mean_spec+se_spec, Age == 10 ~ mean_spec + se_spec, Age %in% c(1,2,3,4,5,6,8,9) ~ mean_spec))

# Check again
f <- ggplot(summ)+
  geom_line(aes(x = wavenumber, y = new_mean, color = as.factor(Age)))+
  # geom_ribbon(aes(x = wavenumber, ymin = new_mean - se_spec, ymax = new_mean + se_spec, fill = as.factor(Age)), alpha = 0.3)+
  # scale_color_manual(values = oo_colors)+
  # scale_fill_manual(values = oo_colors)+
  # scale_x_continuous(breaks=seq(2600,3250,250))+
  scale_x_reverse()+
  theme_classic()

ggplotly(f)

# # first lets see what happens when we multiply by a fraction and add it to the spectra.
# test <- raw_dat_L %>% 
#   filter(file_name == "PACIFIC_COD_89201301202_657_OT1.0")%>%
#   mutate(new_abs = absorbance*1.09)
# 
# ggplot(test)+
#   geom_line(aes(x = wavenumber, y = absorbance))+
#   geom_line(aes(x = wavenumber, y = new_abs))+
#   scale_x_reverse()

# Ok, that works. Now we need to use sim data set, match averaged spectra by age to each "known_age" 

avg_spectra <- summ%>%tidyr::pivot_wider(.,values_from="new_mean",id_cols="Age",names_from="wavenumber") # pivot_wider so I can keep track of what's happening

avg_spectra <- avg_spectra%>%
  rename(known_age = Age) # rename as "known_age" now that these are becoming basis for simulated spectra

# Now we need to find a way to add these all to the simulated age data
avg_spectra$known_age <- as.integer(as.character(avg_spectra$known_age)) #make data types same in two different datasets

sim_df <- left_join(sim_age_data, avg_spectra, by = "known_age") #join

## For now filter this for ages 1-10 ##
sim_df <- sim_df%>%
  filter(known_age %in% c(1,2,3,4,5,6,7,8,9,10))

sim_df <- sim_df[,-1] #remove first column

################# 
# Now we have a sim_df that has "known_age", "error_age", as well as matching spectra for each "known_age" fish! 

# Time to introduce some error

# Pivot longer for plotting and for adding error
sim_df_L <- sim_df%>%tidyr::pivot_longer(.,cols=c(3:ncol(sim_df)),names_to="wavenumber",values_to="absorbance")

sim_df_L$wavenumber <- as.numeric(sim_df_L$wavenumber)

# generate some error around spectra for modeling

## 1 
# use consistent number for error makes some sense this would scale with age because of increase in growth variability?

# in this example, we have 10 ages, 10 * 10 = 100 numbers. 
# Make 10 reps to start with, draw random error and multiply it by spectra. Repeat 10 times in case random chance error matches with error age. 

samp <- list() # creates an empty list
Iter <- 1000 # reps

for (k in 1:Iter){ # make Iter objects in a list 
  samp[[k]] <- sim_df %>% 
    dplyr::mutate(error = rnorm(100, mean = 0, sd = 0.05), across(c(`4016`:`8008`), ~.+error))
}


############################
# Visualize one of these as an example

examp <- samp[[1]]

# now have dataframe in wide format with error introduced
# change to long format, join with sim_df_L
error_spec_L <- examp%>%tidyr::pivot_longer(.,cols=c(3:502),names_to="wavenumber_sim",values_to="absorbance_sim")

final_sim <- cbind(sim_df_L, error_spec_L$wavenumber_sim, error_spec_L$absorbance_sim)

final_sim <- final_sim%>%
  rename(wavenumber_sim = `error_spec_L$wavenumber_sim`, absorbance_sim = `error_spec_L$absorbance_sim`) #rename so fancy

# Plot!!!
rm(error_spec_L)
rm(sim_df_L)
rm(summ)

f <- ggplot(final_sim)+
  geom_line(aes(x = wavenumber, y = absorbance_sim, group = as.factor(known_age), color = as.factor(known_age)))+
  #scale_color_manual(values = rainbow(8)) + 
  geom_line(aes(x = wavenumber, y = absorbance, group = known_age))+
  scale_x_reverse()+
  theme_classic()

ggplotly(f)

#################
# Modeling
known_age_mod <- function(df) {
  pls(df[, c(3:502)], df$known_age, scale = TRUE, ncomp = 6, cv = 1)
}

error_age_mod <- function(df) {
  pls(df[, c(3:502)], df$error_age, scale = TRUE, 8, cv = 1)
}

known_age_list <- map(samp, known_age_mod)
error_age_list <- map(samp, error_age_mod)

# Pull out predictions and make a matrix
extract_preds <- function(mod) {
  x <- mod$cvres$ncomp.selected #allow each to use optimal ncomp
  print(mod$cvres$y.pred[,x,1]) #use x to select preds from matrix
}

known_age_preds <- map(known_age_list, extract_preds) #apply function to mod list
error_age_preds <- map(error_age_list, extract_preds) # apply functiont to mod list

# Make a dataframe that has known_age, error_age, known_age_preds, error_age_preds from each Iter

known_age_preds <- data.frame(matrix(unlist(known_age_preds), ncol=Iter, byrow=F)) 

colnames(known_age_preds) <- paste(colnames(known_age_preds), "T", sep = "_") #T for true

error_age_preds <- data.frame(matrix(unlist(error_age_preds), ncol = Iter, byrow=F))

colnames(error_age_preds) <- paste(colnames(error_age_preds), "E", sep = "_") #E for error or estimate

preds_df <- cbind(sim_df$known_age, sim_df$error_age, known_age_preds, error_age_preds)

# Could I try a pc plot?
plotScores(known_age_list[[1]]$res$cal$xdecomp, show.labels = FALSE, cgroup = sim_df$known_age)

plotScores(error_age_list[[1]]$res$cal$xdecomp, show.labels = FALSE, cgroup = sim_df$known_age)

# Output data
write.csv(preds_df, "C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Simulation Project/Data/all_model_preds_0.05.csv") # preds_df - this can be used to calculate the rest
