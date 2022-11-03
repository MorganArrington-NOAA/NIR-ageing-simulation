# load packages
library(tidyverse)
library(plotly)

# Load in data
raw_dat <- read.csv("~/AFSC A&G Contract/Simulation Project/Data/Tester_Reader_Merged_n1874.csv", header=TRUE) #age reading data

sim_age_data <- read.csv("~/AFSC A&G Contract/Simulation Project/Data/simulated age data mat.csv") #simulated age data

#####################   
# Need to average spectra by age for simulation

## First need to pivot_longer to get column of absorbance
raw_dat_L<-raw_dat%>%tidyr::pivot_longer(.,cols=c(17:ncol(raw_dat)),names_to="wavenumber",values_to="absorbance", names_prefix = "X") 

# Clean up data
raw_dat_L$final_age <- as.factor(raw_dat_L$final_age)
raw_dat_L$read_age <- as.factor(raw_dat_L$read_age)
raw_dat_L$wavenumber <- as.numeric(raw_dat_L$wavenumber)

# Take mean and SE by final_age
summ <- raw_dat_L %>% 
  group_by(wavenumber, final_age) %>%
  summarize(mean_spec = mean(absorbance), se_spec = sd(absorbance)/sqrt(n()))

# Plot it, how much separation is there? Good enough as basis for simulation?
f <- ggplot(summ)+
  geom_line(aes(x = wavenumber, y = mean_spec, color = final_age))+
  geom_ribbon(aes(x = wavenumber, ymin = mean_spec - se_spec, ymax = mean_spec + se_spec, fill = final_age), alpha = 0.3)+
  # scale_color_manual(values = oo_colors)+
  # scale_fill_manual(values = oo_colors)+
  # scale_x_continuous(breaks=seq(2600,3250,250))+
  scale_x_reverse()+
  theme_classic()

ggplotly(f)

#########################################
# Need to mess with these to make them more clean cut and obvious. I like the idea of adding SE or something to 7,8,9,10,11,12 because it'll keep the shape of the curve realistic.

# 1-6 is good
# 7 add SE*2 
# 8 is good
# 9 add SE
# 10 add SE
summ$final_age <- as.numeric(summ$final_age)
summ <- summ%>%
  mutate(new_mean = case_when(final_age == 7 ~ mean_spec+2*se_spec, final_age == 9 | final_age == 10 ~ mean_spec + se_spec, final_age %in% c(1,2,3,4,5,6,8) ~ mean_spec), new_age = case_when(final_age == 6 ~ 7, final_age == 7 ~ 6, final_age %in% c(1,2,3,4,5,8,9,10) ~ final_age))


# Plot again
summ$new_age <- as.factor(summ$new_age)

f <- ggplot(summ)+
  geom_line(aes(x = wavenumber, y = new_mean, color = new_age))+
  #geom_ribbon(aes(x = wavenumber, ymin = new_mean - se_spec, ymax = new_mean + se_spec, fill = new_age), alpha = 0.3)+
  # scale_color_manual(values = oo_colors)+
  # scale_fill_manual(values = oo_colors)+
  # scale_x_continuous(breaks=seq(2600,3250,250))+
  scale_x_reverse()+
  theme_classic()

ggplotly(f)

# first lets see what happens when we multiply by a fraction and add it to the spectra.
test <- raw_dat_L %>% 
  filter(file_name == "PACIFIC_COD_89201301202_657_OT1.0")%>%
  mutate(new_abs = absorbance*1.09)

ggplot(test)+
  geom_line(aes(x = wavenumber, y = absorbance))+
  geom_line(aes(x = wavenumber, y = new_abs))+
  scale_x_reverse()

# Ok, that works. Now we need to use sim data set, match averaged spectra by age to each "known_age" 

avg_spectra <- summ%>%tidyr::pivot_wider(.,values_from="new_mean",id_cols="new_age",names_from="wavenumber") # pivot_wider so I can keep track of what's happening

avg_spectra <- avg_spectra%>%
  rename(known_age = new_age) # rename as "known_age" now that these are becoming basis for simulated spectra

# Now we need to find a way to add these all to the simulated age data
avg_spectra$known_age <- as.integer(as.character(avg_spectra$known_age)) #make data types same in two different datasets

sim_df <- left_join(sim_age_data, avg_spectra, by = "known_age") #join

## For now filter this for ages 1-6 ##
sim_df <- sim_df%>%
  filter(known_age %in% c(1,2,3,4,5,6,7,8,9,10))

sim_df <- sim_df[,-1] #remove first column

################# 
# Now we have a sim_df that has "known_age", "error_age", as well as matching spectra for each "known_age" fish! 

# Time to introduce some error

# Pivot longer for plotting and for adding error
sim_df_L <- sim_df%>%tidyr::pivot_longer(.,cols=c(3:ncol(sim_df)),names_to="wavenumber",values_to="absorbance")

sim_df_L$wavenumber <- as.numeric(sim_df_L$wavenumber)

ggplot(sim_df_L)+
  geom_line(aes(wavenumber, absorbance, color = as.factor(known_age)))+
  scale_x_reverse()

# generate some error around spectra for modeling

## 1 
# use consistent number for error makes some sense this would scale with age because of increase in growth variability?

# in this example, we have 10 ages, 10 * 200 = 2000 numbers. Multiply these by each spectra (grouped by X) and add them to the OG spectra
mat <- matrix( ,nrow = 200, ncol = 10) #nrow = Iter, ncol = # age bins

for(i in 1:10) {
  mat[,i] <- rnorm(200, mean = 1, sd = 0.05)#remove first row from mod_data which is for age 0
}

error_vec <- c(mat)


# ## 2
# draw 200 numbers for age from normal distribution using SE as SD. This is only to get hessien to be defined. 
# 
# SE_vec <- summ %>%
#   group_by(read_age) %>%
#   filter(read_age %in% c(1,2,3,4,5,6))%>% #filter for now
#   summarize(mean_se = mean(se_spec))
# 
# # in this example, we have 6 ages, 6 * 200 = 1200 numbers. Multiply these by each spectra (grouped by X) and add them to the OG spectra
# mat <- matrix( ,nrow = 200, ncol = 6) #nrow = Iter, ncol = # age bins
# 
# for(i in 1:6) {
#   mat[,i] <- rnorm(200, mean = 1, sd = SE_vec$mean_se[i])#remove first row from mod_data which is for age 0
# }
# 
# error_vec <- c(mat)

############################
# now have a vector of errors to multiply by the spectra to introduce error before fitting a model!

error_spec <- sim_df %>%
  mutate(across(.cols = 3:ncol(sim_df), ~.*error_vec))

write.csv(error_spec, file = "C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Simulation Project/Data/simulated_full_data_0.02.csv")

# now have dataframe in wide format with error introduced
# change to long format, join with sim_df_L
error_spec_L <- error_spec%>%tidyr::pivot_longer(.,cols=c(3:ncol(error_spec)),names_to="wavenumber_sim",values_to="absorbance_sim")

final_sim <- cbind(sim_df_L, error_spec_L$wavenumber_sim, error_spec_L$absorbance_sim)

final_sim <- final_sim%>%
  rename(wavenumber_sim = `error_spec_L$wavenumber_sim`, absorbance_sim = `error_spec_L$absorbance_sim`) #rename so fancy

# Plot!!!
rm(error_spec_L)
rm(sim_df_L)
rm(raw_dat_L)
rm(summ)

f <- ggplot(final_sim)+
geom_line(aes(x = wavenumber, y = absorbance_sim, group = as.factor(known_age), color = as.factor(known_age)))+
  #scale_color_manual(values = rainbow(8)) + 
  geom_line(aes(x = wavenumber, y = absorbance, group = known_age))+
  scale_x_reverse()+
  theme_classic()

ggplotly(f)

ggplot(final_sim)+
  geom_point(aes(round(known_age), round(error_age)))

