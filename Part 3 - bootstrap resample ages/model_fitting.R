#################
# Modeling

# packages
library(dplyr)
library(purrr)
library(mdatools)
library(furrr)
library(future)

# library(plotly)
# library(stringr)
# library(ggpubr)

# write function for passing PLS model to each dataset with jittered age
# test
# Might need to store jittered age preds in a matrix, each column different iter. Then fit models in a loop on each subsequent row of response data. Extract only the predictions.

jitter_age_mod <- function(samp) {
  mod <- pls(all_dat[, c(7:506)], samp, scale = TRUE, ncomp = 10, cv = 50)
  x <- mod$cvres$ncomp.selected #allow each to use optimal ncomp
  mod$cvres$y.pred[,x,1] #use x to select preds from matrix
}

# system.time({
# jitter_mod_list <- map(samp, jitter_age_mod)})

## Try this in parallel
n_cores <- availableCores()-2
plan(multisession, workers = n_cores)

system.time({
jitter_mod_list_par <- future_map(samp, jitter_age_mod)})


# # Pull out predictions and make a matrix
# extract_preds <- function(mod) {
#   x <- mod$cvres$ncomp.selected #allow each to use optimal ncomp
#   print(mod$cvres$y.pred[,x,1]) #use x to select preds from matrix
# }
# 
# jitter_mod_preds <- map(jitter_mod_list, extract_preds) #apply function to mod list

# Make a dataframe that has known_age, error_age, known_age_preds, error_age_preds from each Iter

jitter_mod_preds <- data.frame(matrix(unlist(jitter_mod_list), ncol=Iter, byrow=F)) 

jitter_age <- data.frame(matrix(unlist(samp), ncol = Iter, byrow = F))

age_df <- all_dat[,c(1:6)]

preds_df <- cbind(seq(1:873), age_df, jitter_age, jitter_mod_preds)

# Rename columns
prefix1 <- "jitter_age"
prefix2 <- "preds"
suffix <- seq(1:Iter)
names1 <- paste(prefix1, suffix, sep = "_")
names2 <- paste(prefix2, suffix, sep = "_")
names <- c("ID", colnames(age_df), names1, names2)

colnames(preds_df) <- paste(names)

# Output data
write.csv(preds_df, "C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Simulation Project/Data/part 3 preds_indageerror_iter10_2013.csv") # preds_df - this can be used to calculate the rest
