#################
# Modeling

# packages
library(tidyverse)
library(plotly)
library(stringr)
library(mdatools)
library(ggpubr)

# write function for passing PLS model to each sim dataset
known_age_mod <- function(df) {
  pls(df[, c(4:502)], df$known_age, scale = TRUE, ncomp = 3, cv = 1)
}

error_age_mod <- function(df) {
  pls(df[, c(4:502)], df$error_age, scale = TRUE, ncomp = 3, cv = 1)
}

known_age_list <- map(sim_samp_fin, known_age_mod)
error_age_list <- map(sim_samp_fin, error_age_mod)

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

age_df <- sim_samp_fin[[1]][,c(1:3)]

preds_df <- cbind(age_df$known_age, age_df$error_age, known_age_preds, error_age_preds)

# Could I try a pc plot?
plotScores(known_age_list[[1]]$res$cal$xdecomp, show.labels = FALSE, cgroup = age_df$known_age)

plotScores(known_age_list[[1]]$res$cal$xdecomp, c(1,3), show.labels = FALSE, cgroup = age_df$known_age)

plotScores(known_age_list[[1]]$res$cal$xdecomp, c(2,3), show.labels = FALSE, cgroup = age_df$known_age)

plotScores(error_age_list[[1]]$res$cal$xdecomp, show.labels = FALSE, cgroup = age_df$known_age)

# Output data
write.csv(preds_df, "C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Simulation Project/Data/all_model_preds_PCA_0.1.csv") # preds_df - this can be used to calculate the rest
