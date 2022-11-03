# Bootstrap error added to spectra that's matched with known ages and simulated age estimates

# Load in data
# raw_dat <- 

sim_age_data <- read.csv("~/AFSC A&G Contract/Simulation Project/Data/simulated age data mat.csv") #simulated age data

###
# Okay, what do I need to do here. Only for model fit to error ages. Full sim_age_data set, and repeated sample errors in spectra so different error matched to different "specimen" every time. 



### 
# Fit all of these models 



new_df <- replicate(100, {df %>% 
    group_by(final_age) %>% 
    sample_frac(.5)
})

samp <- list() # creates an empty list

for (k in 1:100){ # loops through and uploads each file, depends on simplerspec package
  samp[[k]] <- df %>% 
    group_by(final_age) %>% 
    sample_frac(.5)
}
str(ldf[[1]]) # check first element

rdat <- ldf
# This should output a matrix of resampled data - we can change this into a nested dataframe

# then we can write a function like this:

validate_model <- function(df) {
  predict(PLSr, df[, 17:241], df$final_age, cv = FALSE)
}

new_df <- new_df %>% 
  mutate(model = map(df , validate_model))