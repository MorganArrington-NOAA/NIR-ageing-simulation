# Load in matrix

# How can I simulate data frame from this...
# sample over possible ages using a string of probabilities from puntilizer

# test
library(tidyverse)

agemat <- matrix(data = c(0.999988,	1.15E-05,	0,	0,	0,
                          1.15E-05,	0.999977,	1.15E-05,	0,	0,
                          2.30E-10,	0.0161425,	0.967715,	0.0161425,	2.30E-10,
                          3.18E-12,	1.15E-05,	0.0763062,	0.847364,	0.0763062,
                          3.36E-13,	7.74E-08,	0.00069669,	0.140823,	0.71696
                          
                          
), nrow = 5, byrow = T)

Iter <- 5


sim <- matrix( , nrow = Iter, ncol = max(df$age))

for (i in 1:max(df$age)) {
  sim[,i] <- sample(x = c(1:5),
                    size = Iter,
                    prob = agemat[i,],
                    replace = TRUE)
}

# Full scale
# agemat <- read.csv("~/AFSC A&G Contract/Simulation Project/Puntilizer/B2_S3_S1/ageing error matrix Reader 1.csv", row.names=1) # load in ageing error matrix 

# Load in ageing error matrices for each reader in 2013
# Reader 3 - B1_S2
agemat_3 <- read.csv("~/AFSC A&G Contract/Simulation Project/Puntilizer/Reader 3 tester 6 2010-2019/B1_S2/ageing error matrix Reader 1.csv", row.names=1) # load in ageing error matrix 

# Reader 8 - B2_S2
agemat_8 <- read.csv("~/AFSC A&G Contract/Simulation Project/Puntilizer/Reader 8 tester 6 2010-2019/B2_S2/ageing error matrix Reader 1.csv", row.names=1) # load in ageing error matrix 

# Reader 21 - B2_S1_S3
agemat_21 <- read.csv("~/AFSC A&G Contract/Simulation Project/Puntilizer/Reader 21 tester 6 2010-2019/B2_S1_S3/ageing error matrix Reader 1.csv", row.names=1) # load in ageing error matrix 

# Reader 59 - B0_S1_S3
agemat_59 <- read.csv("~/AFSC A&G Contract/Simulation Project/Puntilizer/Reader 59 tester 6 2010-2019/B0_S1_S3/ageing error matrix Reader 1.csv", row.names=1) # load in ageing error matrix 

# Reader 71 - B2_S1 
agemat_71 <- read.csv("~/AFSC A&G Contract/Simulation Project/Puntilizer/Reader 71 tester 6 2010-2019/B2_S1/ageing error matrix Reader 1.csv", row.names=1) # load in ageing error matrix 

Iter <- 10

known_age <- rep(0:14, each = Iter) #make vector of known ages

sim_3 <- matrix( , nrow = Iter, ncol = 15) #initiate matrix for simulation

set.seed(13)
for (i in 1:15) { #each loop iterates through agemat rows for ages 0:20 and samples from numbers 0-20 based on probabilities
  sim_3[,i] <- sample(x = c(0:14), #drawing from numbers 0-20
                    size = Iter, #can set sample size per iteration/age
                    prob = agemat_3[i,], #iterate through rows of probabilities for age 0 - 20
                    replace = TRUE)
}

error_age <- c(sim_3) #concatonate matrix into vector

sim_3_data <- data.frame(cbind(known_age, error_age)) #join "known_age" and simulated "error_age" 

# Visualize

ggplot(sim_3_data)+
  geom_point(aes(known_age, error_age), alpha = .2)+
  geom_abline(slope = 1, intercept = 0)
  
# Figure
ggplot(sim_71_data)+
  geom_point(aes(x = known_age, y = error_age), color = "grey40", alpha = .5)+
  scale_x_continuous(limits = c(0,14), breaks=seq(0,14,1))+
  scale_y_continuous(limits = c(0,14), breaks=seq(0,14,1))+
  #geom_smooth(aes(x = known_age, y = error_age),method = "lm")+
  geom_abline(slope = 1, intercept = 0)+
  labs(x = "True age",
       y = "Simulated age estimate",
       subtitle = "Pacific cod simulated age data - reader index 71")+
  theme_classic()

# Save data 

write.csv(x = sim_data, file = "C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Simulation Project/Data/simulated age data mat.csv")
