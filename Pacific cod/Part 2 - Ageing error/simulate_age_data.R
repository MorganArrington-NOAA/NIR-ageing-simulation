# Load packages
library(tidyverse)

#1 simulate dataset using sd parameters estimated from puntilizer

known_age <- c(rep(1:20, each = 1000)) #age 1-20

#2 sample with error similar to McBride 2015

mat <- matrix( ,nrow = 1000, ncol = 20)

for(i in 1:20) {
    mat[,i] <- rnorm(1000, mean = i, sd = mod_dat$SD[i+1])#remove first row from mod_data which is for age 0
  }

for(i in 1:20) {
  print(i)
}

error_age <- c(mat)

#3 combine know ages, and error ages into a data frame
sim_age_data <- data.frame(cbind(known_age, error_age))

#sim_age_data <- round(sim_age_data) # round to nearest whole number

ggplot(sim_age_data)+
  geom_point(aes(known_age, error_age), alpha = .2)+
  geom_abline(slope = 1, intercept = 0)

#4 check to make sure sd and cv look similar 
test <- data.frame(test)
sd(test$test)
sd(test$test)/2


test_error <- test %>%
  rowwise()%>%
  mutate(SD = sqrt((((2-mean(c(2,test)))^2)/(2-1))+(((test-mean(c(2,test)))^2)/(2-1))))
         
         #CV = sqrt((((2-mean(c(2,test)))^2)/(2-1))+(((2-mean(c(2,test)))^2)/(2-1)))/mean(c(2, test)))

mean(test_error$SD)
mean(test_error$CV)

check_error <- sim_age_data %>%
  transmute(known_age, error_age)%>%
  rowwise()%>%
  mutate(SD = sqrt((((known_age-mean(c(known_age,error_age)))^2)/(2-1))+(((known_age-mean(c(known_age,error_age)))^2)/(2-1))), CV = sqrt((((known_age-mean(c(known_age,error_age)))^2)/(2-1))+(((known_age-mean(c(known_age,error_age)))^2)/(2-1)))/mean(c(known_age, error_age)))

#  Calculate avg SD and CV by age
check_error <- check_error %>%
  group_by(known_age)%>%
  mutate(avg_SD = mean(SD), avg_CV = mean(CV))

# make summary table
error_by_age <- check_error %>%
  group_by(known_age)%>%
  summarize(mean(SD),mean(CV))

#Print avg SD and CV across all specimens
mean(check_error$SD)
mean(check_error$CV, na.rm = TRUE)

#5 chck to make sure sd and cv look similar by running it through puntilizer

write.csv(x = sim_age_data, file = "C:/Users/marri/OneDrive/Documents/AFSC A&G Contract/Simulation Project/Data/simulated age data.csv")
