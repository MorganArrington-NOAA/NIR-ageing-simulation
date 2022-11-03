#Calculate CV and SD by hand, then plot modeled estimates by age class generated from puntilizer

# load packages
library(tidyverse)

# Load in data
all_dat <- read.csv("~/AFSC A&G Contract/Simulation Project/Data/Tester_Reader_Merged_n1874.csv", header=TRUE) #age reading data

# Load in model output by reader index

# Reader 3 - B1_S2
puntilizer_3 <- read.csv("~/AFSC A&G Contract/Simulation Project/Puntilizer/Reader 3 tester 6 2010-2019/B1_S2/SS_format_Reader 1.csv", row.names=1) # load in ageing error matrix 

# Reader 8 - B2_S2
puntilizer_8 <- read.csv("~/AFSC A&G Contract/Simulation Project/Puntilizer/Reader 8 tester 6 2010-2019/B2_S2/SS_format_Reader 1.csv", row.names=1) # load in ageing error matrix 

# Reader 21 - B2_S1_S3
puntilizer_21 <- read.csv("~/AFSC A&G Contract/Simulation Project/Puntilizer/Reader 21 tester 6 2010-2019/B2_S1_S3/SS_format_Reader 1.csv", row.names=1) # load in ageing error matrix 

# Reader 59 - B0_S1_S3
puntilizer_59 <- read.csv("~/AFSC A&G Contract/Simulation Project/Puntilizer/Reader 59 tester 6 2010-2019/B0_S1_S3/SS_format_Reader 1.csv", row.names=1) # load in ageing error matrix 

# Reader 71 - B2_S1 
puntilizer_71 <- read.csv("~/AFSC A&G Contract/Simulation Project/Puntilizer/Reader 71 tester 6 2010-2019/B2_S1/SS_format_Reader 1.csv", row.names=1) # load in ageing error matrix 


# Save only columns we want to work with for now
#  Caculate SD and CV for each specimen. These calculations are a bit different I think, I think these are based on comparing two age reads, Puntilizer I think is calculating CV relative to "True Age"?
dat <- all_dat %>%
  transmute(file_name, read_age, test_age)%>%
  rowwise()%>%
  mutate(SD = sqrt((((read_age-mean(c(read_age,test_age)))^2)/(2-1))+(((test_age-mean(c(read_age,test_age)))^2)/(2-1))), CV = sqrt((((read_age-mean(c(read_age,test_age)))^2)/(2-1))+(((test_age-mean(c(read_age,test_age)))^2)/(2-1)))/mean(c(read_age, test_age)))

#  Calculate avg SD and CV by age
dat <- dat %>%
  group_by(read_age)%>%
  mutate(avg_SD = mean(SD), avg_CV = mean(CV))

#Print avg SD and CV across all specimens
mean(dat$SD)
mean(dat$CV)

# Mutate puntilizer output so easier to work with
test <- t(puntilizer_71)
test <- as.data.frame(test)
rownames(test) <- as.numeric(test$True_Age)
mod_dat <- test
mod_dat$True_Age <- as.numeric(mod_dat$True_Age)
mod_dat$CV <- as.numeric(mod_dat$CV)
mod_dat$SD <- as.numeric(mod_dat$SD)

# Plot dat with mod_dat on top
ggplot(mod_dat)+
  #geom_point(aes(read_age, SD), alpha = .2)+
  geom_line(data = mod_dat, aes(True_Age, SD), color = "red")
  #geom_line(data = dat, aes(read_age, avg_SD))

ggplot(mod_dat)+
  #geom_point(aes(read_age, CV), alpha = .2)+
  geom_line(aes(True_Age, CV), color = "red")
  #geom_line(data = dat, aes(read_age, avg_CV))

# Plot read and test age
ggplot(dat)+
  geom_point(aes(read_age, test_age))
