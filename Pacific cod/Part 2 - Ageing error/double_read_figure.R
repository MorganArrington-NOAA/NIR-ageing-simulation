# Double read figure
library(tidyverse)

raw_dat <- read.csv("~/AFSC A&G Contract/Simulation Project/Data/Tester_Reader_Merged_n1874.csv")

# Figure
ggplot(raw_dat)+
  geom_point(aes(x = read_age, y = test_age), color = "grey40", alpha = .5)+
  scale_x_continuous(limits = c(0,14), breaks=seq(0,14,1))+
  scale_y_continuous(limits = c(0,14), breaks=seq(0,14,1))+
  geom_smooth(aes(x = read_age, y = test_age),method = "lm")+
  labs(x = "Read age",
       y = "Test age",
       subtitle = "Pacific cod double reads")+
  theme_classic()
