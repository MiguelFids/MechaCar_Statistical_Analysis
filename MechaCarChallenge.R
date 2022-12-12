library(tidyverse)
library(dplyr)
data <- read.csv("MechaCar_mpg.csv")

summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data))
 