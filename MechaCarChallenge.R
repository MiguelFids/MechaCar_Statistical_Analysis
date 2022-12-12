library(tidyverse)
library(dplyr)
data <- read.csv("MechaCar_mpg.csv")

# deliverable 1  
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data))

# deliverable 2
data2 <- read.csv("Suspension_Coil.csv")
total_summary <- data2 %>% summarize(Mean=mean(PSI), Median=median(PSI), Variance=var(data2$PSI), Standard_Deviation=sd(data2$PSI))
total_summary
lot_summary <- data2 %>% group_by(data2$Manufacturing_Lot) %>%  summarize(Mean=mean(PSI), Median=median(PSI), Variance=var(PSI), SD=sd(PSI))
lot_summary

# deliverable 3
sample_table1 <- data2 %>% sample_n(50)
sample_table2 <- data2 %>% sample_n(50)
sample_table3 <- data2 %>% sample_n(50)

t.test(sample_table1$PSI, mu=mean(data2$PSI), subset = Manufacturing_Lot == "Lot 1")
t.test(sample_table2$PSI, mu=mean(data2$PSI), subset = Manufacturing_Lot == "Lot 2")
t.test(sample_table3$PSI, mu=mean(data2$PSI), subset = Manufacturing_Lot == "Lot 3")

