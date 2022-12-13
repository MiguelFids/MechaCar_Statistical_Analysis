library(tidyverse)
library(dplyr)
data <- read.csv("MechaCar_mpg.csv")

# deliverable 1  
model <- lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data)
model

# plot the linear regression lines
model <- lm(mpg ~ vehicle_length, data)
regression_model <- model$coefficients['vehicle_length']*data$vehicle_length + model$coefficients['(Intercept)']
plt <- ggplot(data, aes(x=vehicle_length, y=mpg))
plt + geom_point() + geom_line(aes(y=regression_model))

model <- lm(mpg ~ ground_clearance, data)
regression_model <- model$coefficients['ground_clearance']*data$ground_clearance + model$coefficients['(Intercept)']
plt <- ggplot(data, aes(x=ground_clearance, y=mpg))
plt + geom_point() + geom_line(aes(y=regression_model))

model <- lm(mpg ~ vehicle_weight, data)
regression_model <- model$coefficients['vehicle_weight']*data$vehicle_weight + model$coefficients['(Intercept)']
plt <- ggplot(data, aes(x=vehicle_weight, y=mpg))
plt + geom_point() + geom_line(aes(y=regression_model))

# deliverable 2
data2 <- read.csv("Suspension_Coil.csv")
total_summary <- data2 %>% summarize(Mean=mean(PSI), Median=median(PSI), Variance=var(data2$PSI), Standard_Deviation=sd(data2$PSI))
total_summary
lot_summary <- data2 %>% group_by(data2$Manufacturing_Lot) %>%  summarize(Mean=mean(PSI), Median=median(PSI), Variance=var(PSI), SD=sd(PSI))
lot_summary

# deliverable 3

lst <- list()
sample_table <- data2 %>% sample_n(50)
pop_test <- t.test(sample_table$PSI, mu=mean(data2$PSI))
lst <- append(lst,pop_test$p.value)
lst

sample_table1 <- data2 %>% filter(Manufacturing_Lot == 'Lot1') 
sample_table2 <- data2 %>% filter(Manufacturing_Lot == 'Lot2') 
sample_table3 <- data2 %>% filter(Manufacturing_Lot == 'Lot3') 
test1 <- t.test(sample_table1$PSI, data2$PSI)
test2 <- t.test(sample_table2$PSI, data2$PSI)
test3 <- t.test(sample_table3$PSI, data2$PSI)

Lot <- c("Lot 1", "Lot 2", "Lot 3")
P <- c(test1$p.value, test2$p.value, test3$p.value)

df <- data.frame(Lot, P)
df

