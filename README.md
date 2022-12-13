# MechaCar_Statistical_Analysis
### Miguel Fidelino

## Linear Regression to Predict MPG
Vehicle length and ground clearance are the most significant factors that affect MPG. Interestingly, the weight of the car does not pass the significance level of 5%, but comes very close. 
                   Estimate Std. Error t value Pr(>|t|)
(Intercept)      -1.040e+02  1.585e+01  -6.559 5.08e-08
vehicle_length    6.267e+00  6.553e-01   9.563 2.60e-12
vehicle_weight    1.245e-03  6.890e-04   1.807   0.0776
ground_clearance  3.546e+00  5.412e-01   6.551 5.21e-08

The slope of the linear model will not be considered zero unless there is absolutely no correlation between the two. In our case, no linear regression model has a slope of zero. 

MechaCar has the strongest relationship with the vehicle length.

![Vehicle Length and MPG](LR_Vehicle_Length_and_MPG.png)

The linear regression model has a defined upward trend in terms of vehicle_length and mpg. This is also true for ground clearance and mpg:

![Ground Clearance and MPG](LR_Ground_Clearance_and_MPG.png)

If our significance level was above 5%, vehicle weight would play a factor in determining what best affects MPG.

![Vehicle Weight](LR_Vehicle_Weight_and_MPG.png)


