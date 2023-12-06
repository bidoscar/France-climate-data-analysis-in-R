library(lme4) 
library(MASS)
library(broom)
library(tidyverse)

# Loading the dataset
clim <- read.csv("https://userpage.fu-berlin.de/soga/data/raw-data/Climfrance.csv", sep = ";")
str(clim)


# Converting some variables into numericous type
clim2 <- clim
clim2$p_mean <- as.numeric(gsub(",", "", clim2$p_mean))

clim2$altitude <- as.numeric(gsub(",", "", clim2$altitude))
str(clim2)

names(clim2)


#=============================
# plot using the map of France
#=============================
install.packages("raster")
install.packages("sp")
install.packages("maps")

library(raster)
library(sp)
library(maps)

# Assuming your dataset is named clim2
coordinates(clim2) <- c("lon", "lat")

# Download France shapefile
france <- getData("GADM", country = "FRA", level = 0)

# Plot map of France
plot(france, col = "lightgrey", main = "Locations in France")
# Add points to the map
points(clim2, pch = 16, col = "red")
# Add labels for each station
text(coordinates(clim2), labels = clim2$station, 
     pos = 3, cex = 0.5, col = "blue")

#=======================================
# Exercice 1
#=======================================

climfrar <- clim2[1:34, ]
model1 <- lm(t_mean ~ altitude + lat + lon, data=climfrar)
summary(model1)

# Call:
#   lm(formula = t_mean ~ altitude + lat + lon, data = climfrar)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.76492 -0.32755  0.04337  0.24787  2.58927 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 37.2650364  2.6220099  14.212 7.29e-15 ***
#   altitude    -0.0064139  0.0008688  -7.383 3.17e-08 ***
#   lat         -0.5339603  0.0557546  -9.577 1.24e-10 ***
#   lon          0.0321010  0.0395728   0.811    0.424    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.7308 on 30 degrees of freedom
# Multiple R-squared:  0.8329,	Adjusted R-squared:  0.8162 
# F-statistic: 49.84 on 3 and 30 DF,  p-value: 9.112e-12



# Interpretation
# The model explains 81.62% of the total variance of the dependente variable
# R square adjusted = 0.8162

# (Intercept): The estimated intercept is 37.27. 
# This is the predicted value of t_mean when all predictor variables 
# (altitude, lat, and lon) are zero. 

# The coefficient for altitude is -0.0064. This suggests that for every 
# one-unit increase in altitude, the predicted t_mean decreases by approximately
# 0.0064 units, holding other variables constant.


# Lat: The coefficient for latitude is -0.534. the interpretation is in the the same way
# Lon: The coefficient for longitude is 0.0321. The interpretation is in the same way

# Lon is not significante (pvalue greater than 0.05)
#====================
# Exercice 2
#====================
model2 <- lm(t_mean ~ altitude + lat, data=climfrar)
summary(model2)

# Prediction
# Let see the input for the prediction
clim2[35:36,1:7]

#     station       altitude   lat    
# 35 Mont-Ventoux    1,212    44.16   
# 36  Pic-du-Midi    2,860    42.93 


prediction_data <- data.frame(altitude = c(1212, 2860),
                              lat = c(44.16, 42.93))


predictions <- predict(model2, newdata = prediction_data, interval = "p", level = 0.95)
predictions

#                 Pridicted value         lwr         upr   actuel value
# Mont-Ventoux    6.187574          3.814005    8.561143    -1.2
# Pic-du-Midi     -3.463727         -8.364328   1.436874    3.6

# Interprete the result


#=======================
# Exercice 3
#=======================
names(climfrar)
library(scatterplot3d)

scatter_3d <- with(climfrar, scatterplot3d(climfrar$altitude, climfrar$lat, 
                                           climfrar$t_mean,pch = 16, 
                                           highlight.3d = TRUE,
                                           angle = 45,
))
scatter_3d$plane3d(model2)
