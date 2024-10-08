# ___________________________________________
# Load Data and Libraries
# ___________________________________________

library(dplyr)
library(ggplot2)
library(lubridate)

bikes_raw <- read.csv("hour.csv", header = TRUE, sep = ",")

# Create a new variable 'is_winter' that is 1 if 'Season' is winter (1)
bikes_raw$is_winter <- ifelse(bikes_raw$season == 1, 1, 0)

# convert date to date format
bikes_raw$dteday <- as.Date(bikes_raw$dteday)

# define sin and cos versions of hour
bikes_raw$hr_sin <- sin(2 * pi * bikes_raw$hr / 24)
bikes_raw$hr_cos <- cos(2 * pi * bikes_raw$hr / 24)

# ___________________________________________
# Describe Data with Tables and Graphs
# ___________________________________________

summary(bikes_raw)

x = bikes_raw$hum
y = bikes_raw$cnt

x_bar = mean(x)
y_bar = mean(y)
s_xx = sum( (x - x_bar)^2 )
s_xy = sum( (y - y_bar) * (x - x_bar) )

beta_1_hat = s_xy / s_xx 
beta_0_hat = y_bar - beta_1_hat * x_bar

### see data trends with this plot below

plot(x, y,
     main = "Count versus x",
     xlab = "x", ylab = "Count",
     pch = 16,                       # Solid circle
     col = rgb(0, 0.5, 0.5, alpha = 0.1),# Blue color with 50% transparency
     cex = 0.7)                      # Smaller point size

### var relationship tracker
#      -  is_winter   # dummy
#      -  hr          # definitely nonlinear
#      -  yr          # dummy
#      -  workingday  # dummy
#      -  atemp       # 
#      -  hum         # 
#      -  windspeed   # 

### plot the trend by week

# Extract the week number and year from dteday and create a new column for it
bikes_raw$week <- paste0(year(bikes_raw$dteday), "-W", week(bikes_raw$dteday))

# Aggregate the total rental counts by week
weekly_bikes <- bikes_raw %>%
  group_by(week) %>%
  summarise(weekly_count = sum(cnt))

# Create a bar plot of the weekly counts
ggplot(weekly_bikes, aes(x = week, y = weekly_count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Weekly Bike Rental Counts",
       x = "Week",
       y = "Total Rentals") +
  scale_x_discrete(breaks = weekly_bikes$week[seq(1, nrow(weekly_bikes), by = 9)])
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

### this plot sucks, ignore this
#plot(x, y, 
#     main = "Simple Linear Regression on Bikeshare Usage For Winter vs. Non-Winter", 
#     xlab = "Winter", ylab = "Count")
#
#abline(beta_0_hat, beta_1_hat, 
#       col=c("blue"), lty=2)

# ___________________________________________
# MODEL 1:  all linear relationships
# ___________________________________________

model <- lm(cnt ~ is_winter + hr + workingday + temp + hum + windspeed, 
                    data = bikes_raw)

X = model.matrix(fitted_model)

### residuals plot
plot(model$fitted.values, residuals(model),
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted Plot",
     pch = 16,
     col = adjustcolor("steelblue", alpha.f = 0.25),
     cex = 0.75)
abline(h = 0, col = "red")

### check linearity assumptions

plot