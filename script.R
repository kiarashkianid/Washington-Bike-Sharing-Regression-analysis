# ___________________________________________
# Load Data and Libraries
# ___________________________________________

library(dplyr)
library(ggplot2)
library(lubridate)

bikes_raw <- read.csv("hour.csv", header = TRUE, sep = ",")

# Create a new variable 'is_winter' that is 1 if 'Season' is winter (1)
bikes_raw$is_winter <- ifelse(bikes_raw$season == 1, 1, 0)

# ___________________________________________
# Describe Data
# ___________________________________________

summary(bikes_raw)

x = bikes_raw$is_winter
y = bikes_raw$cnt

x_bar = mean(x)
y_bar = mean(y)
s_xx = sum( (x - x_bar)^2 )
s_xy = sum( (y - y_bar) * (x - x_bar) )

beta_1_hat = s_xy / s_xx 
beta_0_hat = y_bar - beta_1_hat * x_bar

# ___________________________________________
# Plot Data
# ___________________________________________

### plot the trend by week
# Convert 'dteday' to Date format if it is not already
bikes_raw$dteday <- as.Date(bikes_raw$dteday)

# Extract the week number and year from 'dteday' and create a new column for it
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
# Analyze Data
# ___________________________________________

fitted_model = lm(cnt ~ is_winter + hr + yr + workingday + atemp + hum + windspeed, 
                    data = bikes_raw)

X = model.matrix(fitted_model)