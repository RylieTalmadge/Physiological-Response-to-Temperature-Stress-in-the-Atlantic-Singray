library(readxl)
library(ggplot2)

# Read respirometry data
resp_data <- read_excel('data/MetabolicCalc.xlsx', sheet = 'Processed Data', 
                  n_max = 4)

# transpose a matrix
?t() 
tidyr::pivot_longer()

# HSC= MO2 (metabolic rate formula)
# y = K * V*B/M
# y = MO2, K = Slope, V = volume of respirometer - volume of ray, B = constant, M = mass of ray (kg)
# volume (V) is negligible (drop from equation)
calc_HSC <- function(K, B, M){
  K*B/M
}

with(resp_data, calc_HSC(Mass, B, Slope))

# HSC Mean and Standard Deviation
mean(my_data$metabolic_rate)

# HSC Male Mean and Standard Deviation
mean(my_data$metabolic_rate[my_data$gender == "male"])

# HSC Female Mean and Standard Deviation
mean(my_data$metabolic_rate[my_data$gender == "female"])

# HSC 16 Temp Mean and Standard Deviation
mean(my_data$metabolic_rate[my_data$temperature == 16])

# HSC 21 Temp Mean and Standard Deviation
mean(my_data$metabolic_rate[my_data$temperature == 21])

# HSC 26 Temp Mean and Standard Deviation
mean(my_data$metabolic_rate[my_data$temperature == 26])

# Index of Thermal Sensitivity (Q10)
# Q10 = (K2/K1)^(10/t2-t1)
calc_Q10 <- function(K1, K2, t1, t2){
  (K2/K1)^(10/(t2-t1))
}
# NOTE: change to fit data table
with(resp_data, calc_Q10(Mass, B, Slope))

# assign variables from data frame
temperature <- resp_data$Temperature
metabolic_rate <- resp_data$Metabolic_Rate
sex <- resp_data$Sex

# Create a scatterplot
ggplot(resp_data, aes(x = temperature, y = metabolic_rate, color = sex)) +
  geom_point()

# Linear Regression Models
resp_female <- lm(metabolic_rate ~ temperature, data = subset(resp_data, sex == "F"))
summary(resp_female)
plot(resp_female)

resp_male <- lm(metabolic_rate ~ temperature, data = subset(resp_data, sex == "M"))
summary(resp_male)
plot(resp_male)

# visualize the linear regression line on the scatter plot
ggplot(data, aes(x = temperature, y = metabolic_rate, color = sex)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# Normal Data
# Data were tested for normality and non-normal data were rank transformed.
shapiro.test(x)
x_rank <- rank(x)

# ANOVA Analysis
# input data
temperature <- c(20, 20, 20, 25, 25, 25, 30, 30, 30)
metabolic_rate <- c(10, 12, 15, 18, 20, 22, 24, 26, 28)

# create data frame
data <- data.frame(temperature, metabolic_rate)

# perform ANOVA
model <- aov(metabolic_rate ~ temperature, data = data)

# summarize ANOVA results
summary(model)

# Two-way ANOVA to compare sex
anova_model <- aov(Metabolic_Rate ~ Gender + Temperature + Gender:Temperature, data = data)

# Check ANOVA results
summary(anova_model)

