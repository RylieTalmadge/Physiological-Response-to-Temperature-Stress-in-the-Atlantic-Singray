library(readxl)
library(ggplot2)

# Read respirometry data
resp_data <- read_excel('data/MetabolicCalc.xlsx', sheet = 'Processed Data', 
                  n_max = 4)

# transpose matrix 
resp_data <- t(resp_data)

resp_data <- data.frame(resp_data)
names(resp_data) <- resp_data[1, ]
resp_data <- resp_data[-1, ]
resp_data$sample_id <- rownames(resp_data)

head(resp_data)
sapply(resp_data, class)
resp_data$`Metabolic Rate` <- as.numeric(resp_data$`Metabolic Rate`)
resp_data$`Sex` <- as.factor(resp_data$`Sex`)
resp_data$`Temp (˚C)` <- as.numeric(resp_data$`Temp (˚C)`)

tidyr::pivot_longer()

# HSC= MO2 (metabolic rate formula)
# y = K * V*B/M (y = MO2, K = Slope, V = volume of respirometer - volume of ray, B = constant, M = mass of ray (kg))
# Ignore this equation for now, Use later (calculated by hand but will change in the next couple of days when we volume correct the equation)
# The data entered here has those mock calculations done to be sure the rest of the analysis works
calc_HSC <- function(K, B, M){
  K*B/M
}

with(resp_data, calc_HSC(Mass, B, Slope))


# HSC Male Mean
HSC_male_mean <- mean(resp_data$metabolic_rate[resp_data$sex == "male"])

# HSC Female Mean and Standard Deviation
HSC_female_mean <- mean(resp_data$metabolic_rate[resp_data$sex == "female"])

# HSC 16 Temp Mean and Standard Deviation
HSC_16_mean <- mean(resp_data$metabolic_rate[resp_data$temperature == 16])

# HSC 21 Temp Mean and Standard Deviation
HSC_21_mean <- mean(resp_data$metabolic_rate[resp_data$temperature == 21])

# HSC 26 Temp Mean and Standard Deviation
HSC_26_mean <- mean(resp_data$metabolic_rate[resp_data$temperature == 26])

# Index of Thermal Sensitivity (Q10)
# Q10 = (K2/K1)^(10/t2-t1)
# Ignore for now (may not include, depending on volume issue from above)
calc_Q10 <- function(K1, K2, t1, t2){
  (K2/K1)^(10/(t2-t1))
}

with(resp_data, calc_Q10(Mass, B, Slope))

# assign variables from data frame
Temperature <- resp_data$Temperature
Metabolic_rate <- resp_data$Metabolic_Rate
Sex <- resp_data$Sex

# Create a scatterplot
ggplot(resp_data, aes(x = Temperature, y = Metabolic_rate, color = Sex)) +
  geom_point()

# Box plot
ggplot(resp_data, aes(x = factor(temperature), y = Metabolic_rate)) +
  geom_boxplot() +
  labs(x = "Temperature", y = "Metabolic Rate") +
  ggtitle("Metabolic Rates by Temperature")

# Bar graph
# mean metabolic rates by temperature
mean_rates <- aggregate(metabolic_rate ~ temperature, data = resp_data, mean)
# Create the bar graph
ggplot(mean_rates, aes(x = Sex, y = Metabolic_rate, fill = Sex)) +
  geom_col() +
  labs(x = "Gender", y = "Mean Metabolic Rate") +
  ggtitle("Mean Metabolic Rates by Gender")

# Normal Data
# Data were tested for normality and non-normal data were rank transformed.
# Ignore for now until data issue is corrected (see volume issue above)
shapiro.test(x)
x_rank <- rank(x)

# ANOVA Analysis
model <- aov(metabolic_rate ~ temperature, data = resp_data)
summary(model)

# Two-way ANOVA to compare sex
anova_model <- aov(Metabolic_Rate ~ Gender + Temperature + Sex:Temperature, data = resp_data)
summary(anova_model)
