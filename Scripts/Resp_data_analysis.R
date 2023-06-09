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
resp_data$`metabolic_rate` <- as.numeric(resp_data$`metabolic_rate`)
resp_data$`sex` <- as.factor(resp_data$`sex`)
resp_data$`temp` <- as.numeric(resp_data$`temp`)
str(resp_data)

# MO2 (metabolic rate formula)
# y = K * V*B/M (y = MO2, K = Slope, V = volume of respirometer - volume of ray, B = constant, M = mass of ray (kg))
# Ignore this equation for now, Use later (calculated by hand but will change in the next couple of days when we volume correct the equation)
# The data entered here has those mock calculations done to be sure the rest of the analysis works
calc_MO2 <- function(K, B, M){
  K*B/M
}

with(resp_data, calc_MO2(Mass, B, Slope))


# MO2 Mean and Stdv by Sex
mean_sex <- with(resp_data, tapply(`metabolic_rate`, list(sex), mean))
with(resp_data, tapply(`metabolic_rate`, list(sex), sd))

# MO2 Mean and Stdv by Temp
mean_temp <- c(with(resp_data, tapply(`metabolic_rate`, list(temp), mean)))
with(resp_data, tapply(`metabolic_rate`, list(temp), sd))
mean_temp

# Index of Thermal Sensitivity (Q10)
# Q10 = (K2/K1)^(10/t2-t1)
# Ignore for now (may not include, depending on volume issue from above)
calc_Q10 <- function(K1, K2, t1, t2){
  (K2/K1)^(10/(t2-t1))
}

with(resp_data, calc_Q10(Mass, B, Slope))

# Create a scatterplot
ggplot(resp_data, aes(x = temp, y = metabolic_rate, color = sex)) +
  geom_point()

# Box plot
ggplot(resp_data, aes(x = factor(temp), y = metabolic_rate, fill = factor(temp))) +
  geom_boxplot(color = "black") +
  scale_fill_manual(values = c("#ffad33", "#339966", "#66b3ff")) +
  labs(x = "Temperature (˚C)", y = "Metabolic Rate (mg O2 kg−1 h−1)") +
  ggtitle("Metabolic Rates by Temperature")

# Bar graph
# Use aggregate to calculate mean metabolic rate by temperature
mean_rates <- aggregate(metabolic_rate ~ temp, data = resp_data, mean)
mean_rates
# Plot the mean metabolic rates as a bar graph
my_colors <- c("#ffad33", "#339966", "#66b3ff")
my_temps <- c("16", "21", "26")
se <- aggregate(metabolic_rate ~ temp, data = resp_data, function(x) sd(x) / sqrt(length(x)))
se
length(mean_rates$metabolic_rate - se$metabolic_rate)
length(mean_rates$metabolic_rate + se$metabolic_rate)

ggplot(mean_rates, aes(x = factor(temp), y = metabolic_rate, fill = factor(temp))) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin= mean_rates$metabolic_rate - se$metabolic_rate, ymax= mean_rates$metabolic_rate + se$metabolic_rate),
                width = 0.1, position=position_dodge(.9)) +
  labs(title = "Mean Metabolic Rates by Temperature",
       x = "Temperature (˚C)",
       y = "Metabolic Rate (mg O2 kg−1 h−1)") +
  scale_fill_manual(
    name = "Temperature (˚C)",
    labels = my_temps,
    values = my_colors
  )

# Normal Data
# Ignore for now until data issue is corrected (see volume issue above)
  # Perform a Shapiro-Wilk test for each temperature group
shapiro_test_16 <- shapiro.test(metabolic_rates$mean_rate[metabolic_rates$temp == "16"])
shapiro_test_21 <- shapiro.test(metabolic_rates$mean_rate[metabolic_rates$temp == "21"])
shapiro_test_26 <- shapiro.test(metabolic_rates$mean_rate[metabolic_rates$temp == "26"])
  # Print the test results
shapiro_test_16
shapiro_test_21
shapiro_test_26

# ANOVA Analysis
model <- aov(metabolic_rate ~ factor(temp), data = resp_data)
summary(model)

# Tukey's Post Hoc Test
TukeyHSD(model, conf.level = 0.95)
# no significant difference between 26-21, but the 21-16 and 16-26 are
