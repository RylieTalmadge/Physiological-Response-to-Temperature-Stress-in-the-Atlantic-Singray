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
mean_temp <- with(resp_data, tapply(`metabolic_rate`, list(temp), mean))
with(resp_data, tapply(`metabolic_rate`, list(temp), sd))


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
ggplot(resp_data, aes(x = factor(temp), y = metabolic_rate)) +
  geom_boxplot() +
  labs(x = "temp", y = "metabolic_rate") +
  ggtitle("Metabolic Rates by Temperature")

# Bar graph
my_colors <- c("blue", "green", "orange")
my_temps <- c("16", "21", "26")

ggplot(data = resp_data, aes(x = factor(temp), y = metabolic_rate, fill = factor(temp))) + 
  geom_bar(stat = "identity") +
labs(title = "Mean Metabolic Rates by Temperature",
     x = "Temperature (Celsius)",
     y = "Mean Metabolic Rate (MO2)") +
  scale_fill_manual(
    name = "Temperature (Celsius)"
    labels = my_temps
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
