chooseCRANmirror(ind=1)
install.packages("vcd")
install.packages("forcats")
library(ggplot2)
library(forcats)
library(vcd)
library(dplyr)

file_path = "C:\\Users\\17ada\\Desktop\\R\\asrData.csv"

org_data = read.csv(file_path)
data1 = org_data

new_col_names = c(
  "Timestamp" = "SurveyTimestamp",
  "Email Address" = "Email",
  "Your age" = "Age",
  "Gender" = "Gender",
  "Your academic major or field of study?" = "Major",
  "What is your current year of study?" = "YearOfStudy",
  "What is your current living situation?" = "LivingSituation",
  "Do you have any chronic medical condition?" = "ChronicCondition",
  "What is your current employment status?" = "EmploymentStatus",
  "Do you have any specific goals related to your lifestyle (e.g., weight management, stress reduction, etc.)?" = "LifestyleGoals",
  "How many hours of sleep do you typically get per night on an average?" = "HoursOfSleep",
  "Please select the relevant option [How often do you engage in physical activity or exercise per week?]" = "PhysicalActivityFrequency",
  "Please select the relevant option [How often do you go to dine out or order food from outside?]" = "DiningOutFrequency",
  "Please select the relevant option [How often do you smoke or indulge in tobacco products?]" = "SmokingFrequency",
  "Please select the relevant option [How frequently do you consume alcohol or engage in binge drinking?]" = "AlcoholFrequency",
  "How much time do you spend on screens per day(TV, computer, smartphone)?" = "ScreenTime",
  "On a scale of 1 to 5, choose the relevant answer. [I am influenced by peer pressure or social norms when making my lifestyle choices?]" = "PeerInfluence",
  "On a scale of 1 to 5, choose the relevant answer. [Factors, such as my budget or financial situation, affect my lifestyle choices?]" = "FinancialInfluence",
  "On a scale of 1 to 5, choose the relevant answer. [I feel that academic demands and stress impact my ability to maintain a healthy lifestyle?]" = "AcademicStress",
  "On a scale of 1 to 5, choose the relevant answer. [I have received support or guidance related to my lifestyle choices (e.g., from health services, counseling, or friends)?]" = "LifestyleSupport",
  "On a scale of 1 to 5, choose the relevant answer. [I am aware of the impact of my lifestyle choices on my academic performance, and my behavior?]" = "AwarenessImpact",
  "Which of the following strategies do you use to balance academic pressures and a healthy lifestyle?" = "BalancingStrategy"
)

dim(data1)
str(data1)
summary(data1)
head(data1)


colnames(data1) = new_col_names
data1$Major = as.factor(data1$Major)
data1$Gender = as.factor(data1$Gender)
data1$Age = as.factor(data1$Age)
data1$LivingSituation = as.factor(data1$LivingSituation)
data1$ChronicCondition = as.factor(data1$ChronicCondition)
data1$EmploymentStatus = as.factor(data1$EmploymentStatus)
data1$PhysicalActivityFrequency = as.factor(data1$PhysicalActivityFrequency)
data1$HoursOfSleep = as.factor(data1$HoursOfSleep)
data1$AcademicStress = as.factor(data1$AcademicStress)
data1$AlcoholFrequency = as.factor(data1$AlcoholFrequency)
data1$EmploymentStatus = as.factor(data1$EmploymentStatus)

sleep_age_table = table(data1$Age, data1$HoursOfSleep)
data1$AlcoholFrequency
# Dropping timestamp and email
data = data1[, -c(1, 2)]

custom_colors <- c("skyblue", "lightgreen", "coral", "purple")

# Histogram for HoursOfSleep
# Bar plot for Distribution of Hours of Sleep with custom colors
ggplot(data1, aes(x = HoursOfSleep)) +
  geom_bar(fill = custom_colors[1], color = "black") +  # Use the first custom color
  labs(title = "Distribution of Hours of Sleep", x = "Hours of Sleep", y = "Frequency") +
  theme_minimal()
#findings
#There are majority of students having atleast 6-8 hours of sleep

# Histogram for Distribution of Alcohol Frequency with custom colors
# Histogram for Distribution of Alcohol Frequency with custom colors and rotated labels
ggplot(data1, aes(x = AlcoholFrequency, fill = AlcoholFrequency)) +
  geom_bar(color = "black") +
  labs(title = "Distribution of Alcohol Frequency", x = "Alcohol Frequency", y = "Frequency") +
  scale_fill_manual(values = custom_colors) +  # Use custom colors
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




#The bar chart shows the distribution of alcohol frequency among a sample of people. The bar for "Never" is the tallest, followed by "Less than 3 days per week", "3 to 5 days per week", and "More than 5 days per week". This suggests that most people in this sample drink alcohol occasionally or never, and only a small number of people drink alcohol frequently.

#Here are some specific findings from the bar chart:
  
#  The most common alcohol frequency is "Never", with 51% of people in the sample reporting this.
#The second most common alcohol frequency is "Less than 3 days per week", with 38% of people in the sample reporting this.
#The third most common alcohol frequency is "3 to 5 days per week", with 11% of people in the sample reporting this.
#The least common alcohol frequency is "More than 5 days per week", with only 0.02% of people in the sample reporting this.

data$AlcoholFrequencyNumeric <- as.numeric(gsub("[^0-9-]+", "", data$AlcoholFrequency))
data$AlcoholFrequencyNumeric[data$AlcoholFrequency == "Never"] <- 0  # Treat "Never" as 0

library(ggplot2)
ggplot(data, aes(x = AlcoholFrequencyNumeric, fill = LivingSituation)) +
  geom_density(alpha = 0.7) +
  labs(title = "Density Plot of AlcoholFrequency by LivingSituation",
       x = "Alcohol frequency") +
  facet_wrap(~ LivingSituation, scales = "free") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
#The graph shows the distribution of alcohol frequency for different living situations. The blue line represents people who live in a PG, the green line represents people who live in a University/College Hostel, and the red line represents people who live in a Rented Apartment.

#Findings:
  
#  People who live in a PG drink more alcohol than people who live in a University/College Hostel or a Rented Apartment.
#There is a bimodal distribution of alcohol frequency among people who live in a PG, with one peak at around 20 and another peak at around 30.
#There is a unimodal distribution of alcohol frequency among people who live in a University/College Hostel, with a peak at around 20.
#There is a unimodal distribution of alcohol frequency among people who live in a Rented Apartment, with a peak at around 10.

# Box plot for HoursOfSleep across LivingSituation
ggplot(data1, aes(x = LivingSituation, y = as.numeric(HoursOfSleep))) +
  geom_boxplot(fill = custom_colors, color = "black") +
  labs(title = "Box plot of Hours of Sleep by Living Situation", x = "Living Situation", y = "Hours of Sleep")

# Subset data for the T-test
t_test_data <- data1[data1$LivingSituation %in% c("Hostel", "Apartment"), ]

# Extract numeric values from the ranges
data1$HoursOfSleepNumeric <- as.numeric(gsub("[^0-9-]+", "", as.character(data1$HoursOfSleep)))

# Take the midpoint for ranges
data1$HoursOfSleepNumeric <- ifelse(grepl("-", data1$HoursOfSleep),
                                    (as.numeric(strsplit(as.character(data1$HoursOfSleep), "-")[[1]])[1] +
                                       as.numeric(strsplit(as.character(data1$HoursOfSleep), "-")[[1]])[2]) / 2,
                                    data1$HoursOfSleepNumeric)

t_test_data$LivingSituation
# hypothesys 1:t-test(2 variables (independent))
#Null Hypothesis (H0): There is no significant difference in the mean hours of sleep 
#between students living with family and students 
#living in a PG (Paying Guest) accommodation.

#Alternative Hypothesis (H1): There is a significant difference in the mean hours of 
#sleep between students living with family and students living in a PG accommodation.
# Set seed for reproducibility
set.seed(123)

# Create a random sample of 29 observations
random_sample <- data1[sample(nrow(data1), 29), ]

# Extract numeric values from the ranges
random_sample$HoursOfSleepNumeric <- as.numeric(gsub("[^0-9-]+", "", random_sample$HoursOfSleep))

# Take the midpoint for ranges
random_sample$HoursOfSleepNumeric <- ifelse(grepl("-", random_sample$HoursOfSleep),
                                            (as.numeric(strsplit(as.character(random_sample$HoursOfSleep), "-")[[1]])[1] +
                                               as.numeric(strsplit(as.character(random_sample$HoursOfSleep), "-")[[1]])[2]) / 2,
                                            random_sample$HoursOfSleepNumeric)

# Check the structure of the new variable
str(random_sample$HoursOfSleepNumeric)
random_sample$HoursOfSleepNumeric


# Box plot for HoursOfSleep across and living situations
ggplot(random_sample, aes(x =HoursOfSleepNumeric , y = LivingSituation, fill = LivingSituation)) +
  geom_boxplot(color = "black") +
  labs(title = "Box plot of Hours of Sleep by LivingSituations", 
       x = "Hours of sleep", 
       y = "Living situations") +
  theme_minimal()
#The box plot shows the distribution of hours of sleep for different living situations. The blue boxplot represents people who live in a PG, the green boxplot represents people who live in a University/College Hostel, the red boxplot represents people who live in a Rented Apartment, and the purple boxplot represents people who live with family.

#The median number of hours of sleep is highest for people who live with family, followed by people who live in a Rented Apartment, University/College Hostel, and then PG. The interquartile range is also widest for people who live with family, followed by people who live in a Rented Apartment, University/College Hostel, and then PG. This suggests that there is more variability in the number of hours of sleep for people who live with family than for people who live in other living situations.
# Subset data for the t-test
t_test_data_random <- random_sample %>% filter(LivingSituation %in% c("With family", "PG"))

# Conduct the t-test
t_test_result_random <- t.test(HoursOfSleepNumeric ~ LivingSituation, data = t_test_data_random)

t_test_result_random
summary(t_test_result_random)

#descriptive statistics
install.packages("e1071")
library(e1071)

# Calculate skewness and kurtosis for Hours of Sleep
skewness_hours <- skewness(t_test_data_random$HoursOfSleepNumeric)
kurtosis_hours <- kurtosis(t_test_data_random$HoursOfSleepNumeric)
skewness_hours
kurtosis_hours

#Skewness: The skewness value for the Hours of Sleep is approximately 0.0783. Since the skewness is close to zero, it indicates that the distribution is approximately symmetric.
#Kurtosis: The kurtosis value for the Hours of Sleep is approximately -0.6724. A negative kurtosis suggests that the distribution has lighter tails than a normal distribution.                                                                 Skewness of Hours of Sleep in the T-test analysis: 0.0783 (approximately normal distribution).
#Kurtosis of Hours of Sleep in the T-test analysis: -0.6724 (platykurtic distribution).

#Analysis:
#  The p-value of 0.3149 is greater than the common significance level of 0.05.
#Fail to reject the null hypothesis (h0) as there is not enough evidence to suggest a significant difference in means between the PG and With Family groups in terms of hours of sleep.
#The 95% confidence interval includes 0, further supporting the lack of a significant difference.
#Conclusion:
#  Based on the Welch Two Sample t-test, we do not find sufficient evidence to conclude that there is a significant difference in the mean hours of sleep between individuals living with their family (With Family) and individuals in a PG group.

# hypothesys 2: One-way ANOVA

#Null Hypothesis (H0): The mean alcohol frequency is the same across different living
#situations (PG, Rented Apartment, University/College Hostel, With Family).

#Alternative Hypothesis (H1): There is a significant difference in the mean alcohol 
#frequency across different living situations (PG, Rented Apartment,
#University/College Hostel, With Family).

data$LivingSituation <- as.factor(data$LivingSituation)

str(data$LivingSituation)
summary(data$LivingSituation)

# Convert AlcoholFrequency to a numeric variable (assuming it represents a scale)
data$AlcoholFrequencyNumeric <- as.numeric(gsub("[^0-9-]+", "", data$AlcoholFrequency))
data$AlcoholFrequencyNumeric[data$AlcoholFrequency == "Never"] <- 0  # Treat "Never" as 0

# Check the updated structure and summary
str(data$AlcoholFrequencyNumeric)
summary(data$AlcoholFrequencyNumeric)

# Perform One-Way ANOVA
one_way_anova_result <- aov(AlcoholFrequencyNumeric ~ LivingSituation, data = data)
one_way_anova_result

# Display ANOVA results
summary(one_way_anova_result)
#descriptive statistics
# Calculate skewness and kurtosis for Alcohol Frequency
skewness_alcohol <- skewness(data$AlcoholFrequencyNumeric)
kurtosis_alcohol <- kurtosis(data$AlcoholFrequencyNumeric)

# Print the results
cat("Skewness for Alcohol Frequency:", skewness_alcohol, "\n")
cat("Kurtosis for Alcohol Frequency:", kurtosis_alcohol, "\n")
#Skewness for Alcohol Frequency: 4.2436

#Interpretation: The skewness value of 4.2436 indicates a highly positively skewed distribution. The majority of respondents may have lower alcohol frequency, with a few having significantly higher values, leading to the skewness towards the right.
#Kurtosis for Alcohol Frequency: 16.8928

#Interpretation: The kurtosis value of 16.8928 suggests a distribution with heavy tails and a higher peak than a normal distribution. This high positive kurtosis indicates a leptokurtic distribution, emphasizing the presence of outliers or extreme values in the data.
#Analysis:
# The mean alcohol frequency across different living situations (PG, Rented Apartment, University/College Hostel, With Family) is statistically different, as indicated by the p-value of 0.0112.
#The F-statistic value of 3.835 suggests that there is a significant variation in alcohol frequency means among the living situations.
#The effect of living situation on alcohol frequency is significant, supporting the rejection of the null hypothesis.

# The results of the One-Way ANOVA suggest that there is a significant difference in alcohol frequency means among individuals living in different situations. Further post-hoc tests or pairwise comparisons may be conducted to identify specific differences between living situations.

#Conclusion:
#T-Test Hypothesis:
 # The results of the Welch Two Sample t-test do not provide sufficient evidence to conclude a significant difference in mean hours of sleep between individuals living with family and those in PG accommodation (p-value = 0.3149).

#One-Way ANOVA Hypothesis:
#  The One-Way ANOVA suggests a significant difference in alcohol frequency means among individuals living in different situations (p-value = 0.0112), supporting the rejection of the null hypothesis. Further post-hoc tests may explore specific differences between living situations.

