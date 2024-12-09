#Practice Analysis with simulated data (practice for MEES 613 final project)
#Load data
library(readr)
Microplastic_Concentration_Data <- read.csv("~/Documents/MEES 613/Final project/Microplastic_Concentration_Data.csv")
#Summary statistics
summary(Microplastic_Concentration_Data)
#Plot Microplastic concentration by species
library(ggplot2)
# Create the plot of microplastic concentration by species
plot <-ggplot(Microplastic_Concentration_Data, aes(x = Species, y = `Microplastic.Concentration..particles.g.`)) +
  geom_boxplot(fill = "skyblue", color = "darkblue") +
  theme_minimal() +
  labs(
    title = "Microplastic Concentration by Species",
    x = "Species",
    y = "Microplastic Concentration (particles/g)"
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
print(plot)

# Calculate summary statistics by species
library(dplyr)
# Calculate summary statistics by species
summary_stats <- Microplastic_Concentration_Data  %>%
  group_by(Species) %>%
  summarise(
    Count = n(),
    Mean = mean(Microplastic.Concentration..particles.g.),
    Std_Dev = sd(Microplastic.Concentration..particles.g.),
    Min = min(Microplastic.Concentration..particles.g.),
    Q1 = quantile(Microplastic.Concentration..particles.g., 0.25),
    Median = median(Microplastic.Concentration..particles.g.),
    Q3 = quantile(Microplastic.Concentration..particles.g., 0.75),
    Max = max(Microplastic.Concentration..particles.g.)
  )

# Print the summary statistics
print(summary_stats)

#ANOVA comparing microplastic concentration among species
aov_result <- aov(Microplastic.Concentration..particles.g. ~ Species, data = Microplastic_Concentration_Data  )
summary(aov_result)
#Effect sizes calculation for ANOVA

library(lsr)
# Calculate Eta-squared for the ANOVA model
eta_squared <- etaSquared(aov_result, type = 2)  # Type 2 is commonly used
print(eta_squared)
# Perform Tukey's test for multiple comparisons
tukey_result <- TukeyHSD(aov_result)
# Print the Tukey's test results
print(tukey_result)
# Visualize the results of Tukey's test
plot(tukey_result)
#Create density plot to understand the distribution of microplastic concentrations for each species
ggplot( Microplastic_Concentration_Data, aes(x = Microplastic.Concentration..particles.g., fill = Species)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Microplastic Concentration by Species",
       x = "Microplastic Concentration (particles/g)",
       y = "Density") +
  theme_minimal()
#Added violin plots
ggplot(Microplastic_Concentration_Data , aes(x = Species, y = Microplastic.Concentration..particles.g., fill = Species)) +
  geom_violin() +
  labs(title = "Violin Plot of Microplastic Concentration by Species",
       x = "Species",
       y = "Microplastic Concentration (particles/g)") +
  theme_minimal()
#Outlier detection
boxplot_stats <- boxplot(Microplastic.Concentration..particles.g. ~ Species, data = Microplastic_Concentration_Data, plot = FALSE)
outliers <- boxplot_stats$out
print(outliers)

ggplot(Microplastic_Concentration_Data, aes(x = Species, y = Microplastic.Concentration..particles.g.)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 8) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  labs(title = "Boxplot with Outliers Highlighted",
       x = "Species",
       y = "Microplastic Concentration (particles/g)") +
  theme_minimal()
#Dataset modified by adding additional variables 
MPCD <- read.csv("Modified_Microplastic_Concentration_Dataset.csv")
#Summary statistics for additional variables grouped by species
library(dplyr)
MPCD %>%
  group_by(Species) %>%
  summarise(
    Mean_Weight = mean(Weight..g., na.rm = TRUE),
    Mean_Length = mean(Length..cm., na.rm = TRUE),
    Mean_Energy = mean(Energy.Density..kcal.g., na.rm = TRUE)
  )
#Correlation analysis
# Correlation matrix
cor(MPCD[, c("Weight..g.", "Length..cm.", "Energy.Density..kcal.g.", "Microplastic.Concentration..particles.g.")], use = "complete.obs")
library(GGally)
ggpairs(MPCD[, c("Weight..g.", "Length..cm.", "Energy.Density..kcal.g.", "Microplastic.Concentration..particles.g.")])
#Regression Analysis
# Predict Microplastic Concentration based on Length
length_model <- lm(Microplastic.Concentration..particles.g. ~ Length..cm., data = MPCD)
summary(length_model)
# Predict Microplastic Concentration using multiple predictors
model_multi <- lm(Microplastic.Concentration..particles.g. ~ Weight..g. + Length..cm. + Energy.Density..kcal.g., data = MPCD)
summary(model_multi)
#Cluster Analysis
# Scale data and perform k-means clustering
data_scaled <- scale(MPCD[, c("Weight..g.", "Length..cm.", "Energy.Density..kcal.g.")])
set.seed(123)
kmeans_result <- kmeans(data_scaled, centers = 3)
MPCD$Cluster <- as.factor(kmeans_result$cluster)

# Visualize clusters
ggplot(MPCD, aes(x = Weight..g., y = Length..cm., color = Cluster)) +
  geom_point() +
  labs(title = "K-Means Clustering: Weight vs Length", x = "Weight (g)", y = "Length (cm)")

