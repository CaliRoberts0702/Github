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
