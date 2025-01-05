getwd()


library(ggplot2)
library(dplyr)
library(multcompView)

data <- read.csv(file.choose(), header = TRUE)
data


# factors are correctly formatted
data$spirulina <- as.factor(data$spirulina)
data$Zinc <- as.factor(data$Zinc)

# --- Bar Plot ---

# mean and standard error for Tomato Fresh Weight grouped by spirulina
summary_data <- data %>%
  group_by(spirulina) %>%
  summarise(
    mean_weight = mean(Tomato.Fresh.Weight),
    se_weight = sd(Tomato.Fresh.Weight) / sqrt(n())
  )

# ANOVA and Tukey HSD
anova_result <- aov(Tomato.Fresh.Weight ~ spirulina, data = data)
tukey_result <- TukeyHSD(anova_result)

# Significant letters for bar plot
tukey_cld <- multcompLetters4(anova_result, tukey_result)
summary_data$letters <- tukey_cld$`spirulina`$Letters

# bar plot
bar_plot <- ggplot(summary_data, aes(x = spirulina, y = mean_weight, fill = spirulina)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_weight - se_weight, ymax = mean_weight + se_weight), width = 0.2) +
  geom_text(aes(label = letters, y = mean_weight + se_weight + 5), vjust = 0) +
  labs(title = "Effect of Spirulina on Tomato Fresh Weight",
       x = "Spirulina Treatment",
       y = "Mean Tomato Fresh Weight (g)") +
  theme_minimal()

bar_plot

