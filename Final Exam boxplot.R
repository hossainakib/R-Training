getwd()


library(ggplot2)
library(dplyr)
library(multcompView)


data <- read.csv(file.choose(), header = TRUE)


data$spirulina <- as.factor(data$spirulina)
data$Zinc <- as.factor(data$Zinc)

# ANOVA and Tukey HSD for two factors
anova_box <- aov(Tomato.Fresh.Weight ~ spirulina * Zinc, data = data)
tukey_box <- TukeyHSD(anova_box)

# Significant letters for boxplot
tukey_cld_box <- multcompLetters4(anova_box, tukey_box)

# Significant letters for each group
letters_df <- data.frame(
  spirulina = rep(levels(data$spirulina), each = nlevels(data$Zinc)),
  Zinc = rep(levels(data$Zinc), times = nlevels(data$spirulina)),
  Letters = tukey_cld_box$`spirulina:Zinc`$Letters
)

# Summarize data for boxplot annotations
data_summary <- data %>%
  group_by(spirulina, Zinc) %>%
  summarize(
    mean_weight = mean(Tomato.Fresh.Weight),
    se_weight = sd(Tomato.Fresh.Weight) / sqrt(n()),
    .groups = 'drop'
  ) %>%
  left_join(letters_df, by = c("spirulina", "Zinc"))

# Boxplot with significant letters
box_plot <- ggplot(data, aes(x = spirulina, y = Tomato.Fresh.Weight, fill = Zinc)) +
  geom_boxplot() +
  geom_text(data = data_summary, aes(x = spirulina, y = mean_weight + se_weight + 0.5, label = Letters),
            position = position_dodge(width = 0.9), size = 4, color = "black") +
  labs(title = "Effect of Spirulina and Zinc on Tomato Fresh Weight",
       x = "Spirulina Treatment",
       y = "Tomato Fresh Weight (g)") +
  theme_minimal() +
  scale_fill_discrete(name = "Zinc Levels")

box_plot

