# Load necessary libraries
library(tidyverse)
library(ggrepel) # For better labels if needed

# 1. READ THE DATA
syn_data <- read_csv("kenya_ra_adoption_synthetic_data.csv")
syn_data$group <- factor(syn_data$group, levels = c("Control", "RA Farmer")) # Set factor order

# 2. PLOT 1: Adoption Rates for Key Practices
# First, summarize the data to get means and standard errors
plot1_data <- syn_data %>%
  select(group, manure, crop_rotation, intercropping, mulching) %>%
  pivot_longer(cols = -group, names_to = "practice", values_to = "adopted") %>%
  group_by(group, practice) %>%
  summarise(
    mean_adoption = mean(adopted, na.rm = TRUE),
    se_adoption = sd(adopted, na.rm = TRUE) / sqrt(n()),
    .groups = 'drop'
  ) %>%
  mutate(
    practice = factor(practice,
                      levels = c("manure", "crop_rotation", "intercropping", "mulching"),
                      labels = c("Manure Use", "Crop Rotation", "Intercropping", "Mulching"))
  )

# Create the bar plot
plot1 <- ggplot(plot1_data, aes(x = practice, y = mean_adoption, fill = group)) +
  geom_col(position = position_dodge(0.9), color = "black", width = 0.8) +
  geom_errorbar(aes(ymin = mean_adoption - se_adoption, ymax = mean_adoption + se_adoption),
                position = position_dodge(0.9), width = 0.2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("Control" = "#E69F00", "RA Farmer" = "#009E73")) + # Colorblind-friendly palette
  labs(title = "Impact of VBA Intervention on RA Practice Adoption",
       subtitle = "Error bars represent standard error of the mean",
       x = NULL,
       y = "Proportion of Farmers Adopting",
       fill = "Farmer Group") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top")
print(plot1)

# 3. PLOT 2: Adoption Intensity (Number of Practices)
plot2 <- ggplot(syn_data, aes(x = number_of_practices, fill = group)) +
  geom_density(alpha = 0.6, adjust = 1.5) + # adjust smoothes the curve
  scale_fill_manual(values = c("Control" = "#E69F00", "RA Farmer" = "#009E73")) +
  labs(title = "Intensity of RA Practice Adoption per Plot",
       x = "Number of Regenerative Practices Adopted",
       y = "Density",
       fill = "Farmer Group") +
  theme_minimal() +
  theme(legend.position = "top")
print(plot2)

# 4. PLOT 3: Yield vs. Practice Adoption
plot3 <- ggplot(syn_data, aes(x = number_of_practices, y = yield_tha)) +
  geom_jitter(aes(color = group), alpha = 0.6, height = 0, width = 0.2) + # Jitter points
  geom_smooth(aes(group = 1), method = "lm", formula = y ~ x, se = TRUE, color = "black") + # Overall trend line
  scale_color_manual(values = c("Control" = "#E69F00", "RA Farmer" = "#009E73")) +
  labs(title = "Relationship Between RA Practice Adoption and Maize Yield",
       subtitle = "Each additional practice is associated with a ~0.07 t/ha yield increase",
       x = "Number of Regenerative Practices Adopted per Plot",
       y = "Yield (tons per hectare)",
       color = "Farmer Group") +
  theme_minimal() +
  theme(legend.position = "top")
print(plot3)

# 5. (BONUS) PLOT 4: Model the yield effect and plot it clearly
model <- lm(yield_tha ~ number_of_practices, data = syn_data)
summary(model) # Check the coefficient is ~0.07

# Create a prediction dataframe for a smooth line
prediction_data <- data.frame(number_of_practices = seq(0, 4, by = 0.1))
preds <- predict(model, newdata = prediction_data, se.fit = TRUE)
# generate predictions from the lm model
prediction_data$yield <- preds$fit
prediction_data$ymin <- preds$fit - 1.96 * preds$se.fit
prediction_data$ymax <- preds$fit + 1.96 * preds$se.fit

plot4 <- ggplot() +
  geom_jitter(data = syn_data, aes(x = number_of_practices, y = yield_tha, color = group), alpha = 0.3, width = 0.1) +
  geom_ribbon(data = prediction_data, aes(x = number_of_practices, ymin = ymin, ymax = ymax), fill = "grey70", alpha = 0.4) +
  geom_line(data = prediction_data, aes(x = number_of_practices, y = yield), size = 1.2, color = "black") +
  scale_color_manual(values = c("Control" = "#E69F00", "RA Farmer" = "#009E73")) +
  labs(title = "Modelled Effect of Practice Adoption on Yield",
       x = "Number of Regenerative Practices Adopted per Plot",
       y = "Predicted Yield (tons per hectare)",
       color = "Farmer Group") +
  theme_minimal() +
  theme(legend.position = "top")
print(plot4)

# 6. SAVE THE PLOTS
ggsave("figures/plot1_adoption_rates.png", plot1, width = 8, height = 6, dpi = 300)
ggsave("figures/plot2_adoption_intensity.png", plot2, width = 8, height = 6, dpi = 300)
ggsave("figures/plot3_yield_relationship.png", plot3, width = 8, height = 6, dpi = 300)
ggsave("figures/plot4_modelled_effect.png", plot4, width = 8, height = 6, dpi = 300)

