library(tidyverse)
library(readr)
library(magrittr)

# Load field data
# work in all the data we collected (as much as possible)
data_5 <- read_csv("data/field_data/RA_Elicitation_FGDs_datamod5.csv")
data_4 <- read_csv("data/field_data/RA_Elicitation_FGDs_datamod4.csv")
data_3 <- read_csv("data/field_data/RA_Elicitation_FGDs_datamod3.csv")
data_2 <- read_csv("data/field_data/RA_Elicitation_FGDs_data.csv")

# Create real_data step by step with compatible variable names

# Adoption data (replaces manure, crop_rotation, etc.)
adoption_data <- data_3 %>%
  select(`Farmer No.`, `RA Practice`, `Adoption WITH VBA Support (%)`) %>%
  mutate(
    # Count only practices where adoption is > 50% (or choose  our threshold)
    adopted_practice = `Adoption WITH VBA Support (%)` > 50
  ) %>%
  group_by(`Farmer No.`) %>%
  summarise(
    number_of_practices = sum(adopted_practice, na.rm = TRUE),  # Count adopted practices
    avg_adoption_rate = mean(`Adoption WITH VBA Support (%)`, na.rm = TRUE),
    total_practices_evaluated = n(),  # Keep this for reference
    .groups = 'drop'
  )

# Check the variability (this captures the variability)
# summary(adoption_data$number_of_practices)
# table(adoption_data$number_of_practices)

# Practice-specific adoption with compatible names
data_3$`Adoption WITHOUT VBA (%)` <- as.numeric(data_3$`Adoption WITHOUT VBA (%)`)
practice_adoption <- data_3 %>%
  select(`Farmer No.`, `RA Practice`, `Adoption WITH VBA Support (%)`) %>%
  mutate(
    adopted = `Adoption WITH VBA Support (%)` > 50  # Binary adoption (adopted if >50%)
  ) %>%
  pivot_wider(
    names_from = `RA Practice`, 
    values_from = adopted,
    values_fill = FALSE
  )

# Check what practice names we have and rename appropriately
# print(names(practice_adoption))

# Rename based on what's actually in our data
practice_adoption <- practice_adoption %>%
  rename(
    manure = any_of(c("Manure application", "Manure", "manure")),
    mulching = any_of(c("Mulching", "mulching", "Mulch")),
    intercropping = any_of(c("Intercropping", "intercropping")),
    crop_rotation = any_of(c("Crop rotation", "crop_rotation", "Rotation"))
  ) %>%
  # If the columns don't exist, create them as FALSE
  mutate(
    manure = if (!"manure" %in% names(.)) FALSE else manure,
    mulching = if (!"mulching" %in% names(.)) FALSE else mulching,
    intercropping = if (!"intercropping" %in% names(.)) FALSE else intercropping,
    crop_rotation = if (!"crop_rotation" %in% names(.)) FALSE else crop_rotation
  )
   
   # Yield data (convert to tons/hectare equivalent)
   yield_data <- data_4 %>%
 mutate(across(c(`Lowest Yield in bags/ac`, `Highest Yield in bags/ac`), as.numeric)) %>%
 group_by(`Farmer No.`) %>%
 summarise(
   baseline_yield = mean(`Baseline (bags/ac)`, na.rm = TRUE),
   # Convert bags/acre to tons/hectare (approximate conversion)
   yield_tha = mean(`Highest Yield in bags/ac`, na.rm = TRUE) * 0.025,  # Rough conversion
   .groups = 'drop'
 )
 
 # Farmer characteristics (create group variable)
 farmer_chars <- data_2 %>%
   select(`Farmer No.`, `VBA Contact Frequency`, `Peer Adoption Rate`, 
          `Probability of Trial (%)`) %>%
   distinct(`Farmer No.`, .keep_all = TRUE) %>%
   mutate(
     # Create group variable: High VBA contact = "RA Farmer", else "Control"
     group = ifelse(`VBA Contact Frequency` == "High", "RA Farmer", "Control")
   )
 
 # Cost data for practice effects
 cost_data <- data_5 %>%
   group_by(`Farmer No.`, `RA Practice / Activity`) %>%
   summarise(
     avg_total_cost = mean(`Total  Labour Cost (KSh)` + `Total  Inputs Cost (KSh)`, na.rm = TRUE),
     .groups = 'drop'
   )
 
 # Calculate average practice effect from cost-benefit
 practice_effect_data <- data_5 %>%
   group_by(`Farmer No.`) %>%
   summarise(
     # Simple practice effect based on net benefits
     practice_effect = mean(`Total Value (KSh)` - (`Total  Labour Cost (KSh)` + `Total  Inputs Cost (KSh)`), na.rm = TRUE) / 10000,
     .groups = 'drop'
   )
 
 # Combine in field_data
  field_data <- adoption_data %>%
   left_join(practice_adoption, by = "Farmer No.") %>%
   left_join(yield_data, by = "Farmer No.") %>%
   left_join(farmer_chars, by = "Farmer No.") %>%
   left_join(practice_effect_data, by = "Farmer No.") %>%
   # Fill any missing practice columns with FALSE
   mutate(across(any_of(c("manure", "crop_rotation", "intercropping", "mulching")), 
                 ~ ifelse(is.na(.), FALSE, .))) %>%
   # Create household_id and noise variables to match original structure
   mutate(
     household_id = paste0("F", `Farmer No.`),
     noise = rnorm(n(), 0, 0.1),  # Add small random noise
     base_yield = baseline_yield * 0.025  # Convert baseline to tons/hectare
   ) %>%
   select(
     household_id,
     group,
     manure = any_of("manure"),
     crop_rotation = any_of("crop_rotation"), 
     intercropping = any_of("intercropping"),
     mulching = any_of("mulching"),
     number_of_practices,
     base_yield,
     practice_effect,
     noise,
     yield_tha
   ) %>%
   # Convert logical to numeric (0/1) for compatibility
   mutate(across(c(manure, crop_rotation, intercropping, mulching), as.numeric)) %>%
   # Fill any remaining NAs
   replace_na(list(
     manure = 0, crop_rotation = 0, intercropping = 0, mulching = 0,
     number_of_practices = 0, practice_effect = 0, noise = 0
   ))
 
 # Set factor order for group variable
  field_data$group <- factor( field_data$group, levels = c("Control", "RA Farmer"))
 
 # Check the structure
 # str( field_data)
 # summary( field_data)
 
 # run (more or less) our original plot scripts
 # Same thing we shared at Tropentag
 
 # Adoption Rates for Key Practices
 plot1_data <-  field_data %>%
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
 
 # Get sample sizes for each group
 group_counts <- field_data %>%
   count(group) %>%
   mutate(label = paste0(group, " (n=", n, ")"))
 
 # Create bar plot
 plot1 <- ggplot(plot1_data, aes(x = practice, y = mean_adoption, fill = group)) +
   geom_col(position = position_dodge(0.9), color = "black", width = 0.8) +
   geom_errorbar(aes(ymin = mean_adoption - se_adoption, ymax = mean_adoption + se_adoption),
                 position = position_dodge(0.9), width = 0.2) +
   scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
   scale_fill_manual(values = c("Control" = "#E69F00", 
                                "RA Farmer" = "#009E73"),
                     labels = group_counts$label) +
   labs(title = "Impact of VBA Intervention on RA Practice Adoption",
        subtitle = "Error bars represent standard error of the mean",
        x = NULL,
        y = "Proportion of Farmers Adopting",
        fill = "Farmer Group") +
   theme_minimal() +
   theme(axis.text.x = element_text(angle = 45, hjust = 1),
         legend.position = "top")
 # print(plot1)
 
 # Adoption Intensity (Number of Practices)
 plot2 <- ggplot( field_data, aes(x = number_of_practices, fill = group)) +
   geom_density(alpha = 0.6, adjust = 1.5) +
   scale_fill_manual(values = c("Control" = "#E69F00", 
                                "RA Farmer" = "#009E73"),
                     labels = group_counts$label) +
   labs(title = "Intensity of RA Practice Adoption per Plot",
        x = "Number of Regenerative Practices Adopted",
        y = "Density",
        fill = "Farmer Group") +
   theme_minimal() +
   theme(legend.position = "top")
 # print(plot2)
 
 # Yield vs. Practice Adoption
 
 # Probably same as geom_smooth but we do it manually
 # everyone loves a good lm() =) 
 
 # Model the yield effect and plot it clearly
 model <- lm(yield_tha ~ number_of_practices, data =  field_data)
# summary(model)
 
 # Create a prediction dataframe for a smooth line
 prediction_data <- data.frame(number_of_practices = seq(0, max( field_data$number_of_practices, na.rm = TRUE), by = 0.1))
 preds <- predict(model, newdata = prediction_data, se.fit = TRUE)
 prediction_data$yield <- preds$fit
 prediction_data$ymin <- preds$fit - 1.96 * preds$se.fit
 prediction_data$ymax <- preds$fit + 1.96 * preds$se.fit
 
 plot4 <- ggplot() +
   geom_jitter(data =  field_data, aes(x = number_of_practices, y = yield_tha, color = group), alpha = 0.3, width = 0.1) +
   geom_ribbon(data = prediction_data, aes(x = number_of_practices, ymin = ymin, ymax = ymax), fill = "grey70", alpha = 0.4) +
   geom_line(data = prediction_data, aes(x = number_of_practices, y = yield), size = 1.2, color = "black") +
   scale_color_manual(values = c("Control" = "#E69F00", 
                                 "RA Farmer" = "#009E73"),
                                labels = group_counts$label) +
   labs(title = "Modelled Effect of Practice Adoption on Yield",
        x = "Number of Regenerative Practices Adopted per Plot",
        y = "Predicted Yield (tons per hectare)",
        color = "Farmer Group") +
   theme_minimal() +
   theme(legend.position = "top")
 # print(plot4)
 
 # save
 ggsave("figures/plot1_adoption_rates.png", plot1, width = 8, height = 6, dpi = 300)
 ggsave("figures/plot2_adoption_intensity.png", plot2, width = 8, height = 6, dpi = 300)
 ggsave("figures/plot4_yield_relationship.png", plot3, width = 8, height = 6, dpi = 300)
 
 