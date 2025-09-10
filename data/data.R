# Load necessary libraries
library(tidyverse)
set.seed(42) # For reproducibility

# preliminary data from papers (waiting for field data)

# Define the sample size
n_control <- 851
n_ra <- 1081
n_total <- n_control + n_ra

# Create a base dataframe
group <- factor(c(rep("Control", n_control), rep("RA Farmer", n_ra)))

# Initialize an empty dataframe
syn_data <- tibble(
  household_id = 1:n_total,
  group = group,
  
  # Generate practice adoption based on probabilities from the paper
  manure = rbinom(n_total, 1, prob = ifelse(group == "Control", 0.54, 0.76)),
  crop_rotation = rbinom(n_total, 1, prob = ifelse(group == "Control", 0.18, 0.42)),
  intercropping = rbinom(n_total, 1, prob = ifelse(group == "Control", 0.29, 0.54)),
  mulching = rbinom(n_total, 1, prob = ifelse(group == "Control", 0.22, 0.39)),
)

# Calculate the total number of practices adopted (from these 4 key ones)
syn_data <- syn_data %>%
  mutate(
    number_of_practices = manure + crop_rotation + intercropping + mulching
  )

# Generate yield based on the number of practices and group, adding some noise
# The model from the paper: yield increase of 0.07 t/ha per practice
syn_data <- syn_data %>%
  mutate(
    base_yield = ifelse(group == "Control",
                        1.0 + rnorm(n(), 0, 0.3), # Control base yield with noise
                        1.2 + rnorm(n(), 0, 0.3)), # RA base yield is slightly higher
    practice_effect = number_of_practices * 0.07, # The key finding from the paper
    noise = rnorm(n(), 0, 0.2), # Random noise
    yield_tha = base_yield + practice_effect + noise
  ) %>%
  # Ensure yield doesn't go negative
  mutate(yield_tha = pmax(0.5, yield_tha))

# Check the summary statistics to ensure they are realistic
syn_data %>%
  group_by(group) %>%
  summarise(
    across(c(manure, crop_rotation, intercropping, mulching), mean),
    mean_practices = mean(number_of_practices),
    mean_yield = mean(yield_tha),
    sd_yield = sd(yield_tha)
  )

# Save the synthetic dataset to a CSV file
write_csv(syn_data, "data/kenya_ra_adoption_synthetic_data.csv")
