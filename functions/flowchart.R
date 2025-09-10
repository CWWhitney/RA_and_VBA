# Load necessary libraries
library(ggplot2)
library(dplyr)
library(ggrepel)

# 1. DEFINE THE PROCESS STEPS AND THEIR POSITIONS
process_steps <- data.frame(
  step_id = 1:4,
  label = c("Field Data\nCollection", 
            "Causal Analysis\n(PSM)", 
            "Stochastic Model\nInput", 
            "Long-Term\nForecasts"),
  x = c(1, 2, 3, 4),  # x positions
  y = c(1, 1, 1, 1)   # y positions (all on same line)
)

# 2. DEFINE THE ARROWS (CONNECTIONS BETWEEN STEPS)
arrows <- data.frame(
  from = c(1, 2, 3),
  to = c(2, 3, 4)
) %>%
  # Merge with step data to get coordinates
  left_join(process_steps, by = c("from" = "step_id")) %>%
  rename(x_start = x, y_start = y) %>%
  left_join(process_steps, by = c("to" = "step_id")) %>%
  rename(x_end = x, y_end = y)

# 3. CREATE THE FLOWCHART
flowchart <- ggplot() +
  # Add the arrows first (so they appear behind the boxes)
  geom_segment(data = arrows,
               aes(x = x_start + 0.45, xend = x_end - 0.45, 
                   y = y_start, yend = y_end),
               arrow = arrow(type = "closed", length = unit(0.2, "inches")),
               linewidth = 1.2, color = "steelblue") +
  
  # Add the boxes for each process step
  geom_rect(data = process_steps,
            aes(xmin = x - 0.45, xmax = x + 0.45,
                ymin = y - 0.2, ymax = y + 0.2),
            fill = "#009E73", color = "black", alpha = 0.8) +
  
  # Add the text labels inside the boxes
  geom_text(data = process_steps,
            aes(x = x, y = y, label = label),
            color = "white", fontface = "bold", size = 5) +
  
  # Set the coordinate limits and theme
  xlim(0.5, 4.5) +
  ylim(0.5, 1.5) +
  theme_void() +
  labs(title = "Methodological Pipeline",
       subtitle = "From empirical data collection to long-term projections") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 12))

# 4. DISPLAY AND SAVE THE FLOWCHART
print(flowchart)

# Save as high-resolution PNG for your poster
ggsave("methodology_flowchart.png", flowchart, 
       width = 10, height = 4, dpi = 300, bg = "white")