library(ggdag)
library(igraph)
library(patchwork)

# 1. PRIOR DAG (Before Fieldwork - Theoretical)
prior_dag <- dagify(
  VBA ~ Program,
  RA ~ VBA,
  Yield ~ RA,
  Livelihoods ~ Yield,
  coords = list(
    x = c(Program = 1, VBA = 2, RA = 3, Yield = 4, Livelihoods = 5),
    y = c(Program = 2, VBA = 2, RA = 2, Yield = 2, Livelihoods = 2)
  )
)

prior_plot <- ggdag(prior_dag) +
  geom_dag_point(aes(color = name), size = 20, show.legend = FALSE) +
  geom_dag_text(color = "white", size = 3) +
  geom_dag_edges(edge_color = "grey50") +
  scale_color_manual(values = c("Program" = "#E69F00", "VBA" = "#E69F00", 
                                "RA" = "#56B4E9", "Yield" = "#009E73", 
                                "Livelihoods" = "#CC79A7")) +
  labs(title = "PRIOR: Simple Theoretical Model",
       subtitle = "Before Fieldwork & FGDs") +
  theme_dag() +
  theme(plot.title = element_text(color = "#E69F00", face = "bold"))

# 2. UPDATED DAG (After Fieldwork - Data-Informed)
updated_dag <- dagify(
  # Intervention pathways (from your Monte Carlo/fieldwork)
  VBA_density ~ Program,
  Training_quality ~ Program,
  Social_networks ~ Program,
  
  # Adoption mediators (from FGDs - trust, knowledge, access)
  RA_adoption ~ VBA_density + Training_quality + Social_networks + Farmer_trust,
  Knowledge ~ Training_quality + Social_networks,
  Market_access ~ VBA_density,
  Farmer_trust ~ VBA_density + Social_networks,
  
  # Yield pathways (from your data analysis)
  Yield ~ RA_adoption + Soil_quality + Rainfall,
  Input_costs ~ RA_adoption,
  Labor ~ RA_adoption,
  
  # Livelihood pathways (from Monte Carlo economic variables)
  Revenue ~ Yield + Market_access,
  Costs ~ Input_costs + Labor,
  Net_income ~ Revenue + Costs,
  Food_security ~ Net_income,
  Reinvestment ~ Net_income,
  
  # Confounders identified in fieldwork
  Soil_quality ~ Farm_size,
  Farm_size ~ Wealth,
  Wealth ~ RA_adoption,
  
  coords = list(
    x = c(Program = 1, VBA_density = 2, Training_quality = 2, Social_networks = 2,
          RA_adoption = 3, Farmer_trust = 3, Knowledge = 3, Market_access = 3,
          Yield = 4, Input_costs = 4, Labor = 4, Soil_quality = 4, Rainfall = 4,
          Revenue = 5, Costs = 5, Net_income = 6, Food_security = 7, Reinvestment = 7,
          Farm_size = 5, Wealth = 6),
    y = c(Program = 5, VBA_density = 4, Training_quality = 5, Social_networks = 6,
          RA_adoption = 5, Farmer_trust = 6, Knowledge = 4, Market_access = 3,
          Yield = 5, Input_costs = 6, Labor = 4, Soil_quality = 3, Rainfall = 7,
          Revenue = 4, Costs = 6, Net_income = 5, Food_security = 4, Reinvestment = 6,
          Farm_size = 2, Wealth = 3)
  )
)

# Color coding based on what you learned
node_types <- c(
  "Program" = "intervention",
  "VBA_density" = "mechanism", "Training_quality" = "mechanism", "Social_networks" = "mechanism",
  "RA_adoption" = "mediator", "Farmer_trust" = "mediator", "Knowledge" = "mediator", "Market_access" = "mediator",
  "Yield" = "outcome", "Input_costs" = "outcome", "Labor" = "outcome",
  "Revenue" = "economic", "Costs" = "economic", "Net_income" = "economic",
  "Food_security" = "livelihood", "Reinvestment" = "livelihood",
  "Soil_quality" = "confounder", "Rainfall" = "confounder", "Farm_size" = "confounder", "Wealth" = "confounder"
)

color_palette <- c(
  "intervention" = "#E69F00",    # Orange - program elements
  "mechanism" = "#F0E442",      # Yellow - VBA mechanisms  
  "mediator" = "#56B4E9",       # Blue - adoption factors
  "outcome" = "#009E73",        # Green - ag outcomes
  "economic" = "#0072B2",       # Dark blue - economic
  "livelihood" = "#CC79A7",     # Purple - final outcomes
  "confounder" = "#D55E00"      # Red - confounders
)

updated_plot <- ggdag(updated_dag) +
  geom_dag_point(aes(color = node_types[name]), 
                 size = 17, show.legend = TRUE) +
  geom_dag_text(color = "white", size = 2.5) +
  geom_dag_edges(edge_color = "grey40", edge_width = 0.8) +
  # scale_color_manual(name = "Node Type",
  #                    values = color_palette,
  #                    limits = names(color_palette)) +
  labs(title = "UPDATED: Fieldwork-Informed Model",
       subtitle = "After FGDs, Data Analysis & Monte Carlo") +
  theme_dag() +
  theme(plot.title = element_text(color = "#009E73", 
                                  face = "bold"),
        legend.position = "bottom")

# COMBINE PLOTS
combined_plots <- prior_plot / updated_plot +
  plot_annotation(
    title = "Evolution of Causal Understanding: VBA → RA → Livelihoods",
    subtitle = "How fieldwork and data analysis refined our theoretical model",
    caption = "Prior: Simple linear model | Updated: Complex pathways with confounders and mediators"
  )

# SAVE
ggsave("figures/dag_evolution.png", combined_plots, width = 12, height = 10, dpi = 300)
print(combined_plots)
