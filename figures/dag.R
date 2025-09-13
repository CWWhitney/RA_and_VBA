# Load required library
library(igraph)

# Create a simple Directed Acyclic Graph (DAG) for the VBA impact pathway
edges <- data.frame(
  from = c(
    "VBA_Program", "VBA_Program",      # VBA_Program -> VBA_Density, VBA_Training
    "VBA_Density", "VBA_Training", "Farmer_Trust",  # -> RA_Adoption
    "RA_Adoption", "Rainfall", "Drought",           # -> Maize_Yield
    "Maize_Yield", "VBA_Program", "RA_Adoption",    # -> Farm_Revenue, Program_Costs, Adoption_Costs
    "Farm_Revenue", "Program_Costs", "Adoption_Costs"  # -> Net_Benefit
  ),
  to = c(
    "VBA_Density", "VBA_Training",
    "RA_Adoption", "RA_Adoption", "RA_Adoption",
    "Maize_Yield", "Maize_Yield", "Maize_Yield",
    "Farm_Revenue", "Program_Costs", "Adoption_Costs",
    "Net_Benefit", "Net_Benefit", "Net_Benefit"
  )
)

# Create the graph from edge list
dag <- graph_from_data_frame(edges, directed = TRUE)

# Check the vertex names and order
cat("Number of vertices:", vcount(dag), "\n")
cat("Vertex names in order:\n")
print(V(dag)$name)

# Get the vertex names in order to assign attributes correctly
vertex_names <- V(dag)$name

# Create attribute vectors with EXACTLY 12 elements
V(dag)$type <- c(
  "intervention",  # VBA_Program
  "intervention",  # VBA_Density  
  "intervention",  # VBA_Training
  "factor",        # Farmer_Trust
  "outcome",       # RA_Adoption
  "factor",        # Rainfall
  "factor",        # Drought
  "outcome",       # Maize_Yield
  "economic",      # Farm_Revenue
  "economic",      # Program_Costs
  "economic",      # Adoption_Costs
  "result"         # Net_Benefit
)

V(dag)$color <- c(
  "#FF6B6B",       # VBA_Program - Intervention (red)
  "#FF8E8E",       # VBA_Density - Intervention (light red)
  "#FF8E8E",       # VBA_Training - Intervention (light red)
  "#4ECDC4",       # Farmer_Trust - Factor (teal)
  "#45B7D1",       # RA_Adoption - Outcome (blue)
  "#F9C80E",       # Rainfall - Factor (yellow)
  "#F9C80E",       # Drought - Factor (yellow)
  "#45B7D1",       # Maize_Yield - Outcome (blue)
  "#6A0572",       # Farm_Revenue - Economic (purple)
  "#6A0572",       # Program_Costs - Economic (purple)
  "#6A0572",       # Adoption_Costs - Economic (purple)
  "#FF6B6B"        # Net_Benefit - Result (red)
)

V(dag)$shape <- c(
  "square",        # VBA_Program
  "circle",        # VBA_Density
  "circle",        # VBA_Training
  "circle",        # Farmer_Trust
  "square",        # RA_Adoption
  "circle",        # Rainfall
  "circle",        # Drought
  "square",        # Maize_Yield
  "square",        # Farm_Revenue
  "square",        # Program_Costs
  "square",        # Adoption_Costs
  "circle"        # Net_Benefit
)

V(dag)$size <- c(
  25,  # VBA_Program
  20,  # VBA_Density
  20,  # VBA_Training
  20,  # Farmer_Trust
  25,  # RA_Adoption
  15,  # Rainfall
  15,  # Drought
  25,  # Maize_Yield
  20,  # Farm_Revenue
  20,  # Program_Costs
  20,  # Adoption_Costs
  30   # Net_Benefit
)

# Set edge attributes
E(dag)$color <- "grey40"
E(dag)$arrow.size <- 0.8
E(dag)$width <- 2

# Create the plot
# bottom, left, top, right
# png(filename="figures/dag.png")
par(mar = c(1, 1, 2, 1))
plot(dag, 
     layout = layout_with_kk(dag),
     main = "VBA Program Impact Pathway\n(Regenerative Agriculture Adoption in Kenya)",
     vertex.label.cex = 0.8,
     vertex.label.color = "black",
     vertex.frame.color = "white",
     edge.curved = 0.2)

# Add legend
legend("bottomright",
       legend = c("Intervention", "Factor", "Outcome", "Economic", "Result"),
       pt.bg = c("#FF6B6B", "#4ECDC4", "#45B7D1", "#6A0572", "#FF6B6B"),
       pch = c(22, 21, 22, 22, 21),
       pt.cex = 2,
       cex = 0.8,
       bty = "n",
       xpd=TRUE,
       inset=c(-.30, -.11)) #higher right and down

dev.off()
