# VBA_Model_Kenya_25yr.R
# Enhanced model with 25-year timeline and risk events

library(decisionSupport)
source("functions/vv.R")        # For time-varying variables
source("functions/chance_event.R") # For risk events
source("functions/discount.R")  # For NPV calculations

# ---- 1. READ INPUT ESTIMATES ----
inputs <- estimate_read_csv("data/inputs_vba_kenya.csv")
make_variables(decisionSupport::estimate_read_csv(paste("data/inputs_vba_kenya.csv",sep="")))

# The model ####
# ---- 2. DEFINE THE 25-YEAR MODEL FUNCTION ----
vba_yield_impact_25yr <- function() {
  
  # Parameters for 25-year simulation
  n_years <- 25
  
  # --- TIME-VARYING TRENDS ---
  vba_growth_trend <- vv(vba_density, var_CV = 0.1, n = n_years, relative_trend = 0.02)
  trust_growth_trend <- vv(farmer_trust, var_CV = 0.05, n = n_years, relative_trend = 0.01)
  base_yield_trend <- vv(base_yield, var_CV = 0.15, n = n_years, relative_trend = 0.005)
  maize_price_trend <- vv(maize_price, var_CV = 0.1, n = n_years, relative_trend = 0.01)
  
  # --- ANNUAL RISK EVENTS ---
  drought_risk <- chance_event(chance = 0.3, value_if = 0.6, value_if_not = 1)
  vba_program_risk <- chance_event(chance = 0.1, value_if = 0.7, value_if_not = 1)
  policy_risk <- chance_event(chance = 0.15, value_if = 0.8, value_if_not = 1)
  
  # --- ANNUAL CALCULATIONS ---
  # BASELINE SCENARIO (no VBA intervention)
  baseline_adoption <- numeric(n_years)
  baseline_yield <- numeric(n_years)
  baseline_revenue <- numeric(n_years)
  baseline_cost <- numeric(n_years)
  
  # VBA INTERVENTION SCENARIO
  vba_adoption <- numeric(n_years)
  vba_yield <- numeric(n_years)
  vba_revenue <- numeric(n_years)
  vba_cost <- numeric(n_years)
  
  for (year in 1:n_years) {
    
    # --- BASELINE (no VBA) ---
    baseline_adoption[year] <- adoption_rate_baseline
    rainfall_baseline <- rnorm(1, mean = 0, sd = 0.15)
    
    baseline_yield_increase <- yield_impact_ra * baseline_adoption[year]
    baseline_yield[year] <- base_yield_trend[year] * 
      (1 + baseline_yield_increase) * 
      (1 + rainfall_baseline) * 
      drought_risk
    
    baseline_revenue[year] <- baseline_yield[year] * maize_price_trend[year]
    baseline_cost[year] <- ra_adoption_cost * baseline_adoption[year]  # RA adoption costs
    
    # --- VBA INTERVENTION ---
    vba_adoption[year] <- adoption_rate_baseline + 
      (vba_growth_trend[year] / 10) * vba_effectiveness * 
      trust_growth_trend[year] * vba_training *
      vba_program_risk * policy_risk
    
    vba_adoption[year] <- min(vba_adoption[year], 0.95)
    
    rainfall_vba <- rnorm(1, mean = 0, sd = 0.15)
    
    vba_yield_increase <- yield_impact_ra * vba_adoption[year]
    vba_yield[year] <- base_yield_trend[year] * 
      (1 + vba_yield_increase) * 
      (1 + rainfall_vba) * 
      drought_risk
    
    vba_revenue[year] <- vba_yield[year] * maize_price_trend[year]
    
    # VBA program costs + RA adoption costs
    vba_cost[year] <- (vba_program_cost * vba_growth_trend[year]) +  # Annual program cost
      (ra_adoption_cost * vba_adoption[year])         # RA adoption cost
  }
  
  # --- NET BENEFIT CALCULATIONS ---
  baseline_net_benefit <- baseline_revenue - baseline_cost
  vba_net_benefit <- vba_revenue - vba_cost
  
  # Comparative benefit: VBA vs Baseline
  comparative_benefit <- vba_net_benefit - baseline_net_benefit
  
  # --- NPV CALCULATIONS ---
  npv_baseline <- discount(baseline_net_benefit, 
                           discount_rate = discount_rate, 
                           calculate_NPV = TRUE)
  
  npv_vba <- discount(vba_net_benefit, 
                      discount_rate = discount_rate, 
                      calculate_NPV = TRUE)
  
  npv_comparative <- discount(comparative_benefit, 
                              discount_rate = discount_rate, 
                              calculate_NPV = TRUE)
  
  # --- RETURN RESULTS ---
  return(list(
    # NPV Results
    NPV_Baseline = npv_baseline,
    NPV_VBA = npv_vba,
    NPV_Comparative = npv_comparative,
    ROI = (npv_comparative) / discount(vba_cost, discount_rate, calculate_NPV = TRUE),
    
    # Adoption Rates
    Final_Adoption_Baseline = baseline_adoption[25] * 100,
    Final_Adoption_VBA = vba_adoption[25] * 100,
    Adoption_Increase = (vba_adoption[25] - baseline_adoption[25]) * 100,
    
    # Yield Results
    Avg_Yield_Baseline = mean(baseline_yield),
    Avg_Yield_VBA = mean(vba_yield),
    Yield_Increase = mean(vba_yield - baseline_yield),
    
    # Economic Results
    Avg_Net_Benefit_Baseline = mean(baseline_net_benefit),
    Avg_Net_Benefit_VBA = mean(vba_net_benefit),
    Avg_Comparative_Benefit = mean(comparative_benefit),
    
    # Risk Indicators
    Drought_Years = sum(drought_risk < 1)
    Program_Disruption = as.numeric(vba_program_risk < 1)
  ))
}

# ---- 3. RUN THE MONTE CARLO SIMULATION ----
set.seed(42)  # For reproducible results

# Run the simulation 10,000 times

simulation_results <- mcSimulation(
  estimate = estimate_read_csv("data/inputs_vba_kenya.csv"),
  model_function = vba_yield_impact_25yr,
  numberOfModelRuns = 1e4, #run 10,000 times
  functionSyntax = "plainNames"
)

# ---- 4. ANALYZE RESULTS ----
#COMPARATIVE 25-YEAR NPV ANALYSIS

# COMPARATIVE RESULTS (VBA vs Baseline):\n")
#  Mean Additional NPV round(mean(simulation_results$y[, "NPV_Comparative"]) / 1000, 1)#KES/ha
#  90% CI 
round(quantile(simulation_results$y[, "NPV_Comparative"], 0.05) / 1000, 1) 
round(quantile(simulation_results$y[, "NPV_Comparative"], 0.95) / 1000, 1) #KES/ha
#  Probability of Positive NPV 
round(mean(simulation_results$y[, "NPV_Comparative"] > 0) * 100, 1)
#  Return on Investment (ROI) 
round(mean(simulation_results$y[, "ROI"]), 2)

#ADOPTION AND YIELD IMPACTS:\n")
#  Final RA Adoption - Baseline 
round(mean(simulation_results$y[, "Final_Adoption_Baseline"]), 1)
#  Final RA Adoption - VBA 
round(mean(simulation_results$y[, "Final_Adoption_VBA"]), 1)
#  Average Yield Increase 
round(mean(simulation_results$y[, "Yield_Increase"]), 2)#tons/ha/year

