# VBA_Model_Kenya_25yr.R
# Enhanced model with 25-year timeline and risk events

library(decisionSupport)
source("functions/vv.R")        # For time-varying variables
source("functions/chance_event.R") # For risk events
source("functions/discount.R")  # For NPV calculations
source("functions/make_variables.R")  # For testing model steps

# READ INPUT ESTIMATES ----

make_variables(decisionSupport::estimate_read_csv(paste("data/inputs_vba_kenya.csv",sep="")))

# VBA_Causal_Model.R
# A simplified model to estimate the causal effect of VBAs on RA adoption and yield

# DEFINE THE CAUSAL MODEL FUNCTION ----
vba_causal_effect <- function() {
  
  n_years <- 25
  
  # --- ENVIRONMENTAL FACTORS (from our DAG) ---
  # Add simple rainfall and drought effects
  rainfall_effect <- vv(0, var_CV = 0.3, n = n_years)  # Can be 0 if no data
  drought_effect <- chance_event(0.1, n = n_years, value_if = -0.2)  # 10% chance of 20% yield loss
  
  # --- CONTROL GROUP (no VBA) ---
  # Use field data for baseline (good!)
  field_calibrated_baseline <- field_adoption_without_vba / 100
  adoption_control <- vv(field_calibrated_baseline, 
                         var_CV = 0.1, n = n_years, relative_trend = 0.005)
  
  # Add environmental effects to yield
  yield_control <- vv(base_yield, var_CV = yield_variability/base_yield, n = n_years) * 
    (1 + yield_impact_ra * adoption_control + rainfall_effect + drought_effect)
  
  # --- TREATMENT GROUP (with VBA) ---
  # Calculate VBA effect ONCE
  vba_boost <- (field_adoption_with_vba - field_adoption_without_vba) / 100
  vba_effect <- vba_density * vba_effectiveness * vba_training_quality * farmer_trust_start
  
  # Use EITHER field data OR theoretical model, not both
  adoption_treatment <- vv(field_calibrated_baseline + vba_boost,
                           var_CV = 0.08, n = n_years, relative_trend = 0.02)
  
  # Realistic constraints
  adoption_control <- pmin(adoption_control, 0.85)
  adoption_treatment <- pmin(adoption_treatment, 0.85)
  
  # Treatment yield with environmental effects
  yield_treatment <- vv(base_yield, var_CV = yield_variability/base_yield, n = n_years) * 
    (1 + yield_impact_ra * adoption_treatment + rainfall_effect + drought_effect)
  
  # --- COSTS (simplified) ---
  ra_adoption_cost_calibrated <- labor_cost + input_cost
  
  # Control costs
  control_costs <- vv(ra_adoption_cost_calibrated * adoption_control, 
                      var_CV = 0.15, n = n_years)
  
  # Treatment costs
  vba_program_costs <- vv(vba_program_cost * vba_density/100, 
                          var_CV = 0.1, n = n_years)
  treatment_adoption_costs <- vv(ra_adoption_cost_calibrated * adoption_treatment,
                                 var_CV = 0.15, n = n_years)
  treatment_costs <- vba_program_costs + treatment_adoption_costs
  
  # --- REVENUES AND NET BENEFITS ---
  control_revenue <- yield_control * maize_price
  treatment_revenue <- yield_treatment * maize_price
  
  control_net_benefit <- control_revenue - control_costs
  treatment_net_benefit <- treatment_revenue - treatment_costs
  
  adoption_effect <- adoption_treatment - adoption_control
  yield_effect <- yield_treatment - yield_control
  comparative_net_benefit <- treatment_net_benefit - control_net_benefit
  
  # NPV calculations
  npv_control <- discount(control_net_benefit, discount_rate = discount_rate, calculate_NPV = TRUE)
  npv_treatment <- discount(treatment_net_benefit, discount_rate = discount_rate, calculate_NPV = TRUE)
  npv_comparative <- discount(comparative_net_benefit, discount_rate = discount_rate, calculate_NPV = TRUE)
  
  pv_vba_costs <- discount(vba_program_costs, discount_rate = discount_rate, calculate_NPV = TRUE)
  roi <- npv_comparative / pv_vba_costs
  
  # Return results 
  return(list(
    NPV_Comparative = npv_comparative,
    ROI = roi,
    Final_Adoption_Baseline = adoption_control[25] * 100,
    Final_Adoption_VBA = adoption_treatment[25] * 100,
    Yield_Increase = mean(yield_effect),
    Adoption_Effect = mean(adoption_effect) * 100,
    Yield_Effect = mean(yield_effect),
    Mean_Control_Yield = mean(yield_control),
    Mean_Treatment_Yield = mean(yield_treatment),
    Mean_VBA_Cost = mean(vba_program_costs),
    Probability_Positive_NPV = as.numeric(npv_comparative > 0)
  ))
}