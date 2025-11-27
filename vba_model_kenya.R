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
  
  # # Sample from posterior distributions
  # baseline_adoption <- sample(posterior_samples$baseline_adoption, 1)
  # vba_effectiveness <- sample(posterior_samples$vba_effectiveness, 1)
  # yield_impact_ra <- sample(posterior_samples$yield_impact_ra, 1)
  # 
  # Then run the original model with these updated values
  
  # --- CONTROL GROUP (no VBA) ---
  adoption_control <- vv(baseline_adoption, 
                         var_CV = 0.1, 
                         n = n_years,
                         relative_trend = 0.005)
  
  yield_control <- vv(base_yield, var_CV = yield_variability/base_yield, 
                      n = n_years) * 
    (1 + yield_impact_ra * adoption_control)
  
  # CALIBRATE WITH FIELD DATA
  field_calibrated_baseline <- field_adoption_without_vba / 100
  vba_boost <- (field_adoption_with_vba - field_adoption_without_vba) / 100
  
  # Use our existing model but with calibrated baseline
  adoption_control <- vv(field_calibrated_baseline, 
                         var_CV = 0.1, n = n_years, relative_trend = 0.005)
  
  adoption_treatment <- vv(field_calibrated_baseline + (vba_density * vba_effectiveness * vba_boost),
                           var_CV = 0.08, n = n_years, relative_trend = 0.02)
  
  # Continue with our existing calculations...
  adoption_treatment <- pmin(adoption_treatment, 0.85)
  
  # Update costs with field data
  ra_adoption_cost_calibrated <- labor_cost + input_cost
  
  # Control group costs: only RA adoption costs (no VBA program costs)
  control_costs <- vv(ra_adoption_cost * adoption_control, 
                      var_CV = 0.15, 
                      n = n_years)
  # overwrite with new field data
  control_costs <- vv(ra_adoption_cost_calibrated * adoption_control, 
                      var_CV = 0.15, n = n_years)
  
  control_revenue <- yield_control * maize_price
  control_net_benefit <- control_revenue - control_costs
  
# TREATMENT GROUP (with VBA) ---
  vba_effect <- vba_density * 
    vba_effectiveness * 
    vba_training_quality * 
    farmer_trust_start
  
  adoption_treatment <- vv(baseline_adoption + vba_effect,
                           var_CV = 0.08,
                           n = n_years,
                           relative_trend = 0.02)
  
  adoption_treatment <- pmin(adoption_treatment, 0.85)
  
  yield_treatment <- vv(base_yield, var_CV = yield_variability/base_yield, 
                        n = n_years) * 
    (1 + yield_impact_ra * adoption_treatment)
  
  # Treatment group costs: VBA program costs + RA adoption costs
  vba_program_costs <- vv(vba_program_cost * vba_density/100,  # Cost per farmer
                          var_CV = 0.1,
                          n = n_years)
  
  treatment_adoption_costs <- vv(ra_adoption_cost * adoption_treatment,
                                 var_CV = 0.15,
                                 n = n_years)
  
  treatment_costs <- vba_program_costs + treatment_adoption_costs
  # Overwrite with field data
  treatment_costs <- vba_program_costs + 
    vv(ra_adoption_cost_calibrated * adoption_treatment, 
       var_CV = 0.15, n = n_years)
  
  treatment_revenue <- yield_treatment * maize_price
  treatment_net_benefit <- treatment_revenue - treatment_costs
  
  # --- CALCULATE CAUSAL EFFECTS ---
  adoption_effect <- adoption_treatment - adoption_control
  yield_effect <- yield_treatment - yield_control
  comparative_net_benefit <- treatment_net_benefit - control_net_benefit
  
  # --- NPV CALCULATIONS ---
  npv_control <- discount(control_net_benefit, 
                          discount_rate = discount_rate, 
                          calculate_NPV = TRUE)
  
  npv_treatment <- discount(treatment_net_benefit, 
                            discount_rate = discount_rate, 
                            calculate_NPV = TRUE)
  
  npv_comparative <- discount(comparative_net_benefit, 
                              discount_rate = discount_rate, 
                              calculate_NPV = TRUE)
  
  # Calculate ROI: (NPV Comparative) / (Present Value of VBA Program Costs)
  pv_vba_costs <- discount(vba_program_costs, 
                           discount_rate = discount_rate, 
                           calculate_NPV = TRUE)
  
  roi <- npv_comparative / pv_vba_costs
  
  # --- RETURN RESULTS ---
  return(list(
    # NPV Results for our R Markdown
    NPV_Comparative = npv_comparative,
    ROI = roi,
    
    # Adoption Results for our R Markdown
    Final_Adoption_Baseline = adoption_control[25] * 100,
    Final_Adoption_VBA = adoption_treatment[25] * 100,
    Yield_Increase = mean(yield_effect),
    
    # Causal Effects
    Adoption_Effect = mean(adoption_effect) * 100,
    Yield_Effect = mean(yield_effect),
    
    # Additional diagnostic info
    Mean_Control_Yield = mean(yield_control),
    Mean_Treatment_Yield = mean(yield_treatment),
    Mean_VBA_Cost = mean(vba_program_costs),
    Probability_Positive_NPV = as.numeric(npv_comparative > 0)
  ))
}
