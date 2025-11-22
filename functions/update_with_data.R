update_with_data <- function(expert_prior, observed_data, n_sim = 10000) {
  
  posteriors <- list()
  
  # Update 1: BASELINE ADOPTION RATE
  # Prior from experts: baseline_adoption ~ 0.05-0.15
  # Data: Control group adoption = 0.15
  prior_mu <- expert_prior$median[expert_prior$variable == "baseline_adoption"]
  prior_sd <- (expert_prior$upper[expert_prior$variable == "baseline_adoption"] - 
                 expert_prior$lower[expert_prior$variable == "baseline_adoption"]) / 3.29
  
  # Simple Bayesian update for proportion
  n_obs <- 1124  # Control farmers from TABLE 3
  successes <- round(observed_data$baseline_adoption * n_obs)
  
  alpha_post <- prior_mu * (prior_mu * (1 - prior_mu) / prior_sd^2 - 1) + successes
  beta_post <- (1 - prior_mu) * (prior_mu * (1 - prior_mu) / prior_sd^2 - 1) + n_obs - successes
  
  posteriors$baseline_adoption <- rbeta(n_sim, alpha_post, beta_post)
  
  # Update 2: VBA EFFECTIVENESS
  # Prior: vba_effectiveness ~ 0.15-0.5
  # Data: VBA effect = 0.25 (from TABLE 3 difference)
  prior_mu_eff <- expert_prior$median[expert_prior$variable == "vba_effectiveness"]
  prior_sd_eff <- (expert_prior$upper[expert_prior$variable == "vba_effectiveness"] - 
                     expert_prior$lower[expert_prior$variable == "vba_effectiveness"]) / 3.29
  
  # Update with treatment effect data
  data_effect <- observed_data$treatment_effect
  data_se <- 0.09  # Standard error from TABLE 5
  
  # Bayesian update for normal distribution
  posterior_mu <- (prior_mu_eff/prior_sd_eff^2 + data_effect/data_se^2) / 
    (1/prior_sd_eff^2 + 1/data_se^2)
  posterior_sd <- sqrt(1 / (1/prior_sd_eff^2 + 1/data_se^2))
  
  posteriors$vba_effectiveness <- rnorm(n_sim, posterior_mu, posterior_sd)
  
  # Update 3: YIELD IMPACT OF RA
  # Prior: yield_impact_ra ~ 0.1-0.4
  # Data: 0.07 per practice (TABLE 7), but this is per practice, not full adoption
  # Assuming full adoption = 5 practices (median from TABLE 4)
  prior_mu_yield <- expert_prior$median[expert_prior$variable == "yield_impact_ra"]
  prior_sd_yield <- (expert_prior$upper[expert_prior$variable == "yield_impact_ra"] - 
                       expert_prior$lower[expert_prior$variable == "yield_impact_ra"]) / 3.29
  
  data_yield <- observed_data$yield_impact_per_practice * 5  # Scale to full adoption
  data_se_yield <- 0.03  # Approximate SE from TABLE 7
  
  posterior_mu_yield <- (prior_mu_yield/prior_sd_yield^2 + data_yield/data_se_yield^2) / 
    (1/prior_sd_yield^2 + 1/data_se_yield^2)
  posterior_sd_yield <- sqrt(1 / (1/prior_sd_yield^2 + 1/data_se_yield^2))
  
  posteriors$yield_impact_ra <- rnorm(n_sim, posterior_mu_yield, posterior_sd_yield)
  
  return(posteriors)
}