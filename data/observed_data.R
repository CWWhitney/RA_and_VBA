observed_data <- list(
  baseline_adoption = 0.15,
  vba_effect = 0.25,
  treatment_effect = 1.302 / 11,
  yield_impact_per_practice = 0.07,
  practices_vba = c(rep(0, 3), rep(1, 40), rep(2, 83), rep(3, 101), 
                    rep(4, 119), rep(5, 97), rep(6, 61), rep(7, 39),
                    rep(8, 29), rep(9, 20), rep(10, 10), rep(11, 4)),
  practices_control = c(rep(0, 99), rep(1, 252), rep(2, 319), rep(3, 213),
                        rep(4, 107), rep(5, 84), rep(6, 18), rep(7, 19),
                        rep(8, 10), rep(9, 3), rep(10, 0), rep(11, 0))
)

# ---- 3. CORRECTED BAYESIAN UPDATE FUNCTION ----
update_with_data <- function(expert_prior, observed_data, n_sim = 10000) {
  
  # Extract the distribution data frame
  dist_df <- expert_prior$distribution
  
  posteriors <- list()
  
  # Update 1: BASELINE ADOPTION RATE
  # Find the row for baseline_adoption
  base_idx <- which(dist_df$distribution == "tnorm_0_1" & 
                      grepl("baseline", dist_df$label, ignore.case = TRUE))[1]
  
  prior_lower <- dist_df$lower[base_idx]
  prior_upper <- dist_df$upper[base_idx]
  prior_mu <- (prior_lower + prior_upper) / 2  # Calculate median since it's NA
  prior_sd <- (prior_upper - prior_lower) / 3.29
  
  # Simple Bayesian update for proportion
  n_obs <- 1124  # Control farmers from TABLE 3
  successes <- round(observed_data$baseline_adoption * n_obs)
  
  # Ensure parameters are valid for beta distribution
  alpha_prior <- max(prior_mu * (prior_mu * (1 - prior_mu) / prior_sd^2 - 1), 0.1)
  beta_prior <- max((1 - prior_mu) * (prior_mu * (1 - prior_mu) / prior_sd^2 - 1), 0.1)
  
  alpha_post <- alpha_prior + successes
  beta_post <- beta_prior + n_obs - successes
  
  posteriors$baseline_adoption <- rbeta(n_sim, alpha_post, beta_post)
  
  # Update 2: VBA EFFECTIVENESS
  vba_idx <- which(dist_df$distribution == "tnorm_0_1" & 
                     grepl("effectiveness", dist_df$label, ignore.case = TRUE))[1]
  
  prior_lower_eff <- dist_df$lower[vba_idx]
  prior_upper_eff <- dist_df$upper[vba_idx]
  prior_mu_eff <- (prior_lower_eff + prior_upper_eff) / 2
  prior_sd_eff <- (prior_upper_eff - prior_lower_eff) / 3.29
  
  # Update with treatment effect data
  data_effect <- observed_data$treatment_effect
  data_se <- 0.09  # Standard error from TABLE 5
  
  # Bayesian update for normal distribution
  posterior_mu <- (prior_mu_eff/prior_sd_eff^2 + data_effect/data_se^2) / 
    (1/prior_sd_eff^2 + 1/data_se^2)
  posterior_sd <- sqrt(1 / (1/prior_sd_eff^2 + 1/data_se^2))
  
  posteriors$vba_effectiveness <- rnorm(n_sim, posterior_mu, posterior_sd)
  
  # Update 3: YIELD IMPACT OF RA
  yield_idx <- which(dist_df$distribution == "posnorm" & 
                       grepl("yield.*impact", dist_df$label, ignore.case = TRUE))[1]
  
  prior_lower_yield <- dist_df$lower[yield_idx]
  prior_upper_yield <- dist_df$upper[yield_idx]
  prior_mu_yield <- (prior_lower_yield + prior_upper_yield) / 2
  prior_sd_yield <- (prior_upper_yield - prior_lower_yield) / 3.29
  
  data_yield <- observed_data$yield_impact_per_practice * 5  # Scale to full adoption
  data_se_yield <- 0.03  # Approximate SE from TABLE 7
  
  posterior_mu_yield <- (prior_mu_yield/prior_sd_yield^2 + data_yield/data_se_yield^2) / 
    (1/prior_sd_yield^2 + 1/data_se_yield^2)
  posterior_sd_yield <- sqrt(1 / (1/prior_sd_yield^2 + 1/data_se_yield^2))
  
  posteriors$yield_impact_ra <- rnorm(n_sim, posterior_mu_yield, posterior_sd_yield)
  
  return(posteriors)
}
