# bayesian_update.R
#' Perform Bayesian update for expert-elicited parameters
#' 
#' @param expert_prior Dataframe with expert priors (from estimate_read_csv)
#' @param observed_data List of observed data for parameters
#' @param n_sim Number of posterior samples to generate
#' @return List of posterior samples for each parameter
perform_bayesian_update <- function(expert_prior, observed_data, n_sim = 10000) {
  posteriors <- list()
  
  for (i in 1:nrow(expert_prior)) {
    variable <- expert_prior$variable[i]
    distribution <- expert_prior$distribution[i]
    
    # Skip if no observed data for this variable or it's a constant
    if (!variable %in% names(observed_data) || 
        distribution == "const" || 
        is.null(observed_data[[variable]])) {
      next
    }
    
    # Get expert prior parameters
    lower <- expert_prior$lower[i]
    upper <- expert_prior$upper[i]
    median_val <- ifelse(is.na(expert_prior$median[i]), 
                         (lower + upper) / 2, 
                         expert_prior$median[i])
    
    # Get observed data
    obs_data <- observed_data[[variable]]
    
    # Perform Bayesian update based on distribution type
    if (distribution == "posnorm" || distribution == "norm") {
      # Normal distribution update
      prior_mu <- median_val
      prior_sigma <- (upper - lower) / (2 * 1.645)  # Convert 90% CI to SD
      
      # For normal-normal conjugate prior
      n_obs <- length(obs_data)
      data_mean <- mean(obs_data)
      data_sd <- sd(obs_data)
      
      # Handle case where we have only one observation
      if (n_obs == 1) {
        posterior_mu <- (prior_mu/prior_sigma^2 + obs_data/data_sd^2) / 
          (1/prior_sigma^2 + 1/data_sd^2)
        posterior_sigma <- sqrt(1 / (1/prior_sigma^2 + 1/data_sd^2))
      } else {
        posterior_mu <- (prior_mu/prior_sigma^2 + data_mean * n_obs/data_sd^2) / 
          (1/prior_sigma^2 + n_obs/data_sd^2)
        posterior_sigma <- sqrt(1 / (1/prior_sigma^2 + n_obs/data_sd^2))
      }
      
      # Generate posterior samples
      posteriors[[variable]] <- rnorm(n_sim, posterior_mu, posterior_sigma)
      
    } else if (distribution == "tnorm_0_1") {
      # Truncated normal between 0 and 1 (for probabilities)
      prior_mu <- median_val
      prior_sigma <- (upper - lower) / (2 * 1.645)
      
      # Simple Bayesian update for proportions
      n_obs <- length(obs_data)
      successes <- sum(obs_data)  # Assuming binary data for probabilities
      
      # Use Beta distribution as conjugate prior for binomial likelihood
      # Convert normal prior to approximate Beta prior
      alpha_prior <- prior_mu * (prior_mu * (1 - prior_mu) / prior_sigma^2 - 1)
      beta_prior <- (1 - prior_mu) * (prior_mu * (1 - prior_mu) / prior_sigma^2 - 1)
      
      # Ensure parameters are valid
      alpha_prior <- max(alpha_prior, 0.1)
      beta_prior <- max(beta_prior, 0.1)
      
      # Posterior parameters
      alpha_post <- alpha_prior + successes
      beta_post <- beta_prior + n_obs - successes
      
      # Generate posterior samples
      posteriors[[variable]] <- rbeta(n_sim, alpha_post, beta_post)
    }
  }
  
  return(posteriors)
}