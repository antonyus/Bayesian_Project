# ============================================================================
# MODEL QUESTION 6
# ============================================================================




# Model structure
model_structure <- " model {
	for (i in 1:N_region) { # number of regions
	for (j in 1:N_year) { # number of year cohorts
	Y[i, j] ~ dbin(pi[i, j], N[i,j]) # likelihood for a given year j and region i
	logit(pi[i,j])<- beta0[i] + beta1[i] * BirthYear[j] # regression function
	}

    # Non-informative priors for intercept and slope
    beta0[i] ~ dnorm(0, 0.001)
    beta1[i] ~ dnorm(0, 0.001)
  }

}"

writeLines(model_structure, "logistic_model_Q.6.bug")

# Prepare the matrix for Y (vaccinated) and N (sample size) 
# by region and year of birth

Y <- matrix(vacc_data$Vaccinated,
            nrow=length(unique(vacc_data$Geography)), byrow=TRUE)

N <- matrix(vacc_data$Sample.Size,
            nrow=length(unique(vacc_data$Geography)), byrow=TRUE)


# Define the rows as regions and the columns as years
row.names(Y) <- unique(vacc_data$Geography)
colnames(Y) <- min(vacc_data$Birth.Year):max(vacc_data$Birth.Year)

row.names(N) <- unique(vacc_data$Geography)
colnames(N) <- min(vacc_data$Birth.Year):max(vacc_data$Birth.Year)

bugs_data <- list(
  Y = Y,
  N = N,
  BirthYear = vacc_data$Birth.Year,
  N_region = length(unique(vacc_data$Geography)),
  N_year = length(unique(vacc_data$Birth.Year))
)

# Parameters to monitor
params <- c("beta0", "beta1")

# Run the model
fit <- jags(
  data = bugs_data,
  parameters.to.save = params,
  model.file = "logistic_model.bug",
  n.chains = 3,
  n.iter = 5000,
  n.burnin = 1000,
  n.thin = 2
)
