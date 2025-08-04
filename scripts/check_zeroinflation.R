##Check for zero inflation

# Get predicted probabilities from the model
pred_probs <- predict(f1, type = "response")

# Calculate expected zeros (i.e., expected probability of 0 per observation)
# Since binomial: P(y = 0) = 1 - predicted probability
exp_zeros <- mean(1 - pred_probs)

# Calculate observed proportion of zeros in the actual data
obs_zeros <- mean(frugivory_data1$frugivory == 0)

# Print results
cat("Observed zeros:", obs_zeros,
    "\nExpected zeros (from model):", exp_zeros)

sim_res <- simulateResiduals(f1)
plot(sim_res)

f1test <- glmmTMB(frugivory ~ treatment, family = nbinom2, ziformula = ~1, data = frugivory_data1, na.action = na.fail)
summary(f1test)

# GLMMTMB model
res.f1test <- simulateResiduals(m1test, plot = TRUE)
testDispersion(res.f1test)
