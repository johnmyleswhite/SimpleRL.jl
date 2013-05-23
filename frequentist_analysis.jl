include("learner.jl")

# Estimate the sampling distribution of alpha and beta
function sampling_distribution(alpha::Real,
	                           beta::Real,
	                           environment::Vector{Bernoulli},
	                           n_trials::Integer,
	                           n_sims::Integer)
	n_parameters = 2

	n_actions = length(environment)

	estimates = Array(Float64, n_sims, n_parameters)

	learner = TDLearner(alpha, beta, n_actions)

	# TODO: Make this loop faster by reusing history across iterations
	for i in 1:n_sims
		history = simulate!(learner, environment, n_trials)
		alpha_hat, beta_hat = fit(history)
		estimates[i, 1] = alpha_hat
		estimates[i, 2] = beta_hat
	end

	return estimates
end

environment = [Bernoulli(0.1), Bernoulli(0.1), Bernoulli(0.9)]
total_time = @elapsed estimates = sampling_distribution(0.1,
  	                                                    1.0,
  	                                                    environment,
  	                                                    1_000,
  	                                                    10_000)
@printf "Total Time for Simulation Study: %f\n" total_time

writecsv("sampling_distribution.csv", estimates)
