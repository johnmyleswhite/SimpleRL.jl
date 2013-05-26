using SimpleRL

function sampling_distribution(delta::Real,
	                           alpha::Real,
	                           beta::Real,
	                           environment::Vector{Bernoulli},
	                           n_trials::Integer,
	                           n_sims::Integer)
	n_actions = length(environment)
	learner = TDLearner(alpha, beta, n_actions)
	history = Array(Float64, n_trials, 2)

	for i in 1:n_sims
		simulate!(learner, environment, n_trials, history)
		alpha_hat, beta_hat = fit(TDLearner, history)
		@printf "%f,%d,%d,%f,%f,%f,%f\n" delta n_actions n_trials alpha beta alpha_hat beta_hat
	end

	return
end

function run_sims(n_sims::Integer)
	for n_actions in [2]
		for delta in [0.2, 0.4, 0.8]
			environment = Array(Bernoulli, n_actions)

			for index in 1:n_actions
				environment[index] = Bernoulli(0.1 + (index - 1) * delta)
			end

			for alpha in [0.1, 0.5]
				for beta in [0.8, 1.0, 1.2]
					for n_trials in [250, 500, 1_000, 2_000, 3_000]
						sampling_distribution(delta,
							                  alpha,
							                  beta,
							                  environment,
						  	                  n_trials,
						  	                  n_sims)
					end
				end
			end
		end
	end

	return
end

run_sims(2000)
