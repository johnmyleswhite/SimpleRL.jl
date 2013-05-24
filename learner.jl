# Need access to softmax function, RNG's and MLE routines
using Stats
using Distributions
using Optim

# A 1-state TD learner for a bandit task with N actions
immutable TDLearner
	alpha::Float64 # Learning rate
	beta::Float64 # Softmax temperature
	v::Vector{Float64} # Value function for actions
	wv::Vector{Float64} # Values weighted by beta
	probs::Vector{Float64} # Choice probabilities from softmax
end

function TDLearner(alpha::Real, beta::Real, n_actions::Integer)
	v = Array(Float64, n_actions)
	wv = Array(Float64, n_actions)
	probs = Array(Float64, n_actions)
	learner = TDLearner(alpha, beta, v, wv, probs)
	initialize!(learner)
	return learner
end

function Base.show(io::IO, learner::TDLearner)
	@printf "TD Learner Object\n"
	@printf " * alpha: %f\n" learner.alpha
	@printf " * beta: %f\n" learner.beta
end

function initialize!(learner::TDLearner)
	fill!(learner.v, 0.0)
	fill!(learner.wv, 0.0)
	softmax!(learner.wv, learner.probs)
	return
end

function learn!(learner::TDLearner, action::Real, reward::Real)
	pe = reward - learner.v[action]
	learner.v[action] += learner.alpha * pe
	learner.wv[action] = learner.beta * learner.v[action]
	softmax!(learner.wv, learner.probs)
	return
end

choose(learner::TDLearner) = rand(Categorical(learner.probs))

function simulate!(learner::TDLearner,
	               environment::Vector{Bernoulli},
	               n_trials::Integer,
	               history::Matrix{Float64})
	initialize!(learner)
	n_actions = length(environment)
	for t in 1:n_trials
		action = choose(learner)
		reward = rand(environment[action])
		history[t, 1] = action
		history[t, 2] = reward
		learn!(learner, action, reward)
	end
	return
end

function loglikelihood(history::Matrix, alpha::Real, beta::Real)
	n_trials = size(history, 1)
	n_actions = int(max(history[:, 1]))
	learner = TDLearner(alpha, beta, n_actions)
	ll = 0.0
	for t in 1:n_trials
		action, reward = history[t, :]
		ll += log(learner.probs[action])
		learn!(learner, action, reward)
	end
	return ll
end

function fit(history::Matrix)
	n_actions = int(max(history[:, 1]))
	function nll(pseudoparameters::Vector{Float64})
		alpha = invlogit(pseudoparameters[1])
		beta = exp(pseudoparameters[2])
		return -loglikelihood(history, alpha, beta)
	end
	results = optimize(nll, [0.0, 0.0], method = :nelder_mead)
	# TODO: Approximate gradient is not precise enough to work
	# TODO: Test for stationary point and pos. def. Hessian at optimum?
	# results = optimize(nll, [0.0, 0.0], method = :l_bfgs)
	pseudoparameters_hat = results.minimum
	alpha_hat = invlogit(pseudoparameters_hat[1])
	beta_hat = exp(pseudoparameters_hat[2])
	return alpha_hat, beta_hat
end
