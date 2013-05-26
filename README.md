SimpleRl.jl
===========

A bare-bones package for simulating and fitting RL models
to data from bandit tasks.

# Installation

Clone this repo and place it into your `~/.julia` directory.

# Usage

We simulate a TD-learner in a 3-bandit task with Bernoulli arms:

	using Distributions
	using SimpleRL

	alpha = 0.1
	beta = 1.0

	n_actions = 3
	n_trials = 1_000

	learner = TDLearner(alpha, beta, n_actions)

	environment = [Bernoulli(0.1), Bernoulli(0.2), Bernoulli(0.3)]

	history = Array(Float64, n_trials, 2)

	simulate!(learner, environment, n_trials, history)

	loglikelihood(history, alpha, beta)

	alpha_hat, beta_hat = fit(TDLearner, history)