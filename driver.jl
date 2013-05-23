include("learner.jl")

alpha = 0.1
beta = 1.0

n_actions = 3
n_trials = 1_000

learner = TDLearner(alpha, beta, n_actions)

environment = [Bernoulli(0.1), Bernoulli(0.1), Bernoulli(0.3)]

history = simulate!(learner, environment, n_trials)

loglikelihood(history, alpha, beta)

alpha_hat, beta_hat = fit(history)
