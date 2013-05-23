estimates <- read.csv("sampling_distribution.csv", header = FALSE)

names(estimates) <- c("Alpha", "Beta")

ggplot(estimates, aes(x = Alpha, y = Beta)) +
  geom_point(alpha = 0.25)

ggplot(estimates, aes(x = Alpha, fill = 1)) +
  geom_density() +
  theme(legend.position = "none") +
  geom_vline(xintercept = 0.1)

ggplot(estimates, aes(x = Beta, fill = 1)) +
  geom_density() +
  theme(legend.position = "none") +
  geom_vline(xintercept = 1.0)

ggplot(estimates, aes(x = Alpha, y = Beta)) +
  geom_point() +
  geom_smooth(method = "lm")

summary(lm(Beta ~ Alpha, data = estimates))

with(estimates, cor.test(Alpha, Beta))

ggplot(estimates, aes(x = rank(Alpha), y = rank(Beta))) +
  geom_point()

summary(estimates$Alpha)
var(estimates$Alpha)
summary(estimates$Beta)
var(estimates$Beta)

quantile(estimates$Alpha, c(0.025, 0.975))
quantile(estimates$Beta, c(0.025, 0.975))
