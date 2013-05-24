# TODO: Use 0-2.pdf for filenames
library("stringr")

for (delta in c(0.2, 0.4, 0.8))
{
	estimates <- read.csv("sampling_distribution.csv", header = FALSE)

	names(estimates) <- c("Delta", "Actions", "Trials",
		                  "Alpha", "Beta",
		                  "AlphaHat", "BetaHat")

	# There is a problem with rare, aberrant fits
	estimates <- subset(estimates, BetaHat < 2)

	estimates <- subset(estimates, Delta == delta)

	estimates$Actions <- factor(estimates$Actions)
	levels(estimates$Actions) <- c("2 Actions")

	estimates$Trials <- factor(estimates$Trials)
	levels(estimates$Trials) <- c("250 Trials", "500 Trials", "1,000 Trials", "2,000 Trials", "3,000 Trials")

	estimates$Alpha <- factor(estimates$Alpha)
	levels(estimates$Alpha) <- c("Alpha = 0.1", "Alpha = 0.5")

	estimates$Beta <- factor(estimates$Beta)
	levels(estimates$Beta) <- c("Beta = 0.8", "Beta = 1.0", "Beta = 1.2")

	ggplot(estimates, aes(x = AlphaHat, y = BetaHat, color = Beta)) +
	  geom_point() +
	  geom_smooth(method = "lm") +
	  facet_grid(Trials ~ Alpha) +
	  xlab("Estimated Alpha") +
	  ylab("Estimated Beta") +
	  ggtitle("Sampling Distribution of Alpha and Beta")
	ggsave(file.path("graphs", paste("sampling_distribution_", str_replace(delta, "\\.", "-"), ".pdf", sep = "")), height = 7, width = 9)

	ggplot(estimates, aes(x = AlphaHat, fill = factor(Alpha))) +
	  geom_density() +
	  facet_grid(Trials ~ Beta) +
	  xlab("Estimated Alpha") +
	  ylab("Estimated Density Function") +
	  ggtitle("Marginal Sampling Distribution of Alpha")
	ggsave(file.path(paste("marginal_alpha_", str_replace(delta, "\\.", "-"), ".pdf", sep = "")), height = 7, width = 9)

	ggplot(estimates, aes(x = BetaHat, fill = factor(Beta))) +
	  geom_density() +
	  facet_grid(Trials ~ Alpha) +
	  xlab("Estimated Beta") +
	  ylab("Estimated Density Function") +
	  ggtitle("Marginal Sampling Distribution of Beta")
	ggsave(file.path(paste("marginal_beta_", str_replace(delta, "\\.", "-"), ".pdf", sep = "")), height = 7, width = 9)
}

estimates <- read.csv("sampling_distribution.csv", header = FALSE)

names(estimates) <- c("Delta", "Actions", "Trials",
	                  "Alpha", "Beta",
	                  "AlphaHat", "BetaHat")

ddply(estimates,
	  c("Alpha", "Beta", "Trials"),
	  function (df) {coef(lm(BetaHat ~ AlphaHat, data = df))})

ddply(estimates,
	  c("Alpha", "Beta", "Trials"),
	  function (df) {with(df, cor.test(AlphaHat, BetaHat)$conf.int)})

ddply(estimates,
	  c("Alpha", "Beta", "Trials"),
	  function (df) {summary(df$AlphaHat)})

ddply(estimates,
	  c("Alpha", "Beta", "Trials"),
	  function (df) {summary(df$BetaHat)})

tmp <- ddply(estimates,
	  c("Alpha", "Beta", "Trials"),
	  function (df) {data.frame(Mu = mean(df$AlphaHat), Sigma = sd(df$AlphaHat))})
summary(lm(Sigma ~ Alpha + Beta + Trials - 1, data = tmp))

tmp <- ddply(estimates,
	  c("Alpha", "Beta", "Trials"),
	  function (df) {data.frame(Mu = mean(df$BetaHat), Sigma = sd(df$BetaHat))})
summary(lm(Sigma ~ Alpha + Beta + Trials - 1, data = tmp))

tmp <- transform(tmp, N = 1 / sqrt(Trials))
summary(lm(Sigma ~ N - 1, data = tmp))

ddply(estimates,
	  c("Alpha", "Beta", "Trials"),
	  function (df) {quantile(df$AlphaHat, c(0.025, 0.975))})

ddply(estimates,
	  c("Alpha", "Beta", "Trials"),
	  function (df) {quantile(df$BetaHat, c(0.025, 0.975))})
