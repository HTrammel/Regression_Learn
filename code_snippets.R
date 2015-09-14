# Code snippets
# 
# slope calculations

data("mtcars")

x <- mtcars$wt
y <- mtcars$mpg
n <- length(x)

beta1 <- cor(y, x) * sd(y) / sd(x)  # slope
beta0 <- mean(y) - beta1 * mean(x)  # intercept
# residuals
e <- y - beta0 - beta1 * x
# estimate of sigma
sigma <- sqrt(sum(e^2) / (n-2)) 
# sums of squares of x's
ssx <- sum((x - mean(x))^2)
# standard error for slope at intercept
seBeta0 <- (1 / n + mean(x) ^ 2 / ssx) ^ .5 * sigma 
# standard error for around regression line
seBeta1 <- sigma / sqrt(ssx)
# t-statistics
tBeta0 <- beta0 / seBeta0
tBeta1 <- beta1 / seBeta1
# p-values 
pBeta0 <- 2 * pt(abs(tBeta0), df = n - 2, lower.tail = FALSE)
pBeta1 <- 2 * pt(abs(tBeta1), df = n - 2, lower.tail = FALSE)


coefTable <- rbind(c(beta0, seBeta0, tBeta0, pBeta0), c(beta1, seBeta1, tBeta1, pBeta1))
colnames(coefTable) <- c("Estimate", "Std. Error", "t value", "P(>|t|)")
rownames(coefTable) <- c("(Intercept)", "x")

fit <- lm(y~x)
summary(fit)$coefficients

sumCoef <- summary(fit)$coefficients
sumCoef[1,1] + c(-1, 1) * qt(.975, df = fit$df) * sumCoef[1, 2]
sumCoef[2,1] + c(-1, 1) * qt(.975, df = fit$df) * sumCoef[2, 2]
