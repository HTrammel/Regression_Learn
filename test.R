data(mtcars)

w <- mtcars$wt
m <- mtcars$mpg

c_fit <- lm(m~w)
nc <- data.frame(w=mean(mtcars$wt))
nm <- predict(c_fit, nc, interval="c")
print(nm)