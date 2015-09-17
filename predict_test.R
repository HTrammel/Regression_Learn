data("faithful")
attach(faithful)

f_fit <- lm(eruptions~waiting)
nd <- data.frame(waiting=80)
print(predict(f_fit, nd, interval="predict"))