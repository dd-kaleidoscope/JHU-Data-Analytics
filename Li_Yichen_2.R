# homework 2

#Q1
GMAT = c(560,540,520,580,520,620,660,630,550,550,600,537)
GPA = c(3.20,3.44,3.70,3.10,3.00,4.00,3.38,3.83,2.67,2.75,2.33,3.75)

lm.1 = lm(GPA~GMAT)
summary(lm.1)
coef(lm.1)

predict(lm.1, data.frame(GMAT=540))

#Q2
GPA = c(4.0)
IQ = c(110)
Gender = c(1)
Salary = 50 + 20*GPA + 0.07*IQ + 35*Gender + 0.01*GPA*IQ - 10*GPA*Gender
Salary

#Q3
set.seed(1)
X = rnorm(100, mean = 0, sd = 1)
e = rnorm(100, mean = 0, sd = 0.1)
Y = -1 + 0.5*X + e
plot(Y~X)

lm.3 = lm(Y~X)
summary(lm.3)
confint(lm.3)

X_2 = X*X
lm.3.1 = lm(Y~X+X_2)
summary(lm.3.1)


