v <- rnorm(11,0,1)
v2 = cumsum(v)
x <- 1:11
reg <- lm(v2~x)
v3 = reg$coefficients[1]+reg$coefficients[2]*x
v3

