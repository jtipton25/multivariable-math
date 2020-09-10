n <- c(
    5, 10, 20, 50, 100, 200, 300, 400, 500, 750, 1000, 
    1250, 1500, 1750, 2000, 2250, 2500, 2750, 3000
)

times <- rep(0, length(n))
for (i in 1:length(n)) {
    message("On sample size ", n[i])
    
    X        <- rWishart(1, n[i] + 1, diag(n[i]))
    times[i] <- system.time(solve(X[, , 1]))[3]
}

plot(n, times, type = 'l', ylab = "Time in seconds", xlab = "Size of matrix")

