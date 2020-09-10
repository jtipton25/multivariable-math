library(tidyverse)
library(viridis)
n <- c(10, 20, 30, 40, 50, 75, 100, 150, 200, 250)
Sigma <- rWishart(1, n + 1, diag(n[1]))[, , 1]
## test if the Cholesky to inverse is the same as the inverse
all.equal(chol2inv(chol(Sigma)), solve(Sigma))


chol2_inverse <- function(Sigma) {
    return(chol2inv(chol(Sigma)))
}


make_chol_inverse <- function(n) {
    Sigma <- rWishart(1, n + 1, diag(n))[, , 1]
    return(chol2_inverse(Sigma))
}
make_chol <- function(n) {
    Sigma <- rWishart(1, n + 1, diag(n))[, , 1]
    return(chol(Sigma))
}
make_inverse <- function(n) {
    Sigma <- rWishart(1, n + 1, diag(n))[, , 1]
    return(solve(Sigma))
}

timings <- matrix(NA, 3, length(n))
rownames(timings) <- c("cholesky to inverse", "cholesky", "inverse")
colnames(timings) <- n

for (i in 1:length(n)) {
    message("On sample size ", n[i])
    timings[, i] <- summary(
        microbenchmark::microbenchmark(
            make_chol_inverse(n[i]),
            make_chol(n[i]),
            make_inverse(n[i]),
            unit = "s"
        )
    )[, 4]
}

timings
# kableExtra::kable(timings)

matplot(t(timings), type = 'l')

dat <- as.data.frame.table(timings)
dat %>%
    rename(method = Var1, n = Var2, s = Freq) %>%
    
ggplot(aes(x = n, y = s, group = method, color = method)) +
    geom_line() +
    scale_color_viridis_d(end = 0.75) + ## avoid the yellow color at value 1
    theme_bw() +
    xlab("Matrix size") +
    ylab("Average time in seconds")
