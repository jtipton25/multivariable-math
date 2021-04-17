vector_angles <- function(n, B = 5000, sample_type = "integer") {
    
    if (!(sample_type %in% c("integer", "gaussian"))) 
        stop('sample_type must be either "integer" or "gaussian"')
    u_save <- matrix(0, B, n)
    v_save <- matrix(0, B, n)
    angles <- rep(0, B)
    for (i in 1:B) {
        if (sample_type == "integer") {
            u <- sample(-9:9, n, replace = TRUE)
            v <- sample(-9:9, n, replace = TRUE)
        } else if (sample_type == "gaussian") {
            u <- rnorm(n)
            v <- rnorm(n)
        }
        # not allowing u to be a 0 vector
        while(all(u == 0)) {
            if (sample_type == "integer") {
                u <- sample(-9:9, n, replace = TRUE)
            } else if (sample_type == "gaussian") {
                u <- rnorm(n)
            }
        }
        # not allowing v to be a 0 vector
        while(all(v == 0)) {
            if (sample_type == "integer") {
                v <- sample(-9:9, n, replace = TRUE)
            } else if (sample_type == "gaussian") {
                v <- rnorm(n)
            }
        }
        length_u <- drop(sqrt(t(u) %*% u))
        length_v <- drop(sqrt(t(v) %*% v))
        # prevent u and v from being scalar multiples
        while(all((u / length_u) == (v / length_v)) | 
              all((u / length_u) == -(v / length_v))) {
            if (sample_type == "integer") {
                u <- sample(-9:9, n, replace = TRUE)
            } else if (sample_type == "gaussian") {
                u <- rnorm(n)
            }
            while(all(u) == 0) {
                if (sample_type == "integer") {
                    u <- sample(-9:9, n, replace = TRUE)
                } else if (sample_type == "gaussian") {
                    u <- rnorm(n)
                }
            }
            length_u <- drop(sqrt(t(u) %*% u))
        }
        angles[i] <- acos((t(u) %*% v) / (length_u * length_v))
        u_save[i, ] <- u
        v_save[i, ] <- v
    }
    return(data.frame(angles = angles, u = u_save, v = v_save))
}


dat_integer <- rbind(vector_angles(2) %>% select(angles) %>% mutate(n = 2, type = "integer"),
                     rbind(vector_angles(10) %>% select(angles) %>% mutate(n = 10, type = "integer"), 
                           rbind(vector_angles(50) %>% select(angles) %>% mutate(n = 50, type = "integer"),
                                 vector_angles(500) %>% select(angles) %>% mutate(n = 500, type = "integer"))))
dat_gaussian <- rbind(vector_angles(2, sample_type = "gaussian") %>% select(angles) %>% mutate(n = 2, type = "gaussian"),
             rbind(vector_angles(10, sample_type = "gaussian") %>% select(angles) %>% mutate(n = 10, type = "gaussian"), 
                   rbind(vector_angles(50, sample_type = "gaussian") %>% select(angles) %>% mutate(n = 50, type = "gaussian"),
                         vector_angles(500, sample_type = "gaussian") %>% select(angles) %>% mutate(n = 500, type = "gaussian"))))


dat_integer %>%
    ggplot(aes(x = angles, fill = factor(n))) +
    geom_histogram(alpha = 0.5, position = "identity", bins = 100) +
    geom_vline(xintercept = pi / 2, color = "red") +
    ggtitle("As n goes to infinity, randomly distributed vectors tend towards being orthogonal")
dat_gaussian %>%
    ggplot(aes(x = angles, fill = factor(n))) +
    geom_histogram(alpha = 0.5, position = "identity", bins = 100) +
    geom_vline(xintercept = pi / 2, color = "red") +
    ggtitle("As n goes to infinity, randomly distributed vectors tend towards being orthogonal")

dat <- rbind(dat_integer, dat_gaussian)
dat %>%
    ggplot(aes(x = angles, fill = factor(n))) +
    geom_histogram(alpha = 0.5, position = "identity", bins = 100) +
    geom_vline(xintercept = pi / 2, color = "red") +
    ggtitle("As n goes to infinity, randomly distributed vectors tend towards being orthogonal") +
    facet_wrap(~ type, nrow = 2)

# vectors of length 50
out <- vector_angles(50)
hist(out$angles, xlim = c(0, 2*pi))


dat %>%
    group_by(n, type) %>%
    summarize(prop_orthogonal = mean(abs(angles - pi/2) < 0.05))
              