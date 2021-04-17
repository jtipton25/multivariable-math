library(tidyverse)
library(plotly)

plot_tangent <- function(target_fun, grad_fun, a = 3, b = 2, n = 50, xlim = c(-4, 4), ylim = c(-4, 4)) {

    # tangent plane
    tangent_plane <- function(x, y, a, b, target_fun, grad_fun) {
        # grid points x and y to evaluate the plane
        # the point (a, b) at which to calculate the tangent function
        # target_fun is the function of interest
        # grad_fun is the gradient of the target function
        
        grad <- grad_fun(a, b)
        target_fun(a, b) + grad[1] * (x - a) + grad[2] * (y - b)
    }
    
    # generate a grid over which to plot
    x <- seq(xlim[1], xlim[2], length = n)
    y <- seq(ylim[1], ylim[2], length = n)
    dat <- expand_grid(x, y)
    

    dat <- dat %>% 
        # apply the function to the grid
        mutate(z = target_fun(x, y)) %>%
        # calculate the tangent plane
        mutate(z2 = tangent_plane(x, y, a, b, target = target_fun, grad = grad_fun))
    
    
    plot_ly(x = x, y = y, z = matrix(dat$z, n, n)) %>%
        add_surface(
            contours = list(
                z = list(
                    show=TRUE,
                    usecolormap=TRUE,
                    highlightcolor="#ff0000",
                    project=list(z=TRUE)
                )
            ),
            colorbar = list(title = "Function"), showscale = FALSE
        ) %>%
        add_surface(x = x, y = y, z = matrix(dat$z2, n, n), 
                    colorbar = list(title = "Tangent plane"), showscale = FALSE) %>%
        add_trace(x = a, y = b, z = target_fun(a, b),
                  mode = "markers", type = "scatter3d", 
                  marker = list(size = 5, color = "red", symbol = 104),
                  name = paste0("(a=", a, ", b=", b, ")"))
    
}

target_fun <- function(x, y) {
    return(x^2 + y^2)
}
grad_fun <- function(x, y) {
    c(2 * x, 2 * y)
}

plot_tangent(target_fun = target_fun, grad_fun = grad_fun, a=-1, b = 1)
