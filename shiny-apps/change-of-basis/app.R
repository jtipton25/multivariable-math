library(shiny)
library(ggplot2)
library(dasc2594)
library(tidyverse)
library(latex2exp)

shinyApp(
    # First Basis Vector
    ui = fluidPage(
        column(2, 
               sidebarLayout(
                   sidebarPanel(width = 14, id = "sidebar", h5("First Basis Vector"),
                                sliderInput(
                                    inputId = "b11", 
                                    label = withMathJax("\\(b_{11}\\):"),
                                    min = -4, 
                                    max = 4, 
                                    value = 1, 
                                    step = 0.01
                                ),
                                sliderInput(
                                    inputId = "b12", 
                                    label = withMathJax("\\(b_{12}\\):"),
                                    min = -4, 
                                    max = 4, 
                                    value = 1, 
                                    step = 0.01
                                )),
                   mainPanel(width = 0))),
        
        column(2, 
               sidebarLayout(
                   sidebarPanel(width = 14, id = "sidebar", h5("Second Basis Vector"),
                                sliderInput(
                                    inputId = "b21", 
                                    label = withMathJax("\\(b_{21}\\):"),
                                    min = -4, 
                                    max = 4, 
                                    value = 1, 
                                    step = 0.01
                                ),
                                sliderInput(
                                    inputId = "b22", 
                                    label = withMathJax("\\(b_{22}\\):"),
                                    min = -4, 
                                    max = 4, 
                                    value = -1, 
                                    step = 0.01
                                )),
                   mainPanel(width = 0))),
        column(2, 
               sidebarLayout(
                   sidebarPanel(width = 14, id = "sidebar", h5("Coordinates of x with respect to standard basis"),
                                sliderInput(
                                    inputId = "x1", 
                                    label = "x1:",
                                    min = -4, 
                                    max = 4, 
                                    value = 2, 
                                    step = 0.01
                                ),
                                sliderInput(
                                    inputId = "x2", 
                                    label = "x2:",
                                    min = -4, 
                                    max = 4, 
                                    value = 2, 
                                    step = 0.01
                                )), 
                   mainPanel(width = 0))),
        plotOutput("Plot")
    ),
    
    server = function(input, output) {
        
        
        output$Plot = renderPlot({
            transformation_matrix <- tribble(~ x, ~ y,
                                             input$b11, input$b21,
                                             input$b12, input$b22) %>% 
                as.matrix()
            
            grid_start <- construct_grid(xintercepts = -10:10, yintercepts = -10:10) %>% 
                mutate(id = row_number())
            
            grid_trans <- grid_start %>% 
                # need to `transform_df_coords()` twice as each segment is made up of 2 points
                transform_df_coords(x, y, m = transformation_matrix) %>% 
                transform_df_coords(xend, yend, m = transformation_matrix)
            
            grid_all <- bind_rows(
                mutate(grid_start, time = 1),
                mutate(grid_trans, time = 2)
            )
            
            basis_start <- tibble(
                x = c(0, 0),
                y = c(0, 0),
                xend = c(1, 0),
                yend = c(0, 1),
                # `vec` is unnecessary, will just use to differentiate colors
                vec = c("i", "j")
            ) %>% 
                mutate(id = nrow(grid_start) + row_number())
            
            basis_trans <- basis_start %>% 
                transform_df_coords(x, y, m = transformation_matrix) %>% 
                transform_df_coords(xend, yend, m = transformation_matrix)
            
            basis_all <- bind_rows(
                mutate(basis_start, time = 1),
                mutate(basis_trans, time = 2)
            )
            
            x_breaks <- unique(grid_start$x)
            y_breaks <- unique(grid_start$y)
            
            lab <- c("$\\mathbf{b}_1$", "$\\mathbf{b}_2$", "$\\mathbf{x}$")
            
            ggplot(aes(x = x, y = y, group = id), data = filter(grid_all, time == 2))+
                geom_segment(aes(xend = xend, yend = yend)) +
                geom_segment(aes(xend = xend, yend = yend, color = vec), data = filter(basis_all, time == 2), arrow = arrow(length = unit(0.1, "inches")), size = 1.5) +
                # geom_segment(aes(x = 0, y = 0, xend = input$x1, yend = input$x2), arrow = arrow(length = unit(0.1, "inches")), size = 1.5, color = "dark orange", inherit.aes = FALSE) +
                geom_point(aes(x = input$x1, y = input$x2), size = 2, color = "dark orange") +
                annotate('text', x = c(input$b11, input$b21, input$x1), y = c(input$b12, input$b22, input$x2), 
                         label = lapply(lab, function(x) TeX(x, output = 'character')), parse = TRUE, 
                         color = c("blue", "red", "dark orange"), size = 5, hjust = 1, vjust = 0) +
                # geom_text(data = data.frame(x = c(input$b11, input$b21, input$x1), y = c(input$b12, input$b22, input$x2), text = TeX(c("$b_1$", "b_2", "w"))),
                #           aes(x = x + 0.25, y = y + 0.25, label = text), size = 5, inherit.aes = FALSE,
                #           color = c("blue", "red", "dark orange")) +
                # geom_text(data = data.frame(x = -3, y = 3, text = "w"),
                #           aes(x = x, y = y, label = text), inherit.aes = FALSE) +
                scale_color_manual(values = c("i" = "blue", "j" = "red")) +
                scale_x_continuous(breaks = x_breaks, minor_breaks = NULL) +
                scale_y_continuous(breaks = y_breaks, minor_breaks = NULL) +
                coord_cartesian(xlim = c(-4, 4), ylim = c(-4, 4)) + 
                ggtitle(TeX("Linear combinations of $\\mathbf{b}_1$ and $\\mathbf{b}_2$ and a vector $\\mathbf{x}$")) +
                theme_bw(base_size = 22) +
                theme(legend.position = "none") 
            
        })
    },
    
    # options = list(height = 500)
)