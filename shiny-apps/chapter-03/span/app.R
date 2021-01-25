library(shiny)
library(ggplot2)
library(dasc2594)

shinyApp(
    
    ui = fluidPage(
        sliderInput(
            inputId = "u1", 
            label = "u1:",
            min = -4, 
            max = 4, 
            value = 2, 
            step = 0.01
        ),
        sliderInput(
            inputId = "u2", 
            label = "u2:",
            min = -4, 
            max = 4, 
            value = 2, 
            step = 0.01
        ),
        sliderInput(
            inputId = "v1", 
            label = "v1:",
            min = -4, 
            max = 4, 
            value = 2, 
            step = 0.01
        ),
        sliderInput(
            inputId = "v2", 
            label = "v2:",
            min = -4, 
            max = 4, 
            value = 2, 
            step = 0.01
        ),
        sliderInput(
            inputId = "w1", 
            label = "w1:",
            min = -4, 
            max = 4, 
            value = 2, 
            step = 0.01
        ),
        sliderInput(
            inputId = "w2", 
            label = "w2:",
            min = -4, 
            max = 4, 
            value = 2, 
            step = 0.01
        ),
        plotOutput("Plot")
    ),
    
    server = function(input, output) {

        
        output$Plot = renderPlot({
            transformation_matrix <- tribble(~ x, ~ y,
                                             input$u1, input$v1,
                                             input$u2, input$v2) %>% 
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
            
            ggplot(aes(x = x, y = y, group = id), data = filter(grid_all, time == 2))+
                geom_segment(aes(xend = xend, yend = yend)) +
                geom_segment(aes(xend = xend, yend = yend, color = vec), data = filter(basis_all, time == 2), arrow = arrow(length = unit(0.1, "inches")), size = 1.5) +
                # geom_segment(aes(x = 0, y = 0, xend = input$w1, yend = input$w2), arrow = arrow(length = unit(0.1, "inches")), size = 1.5, color = "dark orange", inherit.aes = FALSE) +
                geom_point(aes(x = input$w1, y = input$w2), size = 2, color = "dark orange") +
                geom_text(data = data.frame(x = c(input$u1, input$v1, input$w1), y = c(input$u2, input$v2, input$w2), text = c("u", "v", "w")),
                          aes(x = x + 0.25, y = y + 0.25, label = text), size = 5, inherit.aes = FALSE,
                          color = c("blue", "red", "dark orange")) +
                # geom_text(data = data.frame(x = -3, y = 3, text = "w"),
                #           aes(x = x, y = y, label = text), inherit.aes = FALSE) +
                scale_color_manual(values = c("i" = "blue", "j" = "red")) +
                scale_x_continuous(breaks = x_breaks, minor_breaks = NULL) +
                scale_y_continuous(breaks = y_breaks, minor_breaks = NULL) +
                coord_cartesian(xlim = c(-4, 4), ylim = c(-4, 4)) + 
                theme(legend.position = "none") +
                ggtitle("Linear combinations of u and v and a point w") +
                theme_bw(base_size = 22)
        })
    },
    
    # options = list(height = 500)
)