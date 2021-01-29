library(shiny)
library(ggplot2)

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
        plotOutput("Plot")
    ),
    
    server = function(input, output) {
        output$Plot = renderPlot({
            df_vector <- data.frame(x = c(input$u1, input$v1), y = c(input$u2, input$v2))
            df_polygon <- data.frame(x = c(0, input$u1, input$u1 + input$v1, input$v1), y = c(0, input$u2, input$u2 + input$v2, input$v2))
            ggplot() +
                geom_segment(aes(x = 0, xend = df_vector$x[1], y = 0, yend = df_vector$y[1]), arrow = arrow()) +
                geom_segment(aes(x = 0, xend = df_vector$x[2], y = 0, yend = df_vector$y[2]), arrow = arrow()) +
                geom_vline(xintercept = 0) + 
                geom_hline(yintercept = 0) +
                coord_cartesian(xlim = c(-8, 8), ylim = c(-8, 8)) + 
                geom_polygon(data = df_polygon, aes(x = x, y = y), 
                             fill = "grey", alpha = 0.5) +
                ggtitle("Area of parallelpiped defined by vectors")
        })
    },
    
    # options = list(height = 500)
)