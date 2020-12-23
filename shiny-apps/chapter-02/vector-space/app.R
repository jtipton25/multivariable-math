library(shiny)
library(ggplot2)

shinyApp(
    
    ui = fluidPage(
        sliderInput(
            inputId = "x", 
            label = "x:",
            min = -10, 
            max = 10, 
            value = 2, 
            step = 0.01
        ),
        sliderInput(
            inputId = "y", 
            label = "y:",
            min = -10, 
            max = 10, 
            value = 2, 
            step = 0.01
        ),
        plotOutput("Plot")
    ),
    
    server = function(input, output) {
        output$Plot = renderPlot({
            ggplot(data = data.frame(x = input$x, y = input$y), 
                   aes(x = x, y = y)) +
                geom_segment(aes(x = 0, xend = x, y = 0, yend = y), arrow = arrow()) +
                geom_segment(aes(x = 0, xend = x, y = 0, yend = 0), arrow = arrow(), color = "blue") +
                geom_segment(aes(x = 0, xend = 0, y = 0, yend = y), arrow = arrow(), color = "red") +
                xlim(c(-10, 10)) +
                ylim(c(-10, 10))
        })
    },
    
    # options = list(height = 500)
)