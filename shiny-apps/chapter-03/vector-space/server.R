shinyServer(function(input, output) {
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