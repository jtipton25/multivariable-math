shinyUI(
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
    )
)
    
 