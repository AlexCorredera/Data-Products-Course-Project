library(shiny)

shinyUI(fluidPage(
    titlePanel("Predict Life Expectancy (1975 US state-level data)"),
    sidebarLayout(
        sidebarPanel(
            h5("Select the values of the variables included in the linear model:"),
            sliderInput("sliderMRDR", "What is the murder rate?", 1, 20, value = 7),
            sliderInput("sliderHS", "What is the percent of high-school graduates?", 1, 100, value = 50),
            sliderInput("sliderFROST", "What is the number of frost days?", 1, 356, value = 356),
            h5("Select the variables to be included in the linear model:"),
            checkboxInput("var1", "Murder Rate", value = TRUE),
            checkboxInput("var2", "Percent High-School Graduates", value = TRUE),
            checkboxInput("var3", "Mean Frost Days", value = TRUE),
            
    ),
        mainPanel(
            plotOutput("plot1"),
            h4("Predicted Life Expectancy from Model:"),
            textOutput("pred1"),
            h4("Model Variable P-Values"),
            textOutput("pv1"),
            textOutput("pv2"),
            textOutput("pv3")
    )
)))
