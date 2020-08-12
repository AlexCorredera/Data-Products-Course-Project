library(ggplot2)
library(shiny)

state_data <- state.x77
state_data <- as.data.frame(state_data)
colnames(state_data) <- make.names(colnames(state_data))

state_data$HS.Grad.Binom <- 1:50
for(i in 1:50) {if(state_data$HS.Grad[i] > 50) {
    state_data$HS.Grad.Binom[i] = ">50%"
} else {
    state_data$HS.Grad.Binom[i] = "<50%"
}}
state_data$HS.Grad.Binom <- factor(state_data$HS.Grad.Binom)

shinyServer(function(input, output) {
    model1 <- reactive({
        if(input$var1 && !input$var2 && !input$var3){
            lm(Life.Exp ~ Murder, data = state_data)
        } else if(input$var1 && input$var2 && !input$var3){
            lm(Life.Exp ~ Murder + HS.Grad, data = state_data)
        } else if(input$var1 && input$var2 && input$var3){
            lm(Life.Exp ~ Murder + HS.Grad + Frost, data = state_data)
        } else if(!input$var1 && input$var2 && !input$var3){
            lm(Life.Exp ~ HS.Grad, data = state_data)
        } else if(!input$var1 && input$var2 && input$var3) {
            lm(Life.Exp ~ HS.Grad + Frost, data = state_data)
        } else if(!input$var1 && !input$var2 && input$var3) {
            lm(Life.Exp ~ Frost, data = state_data)
        } else if(input$var1 && !input$var2 && input$var3) {
            lm(Life.Exp ~ Murder + Frost, data = state_data)
        } else {
        }
            })
    
    model1pred <- reactive({
        model1 <- 
            if(input$var1 && !input$var2 && !input$var3){
                lm(Life.Exp ~ Murder, data = state_data)
            } else if(input$var1 && input$var2 && !input$var3){
                lm(Life.Exp ~ Murder + HS.Grad, data = state_data)
            } else if(input$var1 && input$var2 && input$var3){
                lm(Life.Exp ~ Murder + HS.Grad + Frost, data = state_data)
            } else if(!input$var1 && input$var2 && !input$var3){
                lm(Life.Exp ~ HS.Grad, data = state_data)
            } else if(!input$var1 && input$var2 && input$var3) {
                lm(Life.Exp ~ HS.Grad + Frost, data = state_data)
            } else if(!input$var1 && !input$var2 && input$var3) {
                lm(Life.Exp ~ Frost, data = state_data)
            } else if(input$var1 && !input$var2 && input$var3) {
                lm(Life.Exp ~ Murder + Frost, data = state_data)
            } else {
            }   
        mrdrInput <- input$sliderMRDR
        hsInput <- input$sliderHS
        frostInput <- input$sliderFROST
        predict(model1, newdata = data.frame(Population = mean(state_data$Population), Income = mean(state_data$Income),
        Illiteracy = mean(state_data$Illiteracy), Murder = mrdrInput, HS.Grad = hsInput, Frost = frostInput, Area = mean(state_data$Area)))
    })
    
    plot1 <- reactive({
        mrdrInput <- input$sliderMRDR
        hsInput <- input$sliderHS
        frostInput <- input$sliderFROST
        hsBinomInput <- if(hsInput > 50) {">50%"}else {"<50%"}
        ggplot(state_data, aes(Murder, Life.Exp, color = HS.Grad.Binom, size = Frost)) + geom_point() +
           geom_point(aes(mrdrInput, model1pred(), color = "Predicted Value", size = frostInput))
    })
    
    
    output$plot1 <- renderPlot({
        par(mfrow = c(1, 2))    
        plot1()
    })
        
    output$pred1 <- renderText({
        if(input$var1 | input$var2 | input$var3) {
            model1pred() 
        }
        
    })
    
    pv1 <- reactive({
        model1 <- 
            if(input$var1 && !input$var2 && !input$var3){
                lm(Life.Exp ~ Murder, data = state_data)
            } else if(input$var1 && input$var2 && !input$var3){
                lm(Life.Exp ~ Murder + HS.Grad, data = state_data)
            } else if(input$var1 && input$var2 && input$var3){
                lm(Life.Exp ~ Murder + HS.Grad + Frost, data = state_data)
            } else if(!input$var1 && input$var2 && !input$var3){
                lm(Life.Exp ~ HS.Grad, data = state_data)
            } else if(!input$var1 && input$var2 && input$var3) {
                lm(Life.Exp ~ HS.Grad + Frost, data = state_data)
            } else if(!input$var1 && !input$var2 && input$var3) {
                lm(Life.Exp ~ Frost, data = state_data)
            } else if(input$var1 && !input$var2 && input$var3) {
                lm(Life.Exp ~ Murder + Frost, data = state_data)
            } else {
            } 
        if(input$var1) {
            round(summary(model1)$coefficients[2, 4], 5)
        } else {"NA"}
    })
    
    pv2 <- reactive({
        model1 <- 
            if(input$var1 && !input$var2 && !input$var3){
                lm(Life.Exp ~ Murder, data = state_data)
            } else if(input$var1 && input$var2 && !input$var3){
                lm(Life.Exp ~ Murder + HS.Grad, data = state_data)
            } else if(input$var1 && input$var2 && input$var3){
                lm(Life.Exp ~ Murder + HS.Grad + Frost, data = state_data)
            } else if(!input$var1 && input$var2 && !input$var3){
                lm(Life.Exp ~ HS.Grad, data = state_data)
            } else if(!input$var1 && input$var2 && input$var3) {
                lm(Life.Exp ~ HS.Grad + Frost, data = state_data)
            } else if(!input$var1 && !input$var2 && input$var3) {
                lm(Life.Exp ~ Frost, data = state_data)
            } else if(input$var1 && !input$var2 && input$var3) {
                lm(Life.Exp ~ Murder + Frost, data = state_data)
            } else {
            } 
        if(input$var1 && input$var2) {
            round(summary(model1)$coefficients[3, 4], 5)
        } else if(!input$var1 && input$var2) {
            round(summary(model1)$coefficients[2, 4], 5)
        } else {"NA"}
    })
    
    pv3 <- reactive({
        model1 <- 
            if(input$var1 && !input$var2 && !input$var3){
                lm(Life.Exp ~ Murder, data = state_data)
            } else if(input$var1 && input$var2 && !input$var3){
                lm(Life.Exp ~ Murder + HS.Grad, data = state_data)
            } else if(input$var1 && input$var2 && input$var3){
                lm(Life.Exp ~ Murder + HS.Grad + Frost, data = state_data)
            } else if(!input$var1 && input$var2 && !input$var3){
                lm(Life.Exp ~ HS.Grad, data = state_data)
            } else if(!input$var1 && input$var2 && input$var3) {
                lm(Life.Exp ~ HS.Grad + Frost, data = state_data)
            } else if(!input$var1 && !input$var2 && input$var3) {
                lm(Life.Exp ~ Frost, data = state_data)
            } else if(input$var1 && !input$var2 && input$var3) {
                lm(Life.Exp ~ Murder + Frost, data = state_data)
            } else {
            } 
        if(input$var1 && input$var2 && input$var3) {
            round(summary(model1)$coefficients[4, 4], 5)
        } else if(!input$var1 && input$var2 && input$var3) {
            round(summary(model1)$coefficients[3, 4], 5)
        } else if(input$var1 && !input$var2 && input$var3) {
            round(summary(model1)$coefficients[3, 4], 5)
        } else if(!input$var1 && !input$var2 && input$var3) {
            round(summary(model1)$coefficients[2, 4], 5)
        } else {"NA"}
    })
    
    output$pv1 <- renderText({
        paste("Murder: ", pv1())
        
    })
    
    output$pv2 <- renderText({
        paste("HS Grad: ", pv2())
    })
    
    output$pv3 <- renderText({
        paste("Frost: ", pv3())
    })
    
})


