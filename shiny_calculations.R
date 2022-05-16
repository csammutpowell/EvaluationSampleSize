library(plotly)
library(dplyr)
library(shiny)
library(colorspace)
library(survival)
library(mstate)
library(Epi)
library(riskRegression)
library(prodlim)
library(ggplot2)


## function to calculate the theoretical confidence interval
ci_ss<-function(ss,p){qbeta(c(0.025,0.975),ceiling(p*ss)+1,floor((1-p)*ss)+1)}

## function to calculate the probability of failure
failure_ <- function(ss,p,q){pbeta(q,ceiling(p*ss)+1,floor((1-p)*ss)+1)}


## Create page layout with inputs for each of the model variables
ui <- fluidPage(
  
  headerPanel("Expected performance of diagnostic test for a given clinical validation study"),
  sidebarPanel(
    sliderInput('pos','Positive cases',  min = 30, max = 2000,
                value = 90, step = 10, round = 0),
    sliderInput('neg','Negative cases',  min = 30, max = 2000,
                value = 160, step = 10, round = 0),
    sliderInput('as_se','Target Sensitivity',   min = 0.5, max = 1,
                value = .97, step = .01, round = 0),
    sliderInput('lb_se','Lower Bound Sensitivity',   min = 0.5, max = 1,
                value = .93, step = .01, round = 0),
    sliderInput('as_sp', 'Target Specificity',   min = 0.5, max = 1,
                value = .99, step = .01, round = 0),
    sliderInput('lb_sp','Lower Bound Specificity',   min = 0.5, max = 1,
                value = .97, step = .01, round = 0)),
  mainPanel(
    h1("Sensitivity"),
    textOutput("text1"),
    h1("Specificity"),
    textOutput("text2")
    )
)

#build the outputs in a function called server
server <- function(input, output) {
  
  output$text1 = renderText({
    ss.ci.sens <- ci_ss(input$pos,input$as_se)
    ss.f.sens <- failure_(input$pos,input$as_se,input$lb_se)
    txt_context <- paste0("For a clinical validation study with ",input$pos,
                          " positive cases and a TPP for sensitivity of ",round(100*input$as_se,1),
                          "%, we would need to correctly detect at least ",ceiling(input$as_se*input$pos)," of the cases to pass the clinical validation study.")
    
    txt_result <- paste0("Assuming we detect exactly ",ceiling(input$as_se*input$pos)," cases, the 'real-world' 95% CI for sensitivity is expected to be (",
                         round(100*ss.ci.sens[1],1),"%,",round(100*ss.ci.sens[2],1),"%), 
                         with a ",round(100*ss.f.sens,1),"% chance of failure when the lower bound of the acceptability is ",
                         round(100*input$lb_se,1),"%.")
    paste(txt_context,txt_result)
    
  })
  
  output$text2 = renderText({
    ss.ci.spec <- ci_ss(input$neg,input$as_sp)
    ss.f.spec <- failure_(input$neg,input$as_sp,input$lb_sp)
    
    txt_context <- paste0("For a clinical validation study with ",input$neg,
                          " negative cases and a TPP for specificity of ",round(100*input$as_sp,1),
                          "%, we would need to correctly detect at least ",ceiling(input$as_sp*input$neg)," of the cases to pass the clinical validation study.")
    txt_result <- paste0("Assuming we detect exactly ",ceiling(input$as_sp*input$neg)," cases, the 95% CI for specificity is expected to be (",round(100*ss.ci.spec[1],1),"%,",round(100*ss.ci.spec[2],1),"%), with a ",round(100*ss.f.spec,1),
                         "% chance of failure when the lower bound of the acceptability is ",round(100*input$lb_sp,1),"%.")
    paste(txt_context,txt_result)
  })
 
}

shinyApp(ui, server)
