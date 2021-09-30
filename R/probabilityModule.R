library(shiny)
probabilityModelUI <- function(id,
                               title = "Probability Model",
                               seForWall = TRUE) {
  ns <- NS(id)
  
  tagList(
    h2(title),
    div(
      div(
        h3("How probability is calculated using a probability model",
           class = "panel-title"),
        class = "panel-heading"
      ),
      div(
        uiOutput(ns("modelDescription")),
        checkboxInput(ns("standardError"), "Include Standard Errors", FALSE),
        uiOutput(ns("tablePreamble")),
        class = "panel-body"
      ),
      class = "panel panel-info"
    ),
    fluidRow(
      column(4,
             h3("Table of probabilities"),
             tableOutput(ns("table"))
      ),
      column(8,
             h3("Plot of probabilities"),
             plotlyOutput(ns("plot"))
      )
    ),
    fluidRow(
      column(6,
             div(
               div(
                 h3("Calculate a probability from this model", class = "panel-title"),
                 class = "panel-heading"
               ),
               div(
                 wellPanel(
                   sliderInput(ns("probabilityInput"),
                               label = "Upload data to begin",
                               min = 0, max = 1, value = 0, step = 0.05)
                 ),
                 uiOutput(ns("probabilityDescription")),
                 class = "panel-body"
               ),
               class = "panel panel-primary"
             )
      ),
      column(6,
             div(
               div(
                 h3(textOutput(ns("howExtremeText")), class = "panel-title"),
                 class = "panel-heading"
               ),
               div(
                 wellPanel(
                   sliderInput(ns("wallHeight"),
                               label = "How extreme will the event be?",
                               min = 2,
                               max = 1000,
                               value = 100)
                 ),
                 p("A once in a ",
                   textOutput(ns("wallHeightInput"), inline = TRUE),
                   " event corresponds to an exceedance probability ",
                   htmlOutput(ns("wallHeightP"), TRUE),
                   " (to 4 significant figures)."),
                 withMathJax(
                   textOutput(ns("howExtremeAnswerPreamble2")),
                   uiOutput(ns("wallHeightCalculation"), TRUE),
                   checkboxInput(ns("standardErrorWall"),
                                 "Include Standard Error",
                                 FALSE)
                 ),
                 class = "panel-body"
               ),
               class = "panel panel-primary"
             )
      )
    )
  )
}


probabilityModelServer <- function(id, model, units, timeframe, dataType) {
  moduleServer(
    id,
    function(input, output, session) {
      modelParameters <- reactive({model()$getFittedTheta()})
      modelParametersSE <- reactive({model()$getSE()})
      
      output$modelDescription <- renderUI({model()$description()})
      
      output$tablePreamble <- renderUI({
        model()$fittedParameterDescription(input$standardError)
      })
      
      # Create data table of cumulative counts and probabilities
      tableOfProbabilities <- reactive({model()$tableOfProbabilities()})
      output$table <- renderTable(tableOfProbabilities(),
                                  include.rownames = FALSE,
                                  digits = 4)
      outputOptions(output, "table", priority = -2)
      
      # Create a gumbel plot
      output$plot <- renderPlotly({model()$plotly(units())})
      outputOptions(output, "plot", priority = -2)
      
      output$howExtremeText <- renderText({
        switch(dataType(),
               "Wave height" = sprintf("How high should we build a wall to protect from a \"once in a \u2026 %s storm\"?", timeframe()),
               sprintf("How extreme would we expect the %s to be \"once every \u2026 %s\"?", tolower(dataType()), timeframe())  # Default text if nothing else matches
        )
      })
      
      observe({
        req(model())
        label <- paste("Choose a", tolower(dataType()),
                       "to find the probability of exceeding it every",
                       timeframe())
        sliderRange <- range(tableOfProbabilities()$x)
        min <- max(0, sliderRange[1] - 2)
        max <- max(0, sliderRange[2] + 3)
        value <- round(runif(1, sliderRange[1], sliderRange[2]), 1)
        updateSliderInput(session, "probabilityInput", label, value, min, max)
      })
      
      # Find a probability: text description
      output$probabilityDescription <- renderUI({
        return(
          withMathJax(
            p(
              sprintf(
                "The probability of observing a %1$s greater than
							\\(x=%2$0.2f\\) %3$s every %4$s is given by
							$$\\mathrm{Pr}(X>%2$0.2f)=
								1-\\exp\\left[
									-\\exp\\left\\{
										-\\left(
											\\frac{%2$0.2f-%5$0.3f}{%6$0.3f}
										\\right)
									\\right\\}
								\\right]=%7$0.4f\\text{ (to 4 decimal places).}$$",
                tolower(dataType()), #1
                input$probabilityInput, #2
                units(), #3
                timeframe(), #4
                modelParameters()[1], #5
                modelParameters()[2], #6
                model()$exceedanceProb(input$probabilityInput) #7
              )
            )
          )
        )
      })
      outputOptions(output, "probabilityDescription",
                    priority = -3, suspendWhenHidden = TRUE)
      
      # Find a wall height
      output$wallHeightInput <- renderText({
        paste(input$wallHeightInput, timeframe())
      })
      output$wallHeightP <- renderUI({
        withMathJax(
          paste0("\\(p=", signif(1 / input$wallHeight, 4), "\\)")
        )
      })
      
      # Return Standard Errors
      # gumbelWallSE <- reactive({
      # 	hess <- solve(gumbelFit()$hessian)
      # 	del <- matrix(c(1, -log(-log(1 - (1 / input$wallHeight)))),
      # 	              ncol = 1, nrow = 2)
      # 	error = sqrt(t(del) %*% hess %*% del)
      # 	return(error)
      # })
      
      output$wallHeightCalculation <- renderUI({
        return(withMathJax(
          sprintf(
            "$$z_{%3$.0f}=%1$0.3f-%2$0.3f\\log\\left[
						-\\log\\left(1-\\frac{1}{%3$.0f}\\right)
					\\right]=%4$0.2f%5$s\\text{ %6$s (to 2 decimal places).}$$",
            modelParameters()[1], #1
            modelParameters()[2], #2
            input$wallHeight, #3
            model()$returnLevel(input$wallHeight), #4
            ifelse(input$standardErrorWall,
                   sprintf("\\ (%.2f)",
                           model()$returnLevelSE(input$wallHeight)),
                   ""), #5
            units() #6
          )
        ))
      })
      invisible(TRUE)
    }
  )
}