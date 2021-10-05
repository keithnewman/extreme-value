# SPDX-Copyright: Copyright (c) 2016 Keith Newman
# SPDX-License-Identifier: GPL-3.0-or-later

#' Shiny app to introduce concepts of frequentist analysis of
#' extreme-value data
#'
#' @author: Keith Newman
#' @contact: knewma@hotmail.com
#' @version: 1.3.1

library(shiny)
library(shinyjs)
library(plotly)
library(tidyr)

#`%then%` <- shiny:::`%OR%`
# source("R/dataset.R")

shinyServer(
	function(input, output, session) {

		manualData <- eventReactive(
			eventExpr = c(input$submitManualData),
			# TODO: Add try-catch for the errors this can throw.
			valueExpr = {
			  return(
			    tryCatch(
			      {DataFromString$new(input$manualDataInput, units = input$dataUnits)},
			      error = function(e) {
			        session$sendCustomMessage(
			          "dataErrorMessage",
			          "There was an issue with the uploaded data. Please make sure you have provided at least 2 numerical values and that all the values provided are not all equal to each other."
			        )
			        return(NULL)
			      }
			    )
			  )
			},
			ignoreInit = TRUE
		)

		#' Uploaded data from the user.
		#' @return: Numeric vector of numeric values. NULL if no file provided.
		dataset <- reactive({
			if (input$dataInputType == "demo") {
			  d <- DemoData$new(input$demoData)
				updateSelectInput(session, inputId = "dataUnits", selected = d$units)
				updateSelectInput(session, inputId = "dataType", selected = d$type)
				session$sendCustomMessage("updateDemoDescription",
				                          c(d$description, d$mapUrl))
				return(d)
			} else if (input$dataInputType == "file") {
				datafile <- input$dataIn

				validate(need(datafile, "Upload a dataset to begin."))
				req(datafile)
				
				d <- DataFromFile$new(datafile$datapath, sep = input$sepControl)
				d$setUnits(input$dataUnits)
				return(d)
			} else if (input$dataInputType == "manual") {
				validate(need(manualData(), "Submit a valid dataset"),
			           need(length(manualData()) > 1, "Enter at least 2 valid values"))
				# If invalid values were removed, activate the warning.
				session$sendCustomMessage("dataInputError",
				                          is.null(attr(manualData()$getData(),
				                                       "na.action")))
				d <- manualData()
			}
		  session$sendCustomMessage("dataErrorMessage", "") # Clears previous errors
		  return(d)
		})
		
		observe({
		  req(dataset()$units)
		  dataset()$setUnits(input$dataUnits)
		})
		
		# Display the inputted data back out to the display.
		output$datafile <- renderPrint({dataset()$getData()})

		# Length of the Dataset
		dataLength <- reactive({return(dataset()$length())})

		# Clear the manual text box if the user resets the input.
		observeEvent(eventExpr = input$clearManualData, ignoreInit = TRUE,
			handlerExpr = updateTextAreaInput(session,
				                                "manualDataInput",
													              value = "")
		)

		# Sorted version of the input data in ascending order
		output$datafileSorted <- renderPrint({
			return(dataset()$sortData(decreasing = FALSE))
		})

		# Dataset in tibble format
		dataTibble <- reactive({
			return(tibble(x = dataset()))
		})

		output$SummaryTable <- renderTable(dataset()$summaryTable(),
		                                   include.rownames = FALSE)
		outputOptions(output, "SummaryTable", priority = -2)
		
		######## Plots of input data #########
		
		# Histogram of input data
		output$exploratoryPlots <- renderPlotly({
		  dataset()$summaryPlotly(input$dataUnits, input$dataType)
		})
		outputOptions(output, "exploratoryPlots", priority = -1)

		inputUnits <- reactive({input$dataUnits})
		inputTimeframe <- reactive({input$dataTimeframe})
		inputDataType <- reactive({input$dataType})
		
		output$dataUnit <- renderText(inputUnits())
		
		####### Relative frequencies page ###########
		
		### Relative Frequency Model ###
		# Create a Relative Frequency model instance
		relativeFrequency <- reactive({
		  return(RelativeFrequency$new(dataset()))
		})
		
		# Pass the Relative Frequency model to the server module
		probabilityModelServer("RelativeFrequency",
		                       relativeFrequency,
		                       inputUnits,
		                       inputTimeframe,
		                       inputDataType)
		
		## Need to hide the standard error option for the return level as there is
		# no method for its calculation
		shinyjs::hide(NS("RelativeFrequency", id = "standardError"))
		shinyjs::hide(NS("RelativeFrequency", id = "tablePreamble"))
		shinyjs::hide(NS("RelativeFrequency", id = "standardErrorWall"))

		####### Probability model page ###########

		### Two parameter Gumbel Model ###
		# Create a Gumbel model instance
		gumbel <- reactive({
		  return(Gumbel$new(dataset()))
		})

		
		# Pass the Gumbel model to the server module
		probabilityModelServer("Gumbel",
		                       gumbel,
		                       inputUnits,
		                       inputTimeframe,
		                       inputDataType)
		
		### Generalised Extreme Value Model ###
		# Create a GEV model instance
		gev <- reactive({
		  return(GEV$new(dataset()))
		})
		
		# Pass the GEV model to the server module
		probabilityModelServer("GEV",
		                       gev,
		                       inputUnits,
		                       inputTimeframe,
		                       inputDataType)

		### Normal Model ###
		# Create a Normal model instance
		normal <- reactive({
		  return(Normal$new(dataset()))
		})
		
		# Pass the Normal model to the server module
		probabilityModelServer("Normal",
		                       normal,
		                       inputUnits,
		                       inputTimeframe,
		                       inputDataType)
		
		### Exponential Model ###
		# Create a Exponential model instance
		exponential <- reactive({
		  return(Exponential$new(dataset()))
		})
		
		# Pass the Exponential model to the server module
		probabilityModelServer("Exponential",
		                       exponential,
		                       inputUnits,
		                       inputTimeframe,
		                       inputDataType)
		
		### Gamma Model ###
		# Create a Gamma model instance
		gamma <- reactive({
		  return(Gamma$new(dataset()))
		})
		
		# Pass the Gamma model to the server module
		probabilityModelServer("Gamma",
		                       gamma,
		                       inputUnits,
		                       inputTimeframe,
		                       inputDataType)
		
		## Need to hide the standard error option for the return level as there is
		# no method for its calculation
		shinyjs::hide(NS("Gamma", id = "standardErrorWall"))

		######### Comparison page ###########
		# Create data table of cumulative counts and probabilities
		comparisonTable <- reactive({
		  comp <- relativeFrequency()$tableOfProbabilities() %>%
		    add_column(Gumbel = gumbel()$tableOfProbabilities()[, 2],
		               GEV = gev()$tableOfProbabilities()[, 2],
		               Normal = normal()$tableOfProbabilities()[, 2],
		               Exponential = exponential()$tableOfProbabilities()[, 2],
		               Gamma = gamma()$tableOfProbabilities()[, 2])
			return(comp)
		})
		output$comparisonTable <- renderTable({
				tbl <- comparisonTable()
				colnames(tbl)[-(1:2)] <- c("Rel. freq. probability of exceeding x",
			                             "Gumbel probability of exceeding x",
			                             "GEV probability of exceeding x",
			                             "Normal probability of exceeding x",
			                             "Exponential probability of exceeding x",
			                             "Gamma probability of exceeding x")
				return(tbl)
			},
			include.rownames = FALSE,
			digits = 4
		)
		outputOptions(output, "comparisonTable", priority = -3)

		# Create the plot
		allPlotData <- reactive({
			x <- pretty(comparisonTable()$x, n = 200)
			d <- tibble(
				x = x,
			  Gumbel = gumbel()$exceedanceProb(x),
				GEV = gev()$exceedanceProb(x),
				Normal = normal()$exceedanceProb(x),
				Exponential = exponential()$exceedanceProb(x),
				Gamma = gamma()$exceedanceProb(x)
			)
			return(d)
		})

		allGatheredPlotData <- reactive({
			return(pivot_longer(
				allPlotData(),
				cols = c("Gumbel", "GEV", "Normal", "Exponential", "Gamma"),
				names_to = "Model",
				values_to = "Probability"
			))
		})

		output$comparisonPlot <- renderPlotly({
			plot_ly(allGatheredPlotData(), x = ~x, y = ~Probability) %>%
				add_lines(line = list(shape = "spline"),
				          color = ~Model,
									name = ~Model,
									hovertemplate = paste0("Pr(X &#x3e; %{x:.2f} ",
																				 input$dataUnits,
																				 ") = %{y:.4f}")) %>%
			  add_lines(line = list(shape = "hv"),
									data = relativeFrequency()$plotData(),
									name = "Relative frequency",
									hovertemplate = paste0("Pr(X &#x3e; %{x:.2f} ",
																				 input$dataUnits,
																				 ") = %{y:.4f}")) %>%
				add_markers(data = relativeFrequency()$plotData2(),
				            y = ~Zeros,
			              name = "Observations",
									  hovertemplate = paste("%{x:.2f}", input$dataUnits)) %>%
				layout(hovermode = "x unified")
		})

		outputOptions(output, "comparisonPlot", priority = -3)
	}
)
