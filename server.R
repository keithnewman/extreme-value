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
			eventExpr = c(input$submitManualData, input$clearManualData),
			# TODO: Add try-catch for the errors this can throw.
			valueExpr = {return(DataFromString$new(input$manualDataInput,
			                                       units = input$dataUnits))},
			ignoreInit = TRUE
		)

		#' Uploaded data from the user.
		#' @return: Numeric vector of numeric values. NULL if no file provided.
		dataset <- reactive({
			if (input$dataInputType == "demo") {
			  d <- DemoData$new(input$demoData)
				updateSelectInput(session, inputId = "dataUnits", selected = d$units)
				updateSelectInput(session, inputId = "dataType", selected = d$type)
				return(d)
			} else if (input$dataInputType == "file") {
				datafile <- input$dataIn

				validate(need(datafile, "Upload a dataset to begin."))
				req(datafile)

				return(DataFromFile$new(datafile$datapath, sep = input$sepControl))
			} else if (input$dataInputType == "manual") {
				validate(need(manualData(), "Submit a valid dataset"),
			           need(length(manualData()) > 1, "Enter at least 2 valid values"))
				# If invalid values were removed, activate the warning.
				session$sendCustomMessage("dataInputError",
				                          is.null(attr(manualData(), "na.action")))
				return(manualData())
			}
		})
		
		observe({
		  req(dataset()$units)
		  dataset()$setUnits(input$dataUnits)
		})
		
		#
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
		

		# Break points needed for plotting histogram and Relative Frequency table
		dataPrettyBreaks <- reactive({
			return(pretty(x = dataset()$getData(),
			              n = 2 * nclass.FD(dataset()$getData())))
		})

		# Based on the type of the input data, we need to adjust text that appears
		# on the page
		howExtremeTextGenerator <- reactive({
			return(
				switch(input$dataType,
					"Wave height" = "How high should we build a wall?",
					"How extreme could it get?"  # Default text if nothing else matches
				)
			)
		})

		observe({
			req(dataset())
			label <- paste("Choose a", input$dataType,
			               "to find the probability of exceeding it every",
			               input$dataTimeframe)
			sliderRange <- range(relFrequencyPlotData()$x)
			min <- max(0, sliderRange[1] - 2)
      max <- max(0, sliderRange[2] + 3)
      value <- round(runif(1, sliderRange[1], sliderRange[2]), 1)
			updateSliderInput(session, "RFProbabilityInput", label, value, min, max)
			updateSliderInput(session, "gumbelProbabilityInput", label, value, min, max)
			updateSliderInput(session, "GEVProbabilityInput", label, value, min, max)
			updateSliderInput(session, "normalProbabilityInput", label, value, min, max)
			updateSliderInput(session, "expProbabilityInput", label, value, min, max)
			updateSliderInput(session, "gammaProbabilityInput", label, value, min, max)
		})

		output$dataUnit <- renderText(input$dataUnits)
		# Need one of each of these per box in the app
		output$howExtremeText1 <- renderText({howExtremeTextGenerator()})
		output$howExtremeText2 <- renderText({howExtremeTextGenerator()})
		output$howExtremeText3 <- renderText({howExtremeTextGenerator()})
		output$howExtremeText4 <- renderText({howExtremeTextGenerator()})
		output$howExtremeText5 <- renderText({howExtremeTextGenerator()})
		output$howExtremeText6 <- renderText({howExtremeTextGenerator()})


		# Label for the input slider
		howExtremesliderInputLabelGenerator <- reactive({
			return(
				switch(input$dataType,
					"Wave height" = paste("How high should we build a wall to protect from a \"once in a \u2026", input$dataTimeframe, "storm\"?"),
					paste0("How extreme would we expect the ", tolower(input$dataType), " to be \"once every \u2026 ", input$dataTimeframe, "\"?")  # Default text if nothing else matches
				)
			)
		})
		# Need one of each of these per box in the app
		observe(priority = -1, {
			updateSliderInput(session, "RFWallHeightInput", label = howExtremesliderInputLabelGenerator())
			updateSliderInput(session, "gumbelWallHeightInput", label = howExtremesliderInputLabelGenerator())
			updateSliderInput(session, "GEVWallHeightInput", label = howExtremesliderInputLabelGenerator())
			updateSliderInput(session, "normalWallHeightInput", label = howExtremesliderInputLabelGenerator())
			updateSliderInput(session, "expWallHeightInput", label = howExtremesliderInputLabelGenerator())
			updateSliderInput(session, "gammaWallHeightInput", label = howExtremesliderInputLabelGenerator())
		})

		# The sentence before the "how extreme answer is given"
		howExtremeAnswerPreambleGenerator <- reactive({
			return(
				switch(input$dataType,
					"Wave height" = "The required height \\(z\\) of the wall can be calculated as,",
					paste0("We would expect to see an extreme ", tolower(input$dataType), " \\(z\\) equal to,")  # Default text if nothing else matches
				)
			)
		})
		# Need one of each of these per box in the app
		output$howExtremeAnswerPreamble1 <- renderText({howExtremeAnswerPreambleGenerator()})
		output$howExtremeAnswerPreamble2 <- renderText({howExtremeAnswerPreambleGenerator()})
		output$howExtremeAnswerPreamble3 <- renderText({howExtremeAnswerPreambleGenerator()})
		output$howExtremeAnswerPreamble4 <- renderText({howExtremeAnswerPreambleGenerator()})
		output$howExtremeAnswerPreamble5 <- renderText({howExtremeAnswerPreambleGenerator()})
		output$howExtremeAnswerPreamble6 <- renderText({howExtremeAnswerPreambleGenerator()})


		####### Relative frequencies page ###########
		output$RFtablePreamble <- renderUI({
			withMathJax(
				paste0(
					"There are \\(n=",
					dataLength(),
					"\\) observations in the dataset you provided. ",
					"Therefore the probability of exceeding ",
					"\\(x\\) is $$\\mathrm{Pr}(X>x)=\\frac{\\text{Number of observations exceeding }x}{\\text{Total number of observations } (n=",
					dataLength(), ")}$$")
				)
		})

		#' Create data frame of cumulative counts and probabilities
		#' @return Data frame with three columns:
		#'           1) x values at regular intervals.
		#'           2) Number of data points that exceed x
		#'           3) Pr(X > x)
		relFreqTable <- reactive({
			prettyBreakInt <- diff(dataPrettyBreaks())[[1]]

			# x values at regular intervals
			x <- seq(from = min(dataPrettyBreaks()) - 2 * prettyBreakInt,
			         to = max(dataPrettyBreaks()) + 3 * prettyBreakInt,
			         by = prettyBreakInt)

			# How many data points exceed each of these x values
			nExceedingX <- sapply(x, function(xVal) sum(dataset()$getData() > xVal))

			# Find relative exceedance probability
			probExceedX <- nExceedingX / dataLength()

			# Output these values in a data frame. Don't check the column names
			# or it will substitute the " " in the names with ".".
			return(
				data.frame(x = x,
				           `Number of observations exceeding x` = nExceedingX,
				           `Probability of exceeding x` = probExceedX,
				           check.names = FALSE)
			)
		})

		output$RFtable <- renderTable({relFreqTable()},
		                              include.rownames = FALSE)
		outputOptions(output, "RFtable", priority = -1, suspendWhenHidden = FALSE)

		# Data points for the relative frequency plot, but with dummy points either
		# end of the dataset so the line can continue beyond the range of data.
		relFrequencyPlotData <- reactive({
			# Add some dummy points to the left and right of the actual dataset
			LEFT_PADDING = 0.4
			RIGHT_PADDING = 2.0
			x = c(min(dataset()$getData()) - LEFT_PADDING,
			      dataset()$getData(),
						max(dataset()$getData()) + RIGHT_PADDING)

			relFreqData <- tibble(
				x = x,
			  Probability = sapply(x, function(xVal) sum(dataset()$getData() > xVal) / dataLength())
			)
			return(relFreqData)
		})

		# The actual data points with the two dummy endpoints removed.
		relFrequencyPlotData2 <- reactive({
			d <- tibble(x = dataset()$getData(), Zeros = 0)
		})

		output$RFplot <- renderPlotly({
			plot_ly(relFrequencyPlotData(),
			        x = ~x,
							y = ~Probability) %>%
				add_lines(line = list(shape = "hv"),
			            name = "Relative frequency",
								  hovertemplate = paste0("Pr(X &#x3e; %{x:.2f} ",
									                       input$dataUnits,
																				 ") = %{y:.4f}")) %>%
				add_markers(data = relFrequencyPlotData2(),
				            y = ~Zeros,
			              name = "Observations",
									  hovertemplate = paste("%{x:.2f}", input$dataUnits)) %>%
				layout(yaxis = list(title = "Probability"),
				       hovermode = "x unified")
		})
		outputOptions(output, "RFplot", priority = -1, suspendWhenHidden = FALSE)

		# Find a probability: input slider
		output$RFProbabilitySlider <- renderUI({
			req(dataset()$getData())
			sliderRange <- range(relFrequencyPlotData()[, 1])
			return(
				sliderInput("RFProbabilityInput",
				            label = paste(
				            	"Choose a",
				            	input$dataType,
				            	"to find the probability of exceeding it every",
				            	input$dataTimeframe
				            ),
				            min = max(0, sliderRange[1] - 2),
				            max = max(0, sliderRange[2] + 3),
				            value = round(runif(1, sliderRange[1], sliderRange[2]), 1),
				            step = 0.05)
			)
		})
		outputOptions(
			output, "RFProbabilitySlider", priority = -2, suspendWhenHidden = TRUE
		)

		# Find a probability: text description
		output$RFProbabilityDescription <- renderUI({
			req(dataset()$getData())
			exceedances <- sum(dataset()$getData() > input$RFProbabilityInput)
			return(withMathJax(p(sprintf(
				"We have seen %1$i instances when the %2$s has exceeded %3$.2f %4$s in
				our %5$i observations. Therefore, the probability of observing a %2$s
				greater than \\(x=%3$.2f\\) %4$s every %6$s is given by
				$$\\mathrm{Pr}(X>x=%3$.2f)=\\frac{%1$i}{%5$i}=%7$0.4f
				\\text{ (to 4 decimal places).} $$",
				exceedances, #1
				tolower(input$dataType), #2
				input$RFProbabilityInput, #3
				input$dataUnits, #4
				dataLength(), #5
				input$dataTimeframe, #6
				exceedances / dataLength() #7
			))))
		})
		outputOptions(output,
		              "RFProbabilityDescription",
									priority = -3,
									suspendWhenHidden = TRUE)

		# Find a wall height
		output$RFWallHeightInput <- renderText({
			paste(input$RFWallHeightInput, input$dataTimeframe)
		})
		output$RFWallHeightP <- renderUI({
			withMathJax(sprintf("\\(p=%0.4f\\)", 1.0 / input$RFWallHeightInput))
		})

		output$RFWallHeightCalculation <- renderUI({
			req(dataset())
			return(
				withMathJax(
					sprintf(
						"$$x=%0.2f\\text{ %s (to 2 decimal places).}$$",
						dataset()$getData()[dataLength() - ceiling(dataLength() / input$RFWallHeightInput) + 1],
						input$dataUnits
					)
				)
			)
		})

		####### Probability model page ###########

		### Two parameter Gumbel Model ###
		# Create a Gumbel model instance
		gumbel <- reactive({
		  return(Gumbel$new(dataset()))
		})
		
		inputUnits <- reactive({input$dataUnits})
		inputTimeframe <- reactive({input$dataTimeframe})
		inputDataType <- reactive({input$dataType})
		
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
			comp <- relFreqTable()
			names(comp)[3] <- "Relative Frequency"
			comp$Gumbel <- gumbel()$tableOfProbabilities()[, 2]
			comp$GEV <- gev()$tableOfProbabilities()[, 2]
			comp$Normal <- normal()$tableOfProbabilities()[, 2]
			comp$Exponential <- exponential()$tableOfProbabilities()[, 2]
			comp$Gamma <- gamma()$tableOfProbabilities()[, 2]
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
			x <- pretty(relFreqTable()$x, n = 200)
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
									data = relFrequencyPlotData(),
									name = "Relative frequency",
									hovertemplate = paste0("Pr(X &#x3e; %{x:.2f} ",
																				 input$dataUnits,
																				 ") = %{y:.4f}")) %>%
				add_markers(data = relFrequencyPlotData2(),
				            y = ~Zeros,
			              name = "Observations",
									  hovertemplate = paste("%{x:.2f}", input$dataUnits)) %>%
				layout(hovermode = "x unified")
		})

		outputOptions(output, "comparisonPlot", priority = -3)
	}
)
