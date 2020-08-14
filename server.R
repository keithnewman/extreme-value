# SPDX-Copyright: Copyright (c) 2016 Keith Newman
# SPDX-License-Identifier: GPL-3.0-or-later

#' Shiny app to introduce concepts of frequentist analysis of
#' extreme-value data
#'
#' @author: Keith Newman
#' @contact: knewma@hotmail.com
#' @version: 1.1.0

library(shiny)
library(ggplot2)
library(plotly)
library(tidyr)

shinyServer(
	function(input, output) {

		#' Uploaded data from the user.
		#' @return: Numeric vector of numeric values. NULL if no file provided.
		dataset <- reactive({
			datafile <- input$dataIn

			validate(need(datafile, "Upload a dataset to begin."))
			req(datafile)

			d <- as.numeric(unlist(read.table(datafile$datapath,
			                                  sep = input$sepControl)))
			return(d)
		})
		#
		output$datafile <- renderPrint({dataset()})

		# Length of the Dataset
		dataLength <- reactive({return(length(dataset()))})

		# Sorted version of the input data in ascending order
		output$datafileSorted <- renderPrint({
			return(sort(dataset(), decreasing = FALSE))
		})

		# Dataset in tibble format
		dataTibble <- reactive({
			return(tibble(x = dataset()))
		})

		# Break points needed for plotting histogram and Relative Frequency table
		dataPrettyBreaks <- reactive({
			return(pretty(x = dataset(),
			              n = 2 * nclass.FD(dataset())))
		})

		# Based on the type of the input data, we need to adjust text that appears
		# on the page
		howExtremeTextGenerator <- reactive({
			return(
				switch(input$dataType,
					"Sea-surge height" = "How high should we build a wall?",
					"How extreme could it get?"  # Default text if nothing else matches
				)
			)
		})
		# Need one of each of these per box in the app
		output$howExtremeText1 <- renderText({howExtremeTextGenerator()})
		output$howExtremeText2 <- renderText({howExtremeTextGenerator()})
		output$howExtremeText3 <- renderText({howExtremeTextGenerator()})

		# Label for the input slider
		howExtremesliderInputLabelGenerator <- reactive({
			return(
				switch(input$dataType,
					"Sea-surge height" = p("How high should we build a wall to protect from a \"once in a ", HTML("&hellip;"), " ", input$dataTimeframe, " storm\"?"),
					p("How extreme would we expect the ", input$dataType, " to be \"once every ",HTML("&hellip;")," ", input$dataTimeframe, "\"?")  # Default text if nothing else matches
				)
			)
		})
		# Need one of each of these per box in the app
		output$howExtremeSliderLabel1 <- renderUI({howExtremesliderInputLabelGenerator()})
		output$howExtremeSliderLabel2 <- renderUI({howExtremesliderInputLabelGenerator()})
		output$howExtremeSliderLabel3 <- renderUI({howExtremesliderInputLabelGenerator()})

		# The sentence before the "how extreme answer is given"
		howExtremeAnswerPreambleGenerator <- reactive({
			return(
				switch(input$dataType,
					"Sea-surge height" = "The required height \\(x\\) of the wall can be calculated as,",
					paste0("We would expect to see an extreme ", input$dataType, " \\(x\\) equal to,")  # Default text if nothing else matches
				)
			)
		})
		# Need one of each of these per box in the app
		output$howExtremeAnswerPreamble1 <- renderText({howExtremeAnswerPreambleGenerator()})
		output$howExtremeAnswerPreamble2 <- renderText({howExtremeAnswerPreambleGenerator()})
		output$howExtremeAnswerPreamble3 <- renderText({howExtremeAnswerPreambleGenerator()})

		######## Plots of input data #########

		# Histogram of input data
		output$exploratoryPlots <- renderPlotly({
			units <- input$dataUnits
			xlabel <- paste0(input$dataType, " maxima (", input$dataUnits, ")")
			hist_ <- plot_ly(dataTibble(), x = ~x, type = "histogram", name = xlabel) %>%
    		layout(xaxis = list(title = xlabel),
				       yaxis = list(title = "Frequency",
			 	                    showline = TRUE,
			 	                    showticklabels = TRUE,
													  showgrid = TRUE))
			boxp_ <- plot_ly(dataTibble(), x = ~x, type = "box", name = xlabel) %>%
				layout(xaxis = list(title = xlabel,
														showticklabels = TRUE,
														showgrid = TRUE),
							 yaxis = list(title = "",
														showline = FALSE,
														showticklabels = FALSE,
														showgrid = FALSE))
			ss <- subplot(hist_,
			              # plotly_empty(),
										boxp_,
			              nrows = 2,
			              heights = c(0.8, 0.2),
			              # widths = c(0.8, 0.2),
			              shareX = TRUE,
									  titleY = TRUE)
			return(layout(ss, title=xlabel, showlegend=FALSE))
		})
		outputOptions(output, "exploratoryPlots", priority = -1)

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
			nExceedingX <- sapply(x, function(xVal) sum(dataset() > xVal))

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
			x = c(min(dataset()) - LEFT_PADDING,
			      dataset(),
						max(dataset()) + RIGHT_PADDING)

			relFreqData <- tibble(
				x = x,
			  Probability = sapply(x, function(xVal) sum(dataset() > xVal) / dataLength())
			)
			return(relFreqData)
		})

		# The actual data points with the two dummy endpoints removed.
		relFrequencyPlotData2 <- reactive({
			d <- tibble(x = dataset(), Zeros = 0)
		})

		output$RFplot <- renderPlotly({
			plot_ly(relFrequencyPlotData(),
			        x = ~x,
							y = ~Probability) %>%
				add_lines(line = list(shape = "hv"),
			            name = "Probability",
								  hovertemplate = paste0("Pr(X &#x3e; %{x:.2f} ",
									                       input$dataUnits,
																				 ") = %{y:.4f}")) %>%
				add_markers(data = relFrequencyPlotData2(),
				            y = ~Zeros,
			              name = "Observations",
									  hovertemplate = paste("%{x:.2f}", input$dataUnits))
		})
		outputOptions(output, "RFplot", priority = -1, suspendWhenHidden = FALSE)

		# Find a probability: input slider
		output$RFProbabilitySlider <- renderUI({
			req(dataset())
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
			req(dataset())
			exceedances <- sum(dataset() > input$RFProbabilityInput)
			return(withMathJax(p(sprintf(
				"We have seen %1$i instances when the %2$s has exceeded %3$.2f %4$s in
				our %5$i observations. Therefore, the probability of observing a %2$s
				greater than \\(x=%3$.2f\\) %4$s every %6$s is given by
				$$\\mathrm{Pr}(X>x=%3$.2f)=\\frac{%1$i}{%5$i}=%7$0.4f
				\\text{ (to 4 significant figures).} $$",
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
						dataset()[dataLength() - ceiling(dataLength() / input$RFWallHeightInput) + 1],
						input$dataUnits
					)
				)
			)
		})

		####### Probability model page ###########
		### Two parameter Gumbel Model ###
		output$dataTypeTPGM <- renderText({tolower(input$dataType)})

		#' Negative log-likelihood for the Gumbel model
		gumbel.loglik <- function(theta) {
			mu <- theta[1]
			sigma <- theta[2]
			loglik <- -length(dataset()) * log(sigma) -
									sum(exp(-((dataset() - mu) / sigma))) -
									sum((dataset() - mu) / sigma)
			return(-loglik)
		}

		dgumbel <- function(x, mu, sigma) {
			return(1 - exp(-exp(-((x - mu) / sigma))))
		}

		#' Find the maximum likelihood estimates of the Gumbel distribution
		gumbelParameters <- reactive({
			#set initial values for the parameter vector theta=c(mu,sigma)
			theta <- c(mean(dataset()), sd(dataset()))
			return(nlm(gumbel.loglik, theta)$est)
		})

		output$gumbelTablePreamble <- renderUI({
			withMathJax(
				sprintf(
					"For the data you provided, we have found that \\(\\mu=%0.3f\\)
					and \\(\\sigma=%0.3f\\) (Both values given to 3 decimal places).",
					gumbelParameters()[1],
					gumbelParameters()[2]
				)
			)
		})

		# Create data table of cumulative counts and probabilities
		gumbelTable <- reactive({
			x <- relFreqTable()$x
			prExceedX <- sapply(x,
                          dgumbel,
													mu = gumbelParameters()[1],
													sigma = gumbelParameters()[2])
			return(data.frame(x = x,
			                  `Probability of exceeding x` = prExceedX,
											  check.names = FALSE))
		})
		output$gumbelTable <- renderTable({
			return(gumbelTable())
		}, include.rownames = FALSE)
		outputOptions(
			output, "gumbelTable", priority = -2, suspendWhenHidden = TRUE
		)

		# Create a gumbel plot
		output$gumbelPlot <- renderPlotly({
			x <- seq(from = min(gumbelTable()$x),
               to = max(gumbelTable()$x),
							 by = 0.01)
		  p <- sapply(x,
                  dgumbel,
									mu = gumbelParameters()[1],
									sigma = gumbelParameters()[2])
			d <- tibble(x = x, Probability = p)
			return(plot_ly(d, x = ~x, y = ~Probability) %>%
							add_lines(line = list(shape = "spline"),
							          name = "Probability",
							          hovertemplate = paste0("Pr(X &#x3e; %{x:.2f} ",
																			         input$dataUnits,
																			         ") = %{y:.4f}")))
		})
		outputOptions(output, "gumbelPlot", priority = -2)

		# Find a probability: input slider
		output$gumbelProbabilitySlider <- renderUI({
			validate(need(input$dataIn, ""))
			sliderRange <- range(relFrequencyPlotData()[, 1])
			return(
				sliderInput("gumbelProbabilityInput",
					label = h4(
						"Choose a ",
						input$dataType,
						" to find the probability of exceeding it every ",
						input$dataTimeframe
					),
					min = max(0, sliderRange[1] - 2),
					max = max(0, sliderRange[2] + 3),
					value = round(runif(1, sliderRange[1], sliderRange[2]), 1),
					step = 0.05
				)
			)
		})
		outputOptions(
			output, "gumbelProbabilitySlider", priority = -2, suspendWhenHidden = TRUE
		)
		# Find a probability: text description
		output$gumbelProbabilityDescription <- renderUI({
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
								\\right]=%7$0.4f\\text{ (to 4 significant figures).}$$",
							tolower(input$dataType), #1
							input$gumbelProbabilityInput, #2
							input$dataUnits, #3
							input$dataTimeframe, #4
							gumbelParameters()[1], #5
							gumbelParameters()[2], #6
							dgumbel(input$gumbelProbabilityInput,
								      gumbelParameters()[1],
											gumbelParameters()[2]) #7
						)
					)
				)
			)
		})
		outputOptions(output,
			"gumbelProbabilityDescription", priority = -3, suspendWhenHidden = TRUE
		)

		# Find a wall height
		output$gumbelWallHeightInput <- renderText({
			paste(input$gumbelWallHeightInput, input$dataTimeframe)
		})
		output$gumbelWallHeightP <- renderUI({
			withMathJax(
				paste0("\\(p=", signif(1 / input$gumbelWallHeightInput, 4), "\\)")
			)
		})
		output$gumbelWallHeightCalculation <- renderUI({
			return(
				sprintf(
					"$$x=%1$0.3f-%2$0.3f\\log\\left[
						-\\log\\left(1-\\frac{1}{%3$0.2f}\\right)
					\\right]=%4$0.2f\\text{ %5$s (to 2 decimal places).}$$",
					gumbelParameters()[1], #1
					gumbelParameters()[2], #2
					input$gumbelWallHeightInput, #3
					gumbelParameters()[1] - gumbelParameters()[2] * log(-log(1 - (1 / input$gumbelWallHeightInput))), #4
					input$dataUnits #5
				)
			)
		})

		### Normal Distribution ###
		output$dataTypeND <- renderText({tolower(input$dataType)})

		normalParameterMean <- reactive({return(mean(dataset()))})
		normalParameterSD <- reactive({return(sd(dataset()))})

		output$normalTablePreamble <- renderUI({
			validate(
				need(
					input$dataIn, "Please upload a dataset to see more information!"
				)
			)
			withMathJax(
				paste0(
					"For the data you provided, we have found that \\(\\mu=",
					round(normalParameterMean(), 3),
					"\\) and \\(\\sigma=",
					round(normalParameterSD(), 3),
					"\\) (Both values given to 3 decimal places)."
				)
			)
		})

		# Create data table of cumulative counts and probabilities
		makeNormalTable <- function (d) {
			norm <- data.frame(x = relFreqTable()$x)
			norm$`Probability of exceeding x` <- 1 - pnorm(norm$x, normalParameterMean(), normalParameterSD())
			return(norm)
		}
		normalTable <- reactive({makeNormalTable(dataset())})
		output$normalTable <- renderTable({
			validate(need(input$dataIn, ""))
			return(normalTable())
		}, include.rownames = FALSE)
		outputOptions(
			output, "normalTable", priority = -2, suspendWhenHidden = FALSE
		)

		# Create the plot
		output$normalPlot <- renderPlot({
			validate(need(input$dataIn, ""))
			return(
				ggplot(data.frame(x=range(relFrequencyPlotData()[, 1])), aes(x)) +
				stat_function(
					fun = function(x) {
						return(1 - pnorm(x, normalParameterMean(), normalParameterSD()))
					}
				) +
				scale_x_continuous(paste0(input$dataType, " (", input$dataUnits, ")")) +
				scale_y_continuous("Probability density") +
				theme_bw()
			)
		})
		outputOptions(output, "normalPlot", priority = -2, suspendWhenHidden = FALSE)

		# Find a probability: input slider
		output$normalProbabilitySlider <- renderUI({
			validate(need(input$dataIn, ""))
			sliderRange <- range(relFrequencyPlotData()[, 1])
			return(
				sliderInput(
					"normalProbabilityInput",
					label = h4(
						"Choose a ",
						input$dataType,
						" to find the probability of exceeding it every ",
						input$dataTimeframe
					),
					min = max(0, sliderRange[1] - 2),
					max = max(0, sliderRange[2] + 3),
					value = round(runif(1, sliderRange[1], sliderRange[2]), 1),
					step = 0.05
				)
			)
		})
		outputOptions(
			output, "normalProbabilitySlider", priority = -2, suspendWhenHidden = TRUE
		)
		# Find a probability: text description
		output$normalProbabilityDescription <- renderUI({
			validate(need(input$normalProbabilityInput, ""))
			return(
				withMathJax(
					p(
						paste0(
							"The probability of observing a ",
							tolower(input$dataType),
							" greater than \\(x=",
							input$normalProbabilityInput,
							"\\) ",
							input$dataUnits,
							" every ", input$dataTimeframe," is given by $$\\mathrm{Pr}(X>",
							input$normalProbabilityInput,
							")=1-\\Phi\\left(\\frac{",
								input$normalProbabilityInput, "-", round(normalParameterMean(), 3),
							"}{",
								round(normalParameterSD(), 3),
							"}\\right)=",
							signif(1 - pnorm(input$normalProbabilityInput, normalParameterMean(), normalParameterSD()), 4),
							"\\text{ (to 4 significant figures).}$$"
						)
					)
				)
			)
		})
		outputOptions(output,
			"normalProbabilityDescription", priority = -3, suspendWhenHidden = TRUE
		)

		# Find a wall height
		output$normalWallHeightInput <- renderText({
			paste(input$normalWallHeightInput, input$dataTimeframe)
		})
		output$normalWallHeightP <- renderUI({
			withMathJax(
				paste0("\\(p=", signif(1 / input$normalWallHeightInput, 4), "\\)")
			)
		})
		output$normalWallHeightCalculation <- renderUI({
			validate(
				need(
					input$dataIn,
					"This calculation is unavailable as no data has been uploaded!"
				)
			)
			return(
				withMathJax(
					paste0(
						"$$x=",
						round(normalParameterSD(), 3),
						"\\times\\Phi^{-1}\\left(\\frac{1}{",
						input$normalWallHeightInput,
						"}\\right)+",
						round(normalParameterMean(), 3),
						"=",
						round(qnorm(1 - (1 / input$normalWallHeightInput), normalParameterMean(), normalParameterSD()), 2),
						"\\text{ ",
						input$dataUnits,
						" (to 2 decimal places).}$$"
					)
				)
			)
		})

		######### Comparison page ###########
		# Create data table of cumulative counts and probabilities
		makeComparisonTable <- function () {
			comp <- relFreqTable()
			names(comp)[3] <- "Rel. freq. probability of exceeding x"
			comp$`Gumbel probability of exceeding x` <- gumbelTable()[, 2]
			comp$`Normal probability of exceeding x` <- normalTable()[, 2]
			return(comp)
		}
		comparisonTable <- reactive({makeComparisonTable()})
		output$comparisonTable <- renderTable({
			validate(need(input$dataIn, "Please upload a dataset to see more information!"))
			return(comparisonTable())
		}, include.rownames = FALSE)
		outputOptions(output, "comparisonTable", priority = -3)

		# Create the plot
		output$comparisonPlot <- renderPlot({
			validate(need(input$dataIn, "Please upload a dataset to see this plot!"))
			return(
				ggplot(data.frame(x=range(relFrequencyPlotData()[, 1])), aes(x)) +
				stat_function(  # Gumbel line in red
					fun = function(x) {
						return(
							1-exp(-exp(-((x-gumbelParameters()[1]) / gumbelParameters()[2])))
						)
					}, colour = "red"
				) +
				stat_function(  # Normal line in blue
					fun = function(x) {
						return(1 - pnorm(x, normalParameterMean(), normalParameterSD()))
					}, col="blue"
				) +
				geom_step(  # Relative frequency probability line
					data = relFrequencyPlotData(), aes(Height, Probability), colour="grey"
				) +
				geom_point(  # Observations
					aes(Height, Probability),
					data = relFrequencyPlotData2(),
					size = 5,
					shape = 4
				) +
				scale_x_continuous(paste0(input$dataType, " (", input$dataUnits, ")")) +
				scale_y_continuous("Probability density") +
				theme_bw()
			)
		})
		outputOptions(output, "comparisonPlot", priority = -3)
	}
)
