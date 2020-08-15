# SPDX-Copyright: Copyright (c) 2016 Keith Newman
# SPDX-License-Identifier: GPL-3.0-or-later

#' Shiny app to introduce concepts of frequentist analysis of
#' extreme-value data
#'
#' @author: Keith Newman
#' @contact: knewma@hotmail.com
#' @version: 1.1.0

library(shiny)
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

		output$SummaryTable <- renderTable({
				# Find min, LQ, median, UQ and max in one go
				summaryQuant <- quantile(dataset())

				Summary <- data.frame(l    = length(dataset()),
															Mean = mean(dataset()),
															sd   = sd(dataset()),
															min  = summaryQuant[1],
															lq   = summaryQuant[2],
															med  = summaryQuant[3],
															uq   = summaryQuant[4],
															max  = summaryQuant[5])

				colnames(Summary) <- c("Number of Observations",
															 "Mean",
															 "Standard Deviation",
															 "Minimum Observation",
															 "Lower Quartile",
															 "Median",
															 "Upper Quartile",
															 "Maximum Observation")
				return(Summary)
			},
			include.rownames = FALSE
		)
		outputOptions(output, "SummaryTable", priority = -2)

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
					"Wave height" = "How high should we build a wall?",
					"How extreme could it get?"  # Default text if nothing else matches
				)
			)
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
					"Wave height" = p("How high should we build a wall to protect from a \"once in a ", HTML("&hellip;"), " ", input$dataTimeframe, " storm\"?"),
					p("How extreme would we expect the ", input$dataType, " to be \"once every ",HTML("&hellip;")," ", input$dataTimeframe, "\"?")  # Default text if nothing else matches
				)
			)
		})
		# Need one of each of these per box in the app
		output$howExtremeSliderLabel1 <- renderUI({howExtremesliderInputLabelGenerator()})
		output$howExtremeSliderLabel2 <- renderUI({howExtremesliderInputLabelGenerator()})
		output$howExtremeSliderLabel3 <- renderUI({howExtremesliderInputLabelGenerator()})
		output$howExtremeSliderLabel4 <- renderUI({howExtremesliderInputLabelGenerator()})
		output$howExtremeSliderLabel5 <- renderUI({howExtremesliderInputLabelGenerator()})
		output$howExtremeSliderLabel6 <- renderUI({howExtremesliderInputLabelGenerator()})

		# The sentence before the "how extreme answer is given"
		howExtremeAnswerPreambleGenerator <- reactive({
			return(
				switch(input$dataType,
					"Wave height" = "The required height \\(x\\) of the wall can be calculated as,",
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

		pegumbel <- function(x, mu, sigma) {
			return(1 - exp(-exp(-((x - mu) / sigma))))
		}

		#' Find the maximum likelihood estimates of the Gumbel distribution
		gumbelFit <- reactive({
			#set initial values for the parameter vector theta=c(mu,sigma)
			theta <- c(mean(dataset()), sd(dataset()))
			return(nlm(gumbel.loglik, theta, hessian=TRUE))
		})
		gumbelParameters <- reactive({gumbelFit()$est})
		gumbelParametersSE <- reactive({
			hess <- gumbelFit()$hessian
			errors <- sqrt(diag(solve(hess)))
			return(errors)
		})

		output$gumbelTablePreamble <- renderUI({
			withMathJax(
				sprintf(
					"For the data you provided, we have found that \\(\\mu=%0.3f%s\\)
					and \\(\\sigma=%0.3f%s\\) (Both values given to 3 decimal places).",
					gumbelParameters()[1],
					ifelse(input$standardErrorGumbel, sprintf(" \\left(%0.3f\\right)", gumbelParametersSE()[1]), ""),
					gumbelParameters()[2],
					ifelse(input$standardErrorGumbel, sprintf(" \\left(%0.3f\\right)", gumbelParametersSE()[2]), "")
				)
			)
		})

		# Create data table of cumulative counts and probabilities
		gumbelTable <- reactive({
			x <- relFreqTable()$x
			prExceedX <- pegumbel(x,
													  mu = gumbelParameters()[1],
													  sigma = gumbelParameters()[2])
			return(data.frame(x = x,
			                  `Probability of exceeding x` = prExceedX,
											  check.names = FALSE))
		})
		output$gumbelTable <- renderTable({
			return(gumbelTable())
		}, include.rownames = FALSE)
		outputOptions(output, "gumbelTable", priority = -2)

		# Create a gumbel plot
		output$gumbelPlot <- renderPlotly({
			d <- allPlotData()[, c("x", "Gumbel")]
			return(plot_ly(d, x = ~x, y = ~Gumbel) %>%
							add_lines(line = list(shape = "spline"),
							          name = "Gumbel",
							          hovertemplate = paste0("Pr(X &#x3e; %{x:.2f} ",
																			         input$dataUnits,
																			         ") = %{y:.4f}")) %>%
							layout(yaxis = list(title = "Probability")))
		})
		outputOptions(output, "gumbelPlot", priority = -2)

		# Find a probability: input slider
		output$gumbelProbabilitySlider <- renderUI({
			validate(need(input$dataIn, ""))
			sliderRange <- range(relFrequencyPlotData()[, 1])
			return(
				sliderInput("gumbelProbabilityInput",
					label = paste(
						"Choose a",
						input$dataType,
						"to find the probability of exceeding it every",
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
								\\right]=%7$0.4f\\text{ (to 4 decimal places).}$$",
							tolower(input$dataType), #1
							input$gumbelProbabilityInput, #2
							input$dataUnits, #3
							input$dataTimeframe, #4
							gumbelParameters()[1], #5
							gumbelParameters()[2], #6
							pegumbel(input$gumbelProbabilityInput,
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

		# Return Standard Errors
		gumbelWallSE <- reactive({
			hess <- solve(gumbelFit()$hessian)
			del <- matrix(c(1, -log(-log(1 - (1 / input$gumbelWallHeightInput)))),
			              ncol = 1, nrow = 2)
			error = sqrt(t(del) %*% hess %*% del)
			return(error)
		})

		output$gumbelWallHeightCalculation <- renderUI({
			return(withMathJax(
				sprintf(
					"$$z_{%3$.0f}=%1$0.3f-%2$0.3f\\log\\left[
						-\\log\\left(1-\\frac{1}{%3$.0f}\\right)
					\\right]=%4$0.2f%5$s\\text{ %6$s (to 2 decimal places).}$$",
					gumbelParameters()[1], #1
					gumbelParameters()[2], #2
					input$gumbelWallHeightInput, #3
					gumbelParameters()[1] - gumbelParameters()[2] * log(-log(1 - (1 / input$gumbelWallHeightInput))), #4
					ifelse(input$standardErrorGumbelWall,
						     sprintf("\\ (%.2f)", gumbelWallSE()), ""), #5
					input$dataUnits #6
				)
			))
		})

		### Generalised Extreme Value Model ###
		output$dataTypeGEV <- renderText({tolower(input$dataType)})

		#' Negative log-likelihood for the GEV model
		GEV.loglik <- function(theta){
			mu <- theta[1]
			sigma <- theta[2]
			xi <- theta[3]
			m <- min((1 + (xi * (dataset() - mu) / sigma)))
			delta <- sqrt(.Machine$double.eps)
			if (m < delta) return(.Machine$double.xmax)
			if (sigma < delta) return(.Machine$double.xmax)
			if(xi == 0) {
				loglik = -length(dataset()) * log(sigma) - sum((dataset() - mu) / sigma) - sum(exp(-((dataset() - mu) / sigma)))
			} else {
				loglik = -length(dataset()) * log(sigma) - (1 / xi + 1) * sum(log(1 + (xi * (dataset() - mu) / sigma))) - sum((1 + (xi * (dataset() - mu) / sigma)) ** (-1 / xi))
			}
			return(-loglik)
		}

		pegev <- function(x, mu, sigma, xi) {
			return(1 - exp(-(1 + xi * ((x - mu) / sigma)) ^ (-1 / xi)))
		}

		#' Find the maximum likelihood estimates of the Gumbel distribution
		gevFit <- reactive({
			#set initial values for the parameter vector theta=c(mu, sigma, xi)
			theta <- c(mean(dataset()), sd(dataset()), 0.1)
			return(nlm(GEV.loglik, theta, hessian=TRUE))
		})
		gevParameters <- reactive({gevFit()$est})
		gevParametersSE <- reactive({
			hess <- gevFit()$hessian
			errors <- sqrt(diag(solve(hess)))
			return(errors)
		})

		output$GEVTablePreamble <- renderUI({
			withMathJax(
				sprintf(
					"For the data you provided, we have found that \\(\\mu=%0.3f%s\\)
					\\(\\sigma=%0.3f%s\\) and \\(\\xi=%0.3f%s\\)
					(All values given to 3 decimal places).",
					gevParameters()[1],
					ifelse(input$standardErrorGEV, sprintf(" \\left(%0.3f\\right)", gevParametersSE()[1]), ""),
					gevParameters()[2],
					ifelse(input$standardErrorGEV, sprintf(" \\left(%0.3f\\right)", gevParametersSE()[2]), ""),
					gevParameters()[3],
					ifelse(input$standardErrorGEV, sprintf(" \\left(%0.3f\\right)", gevParametersSE()[3]), "")
				)
			)
		})

		# Create data table of cumulative counts and probabilities
		GEVTable <- reactive({
			x <- relFreqTable()$x
			prExceedX <- pegev(x,
				                 gevParameters()[1],
				                 gevParameters()[2],
				                 gevParameters()[3])
			return(data.frame(x = x,
			                  `Probability of exceeding x` = prExceedX,
											  check.names = FALSE))
		})
		output$GEVTable <- renderTable({GEVTable()}, include.rownames = FALSE)
		outputOptions(output, "GEVTable", priority = -2)

		# Create a GEV plot
		output$GEVPlot <- renderPlotly({
			d <- allPlotData()[, c("x", "GEV")]
			return(plot_ly(d, x = ~x, y = ~GEV) %>%
							add_lines(line = list(shape = "spline"),
							          name = "GEV",
							          hovertemplate = paste0("Pr(X &#x3e; %{x:.2f} ",
																			         input$dataUnits,
																			         ") = %{y:.4f}")) %>%
							layout(yaxis = list(title = "Probability")))
		})
		outputOptions(output, "GEVPlot", priority = -2)

		# Find a probability: input slider
		output$GEVProbabilitySlider <- renderUI({
			validate(need(input$dataIn, ""))
			sliderRange <- range(relFrequencyPlotData()[, 1])
			return(
				sliderInput("GEVProbabilityInput",
					label = paste(
						"Choose a",
						input$dataType,
						"to find the probability of exceeding it every",
						input$dataTimeframe
					),
					min = max(0, sliderRange[1] - 2),
					max = max(0, sliderRange[2] + 3),
					value = round(runif(1, sliderRange[1], sliderRange[2]), 1),
					step = 0.05
				)
			)
		})
		outputOptions(output, "GEVProbabilitySlider", priority = -2)

		# Find a probability: text description
		output$GEVProbabilityDescription <- renderUI({
			return(
				withMathJax(
					p(
						sprintf(
							"The probability of observing a %1$s greater than
							\\(x=%2$0.2f\\) %3$s every %4$s is given by
							$$\\mathrm{Pr}(X>%2$0.2f)=
								1-\\exp\\left\\{-\\left[1+%7$0.3f
									-\\left(
										\\frac{%2$0.2f-%5$0.3f}{%6$0.3f}
									\\right)\\right]^{-\\frac{1}{%7$0.3f}}
								\\right\\}
								=%8$0.4f\\text{ (to 4 decimal places).}$$",
							tolower(input$dataType), #1
							input$GEVProbabilityInput, #2
							input$dataUnits, #3
							input$dataTimeframe, #4
							gevParameters()[1], #5
							gevParameters()[2], #6
							gevParameters()[3], #7
							pegev(input$GEVProbabilityInput,
								    gevParameters()[1],
								    gevParameters()[2],
										gevParameters()[3]) #8
						)
					)
				)
			)
		})
		outputOptions(output, "GEVProbabilityDescription", priority = -3)

		# Find a wall height
		output$GEVWallHeightInput <- renderText({
			paste(input$GEVWallHeightInput, input$dataTimeframe)
		})
		output$GEVWallHeightP <- renderUI({
			withMathJax(
				paste0("\\(p=", signif(1 / input$GEVWallHeightInput, 4), "\\)")
			)
		})

		# Return Standard Errors
		gevWallSE <- reactive({
			hess <- solve(gevFit()$hessian)
			est <- gevParameters()
			y = -log(1 - (1 / input$GEVWallHeightInput))
			del = matrix(c(1,
			               -(est[3]^(-1)) * (1 - y^(-est[3])),
									   (est[2] * est[3]^(-2) * (1 - y^(-est[3]))) -
										   (est[2] * est[3]^(-1) * y^(-est[3]) * log(y))),
			             ncol = 1, nrow = 3)
			error = sqrt(t(del) %*% hess %*% del)
			return(error)
		})

		output$GEVWallHeightCalculation <- renderUI({
			return(withMathJax(
				sprintf(
					"$$z_{%4$.0f}=%1$0.3f+\\frac{%2$0.3f}{%3$0.3f}
					\\left\\{
						\\left[
							\\log\\left(\\frac{%4$.0f}{%4$.0f-1}\\right)
						\\right]^{-(%3$0.3f)}-1
					\\right\\}=%5$0.2f%6$s\\text{ %7$s (to 2 decimal places).}$$",
					gevParameters()[1], #1
					gevParameters()[2], #2
					gevParameters()[3], #3
					input$gumbelWallHeightInput, #4
					gevParameters()[1] + ((gevParameters()[2] / gevParameters()[3]) * ((log(input$GEVWallHeightInput / (input$GEVWallHeightInput - 1))) ^ (-gevParameters()[3]) - 1)), #5
					ifelse(input$standardErrorGEVWall, sprintf(" (%0.2f)", gevWallSE()[1]), ""), #6
					input$dataUnits #7
				)
			))
		})

		### Normal Distribution ###
		output$dataTypeND <- renderText({tolower(input$dataType)})

		normalParameterMean <- reactive({return(mean(dataset()))})
		normalParameterSD <- reactive({return(sd(dataset()))})

		normal_loglik <- function(theta) {
			mu <- theta[1]
			sigma <- theta[2]
			return(-sum(dnorm(dataset(), mu, theta, log = TRUE)))
		}

		normalFit <- reactive({
			theta <- c(normalParameterMean(), normalParameterSD())
			return(nlm(normal_loglik, theta, hessian = TRUE))
		})

		normalParametersSE <- reactive({
			hess <- normalFit()$hessian
			return(sqrt(diag(solve(hess))))
		})

		output$normalTablePreamble <- renderUI({
			withMathJax(
				sprintf(
					"For the data you provided, we have found that \\(\\mu=%0.3f%s\\)
					and \\(\\sigma=%0.3f%s\\) (Both values given to 3 decimal places).",
					normalParameterMean(),
					ifelse(input$standardErrorNormal,
						     sprintf(" \\left(%0.3f\\right)", normalParametersSE()[1]), ""),
					normalParameterSD(),
					ifelse(input$standardErrorNormal,
					       sprintf(" \\left(%0.3f\\right)", normalParametersSE()[2]), "")
				)
			)
		})

		# Create data table of cumulative counts and probabilities
		normalTable <- reactive({
			x <- relFreqTable()$x
			prExceedX <- 1 - pnorm(x, normalParameterMean(), normalParameterSD())
			return(data.frame(x = x,
			                  `Probability of exceeding x` = prExceedX,
											  check.names = FALSE))
		})

		output$normalTable <- renderTable({normalTable()}, include.rownames = FALSE)
		outputOptions(output, "normalTable", priority = -2)

		# Create a gumbel plot
		output$normalPlot <- renderPlotly({
			d <- allPlotData()[, c("x", "Normal")]
			return(plot_ly(d, x = ~x, y = ~Normal) %>%
							add_lines(line = list(shape = "spline"),
							          name = "Normal",
							          hovertemplate = paste0("Pr(X &#x3e; %{x:.2f} ",
																			         input$dataUnits,
																			         ") = %{y:.4f}")) %>%
							layout(yaxis = list(title = "Probability")))
		})
		outputOptions(output, "normalPlot", priority = -2)

		# Find a probability: input slider
		output$normalProbabilitySlider <- renderUI({
			validate(need(input$dataIn, ""))
			sliderRange <- range(relFrequencyPlotData()[, 1])
			return(
				sliderInput(
					"normalProbabilityInput",
					label = paste(
						"Choose a",
						input$dataType,
						"to find the probability of exceeding it every",
						input$dataTimeframe
					),
					min = max(0, sliderRange[1] - 2),
					max = max(0, sliderRange[2] + 3),
					value = round(runif(1, sliderRange[1], sliderRange[2]), 1),
					step = 0.05
				)
			)
		})
		outputOptions(output, "normalProbabilitySlider", priority = -2)

		# Find a probability: text description
		output$normalProbabilityDescription <- renderUI({
			return(
				withMathJax(
					p(
						sprintf(
							"The probability of observing a %1$s greater than
							\\(x=%2$0.2f\\) %3$s every %4$s is given by
							$$\\mathrm{Pr}(X>%2$0.2f)=
								1-\\Phi\\left(\\frac{%2$0.2f-%5$0.3f}{%6$0.3f}\\right)
								=%7$0.4f\\text{ (to 4 decimal places).}$$",
							tolower(input$dataType), #1
							input$normalProbabilityInput, #2
							input$dataUnits, #3
							input$dataTimeframe, #4
							normalParameterMean(), #5
							normalParameterSD(), #6
							1 - pnorm(input$normalProbabilityInput,
								        normalParameterMean(),
												normalParameterSD()) #7
						)
					)
				)
			)
		})
		outputOptions(output, "normalProbabilityDescription", priority = -3)

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
			return(
				withMathJax(
					sprintf(
						"$$z_{%3$.0f}=%2$0.3f\\times
						\\Phi^{-1}\\left(\\frac{1}{%3$.0f}\\right)+%1$0.3f=
						%4$0.2f\\text{ %5$s (to 2 decimal places).}$$",
						normalParameterMean(), #1
						normalParameterSD(), #2
						input$gumbelWallHeightInput, #3
						qnorm(1 - (1 / input$normalWallHeightInput), normalParameterMean(), normalParameterSD()), #4
						input$dataUnits #5
					)
				)
			)
		})

		### Exponential Model ###
		output$dataTypeExp <- renderText({tolower(input$dataType)})

		expParameterMean <- reactive({return(mean(dataset()))})
		expParameterLambda <- reactive({return(1 / expParameterMean())})
		expParameterSD <- reactive({return(sd(dataset()))})
		expParameterSE <- reactive({
			return(1 / (expParameterMean() * sqrt(dataLength())))
		})

		output$expTablePreamble <- renderUI({
			withMathJax(
				sprintf(
					"For the data you provided, we have found that \\(\\lambda=%0.3f%s\\)
					(Given to 3 decimal places).",
					expParameterLambda(),
					ifelse(input$standardErrorExp,
						     sprintf(" \\left(%0.3f\\right)", expParameterSE()),
								 "")
				)
			)
		})

		# Create data table of cumulative counts and probabilities
		expTable <- reactive({
			x <- relFreqTable()$x
			prExceedX <- 1 - pexp(x, expParameterLambda())
			return(data.frame(x = x,
			                  `Probability of exceeding x` = prExceedX,
											  check.names = FALSE))
		})

		output$expTable <- renderTable({expTable()}, include.rownames = FALSE)
		outputOptions(output, "expTable", priority = -2)

		# Create a gumbel plot
		output$expPlot <- renderPlotly({
			d <- allPlotData()[, c("x", "Exponential")]
			return(plot_ly(d, x = ~x, y = ~Exponential) %>%
							add_lines(line = list(shape = "spline"),
							          name = "Exponential",
							          hovertemplate = paste0("Pr(X &#x3e; %{x:.2f} ",
																			         input$dataUnits,
																			         ") = %{y:.4f}")) %>%
							layout(yaxis = list(title = "Probability")))
		})
		outputOptions(output, "expPlot", priority = -2)

		# Find a probability: input slider
		output$expProbabilitySlider <- renderUI({
			validate(need(input$dataIn, ""))
			sliderRange <- range(relFrequencyPlotData()[, 1])
			return(
				sliderInput(
					"expProbabilityInput",
					label = paste(
						"Choose a",
						input$dataType,
						"to find the probability of exceeding it every",
						input$dataTimeframe
					),
					min = max(0, sliderRange[1] - 2),
					max = max(0, sliderRange[2] + 3),
					value = round(runif(1, sliderRange[1], sliderRange[2]), 1),
					step = 0.05
				)
			)
		})
		outputOptions(output, "expProbabilitySlider", priority = -2)

		# Find a probability: text description
		output$expProbabilityDescription <- renderUI({
			return(
				withMathJax(
					p(
						sprintf(
							"The probability of observing a %1$s greater than
							\\(x=%2$0.2f\\) %3$s every %4$s is given by
							$$\\mathrm{Pr}(X>%2$0.2f)=
								\\exp(-%5$0.3f\\times{}%2$0.2f)
								=%6$0.4f\\text{ (to 4 decimal places).}$$",
							tolower(input$dataType), #1
							input$expProbabilityInput, #2
							input$dataUnits, #3
							input$dataTimeframe, #4
							expParameterLambda(), #5
							1 - pexp(input$expProbabilityInput, expParameterLambda()) #6
						)
					)
				)
			)
		})
		outputOptions(output, "expProbabilityDescription", priority = -3)

		# Find a wall height
		output$expWallHeightInput <- renderText({
			paste(input$expWallHeightInput, input$dataTimeframe)
		})
		output$expWallHeightP <- renderUI({
			withMathJax(
				paste0("\\(p=", signif(1 / input$expWallHeightInput, 4), "\\)")
			)
		})

		# Return Standard Errors
		expWallSE <- reactive({
		  del = -(expParameterMean() ^ 2) * log(input$expWallHeightInput)
		  hess = (expParameterLambda() ^ 2) / dataLength()
		  error = sqrt(t(del) %*% hess %*% del)
		  return(error)
		})

		output$expWallHeightCalculation <- renderUI({
			return(
				withMathJax(
					sprintf(
						"$$z_{%2$.0f}=%1$0.3f
						\\log\\left(%2$.0f\\right)=
						%3$0.2f%4$s\\text{ %5$s (to 2 decimal places).}$$",
						expParameterLambda(), #1
						input$gumbelWallHeightInput, #2
						expParameterMean() * log(input$expWallHeightInput), #3
						ifelse(input$standardErrorExpWall,
							     sprintf(" \\left(%0.2f\\right)", expWallSE()[1]),
									 ""), #4
						input$dataUnits #5
					)
				)
			)
		})

		### Gamma Model ###
		output$dataTypeGM <- renderText({tolower(input$dataType)})

		#' Negative log-likelihood for the Gamma model
		gamma.loglik <- function(theta){
			alpha = theta[1]
			beta = theta[2]

			delta = sqrt(.Machine$double.eps)
			if (alpha < delta) return(.Machine$double.xmax)
			if (beta < delta) return(.Machine$double.xmax)
			loglik = length(dataset()) * alpha * log(beta) -
			           length(dataset()) * log(gamma(alpha)) +
								 (alpha - 1) * sum(log(dataset())) -
								 beta * length(dataset()) * mean(dataset())
			return(-loglik)
		}

		#' Find the maximum likelihood estimates of the Gumbel distribution
		gammaFit <- reactive({
			#set initial values for the parameter vector theta = c(alpha, beta)
			theta <- c(mean(dataset())^2 / sd(dataset())^2,
			           mean(dataset()) / sd(dataset())^2)
			return(nlm(gamma.loglik, theta, hessian = TRUE))
		})
		gammaParameters <- reactive({gammaFit()$est})
		gammaParametersSE <- reactive({
			hess <- gammaFit()$hessian
			errors <- sqrt(diag(solve(hess)))
			return(errors)
		})

		output$gammaTablePreamble <- renderUI({
			withMathJax(
				sprintf(
					"For the data you provided, we have found that \\(\\alpha=%0.3f%s\\)
					and \\(\\beta=%0.3f%s\\)
					(Both values given to 3 decimal places).",
					gammaParameters()[1],
					ifelse(input$standardErrorGamma, sprintf(" \\left(%0.3f\\right)", gammaParametersSE()[1]), ""),
					gammaParameters()[2],
					ifelse(input$standardErrorGamma, sprintf(" \\left(%0.3f\\right)", gammaParametersSE()[2]), "")
				)
			)
		})

		# Create data table of cumulative counts and probabilities
		gammaTable <- reactive({
			x <- relFreqTable()$x
			prExceedX <- 1 - pgamma(x, gammaParameters()[1], gammaParameters()[2])
			return(data.frame(x = x,
												`Probability of exceeding x` = prExceedX,
												check.names = FALSE))
		})
		output$gammaTable <- renderTable({gammaTable()}, include.rownames = FALSE)
		outputOptions(output, "gammaTable", priority = -2)

		# Create a GEV plot
		output$gammaPlot <- renderPlotly({
			d <- allPlotData()[, c("x", "Gamma")]
			return(plot_ly(d, x = ~x, y = ~Gamma) %>%
							add_lines(line = list(shape = "spline"),
												name = "Gamma",
												hovertemplate = paste0("Pr(X &#x3e; %{x:.2f} ",
																							 input$dataUnits,
																							 ") = %{y:.4f}")) %>%
							layout(yaxis = list(title = "Probability")))
		})
		outputOptions(output, "gammaPlot", priority = -2)

		# Find a probability: input slider
		output$gammaProbabilitySlider <- renderUI({
			sliderRange <- range(relFrequencyPlotData()[, 1])
			return(
				sliderInput("gammaProbabilityInput",
					label = paste(
						"Choose a",
						input$dataType,
						"to find the probability of exceeding it every",
						input$dataTimeframe
					),
					min = max(0, sliderRange[1] - 2),
					max = max(0, sliderRange[2] + 3),
					value = round(runif(1, sliderRange[1], sliderRange[2]), 1),
					step = 0.05
				)
			)
		})
		outputOptions(output, "gammaProbabilitySlider", priority = -2)

		# Find a probability: text description
		output$gammaProbabilityDescription <- renderUI({
			return(
				withMathJax(
					p(
						sprintf(
							"The probability of observing a %1$s greater than
							\\(x=%2$0.2f\\) %3$s every %4$s is given by
							$$\\mathrm{Pr}(X>%2$0.2f)=
								\\frac{%6$0.3f^{%5$0.3f}}
								{\\Gamma(%5$0.3f)}
								\\int_{%2$0.2f}^{\\infty}{t^{%5$0.3f-1}
								\\exp\\left(-%6$0.3f t\\right)\\mathop{dt}}
								=%7$0.4f\\text{ (to 4 decimal places).}$$",
							tolower(input$dataType), #1
							input$gammaProbabilityInput, #2
							input$dataUnits, #3
							input$dataTimeframe, #4
							gammaParameters()[1], #5
							gammaParameters()[2], #6
							1 - pgamma(input$gammaProbabilityInput,
								         gammaParameters()[1],
												 gammaParameters()[2]) #7
						)
					)
				)
			)
		})
		outputOptions(output, "gammaProbabilityDescription", priority = -3)

		# Find a wall height
		output$gammaWallHeightInput <- renderText({
			paste(input$gammaWallHeightInput, input$dataTimeframe)
		})
		output$gammaWallHeightP <- renderUI({
			withMathJax(
				paste0("\\(p=", signif(1 / input$gammaWallHeightInput, 4), "\\)")
			)
		})

		output$gammaWallHeightCalculation <- renderUI({
			return(withMathJax(
				sprintf(
					"$$z_{%3$.0f}
					=\\mathrm{F}^{-1}\\left(\\frac{1}{%3$.0f}, %1$0.3f, %2$0.3f\\right)
					=%4$0.2f\\text{ %5$s (to 2 decimal places).}$$",
					gammaParameters()[1], #1
					gammaParameters()[2], #2
					input$gammaWallHeightInput, #3
					qgamma(1 - (1 / input$gammaWallHeightInput),
					       gammaParameters()[1],
								 gammaParameters()[2]), #4
					input$dataUnits #5
				)
			))
		})

		######### Comparison page ###########
		# Create data table of cumulative counts and probabilities
		comparisonTable <- reactive({
			comp <- relFreqTable()
			names(comp)[3] <- "Relative Frequency"
			comp$Gumbel <- gumbelTable()[, 2]
			comp$GEV <- GEVTable()[, 2]
			comp$Normal <- normalTable()[, 2]
			comp$Exponential <- expTable()[, 2]
			comp$Gamma <- gammaTable()[, 2]
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
			include.rownames = FALSE
		)
		outputOptions(output, "comparisonTable", priority = -3)

		# Create the plot
		allPlotData <- reactive({
			x <- seq(from = min(relFreqTable()$x),
               to = max(relFreqTable()$x),
							 by = 0.01)
			d <- tibble(
				x = x,
			  Gumbel = pegumbel(x, gumbelParameters()[1], gumbelParameters()[2]),
				GEV = pegev(x, gevParameters()[1], gevParameters()[2], gevParameters()[3]),
				Normal = 1 - pnorm(x, normalParameterMean(), normalParameterSD()),
				Exponential = 1 - pexp(x, expParameterLambda()),
				Gamma = 1 - pgamma(x, gammaParameters()[1], gammaParameters()[2])
			)
			return(d)
		})

		allGatheredPlotData <- reactive({
			return(gather(allPlotData(),
			              "Gumbel", "GEV", "Normal", "Exponential", "Gamma",
			              key = "Model", value = "Probability"))
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
