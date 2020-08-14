# SPDX-Copyright: Copyright (c) 2016 Keith Newman
# SPDX-License-Identifier: GPL-3.0-or-later

#' Shiny app to introduce concepts of frequentist analysis of
#' extreme-value data
#'
#' @author: Keith Newman
#' @contact: knewma@hotmail.com
#' @version: 1.0.0

library(shiny)
library(ggplot2)

shinyServer(
	function(input, output) {

		# Uploaded data is going in its own environment so that it can be extracted
		# from the function when being uploaded.
		# TODO: Look for a better way of handling this scoping issue.
		dataProvided <- new.env()
		dataRead <- reactive({
			# For example dataset.  Not stable enough
			# Consider a hyperlink to where example datasets are hosted?
#~ 			if (input$useExampleDataset) {
#~ 				input$dataIn <- TRUE
#~ 				assign("datafile", c(1:20), envir=dataProvided)
#~ 				return(dataProvided$datafile)
#~ 			}

			# input$control will be NULL initially. After the user selects
			# and uploads a file, it will be a data frame with 'name',
			# 'size', 'type', and 'datapath' columns. The 'datapath'
			# column will contain the local filenames where the data can
			# be found.
			uploadedControlFile <- input$dataIn

			validate(need(input$dataIn, "Please upload a dataset using the menu on the left."))
			if (is.null(uploadedControlFile))
				return(NULL)

			assign("datafile",
				as.numeric(
					unlist(
						read.table(uploadedControlFile$datapath, sep = input$sepControl)
					)
				),
				envir = dataProvided
			)
			return(dataProvided$datafile)
		})
		output$datafile <- renderPrint({dataRead()})

		# Sorted version of the input data
		output$datafileSorted <- renderPrint({
			validate(
				need(
					input$dataIn,
					"Please upload a dataset using the menu on the left."
				)
			)
			return(sort(dataProvided$datafile))
		})

		# Break points needed for plotting histogram and Relative Frequency table
		dataPrettyBreaks <- reactive({
			return(pretty(dataProvided$datafile, 2 * nclass.FD(dataProvided$datafile)))
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
					"Sea-surge height" = p("How high should we build a wall to protect from a \"once in a ",HTML("&hellip;")," ", input$dataTimeframe, " storm\"?"),
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

		# Summary plots of input data.  Histogram and boxplot.
		output$datafileHistogram <- renderPlot({
			validate(
				need(input$dataIn, "Please upload a dataset to view the histogram.")
			)
			#hist(dataProvided$datafile, main = "Histogram of maxima", xlab = "Maxima values")
			return(
				qplot(dataProvided$datafile,
					main = paste("Histogram of", tolower(input$dataType), "maxima"),
					xlab = paste0(input$dataType, " maxima (", input$dataUnits, ")"),
					ylab = "Frequency"
				) +
				stat_bin(
					geom = "histogram",
					breaks = dataPrettyBreaks(),
					colour = "black",
					fill = "white"
				) +
				theme_bw()
			)
		})
		output$datafileBoxplot <- renderPlot({
			validate(
				need(input$dataIn, "Please upload a dataset to view the boxplot.")
			)
			#boxplot(dataProvided$datafile, main = "Boxplot of maxima", xlab = "Maxima values", horizontal = TRUE)
			return(
				qplot(
					factor(0),
					dataProvided$datafile,
					geom = "boxplot",
					main = paste("Boxplot of", tolower(input$dataType), "maxima"),
					ylab = paste0(input$dataType, " maxima (", input$dataUnits, ")"),
					xlab = " "
				) + coord_flip() + theme_bw()
			)
		})
		outputOptions(output, "datafileHistogram", priority = -1)
		outputOptions(output, "datafileBoxplot", priority = -1)

		####### Relative frequencies page ###########
		output$RFtablePreamble <- renderUI({
			validate(
				need(input$dataIn, "Please upload a dataset to see this information!")
			)
			withMathJax(
				paste0(
					"There are \\(n=",
					length(dataProvided$datafile),
					"\\) observations in the dataset you provided. ",
					"Therefore the probability of exceeding ",
					"\\(x\\) is $$\\mathrm{Pr}(X>x)=\\frac{\\text{Number of observations exceeding }x}{\\text{Total number of observations } (n=",
					length(dataProvided$datafile), ")}$$")
				)
		})

		# Create data table of cumulative counts and probabilities
		makeRelFreqTable <- function (d) {
			prettyBreakInt <- diff(dataPrettyBreaks())[[1]]
			relFreq <- data.frame(
				x = seq(
					min(dataPrettyBreaks()) - 2 * prettyBreakInt,
					max(dataPrettyBreaks()) + 3 * prettyBreakInt,
					prettyBreakInt
				)
			)
			#relFreq <- data.frame(x = seq(floor(min(d) - 1), ceiling(max(d) + 1), 0.5))
			relFreq$`Number of observations exceeding x` <- apply(relFreq, 1, function(a) sum(d > a))
			relFreq$`Probability of exceeding x` <- relFreq[, 2] / length(d)
			return(relFreq)
		}
		relFreqTable <- reactive({makeRelFreqTable(dataProvided$datafile)})
		output$RFtable <- renderTable({
			validate(need(input$dataIn, ""))
			return(relFreqTable())
		}, include.rownames = FALSE)
		outputOptions(output, "RFtable", priority = -1, suspendWhenHidden = FALSE)

		# Create data for the plot
		makeRelFreqPlotData <- function (d) {
			h = c(min(d) - 0.4, d, max(d) + 2)
			relFreqPlot <- data.frame(
				Height = h,
				Probability = sapply(h, function(x) sum(d > x) / length(d))
			)
			return(relFreqPlot)
		}
		relFreqPlotData <- reactive({makeRelFreqPlotData(dataProvided$datafile)})
		relFreqPlotData2 <- reactive({  # For points on the plot
			relFreqPlotData()[-c(1, nrow(relFreqPlotData())), ]  # Remove fake endpoints
		})
		output$RFplot <- renderPlot({
			validate(need(input$dataIn, ""))
			return(
				ggplot(relFreqPlotData(), aes(Height, Probability)) +
				geom_step(colour = "grey") +
				geom_point(
					aes(Height, Probability),
					relFreqPlotData2(),
					size = 4,
					shape = 4
				) +
				scale_x_continuous(paste0(input$dataType, " (", input$dataUnits, ")")) +
				theme_bw()
			)
		})

		# Find a probability: input slider
		output$RFProbabilitySlider <- renderUI({
			validate(need(input$dataIn, ""))
			sliderRange <- range(relFreqPlotData()[, 1])
			return(
				sliderInput("RFProbabilityInput",
					label = h4(
						"Choose a ",
						input$dataType,
						" to find the probability of exceeding it every ",
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
			validate(need(input$RFProbabilityInput, ""))
			exceedances <- sum(dataProvided$datafile > input$RFProbabilityInput)
			return(withMathJax(p(paste0("We have seen ", exceedances," instances when the ", tolower(input$dataType)," has exceeded ", input$RFProbabilityInput, " ", input$dataUnits, " in our ", length(dataProvided$datafile)," observations.  Therefore, the probability of observing a ", tolower(input$dataType), " greater than \\(x=",input$RFProbabilityInput,"\\) ", input$dataUnits, " every ", input$dataTimeframe," is given by $$\\mathrm{Pr}(X>",input$RFProbabilityInput,")=\\frac{", exceedances, "}{", length(dataProvided$datafile), "}=",signif(exceedances / length(dataProvided$datafile), 4),"\\text{ (to 4 significant figures).}$$"))))
		})
		outputOptions(
			output, "RFProbabilityDescription", priority = -3, suspendWhenHidden = TRUE
		)

		# Find a wall height
		output$RFWallHeightInput <- renderText({
			paste(input$RFWallHeightInput, input$dataTimeframe)
		})
		output$RFWallHeightP <- renderUI({
			withMathJax(
				paste0("\\(p=", signif(1 / input$RFWallHeightInput, 4), "\\)")
			)
		})
		output$RFWallHeightCalculation <- renderUI({
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
						dataProvided$datafile[length(dataProvided$datafile) - ceiling(length(dataProvided$datafile) / input$RFWallHeightInput) + 1],
						"\\text{ ", input$dataUnits," (to 2 decimal places).}$$"
					)
				)
			)
		})
		outputOptions(output, "RFplot", priority = -1, suspendWhenHidden = FALSE)

		####### Probability model page ###########
		### Two parameter Gumbel Model ###
		output$dataTypeTPGM <- renderText({tolower(input$dataType)})

		calculateGumbelParameters <- function(dataset) {
			#set initial values for the parameter vector theta=c(mu,sigma)
			theta <- c(mean(dataset), sd(dataset))
			gumbel.loglik <- function(theta){
				mu <- theta[1]
				sigma <- theta[2]
				loglik <- -length(dataset) * log(sigma) - sum(exp(-((dataset - mu) / sigma))) - sum((dataset - mu) / sigma)
				return(-loglik)
			}
			return(nlm(gumbel.loglik, theta)$est)
		}
		gumbelParameters <- reactive({calculateGumbelParameters(dataProvided$datafile)})

		output$gumbelTablePreamble <- renderUI({
			validate(
				need(input$dataIn, "Please upload a dataset to see more information!")
			)
			withMathJax(
				paste0(
					"For the data you provided, we have found that \\(\\mu=",
					round(gumbelParameters()[1], 3),
					"\\) and \\(\\sigma=",
					round(gumbelParameters()[2], 3),
					"\\) (Both values given to 3 decimal places)."
				)
			)
		})

		# Create data table of cumulative counts and probabilities
		makeGumbelTable <- function (d) {
			gumb <- data.frame(x = relFreqTable()$x)
			gumb$`Probability of exceeding x` <- sapply(
				gumb[, 1],
				function(x) {
					return(
						1 - exp(-exp(-((x-gumbelParameters()[1]) / gumbelParameters()[2])))
					)
				}
			)
			return(gumb)
		}
		gumbelTable <- reactive({makeGumbelTable(dataProvided$datafile)})
		output$gumbelTable <- renderTable({
			validate(need(input$dataIn, ""))
			return(gumbelTable())
		}, include.rownames = FALSE)
		outputOptions(
			output, "gumbelTable", priority = -2, suspendWhenHidden = FALSE
		)

		# Create the plot
		output$gumbelPlot <- renderPlot({
			validate(need(input$dataIn, ""))
			return(
				ggplot(data.frame(x=range(relFreqPlotData()[, 1])), aes(x)) +
				stat_function(
					fun = function(x) {
						return(
							1-exp(-exp(-((x-gumbelParameters()[1]) / gumbelParameters()[2])))
						)
					}
				) +
				scale_x_continuous(paste0(input$dataType, " (", input$dataUnits, ")")) +
				scale_y_continuous("Probability density") +
				theme_bw()
			)
		})
		outputOptions(
			output, "gumbelPlot", priority = -2, suspendWhenHidden = FALSE
		)

		# Find a probability: input slider
		output$gumbelProbabilitySlider <- renderUI({
			validate(need(input$dataIn, ""))
			sliderRange <- range(relFreqPlotData()[, 1])
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
			validate(need(input$gumbelProbabilityInput, ""))
			return(
				withMathJax(
					p(
						paste0(
							"The probability of observing a ",
							tolower(input$dataType),
							" greater than \\(x=",input$gumbelProbabilityInput,"\\) ",
							input$dataUnits,
							" every ", input$dataTimeframe," is given by $$\\mathrm{Pr}(X>",
							input$gumbelProbabilityInput,
							")=1-\\exp\\left[-\\exp\\left\\{-\\left(\\frac{",
							input$gumbelProbabilityInput,
							"-",
							round(gumbelParameters()[1], 3),
							"}{",
							round(gumbelParameters()[2], 3),
							"}\\right)\\right\\}\\right]=",
							signif(1-exp(-exp(-((input$gumbelProbabilityInput-gumbelParameters()[1]) / gumbelParameters()[2]))), 4),
							"\\text{ (to 4 significant figures).}$$"
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
			validate(
				need(
					input$dataIn,
					"This calculation is unavailable as no data has been uploaded!"
				)
			)
			return(
				paste0(
					"$$x=",
					round(gumbelParameters()[1], 3),
					"-",
					round(gumbelParameters()[2], 3),
					"\\log\\left[-\\log\\left(1-\\frac{1}{",
					input$gumbelWallHeightInput,
					"}\\right)\\right]=",
					round(gumbelParameters()[1]-gumbelParameters()[2]*log(-log(1-(1 / input$gumbelWallHeightInput))), 2),
					"\\text{ ",
					input$dataUnits,
					" (to 2 decimal places).}$$"
				)
			)
		})

		### Normal Distribution ###
		output$dataTypeND <- renderText({tolower(input$dataType)})

		normalParameterMean <- reactive({return(mean(dataProvided$datafile))})
		normalParameterSD <- reactive({return(sd(dataProvided$datafile))})

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
		normalTable <- reactive({makeNormalTable(dataProvided$datafile)})
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
				ggplot(data.frame(x=range(relFreqPlotData()[, 1])), aes(x)) +
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
			sliderRange <- range(relFreqPlotData()[, 1])
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
				ggplot(data.frame(x=range(relFreqPlotData()[, 1])), aes(x)) +
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
					data = relFreqPlotData(), aes(Height, Probability), colour="grey"
				) +
				geom_point(  # Observations
					aes(Height, Probability),
					data = relFreqPlotData2(),
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
