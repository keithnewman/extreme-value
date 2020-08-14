# SPDX-Copyright: Copyright (c) 2016 Keith Newman
# SPDX-License-Identifier: GPL-3.0-or-later

#' Shiny app to introduce concepts of frequentist analysis of
#' extreme-value data
#'
#' @author: Keith Newman
#' @contact: knewma@hotmail.com
#' @version: 1.1.0

library(shiny)
shinyUI(
	fluidPage(
		theme = "cosmo.css",
		navbarPage(
			title="Extreme Value Explorer",
			tabPanel("Data Upload",
				fluidRow(
					column(3,
						wellPanel(
							h2("Data Upload"),
							fileInput('dataIn', 'Upload your own data file',
									  accept=c(#'text/csv',
									  		 #'text/comma-separated-values,text/plain',
									  		 #'.csv',
									  		 'text/plain','.txt')),
							#checkboxInput("useExampleDataset", label = p(HTML("&hellip;"), "or check this box to use our example dataset about sea-surge heights measured in feet.", tags$small("Note: this will override any datafile you have uploaded!")), value = FALSE),
							h3("What it your data about?"),
							selectInput("dataType", label = p("Select what your data represents:"),
								choices = list(
									"Sea-surge heights" = "Sea-surge height",
									"Windspeed" = "Windspeed",
									"Rainfall" = "Rainfall",
									"Temperature" = "Temperature"
								),
								selected = "Sea-surge height"
							),
							selectInput("dataUnits", label = p("Select the units for your data:"),
								choices = list(
									`Length (metric)` = c("Metre (m)" = "m", "Centimetre (cm)" = "cm", "Millimetre (mm)" = "mm"),
									`Length (imperial)` = c("Yard (yd)" = "yd", "Feet (ft)" = "ft", "Inch (in)" = "in"),
									`Volume (metric)` = c("Litre (L)" = "L", "Millilitre (ml)" = "ml"),
									`Speed` = c("Metres per second (m/s)" = "m/s", "Miles per hour (mph)" = "mph", "Kilometres per hour (kph)" = "kph"),
									`Temperature` = c("Centigrade/Celcius (\u00B0C)" = "\u00B0C", "Farenheit (\u00B0F)" = "\u00B0F", "Kelvin (K)" = "K")
								),
								selected = "ft"
							),
							selectInput("dataTimeframe", label = p("Select frequently was your data measured:"),
								choices = list(
									`Years` = c(
										"Every decade (10 years)" = "decade",
										"Every 2 years" = "two-year period",
										"Annually (1 Year)" = "year"
									),
									`Months` = c(
										"Every 6 months" = "six months",
										"Quarterly (3 months)" = "three months",
										"Monthly" = "month"
									),
									`Weeks` = c(
										"Every 3 weeks" = "3 weeks",
										"Fortnigtly (2 weeks)" = "fortnight",
										"Weekly" = "week"
									),
									`Daily` = c(
										"Every 2 days" = "two day period",
										"Daily" = "daily"
									),
									`Hours, Minutes, Seconds` = c(
										"Hourly" = "hour",
										"Every 30 minutes" = "30 minutes",
										"Every minute" = "minute",
										"Every 30 seconds" = "30 seconds",
										"Every second" = "second"
									)
								),
								selected = "year"
							)
						)
					),
					column(9,
						h3("Data Preview"),
						p("These are the ", em("observed"), " values found in the data file you uploaded"),
						verbatimTextOutput("datafile"),
						p("Data sorted into ascending order:"),
						verbatimTextOutput("datafileSorted")
					)
				),
				fluidRow(
					column(6,
						h3("Histogram"),
						plotOutput("datafileHistogram")
					),
					column(6,
						h3("Boxplot"),
						plotOutput("datafileBoxplot")
					)
				)
			), # End of Data upload tab
			tabPanel("Relative frequency",
				h1("Relative Frequency"),
				div(
					div(
						h3("How probability is calculated using relative frequency", class = "panel-title"),
						class = "panel-heading"
					),
					div(
						uiOutput("RFtablePreamble"),
						class = "panel-body"
					),
					class = "panel panel-info"
				),
				fluidRow(
					column(4,
						h3("Table of probabilities"),
						tableOutput("RFtable")
					),
					column(8,
						h3("Plot of probabilities"),
						plotOutput("RFplot", height=500)
					)
				),
				fluidRow(
					column(6,
						div(
							div(
								h3("Calculate a probability from this model", class = "panel-primary"),
								class = "panel-heading"
							),
							div(
								wellPanel(uiOutput("RFProbabilitySlider")),
								uiOutput("RFProbabilityDescription"),
								class = "panel-body"
							),
							class = "panel panel-primary"
						)
					),
					column(6,
						div(
							div(
								h3(textOutput("howExtremeText1"), class = "panel-primary"),
								class = "panel-heading"
							),
							div(
								wellPanel(
									sliderInput("RFWallHeightInput",
										label = list(
											h4("Choose how rare the event should be."),
											uiOutput("howExtremeSliderLabel1")
										), min = 2, max = 1000, value = 100
									)
								),
								p("A once in a ",textOutput("RFWallHeightInput",,T)," event corresponds to an exceedance probability ",htmlOutput("RFWallHeightP",T), " (to 4 significant figures)."),
								withMathJax(
									textOutput("howExtremeAnswerPreamble1"),
									uiOutput("RFWallHeightCalculation",T)
								),
								class = "panel-body"
							),
							class = "panel panel-primary"
						)
					)
				)
			), # End of Relative Frequency tab
			tabPanel("Probability Model",
				h1("Probability Model"),
				tabsetPanel(type = "tabs",
					tabPanel("Two-parameter Gumbel Model",
						h2("Two-parameter Gumbel Model"),
						div(
							div(
								h3("How probability is calculated using a probability model", class = "panel-title"),
								class = "panel-heading"
							),
							div(
								withMathJax(
									p("The probability of a", textOutput("dataTypeTPGM",,T), "exceeding a threshold \\(x\\) is given by the formula,$$\\mathrm{Pr}(X>x)=1-\\exp\\left[-\\exp\\left\\{-\\left(\\frac{x-\\mu}{\\sigma}\\right)\\right\\}\\right]\\text{,}$$where:"),
									tags$ul(
										tags$li("\\(\\mu\\) is the ", em("location"), " parameter,"),
										tags$li("\\(\\sigma\\) is the ", em("scale"), " parameter,"),
										tags$li("\\(X\\) is our ", em("random variable"), ","),
										tags$li("\\(x\\) is the ", em("value"), " of our random variable,"),
										tags$li("\\(\\exp\\) is the ", em("exponential function"), ".")
									)
								),
								uiOutput("gumbelTablePreamble"),
								class = "panel-body"
							),
							class = "panel panel-info"
						),
						fluidRow(
							column(4,
								h3("Table of probabilities"),
								tableOutput("gumbelTable")
							),
							column(8,
								h3("Plot of probabilities"),
								plotOutput("gumbelPlot", height=500)
							)
						),
						fluidRow(
							column(6,
								div(
									div(
										h3("Calculate a probability from this model", class = "panel-primary"),
										class = "panel-heading"
									),
									div(
										wellPanel(uiOutput("gumbelProbabilitySlider")),
										uiOutput("gumbelProbabilityDescription"),
										class = "panel-body"
									),
									class = "panel panel-primary"
								)
							),
							column(6,
								div(
									div(
										h3(textOutput("howExtremeText2"), class = "panel-primary"),
										class = "panel-heading"
									),
									div(
										wellPanel(
											sliderInput("gumbelWallHeightInput",
												label = list(
													h4("Choose how rare the event should be."),
													uiOutput("howExtremeSliderLabel2")
												), min = 2, max = 1000, value = 100
											)
										),
										p("A once in a ",textOutput("gumbelWallHeightInput",,T)," event corresponds to an exceedance probability ",htmlOutput("gumbelWallHeightP",T), " (to 4 significant figures)."),
										withMathJax(
											textOutput("howExtremeAnswerPreamble2"),
											uiOutput("gumbelWallHeightCalculation",T)
										),
										class = "panel-body"
									),
									class = "panel panel-primary"
								)
							)
						)
					),
					tabPanel("Normal Distribution",
						h2("Normal Distribution"),
						div(
							div(
								h3("How probability is calculated using a probability model", class = "panel-title"),
								class = "panel-heading"
							),
							div(
								withMathJax(
									p("The probability of a", textOutput("dataTypeND",,T), "exceeding a threshold \\(x\\) is given by the formula,$$\\mathrm{Pr}(X>x)=1-\\Phi\\left(\\frac{x-\\mu}{\\sigma}\\right)=\\frac{1}{\\sqrt{2\\pi}}\\int_{\\frac{x-\\mu}{\\sigma}}^{\\infty}{\\exp\\left(-\\frac{t^2}{2}\\right)\\mathop{dt}}\\text{,}$$where:"),
									tags$ul(
										tags$li("\\(\\mu\\) is the ", em("mean"), " parameter,"),
										tags$li("\\(\\sigma\\) is the ", em("standard deviation"), " parameter,"),
										tags$li("\\(X\\) is our ", em("random variable"), ","),
										tags$li("\\(x\\) is the ", em("value"), " of our random variable,"),
										tags$li("\\(\\exp\\) is the ", em("exponential function"), ","),
										tags$li("\\(\\Phi\\) is the ", em("cumulative distribution function"), "of the standard Normal distribution, which can be found in statistical tables.")
									)
								),
								uiOutput("normalTablePreamble"),
								class = "panel-body"
							),
							class = "panel panel-info"
						),
						fluidRow(
							column(4,
								h3("Table of probabilities"),
								tableOutput("normalTable")
							),
							column(8,
								h3("Plot of probabilities"),
								plotOutput("normalPlot", height=500)
							)
						),
						fluidRow(
							column(6,
								div(
									div(
										h3("Calculate a probability from this model", class = "panel-primary"),
										class = "panel-heading"
									),
									div(
										wellPanel(uiOutput("normalProbabilitySlider")),
										uiOutput("normalProbabilityDescription"),
										class = "panel-body"
									),
									class = "panel panel-primary"
								)
							),
							column(6,
								div(
									div(
										h3(textOutput("howExtremeText3"), class = "panel-primary"),
										class = "panel-heading"
									),
									div(
										wellPanel(
											sliderInput(
												"normalWallHeightInput",
												label = list(
													h4("Choose how rare the event should be."),
													uiOutput("howExtremeSliderLabel3")
												), min = 2, max = 1000, value = 100
											)
										),
										p("A once in a ",textOutput("normalWallHeightInput",,T)," event corresponds to an exceedance probability ",htmlOutput("normalWallHeightP",T), " (to 4 significant figures)."),
										withMathJax(
											textOutput("howExtremeAnswerPreamble3"),
											uiOutput("normalWallHeightCalculation",T)
										),
										class = "panel-body"
									),
									class = "panel panel-primary"
								)
							)
						)
					)
				)
			), # End of Probability Model page
			tabPanel("Comparisons",
				h1("Comparison in results"),
				fluidRow(
					column(12,
						h3("Table of probabilities"),
						tableOutput("comparisonTable")
					),
					column(12,
						h3("Plot of probabilities"),
						plotOutput("comparisonPlot", height = 600),
						tags$ul(
							tags$li("Observed values are represented as black crosses"),
							tags$li("Relative frequency in Gray"),
							tags$li("Gumbel model in Red"),
							tags$li("Normal model in Blue")
						)
					)
				)
			) # End of Comparison tab
		) # End of Navbar
	)
)
