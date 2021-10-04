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
library(shinyWidgets)
library(plotly)
shinyUI(
	navbarPage(
		theme = "cerulean.css",
		title = "Extreme Value Explorer",
		id = "navbar-pages",
		collapsible = TRUE,
		inverse = TRUE,
		tabPanel("Data Upload",
			useShinyjs(),
	  	singleton(tags$head(
	  	  tags$link(rel = "stylesheet",
	  	            href = "https://use.fontawesome.com/releases/v5.6.1/css/solid.css",
	  	            integrity = "sha384-aj0h5DVQ8jfwc8DA7JiM+Dysv7z+qYrFYZR+Qd/TwnmpDI6UaB3GJRRTdY8jYGS4",
	  	            crossorigin = "anonymous"),
	  	  tags$link(rel = "stylesheet",
	  	            href = "https://use.fontawesome.com/releases/v5.6.1/css/fontawesome.css",
	  	            integrity = "sha384-WK8BzK0mpgOdhCxq86nInFqSWLzR5UAsNg0MGX9aDaIIrFWQ38dGdhwnNCAoXFxL",
	  	            crossorigin = "anonymous"),
				tags$script(src = "error_message.js")
			)),
			fluidRow(
				column(3,
					wellPanel(
						h2("Data Upload"),
						radioGroupButtons(inputId = "dataInputType",
					                    label = "Data input method",
														  choices = c(`File upload` = "file",
																				  `Manual entry` = "manual",
																				  `Demo data` = "demo"),
															justified = TRUE,
															checkIcon = list(
													      yes = tags$i(class = "fa fa-check-square"))),
						conditionalPanel(condition = 'input.dataInputType == "demo"',
							selectInput(inputId = "demoData",
													label = "Select a demo dataset",
													choices = c(
														"Wave Height"           = "Wave Height",
														"Rainfall"              = "Rainfall",
														"Sunshine"              = "Sunshine",
														"Degassing Burst Force" = "Degassing Burst Force",
														"Temperature"           = "Temperature",
														"Snow Depth"            = "Snow Depth",
														"Windspeed"             = "Windspeed"
													),
													selected = "Wave Height")
						),
						conditionalPanel(condition = 'input.dataInputType != "demo"',
							conditionalPanel(condition = 'input.dataInputType == "file"',
								h3("File Upload"),
								fileInput('dataIn', 'Upload your own data file',
											    accept = c(#'text/csv',
													#'text/comma-separated-values,text/plain',
													#'.csv',
													'text/plain','.txt')),
							  pickerInput(
								  inputId = "sepControl",
								  label = HTML('Separator
    								<a tabindex="0" data-toggle="popover" class="text-info" title="Separator" data-html="true" data-trigger="hover" data-placement="auto right" data-content="
    								<p>Select what character is used to separate values in the data file that you upload.
    								Multi-space will assume the separator is any amount of &lsquo;white space&rsquo;, that is one or more spaces, tabs, newlines or carriage returns.
    								If one of your columns contains text that has spaces, but this text is not enclosed in quotation marks, you may see an error message of &ldquo;More columns than column names&rdquo;.
    								This can be fixed by enclosing the strings in quotation marks or using tab or comma seperation in your datafile.</p>
    								<p>You may have to override this setting to one of the following:</p>
    								<ul>
    									<li>Comma (&quot;,&quot;)</li>
    									<li>Space (&quot;&nbsp;&quot;)</li>
    									<li>Semi-colon (&quot;;&quot;)</li>
    									<li>Tab (&quot;&#9;&quot;)</li>
    								</ul>
    								"><i class="fas fa-question-circle"></i></a>'),
								  choices = c(`Multi-space` = "",
								              Comma = ',',
								              Space = " ",
								              Semicolon = ';',
								              Tab = "\t"),
								  choicesOpt = list(subtext = c("[\\s]+",
								                                "[,]",
								                                "[\\s]",
								                                "[;]",
								                                "[\\t]")),
								  selected = ""
								)
							),
							h3("What is your data about?"),
							selectInput("dataType", label = p("What does your data represent?"),
								choices = c("Wave height",
								            "Windspeed",
								            "Rainfall",
								            "Snowfall",
								            "Sunshine",
								            "Temperature",
								            "Force",
								            "Power",
								            "Energy"),
								selected = "Wave height"
							),
							selectInput("dataTimeframe", label = p("How frequently was your data measured?"),
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
						), # End of file upload conditional panel
						selectInput(
							inputId = "dataUnits",
							label = p("What units are your data measured in?"),
							choices = list(
								`Length (metric)` = c("Kilometre (km)" = "km",
																			"Metre (m)" = "m",
								                      "Centimetre (cm)" = "cm",
								                      "Millimetre (mm)" = "mm"),
								`Length (imperial)` = c("Inch (in)" = "in",
								                        "Feet (ft)" = "ft",
																				"Yard (yd)" = "yd",
																				"Mile" = "mile",
																			  "Nautical miles" = "nm"),
								`Volume (metric)` = c("Litre (L)" = "L",
								                      "Millilitre (ml)" = "ml"),
								`Speed` = c("Metres per second (m/s)" = "m/s",
								            "Miles per hour (mph)" = "mph",
								            "Kilometres per hour (kph)" = "kph"),
								`Temperature` = c("Centigrade/Celcius (\u00B0C)" = "\u00B0C",
								                  "Farenheit (\u00B0F)" = "\u00B0F",
								                  "Kelvin (K)" = "K"),
								`Weight and force` = c("Milligrams" = "mg",
							                         "Grams" = "g",
																		   "Kilograms" = "kg",
																		   "Metric ton" = "tonne",
																		   "Tons (Imperial)" = "ton",
																		   "Newtons" = "N"),
								`Pressure` = c("Pascals" = "Pa",
							                 "Pounds per square-inch" = "psi",
							                 "Bar" = "bar",
							                 "Millibar" = "mb"),
								`Time` = c("Milliseconds" = "ms",
							             "Seconds" = "s",
												   "Minutes" = "min",
												   "Hours" = "h",
												   "Days" = "days",
												 	 "Weeks" = "weeks",
												   "Months" = "months",
												   "Years" = "years",
												   "Jiffy" = "jiffy"),
								`Frequency` = c("Hertz" = "Hz"),
								`Energy` = c("Joules" = "J",
							               "Kilojoules" = "kJ",
														 "Megajoules" = "MJ",
													   "(Small/gram) calories" = "cal",
													   "(Large/food) Calories" = "kcal",
													   "British thermal unit" = "BTU"),
								`Power` = c("Watts" = "W",
								            "Kilowatts" = "kW",
							              "Megawatts" = "MW",
													  "GigaWatts" = "GW",
													  "Volts" = "V",
													  "Millivolts" = "mV",
													  "Amps" = "A",
													  "Milliamps" = "mA",
													  "Ohms (Resistance)" = "\u2126",
													  "Horsepower" = "hp",
													  "Brake horsepower" = "bhp",
													  "Pferdest\u00E4rke (PS)" = "PS"),
								`Torque` = c("Pound-foot (lb-ft)" = "lb-ft",
							               "Newton-metres" = "Nm")
							),
							selected = "ft"
						)
					)
				),
				column(9,
				  # An error message box that can display problems in the data.
				  tags$div(class = "alert alert-danger hidden",
				           id = "data-danger-message",
				           role = "alert",
				           p("ERROR: ", tags$span(id = "data-danger-summary"))),
					conditionalPanel(condition = 'input.dataInputType == "manual"',
						fluidRow(column(12,
							div(
								div(
									h2("Manual data input", class = "panel-title"),
									class = "panel-heading"
								),
								div(
									textAreaInput(inputId = "manualDataInput",
									              label = "Enter data",
														    value = "",
													      width = "100%",
													      placeholder = 'Type values separated by commas "," or spaces " "...'),
									actionButton("submitManualData",
															 "Submit",
														   class = "btn-primary"),
									actionButton("clearManualData",
								               "Clear",
														   class = "btn-default"),
								  HTML('<div class="alert alert-warning hidden" role="alert" id="input-error" style="margin-top:1em;">
												 <p>Warning: Some of the input values were not valid numbers. These have been removed.</p>
											 </div>'),
									class = "panel-body"
								),
								class = "panel panel-primary"
							)
						))
					),
					h3("Data Preview"),
					conditionalPanel(condition = 'input.dataInputType == "demo"',
						conditionalPanel(condition = "input.demoData == 'Wave Height'",
														 p("This is a data set of annual maximum daily wave heights (feet), from 1955", HTML("&ndash;"), "2004 (inclusive), recorded in Shell Beach, Louisiana."),
														 HTML('<iframe src="https://www.google.com/maps/embed?pb=!1m18!1m12!1m3!1d625421.7674940665!2d-89.84933232026187!3d30.008162466602613!2m3!1f0!2f0!3f0!3m2!1i1024!2i768!4f13.1!3m3!1m2!1s0x889e6b15cc0c6a27%3A0xdac7de4e803b3709!2sShell+Beach%2C+LA+70085%2C+USA!5e0!3m2!1sen!2suk!4v1468844454013" height="450" frameborder="0" style="border:0;width:100%" allowfullscreen></iframe>')),
						conditionalPanel(condition = "input.demoData == 'Rainfall'",
														 p("This is a data set of annual maximum daily rainfall totals (mm), from 1991", HTML("&ndash;"), "2011 (inclusive), recorded in in Eskdale, Lake District."),
														 HTML('<iframe src="https://www.google.com/maps/embed?pb=!1m18!1m12!1m3!1d74286.42234500349!2d-3.3119922913158697!3d54.41967578624567!2m3!1f0!2f0!3f0!3m2!1i1024!2i768!4f13.1!3m3!1m2!1s0x487cc9e3c7209ed5%3A0xc9d13abd308ed866!2sRiver+Esk!5e0!3m2!1sen!2suk!4v1460108413731" height="450" frameborder="0" style="border:0;width:100%;" allowfullscreen></iframe>')),
						conditionalPanel(condition = "input.demoData == 'Sunshine'",
														 p("This is a data set of annual maximum daily total hours of sunshine, from 1983", HTML("&ndash;"), "2007 (inclusive), recorded in Santiago de Compostela, Spain."),
														 HTML('<iframe src="https://www.google.com/maps/embed?pb=!1m18!1m12!1m3!1d23389.198462767123!2d-8.562279219643163!3d42.880235063533604!2m3!1f0!2f0!3f0!3m2!1i1024!2i768!4f13.1!3m3!1m2!1s0xd2efe44e2dd71a7%3A0xe0146888c087e311!2sSantiago+de+Compostela%2C+A+Coru%C3%B1a%2C+Spain!5e0!3m2!1sen!2suk!4v1460108546059" height="450" frameborder="0" style="border:0;width:100%;" allowfullscreen></iframe>')),
						conditionalPanel(condition = "input.demoData == 'Degassing Burst Force'",
														 p("This is a data set of annual maximum force (kg) produced by seismic degassing bursts, recorded in Kilauea Volcano, Hawaii."),
														 HTML('<iframe src="https://www.google.com/maps/embed?pb=!1m18!1m12!1m3!1d572357.9624850417!2d-155.39885165508423!3d19.53699818350608!2m3!1f0!2f0!3f0!3m2!1i1024!2i768!4f13.1!3m3!1m2!1s0x7953da0a55051cf5%3A0x94923da0477efc91!2sKilauea+Volcano!5e0!3m2!1sen!2suk!4v1460108682893" height="450" frameborder="0" style="border:0;width:100%;" allowfullscreen></iframe>')),
						conditionalPanel(condition = "input.demoData == 'Temperature'",
														 p("This is a data set of annual maximum daily temperatures (\u00B0C), from 1881", HTML("&ndash;"), "2012 (inclusive), recorded in St. Petersburg, Russia."),
														 HTML('<iframe src="https://www.google.com/maps/embed?pb=!1m18!1m12!1m3!1d2421837.017553923!2d27.29723716719637!3d60.10152812360904!2m3!1f0!2f0!3f0!3m2!1i1024!2i768!4f13.1!3m3!1m2!1s0x4696378cc74a65ed%3A0x6dc7673fab848eff!2sSt+Petersburg%2C+Russia!5e0!3m2!1sen!2suk!4v1460109147240" height="450" frameborder="0" style="border:0;width:100%;" allowfullscreen></iframe>')),
						conditionalPanel(condition = "input.demoData == 'Snow Depth'",
														 p("This is a data set of annual maximum daily snow depths (cm), from 1961", HTML("&ndash;"), "2012 (inclusive), recorded in Falun, Sweden."),
														 HTML('<iframe src="https://www.google.com/maps/embed?pb=!1m18!1m12!1m3!1d421799.8548247323!2d15.467781997783542!3d60.58702561878502!2m3!1f0!2f0!3f0!3m2!1i1024!2i768!4f13.1!3m3!1m2!1s0x466764d555730ac5%3A0x8d6ff2a697970634!2sFalun%2C+Sweden!5e0!3m2!1sen!2suk!4v1460108971628" height="450" frameborder="0" style="border:0;width:100%;" allowfullscreen></iframe>')),
						conditionalPanel(condition = "input.demoData == 'Windspeed'",
														 p("This is a data set of annual maximum daily maximum windspeeds (m/s), from 1971", HTML("&ndash;"), "2015 (inclusive), recorded in Bamberg, Germany."),
														 HTML('<iframe src="https://www.google.com/maps/embed?pb=!1m18!1m12!1m3!1d785351.6963817229!2d10.781706581133726!3d49.7167268651942!2m3!1f0!2f0!3f0!3m2!1i1024!2i768!4f13.1!3m3!1m2!1s0x47a2230538d4beb5%3A0x41db728f061d980!2sBamberg%2C+Germany!5e0!3m2!1sen!2suk!4v1460109059151" height="450" frameborder="0" style="border:0;width:100%;" allowfullscreen></iframe>')),
					),
					p("These are the ", em("observed"), " values found in the data file you uploaded"),
					verbatimTextOutput("datafile"),
					p("Data sorted into ascending order:"),
					verbatimTextOutput("datafileSorted")
				)
			),
			fluidRow(
				column(12,
					h3("Summary Table (", textOutput("dataUnit", inline = TRUE),")"),
					tableOutput("SummaryTable"),
					h3("Exploratory plots"),
					plotlyOutput("exploratoryPlots")
				)
			)
		), # End of Data upload tab
		tabPanel(
		  "Relative frequency",
		  probabilityModelUI("RelativeFrequency", "Relative Frequency")
		), # End of Relative Frequency tab
		tabPanel("Probability Model",
			h1("Probability Model"),
			tabsetPanel(type = "tabs", id = "model-tabs",
			  tabPanel(
			    "Two-parameter Gumbel Model",
			    probabilityModelUI("Gumbel", "Two-parameter Gumbel Model")
			  ),
				tabPanel(
				  "Generalised Extreme Value Model",
				   probabilityModelUI("GEV", "Generalised Extreme Value Model")
				),
				tabPanel(
				  "Normal Model",
				   probabilityModelUI("Normal", "Normal Model")
				),
				tabPanel(
				  "Exponential Model",
				   probabilityModelUI("Exponential", "Exponential Model")
				),
				tabPanel(
				  "Gamma Model",
				   probabilityModelUI("Gamma", "Gamma Model")
				)
			)
		), # End of Probability Model page
		tabPanel("Comparisons",
			h1("Comparison of models"),
			fluidRow(
				column(12,
					h3("Table of probabilities"),
					tableOutput("comparisonTable")
				),
				column(12,
					h3("Plot of probabilities"),
					plotlyOutput("comparisonPlot")
				)
			)
		) # End of Comparison tab
	) # End of Navbar
)
