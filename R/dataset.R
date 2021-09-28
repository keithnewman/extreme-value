#' R6 class for storing the dataset.
#'
#'
#' 

library(R6)
library(tibble)
library(dplyr)
library(plotly)
library(assertthat)

Dataset <- R6Class("Dataset",
  public = list(
    units = NULL,
    name = "",
    type = "",
    
    initialize = function(data, units) {
      self$setData(data)
      self$setUnits(units)
      invisible(self)
    },
    
    length = function() {
      return(base::length(self$getData()))
    },
    
    getData = function() {
      return(private$data$x)
    },
    
    sortData = function(decreasing = FALSE) {
      return(base::sort(private$data$x, decreasing = decreasing))
    },
    
    summaryTable = function() {
      # Find min, LQ, median, UQ and max in one go
      summaryQuant <- quantile(self$getData())
      
      summary_ <- data.frame(l   = self$length(),
                            Mean = mean(self$getData()),
                            sd   = sd(self$getData()),
                            min  = summaryQuant[1],
                            lq   = summaryQuant[2],
                            med  = summaryQuant[3],
                            uq   = summaryQuant[4],
                            max  = summaryQuant[5])
      
      colnames(summary_) <- c("Number of Observations",
                             "Mean",
                             "Standard Deviation",
                             "Minimum Observation",
                             "Lower Quartile",
                             "Median",
                             "Upper Quartile",
                             "Maximum Observation")
      return(summary_)
    },
    
    summaryPlotly = function() {
      xlabel <- paste0(self$type, " maxima (", self$units, ")")
      hist_ <- plot_ly(private$data, x = ~x, type = "histogram", name = xlabel) %>%
        layout(xaxis = list(title = xlabel),
               yaxis = list(title = "Frequency",
                            showline = TRUE,
                            showticklabels = TRUE,
                            showgrid = TRUE))
      boxp_ <- plot_ly(private$data, x = ~x, type = "box", name = xlabel) %>%
        layout(xaxis = list(title = xlabel,
                            showticklabels = TRUE,
                            showgrid = TRUE),
               yaxis = list(title = "",
                            showline = FALSE,
                            showticklabels = FALSE,
                            showgrid = FALSE))
      ss <- subplot(hist_,
                    boxp_,
                    nrows = 2,
                    heights = c(0.8, 0.2),
                    shareX = TRUE,
                    titleY = TRUE)
      return(layout(ss, title = xlabel, showlegend = FALSE))
    },
    
    setData = function(newData) {
      assertthat::assert_that(is.numeric(newData),
                              base::length(newData) >= 2)
      private$data <- tibble::tibble(x = newData)
      invisible(private$data)
    },
    
    setUnits = function(units) {
      assertthat::assert_that(is.character(units),
                              assertthat::is.scalar(units))
      self$units <- units
      invisible(self$units)
    }
    
    
  ),
  private = list(
    data = NULL
  )
)

DemoData <- R6Class("DemoData",
  inherit = Dataset,
  public = list(
    initialize = function(datasetName) {
      assertthat::assert_that(datasetName %in% base::names(private$demodata))
      super$initialize(private$demodata[[datasetName]]$data,
                       private$demodata[[datasetName]]$units)
      self$name <- datasetName
      self$type <- private$demodata[[datasetName]]$type
      invisible(self)
    }
  ),
  private = list(
    demodata = list(
      `Wave Height` = list(
        data = c(8.5,8.9,9.1,8.9,8.4,9.7,9.1,9.6,8.7,9.3,9.6,9.3,8.7,
                 9.0,8.8,8.9,8.9,12.2,7.8,7.7,8.3,8.1,7.3,6.8,6.7,7.3,
                 7.6,8.2,8.6,9.8,9.5,7.4,7.3,10.2,10.3,10.4,8.8,9.7,
                 10.0,10.8,11.1,12.7,11.5,11.8,12.6,13.0,10.5,10.5,
                 10.0,9.4),
        units = "ft",
        type = "Wave height"
      ),
      Rainfall = list(
        data = c(333,213,790,343,351,521,307,305,352,277,319,319,339,262,
                 285,297,327,620,350,545,258),
        units = "mm",
        type = "Rainfall"
      ),
      Sunshine = list(
        data = c(12.8,10.7,11.9,13.1,13.3,10.7,11.1,11.9,12.1,11.3,12.5,
                 12.1,12.0,12.4,11.8,10.5,10.9,10.1,11.7,12.7,10.6,11.6,9.9,
                 9.6,12.0),
        units = "h",
        type = "Sunshine"
      ),
      `Degassing Burst Force` = list(
        data = c(99983,100067,99905,100367,99980,99970,
                 100086,99988,99912,100084,100432,99921,
                 100123,99913,100240,100448,100162,100296,
                 100015,100122,99930,99997,100710,100136,
                 99946,99918,99962,100052),
        units = "kg",
        type = "Force"
      ),
      Temperature = list(
        data = c(29.2,27.1,26.1,31.3,32.0,29.8,26.1,29.5,27.4,29.8,29.8,31.6,
                 26.9,29.5,31.7,28.4,30.9,29.2,29.1,31.2,25.2,27.0,23.7,31.3,
                 29.9,29.9,29.6,27.5,31.0,30.3,32.3,28.2,32.0,29.5,30.0,32.5,
                 30.3,28.9,31.7,28.7,28.1,26.1,27.9,29.9,30.9,30.3,25.2,29.2,
                 29.4,29.9,31.6,30.5,27.3,29.7,33.1,31.0,32.3,28.7,32.2,32.2,
                 27.6,27.6,29.7,29.5,29.5,28.6,30.9,28.6,27.7,28.9,28.7,29.7,
                 30.4,29.6,31.3,30.3,30.9,31.2,31.4,31.8,26.3,32.9,28.2,27.8,
                 28.6,31.2,29.4,29.6,33.0,28.8,33.6,30.9,28.5,29.2,28.4,31.0,
                 28.1,28.2,30.3,29.8,28.4,28.9,30.4,33.5,29.5,27.9,31.5,31.4,
                 27.4,27.8,31.9,26.8,32.0,31.0,28.8,29.8,34.6,33.7,30.2,32.9,
                 31.2,31.6,28.3,29.8,34.3,32.8,29.9,26.8,37.1,31.5,32.6),
        units = "\u00B0C",
        type = "Temperature"
      ),
      `Snow Depth` = list(
        data = c(33,29,24,20,50,91,70,54,80,63,31,34,33,13,14,23,78,52,
                 48,36,47,73,26,56,63,61,59,69,32,30,46,31,19,42,51,25,
                 26,34,47,34,31,48,32,38,22,50,28,18,47,64,68),
        units = "cm",
        type = "Snowfall"
      ),
      Windspeed = list(
        data = c(340,240,220,273,266,250,270,250,242,189,269,212,230,249,
                 188,248,201,227,201,278,190,293,231,268,257,185,201,226,
                 310,210,191,223,182,229,226,237,256,212,250,219,243,241,
                 196,202,251),
        units = "m/s",
        type = "Windspeed"
      )
    ) # End of demo dataset list
  )
)

DataFromString <- R6Class(
  "DataFromString",
  inherit = Dataset,
  public = list(
    initialize = function(dataString, units) {
      assertthat::assert_that(is.character(dataString), is.character(units))
      super$initialize(
        # Split by any amount of commas, semi-colon, whitespace and newline
        na.omit(as.numeric(strsplit(dataString, "[/,/;/ \n]+")[[1]])),
        units = units
      )
      invisible(self)
    }
  )
)

DataFromFile <- R6Class(
  "DataFromFile",
  inherit = Dataset,
  public = list(
    initialize = function(file, sep = ",", units = "", ...) {
      assertthat::assert_that(assertthat::is.readable(file),
                              is.character(sep),
                              is.character(units))
      super$initialize(
        # Split by any amount of commas, semi-colon, whitespace and newline
        na.omit(as.numeric(unlist(read.table(datafile$datapath,
                                             sep = input$sepControl,
                                             ...)))),
        units = units
      )
      invisible(self)
    }
  )
)
