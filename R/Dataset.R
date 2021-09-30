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
    
    summaryPlotly = function(units = self$units, dataType = self$type) {
      xlabel <- paste0(dataType, " maxima (", units, ")")
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
      invisible(private$data$x)
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

DataFromString <- R6Class(
  "DataFromString",
  inherit = Dataset,
  public = list(
    initialize = function(dataString, units) {
      self$setData(dataString)
      super$setUnits(units)
      invisible(self)
    },
    
    setData = function(dataString) {
      assertthat::assert_that(is.character(dataString))
      invisible(
        super$setData(
          na.omit(as.numeric(strsplit(dataString, "[/,/;/ \n]+")[[1]]))
        )
      )
    }
  )
)

DataFromFile <- R6Class(
  "DataFromFile",
  inherit = Dataset,
  public = list(
    initialize = function(file, sep = ",", units = "", ...) {
      self$setData(file, sep, units, ...)
      super$setUnits(units)
      invisible(self)
    },
    
    setData = function(file, sep = ",", units = "", ...) {
      assertthat::assert_that(assertthat::is.readable(file),
                              is.character(sep))
      invisible(
        super$setData(
          na.omit(as.numeric(unlist(
            read.table(datafile$datapath, sep = input$sepControl, ...)
          )))
        )
      )
    }
  )
)
