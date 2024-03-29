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
      assertthat::assert_that(length(unique(newData)) > 1,
                              msg = "All inputted values are identical!")
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
