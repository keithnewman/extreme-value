#' R6 class for reading a dataset from an uploaded file.

library(R6)
library(assertthat)

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
