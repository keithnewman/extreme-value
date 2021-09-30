#' R6 class for creating a dataset from a user inputted string.

library(R6)
library(assertthat)

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
