#' R6 parent class for common methods relating to an extreme value model.
#'
#'
#' 

library(R6)
library(assertthat)
library(purrr)

RelativeFrequency <- R6Class(
  "RelativeFrequency",
  inherit = EVModel,
  public = list(
    
    initialize = function(data) {
      self$setData(data)
      private$DISTRIBUTION_NAME = "Relative Freqency"
    },
    
    description = function(dataType = private$data$type) {
      withMathJax(
        sprintf(
          "There are \\(n=%1$d\\) observations in the dataset you provided.
          Therefore the probability %2$s of exceeding \\(x\\) is
          $$\\mathrm{Pr}(X>x)=\\frac{\\text{Number of observations exceeding }x}
          {\\text{Total number of observations } (n=%1$d)}$$",
          private$n,
          tolower(dataType)
        )
      )
    },
    
    exceedanceProb = function(x) {
      return(sum(private$data$getData() > x) / private$n)
    },
    
    fittedParameterDescription = function(showStandardError = TRUE) {
      # No parameters to fit in this model.
      return(NULL)
    },
    
    logLikelihood = function(theta, negative = FALSE) {
      return(0)
    },
    
    plotData = function() {
      return(private$makePlotData())
    },
    
    plotData2 = function() {
      return(private$makePlotData2())
    },
    
    # @override
    plotly = function(units = private$data$units) {
      plot_ly(private$makePlotData(),
              x = ~x,
              y = ~Probability) %>%
        add_lines(line = list(shape = "hv"),
                  name = private$DISTRIBUTION_NAME,
                  hovertemplate = paste0("Pr(X &#x3e; %{x:.2f} ",
                                         units,
                                         ") = %{y:.4f}")) %>%
        add_markers(data = private$makePlotData2(),
                    y = ~Zeros,
                    name = "Observations",
                    hovertemplate = paste("%{x:.2f}", units)) %>%
        layout(yaxis = list(title = "Probability"),
               hovermode = "x unified")
    },
    
    probabilityCalcEquation = function(x,
                                       timeframe,
                                       units = private$data$units,
                                       dataType = private$data$type) {
      exceedances <- sum(private$data$getData() > x)
      withMathJax(
        p(
          sprintf(
            "We have seen %1$i instances when the %2$s has exceeded %3$.2f %4$s in
    				our %6$i observations. Therefore, the probability of observing a %2$s
    				greater than \\(x=%3$.2f\\) %4$s every %5$s is given by
    				$$\\mathrm{Pr}(X>x=%3$.2f)=\\frac{%1$i}{%6$i}=%7$0.4f
    				\\text{ (to 4 decimal places).} $$",
            exceedances, #1
            tolower(dataType), #2
            x, #3
            units, #4
            timeframe, #5
            private$n, #6
            self$exceedanceProb(x) #7
          )
        )
      )
    },
    
    returnLevelEquation = function(r,
                                   standardError = TRUE,
                                   units = private$data$units) {
      return(
        withMathJax(
          sprintf(
            "$$x=%0.2f\\text{ %s (to 2 decimal places).}$$",
            self$returnLevel(r),
            units
          )
        )
      )
    },
    
    returnLevel = function(r) {
      return(private$data$getData()[private$n - ceiling(private$n / r) + 1])
    },
    
    returnLevelSE = function(r) {
      stop("Method not available for Gamma model!")
    },
    
    setData = function(newData) {
      super$setData(newData)
      private$n <- private$data$length()
    },
    
    tableOfProbabilities = function() {
      tab <- tibble::tibble(x = private$findPrettyBreaks()) %>%
        mutate(
          `Number of observations exceeding x` = map_int(x, private$observationsExceedingX),
          `Probability of exceeding x` = map_dbl(x, self$exceedanceProb)
        )
      return(tab)
    }
  ),
  private = list(
    n = NULL,
    makePlotData = function() {
      LEFT_PADDING = 0.4
      RIGHT_PADDING = 2.0
      x = c(min(private$data$getData()) - LEFT_PADDING,
            private$data$sortData(),
            max(private$data$getData()) + RIGHT_PADDING)
      
      relFreqData <- tibble(x = x) %>%
        mutate(Probability = map_dbl(x, self$exceedanceProb))
      return(relFreqData)
    },
    
    # The actual data points with the two dummy endpoints removed.
    makePlotData2 = function() {
      tibble(x = private$data$getData(), Zeros = 0)
    },
    
    observationsExceedingX = function(x) {
      sum(private$data$getData() > x)
    },
    
    optimiseIfNeeded = function() {
      # No need to optimise this model
      invisible(NULL)
    }
  )
)