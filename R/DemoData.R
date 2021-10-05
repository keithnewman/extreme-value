#' R6 class for storing the Demo datasets

library(R6)
library(assertthat)

DemoData <- R6Class(
  "DemoData",
  inherit = Dataset,
  public = list(
    description = NULL,
    mapUrl = NULL,
    initialize = function(datasetName) {
      assertthat::assert_that(datasetName %in% base::names(private$demodata))
      super$initialize(private$demodata[[datasetName]]$data,
                       private$demodata[[datasetName]]$units)
      self$name <- datasetName
      self$type <- private$demodata[[datasetName]]$type
      self$description <- private$demodata[[datasetName]]$description
      self$mapUrl <- private$demodata[[datasetName]]$mapUrl
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
        type = "Wave height",
        description = "This is a data set of annual maximum daily wave heights (feet), from 1955&ndash;2004 (inclusive), recorded in Shell Beach, Louisiana.",
        mapUrl = "https://www.google.com/maps/embed?pb=!1m18!1m12!1m3!1d625421.7674940665!2d-89.84933232026187!3d30.008162466602613!2m3!1f0!2f0!3f0!3m2!1i1024!2i768!4f13.1!3m3!1m2!1s0x889e6b15cc0c6a27%3A0xdac7de4e803b3709!2sShell+Beach%2C+LA+70085%2C+USA!5e0!3m2!1sen!2suk!4v1468844454013"
      ),
      Rainfall = list(
        data = c(333,213,790,343,351,521,307,305,352,277,319,319,339,262,
                 285,297,327,620,350,545,258),
        units = "mm",
        type = "Rainfall",
        description = "This is a data set of annual maximum daily rainfall totals (mm), from 1991&ndash;2011 (inclusive), recorded in in Eskdale, Lake District.",
        mapUrl = "https://www.google.com/maps/embed?pb=!1m18!1m12!1m3!1d74286.42234500349!2d-3.3119922913158697!3d54.41967578624567!2m3!1f0!2f0!3f0!3m2!1i1024!2i768!4f13.1!3m3!1m2!1s0x487cc9e3c7209ed5%3A0xc9d13abd308ed866!2sRiver+Esk!5e0!3m2!1sen!2suk!4v1460108413731"
      ),
      Sunshine = list(
        data = c(12.8,10.7,11.9,13.1,13.3,10.7,11.1,11.9,12.1,11.3,12.5,
                 12.1,12.0,12.4,11.8,10.5,10.9,10.1,11.7,12.7,10.6,11.6,9.9,
                 9.6,12.0),
        units = "h",
        type = "Sunshine",
        description = "This is a data set of annual maximum daily total hours of sunshine, from 1983&ndash;2007 (inclusive), recorded in Santiago de Compostela, Spain.",
        mapUrl = "https://www.google.com/maps/embed?pb=!1m18!1m12!1m3!1d23389.198462767123!2d-8.562279219643163!3d42.880235063533604!2m3!1f0!2f0!3f0!3m2!1i1024!2i768!4f13.1!3m3!1m2!1s0xd2efe44e2dd71a7%3A0xe0146888c087e311!2sSantiago+de+Compostela%2C+A+Coru%C3%B1a%2C+Spain!5e0!3m2!1sen!2suk!4v1460108546059"
      ),
      `Degassing Burst Force` = list(
        data = c(99983,100067,99905,100367,99980,99970,
                 100086,99988,99912,100084,100432,99921,
                 100123,99913,100240,100448,100162,100296,
                 100015,100122,99930,99997,100710,100136,
                 99946,99918,99962,100052),
        units = "kg",
        type = "Force",
        description = "This is a data set of annual maximum force (kg) produced by seismic degassing bursts, recorded in Kilauea Volcano, Hawaii.",
        mapUrl = "https://www.google.com/maps/embed?pb=!1m18!1m12!1m3!1d572357.9624850417!2d-155.39885165508423!3d19.53699818350608!2m3!1f0!2f0!3f0!3m2!1i1024!2i768!4f13.1!3m3!1m2!1s0x7953da0a55051cf5%3A0x94923da0477efc91!2sKilauea+Volcano!5e0!3m2!1sen!2suk!4v1460108682893"
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
        type = "Temperature",
        description = "This is a data set of annual maximum daily temperatures (\u00B0C), from 1881&ndash;2012 (inclusive), recorded in St. Petersburg, Russia.",
        mapUrl = "https://www.google.com/maps/embed?pb=!1m18!1m12!1m3!1d2421837.017553923!2d27.29723716719637!3d60.10152812360904!2m3!1f0!2f0!3f0!3m2!1i1024!2i768!4f13.1!3m3!1m2!1s0x4696378cc74a65ed%3A0x6dc7673fab848eff!2sSt+Petersburg%2C+Russia!5e0!3m2!1sen!2suk!4v1460109147240"
      ),
      `Snow Depth` = list(
        data = c(33,29,24,20,50,91,70,54,80,63,31,34,33,13,14,23,78,52,
                 48,36,47,73,26,56,63,61,59,69,32,30,46,31,19,42,51,25,
                 26,34,47,34,31,48,32,38,22,50,28,18,47,64,68),
        units = "cm",
        type = "Snowfall",
        description = "This is a data set of annual maximum daily snow depths (cm), from 1961&ndash;2012 (inclusive), recorded in Falun, Sweden.",
        mapUrl = "https://www.google.com/maps/embed?pb=!1m18!1m12!1m3!1d421799.8548247323!2d15.467781997783542!3d60.58702561878502!2m3!1f0!2f0!3f0!3m2!1i1024!2i768!4f13.1!3m3!1m2!1s0x466764d555730ac5%3A0x8d6ff2a697970634!2sFalun%2C+Sweden!5e0!3m2!1sen!2suk!4v1460108971628"
      ),
      Windspeed = list(
        data = c(340,240,220,273,266,250,270,250,242,189,269,212,230,249,
                 188,248,201,227,201,278,190,293,231,268,257,185,201,226,
                 310,210,191,223,182,229,226,237,256,212,250,219,243,241,
                 196,202,251),
        units = "m/s",
        type = "Windspeed",
        description = "This is a data set of annual maximum daily maximum windspeeds (m/s), from 1971&ndash;2015 (inclusive), recorded in Bamberg, Germany.",
        mapUrl = "https://www.google.com/maps/embed?pb=!1m18!1m12!1m3!1d785351.6963817229!2d10.781706581133726!3d49.7167268651942!2m3!1f0!2f0!3f0!3m2!1i1024!2i768!4f13.1!3m3!1m2!1s0x47a2230538d4beb5%3A0x41db728f061d980!2sBamberg%2C+Germany!5e0!3m2!1sen!2suk!4v1460109059151"
      )
    ) # End of demo dataset list
  )
)