#' Reads an Accident File
#'
#' This is a simple function that reads in a file using \code{readr::read_csv} and
#' converts it to a \code{tibble}.
#' Requires the following packages: \code{readr}, \code{dplyr}
#'
#' @param filename A character string giving the filename
#'
#' @return A \code{tibble} containing the read-in data
#'
#' @examples
#' \dontrun{
#' fars_read("accident_2013.csv")
#' }
#'
#' @export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Makes a Filename for Use in \code{fars_read}
#'
#' This is a simple function that takes in a year (e.g., 2013) or vector of
#' years and creates a text string to use as a filename input to
#' \code{\link{fars_read}}.
#'
#' @param year A year value or vector of such. Will be coerced to an integer.
#'
#' @return A text string or vector of such corresponding to file names.
#'
#' @examples
#' \dontrun{
#' make_filename(2013)
#' make_filename('2013') #NOTE: coerced to integer
#' }
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Reads Datasets for Provided Year(s)
#'
#' Reads a list of files specified by a set of year values, returning a list
#' of data sets
#' Requires the following packages: \code{readr}, \code{dplyr},\code{magrittr}
#'
#' @param years A vector or list of years, presented as text or numbers
#'
#' @return A list of data sets.
#'
#' @examples
#' \dontrun{
#' #These all produce the same output
#' fars_read_years(c(2013,2014))
#' fars_read_years(c('2013','2014'))
#' fars_read_years(list(2013,2014))
#' fars_read_years(list('2013','2014'))
#' }
#'
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' Count Accidents by Provided Year(s)
#'
#' Reads a list of files specified by a set of year values, counts the
#' number of records in each year-month combination, and pivots to
#' wide format with separate columns per year
#' Requires the following packages: \code{readr}, \code{dplyr}, \code{tidyr},
#' \code{magrittr}
#'
#' @param years A vector or list of years, presented as text or numbers
#'
#' @return A table with one row per month and columns corresponding to the
#' month and each provided year, where the latter columns contain the number
#' of accidents in that month in that year.
#'
#' @examples
#' \dontrun{
#' #These all produce the same output
#' fars_summarize_years(c(2013,2014))
#' fars_summarize_years(c('2013','2014'))
#' fars_summarize_years(list(2013,2014))
#' fars_summarize_years(list('2013','2014'))
#' }
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Plot Accidents for Provided Year
#'
#' Reads a file specified by a year value and plots accidents in the state
#' Requires the following packages: \code{readr}, \code{dplyr}, \code{tidyr},
#' \code{maps}, \code{graphics}, \code{magrittr}
#' If a state number is not contained in the file, this function will throw
#' an error.
#' If no accidents are available for the state provided, this function will
#' print a message and return \code{NULL}
#'
#' @param state.num A state number; accepts text or number but is coerced
#' to number
#' @param year A year; accepts text or number but is coerced to number
#'
#' @return A plot of accidents in the state from the provided year
#'
#' @examples
#' \dontrun{
#' #These all produce the same output
#' fars_map_state(2013)
#' fars_map_state('2013')
#' }
#'
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
