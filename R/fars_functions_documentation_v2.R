library(readr)
library(dplyr)
library(tidyr)
library(maps)
library(magrittr)

#' Read csv file into tibble
#'
#' This function allows to read data from any csv file.
#' Specifically, however, the function is used to read data from the US National Highway
#' Traffic Safety Administration's Fatality Analysis Reporting System (FARS), which is a
#' nationwide census providing the American public yearly data regarding fatal
#' injuries suffered in motor vehicle traffic crashes.
#'
#' @param filename A character string with the name of the file.
#'
#' @return This function returns the data read in from the file as a tibble,
#'     a data frame with class tbl_df.
#'
#' @note The file needs to be in the current working directory.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @examples
#' fars_read("accident_2013.csv.bz2")
#' \dontrun{
#'   fars_read("anyFile.csv")
#' }
#'
#' @export
fars_read <- function(filename) {
  filename <- system.file("extdata", filename, package = "farsvis")
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Create filename for FARS data for specific year
#'
#' This function creates a specific filename for a year to later read FARS data for
#' the particular year stored in a file with this filename.
#'
#' @param year A numeric or character string specifying a year.
#'
#' @return This function returns a character string with the name of the file. The
#'     filename is created by inserting the year into the string
#'     "accident_\code{year}.csv.bz2".
#'
#' @note The year needs to be between 2013 and 2015.
#'
#' @examples
#' \dontrun{
#'   make_filename(2014)
#'   make_filename("2013")
#' }
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Read files with FARS data for several years
#'
#' This function allows to read FARS data from files for one or more years.
#'
#' @param years A single numeric or character string specifying a year or
#'     a vector of numerics or character strings of year(s).
#'
#' @return This function returns a list of tibbles. Each tibble containts FARS data
#'     for one year stored in individual files.
#'
#' @importFrom dplyr mutate select
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#'   fars_read_years(2015)
#'   fars_read_years(c(2013, 2014))
#'   fars_read_years(c("2014", "2013"))
#' }
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select_(.dots=c('MONTH', 'year'))
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' Generate a summary of accidents per month and year from FARS data
#'
#' This function generates a summary of accidents per month and year from FARS data
#' by reading in the file(s) with the FARS data for one or more years.
#'
#' @param years A single numeric or character string specifying a year or
#'     a vector of numerics or character strings of year(s).
#'
#' @return This function returns a tibble with a row for each month and columns
#'     for each year. The entries in the cell correspond to the number of
#'     accidents in that month and year.
#'
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#' @importFrom magrittr %>%
#'
#' @examples
#' fars_summarize_years(2015)
#' fars_summarize_years(c(2013, 2014))
#' fars_summarize_years(c("2014", "2013"))
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by_(~year, ~MONTH) %>%
    dplyr::summarize_(n = ~n()) %>%
    tidyr::spread_(key_col = 'year', value_col = 'n')
}

#' Display a map by US state depicting location of accidents based on FARS data
#'
#' This function displays a map by US state. The map depicts the location of
#'     accidents based on FARS data, with a dot for each accident.
#'
#' @param state.num Number of the US state.
#' @param year A single numeric or character string specifying a year.
#'
#' @return This function returns a map with the location of accidents based on
#'     the FARS data for a specific US state and year. The map contains a dot
#'     for each accident.
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @examples
#' fars_map_state(1, 2013)
#' fars_map_state(5, "2014")
#'
#' @export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter_(data, ~STATE == state.num)
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
