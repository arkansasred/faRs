#' Read in FARS data
#'
#' @note error returned if file does not exist
#'
#' @param filename A character string giving the path to the file containing the FARS data
#'
#' @return This function returns a tibble of FARS data
#'
#' @examples \dontrun file<-make_filename(2014); fars_2014<-fars_read(file)
#'
#' @importFrom readr read_csv
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

#' Make filename for FARS data in a given year
#'
#' @param year integer of year
#'
#' @return This function returns a filename to be used with fars_read
#'
#' @examples \dontrun file<-make_filename(2014); fars_2014<-fars_read(file)
#'
#' @export

make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Read in multiple years of FARS data
#'
#' @note NULL returned if no data for any input years
#'
#' @param years a vector of integer years, if data for any years don't exist, no data will be returned
#'
#' @return this function returns a tibble with data from all years of interest
#'
#' @examples \dontrun fars<-fars_read_years(c(2012,2013,2014))
#'
#' @importFrom dplyr mutate select
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

#' Summarize record counts by year
#'
#' @inheritParams years
#'
#' @return This function returns a tibble with summaries of record counts by month for each Year of interest
#'
#' @examples \dontRun fars<-fars_summarize_years(c(2012,2013,2014))
#'
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#'
#' @export

fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Make a map of accidents in a state
#'
#' This function will produce a map of accidents within a state for a given year
#'
#' @note Error returned if invalid state.num, NULL returned if no accidents occured in a given state/year
#'
#' @inheritParams year
#' @param state.num integer reperesenting state of interest
#'
#' @return This function returns a graphics object that is a map of accidents within the
#' given state for the given year.  If there is no data for that year or the state number is invalid, the function
#' will return a NULL.
#'
#' @examples \dontrun fars_map_state(1,2014)
#'
#' @importFrom maps map
#' @importFrom dplyr filter
#' @importFrom graphics points
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
