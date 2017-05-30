#' Split metrics
#'
#' Split metrics into BCG levels for calibration
#' 
#' @param met_in input \code{data.frame} of coral metrics
#' @param splt \code{list} of split values for each metric from 0 to 1
#' @param bcgs \code{list} of BCG levels for each metric
#' @param both logical if a \code{data.frame} is returned in long format that includes raw and bcg levels fore ach metric
#'
#' @details The inputs for \code{splt} and {bcgs} are separate lists where each element is named as a metric in \code{met_in}.  Numeric values for for each elemene tin \code{splt} must be monotonic in the range from and including zero to one.  These values are converted to the scale of each metric for assigning BCG levels.  Numeric values in each element of \code{bcgs} are the BCG levels assigned to each split in \code{splt}, with total length of each element equal to the lenght of each element in \code{splt} - 1.    
#' 
#' @return A \code{data.frame} with metric values converted to BCG levels.  This will have the same dimensions as \code{met_in} unless \code{both = TRUE}. \code{NA} values are returned for non-unique metrics.  
#' 
#' @import dplyr
#' 
#' @export
#'
#' @examples
#' library(dplyr)
#' 
#' # get metrics, number of metrics, and number of bcg levels
#' met_in <- get_stony_mets(crl_dem) 
#' nmets <- ncol(met_in) - 1
#' nbcgs <- 4
#' 
#' # setup splits, these may differ by metric
#' splt <- rep(seq(0, 1, length = nbcgs + 1), times = nmets) %>% 
#'  matrix(nrow = nbcgs + 1) %>% 
#'  data.frame %>% 
#'  as.list
#' names(splt) <- names(met_in)[-1]
#' 
#' # setup bcg levels, one minus splts, reverse for bleached, diseased
#' # this approach can include non-monotonic change
#' bcgs <- rep(seq(nbcgs + 1, 2), times = nmets) %>% 
#'  matrix(ncol = nmets) %>% 
#'  data.frame
#' names(bcgs) <- names(met_in)[-1]
#' bcgs[, names(bcgs) %in% c('bleached', 'diseased')] <- seq(2, nbcgs + 1)
#' bcgs <- as.list(bcgs)
#' 
#' split_mets(met_in, splt = splt, bcgs = bcgs)
split_mets <- function(met_in, splt, bcgs, both = FALSE){
  
  # put in long format, nest
  tosplt <- gather(met_in, 'var', 'val', -station_code) %>% 
    group_by(var) %>% 
    tidyr::nest()

  # make sure splt is correct 
  if(!any(names(splt) %in% tosplt$var))
    stop('names in splt must equal metric names in met_in')

  # put splt in long format, nest
  splt <- as.data.frame(splt) %>% 
    tidyr::gather('var', 'val') %>% 
    group_by(var) %>% 
    tidyr::nest()
  
  # put bcg in long format, nest
  bcgs <- as.data.frame(bcgs) %>% 
    tidyr::gather('var', 'val') %>% 
    group_by(var) %>% 
    tidyr::nest()

  # reassign metric values to bcg levels based on splts
  out <- left_join(tosplt, splt, by = 'var') %>% 
    left_join(., bcgs, by = 'var') %>% 
    split(.$var) %>% 
    purrr::map(function(x){ 
   
      # get bcg, values, rescale brks by range of values
      bcgs <- x$data[[1]]$val
      vals <- x$data.x[[1]]$val
      rngs <- range(vals, na.rm = TRUE)
      brks <- x$data.y[[1]]$val %>% 
        scales::rescale(rngs)

      # step out if no unique values
      if(length(unique(vals)) == 1) 
        bcg <- rep(NA, length(vals))
      else 
        bcg <- cut(vals, breaks = brks, labels = bcgs, include.lowest = TRUE)

      # cut, assign bcg level
      bcg <- bcg %>% 
        as.character %>%
        as.numeric %>% 
        data.frame(
          x$data.x[[1]], bcg = .
          )
  
      return(bcg)
      
    }) %>% 
    do.call('rbind', .) %>% 
    tibble::rownames_to_column(var = 'var') %>% 
    mutate(var = gsub('\\.[0-9]+', '', var))

  # return raw and bcg levels if TRUE
  if(both) return(out)

  # otherwise only bcg levels for metrics
  out <- select(out, -val) %>% 
    tidyr::spread(var, bcg)
  
  return(out)

}