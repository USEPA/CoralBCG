#' Get probability distributions
#'
#' Get probability distributions from raw metrics
#' 
#' @param met_in input \code{data.frame} of coral metrics
#' @param scr_in input \code{data.frame} of site BCG scores, see details
#' @param n numeric indicating number of values across the range for each metric to predict the density curves
#'
#' @details The input \code{scr_in} data has two columns labelled \code{station_code} and \code{scr}. The station codes should match those in \code{met_in}. The \code{scr} values for each station can be numeric or character string BCG levels that represent qualitative rankings. 
#' 
#' The density curves are based on maximum-likelihood estimates of the mean and standard deviation for a normal curve corresponding to the raw metric data at each BCG level.
#' 
#' @return A two-element list named \code{met_in} and \code{dests}, where the former is the joined input data with BCG scores in \code{scr_in} and the latter is estimated density curves from the raw distributions of each. 
#'  
#' @seealso \code{\link{plot_fuzz}}
#' 
#' @import dplyr ggplot2
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' # coral metrics
#' met_in <- get_stony_mets(crl_dem)
#' 
#' # bcg scores for each station
#' station_code <- c(1:5)
#' scr <- c(2, 5, 3, 2, 4)
#' scr_in <- data.frame(station_code, scr) 
#' 
#' # get probs
#' get_probs(met_in, scr_in)
#' } 
get_probs <- function(met_in, scr_in, n = 1000){

  # station code matches for metrics and scores
  mtch <- met_in$station_code %in% scr_in$station_code

  # sanity checks
  if(sum(mtch) == 0)
    stop('No station_code matches between metrics and scores')
  
  # join bcg scores with metric data
  met_in <- filter(met_in, mtch) %>% 
    left_join(scr_in,  by = 'station_code') %>% 
    gather('var', 'val', -station_code, -scr) 

  # get density function estimates for bcg levels within metrics
  # return data.frame in long format of probabilities of each metric
  dests <- met_in %>% 
    group_by(var) %>% 
    mutate(
      minv = min(val, na.rm = T), 
      maxv = max(val, na.rm = T)
      ) %>% 
    group_by(var, scr) %>% 
    nest %>% 
    mutate(
      
      ests = purrr::map(data, function(x){
        
        maxv <- unique(x$maxv)
        minv <- unique(x$minv)
        ests <- MASS::fitdistr(x$val, 'normal')$estimate
        vals <- seq(minv, maxv, length = n)
        out <- dnorm(vals, ests[1], ests[2]) %>% 
          data.frame(vals = vals, pr = .)
        
        return(out)
        
      })
    ) %>% 
    select(var, scr, ests) %>% 
    unnest %>% 
    mutate(pr = ifelse(is.infinite(pr), 0, pr))
  
  # output
  out <- list(met_in = met_in, dests = dests)
  return(out)
  
}
        

  