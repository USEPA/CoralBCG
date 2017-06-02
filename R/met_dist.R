#' Get distribution estimates
#' 
#' Get distribution estimates for BCG metrics by level
#'  
#' @param met_in input \code{data.frame} of coral metrics
#' @param scr_in input \code{data.frame} of site BCG scores, see details
#'
#' @details The input \code{scr_in} data has two columns labelled \code{station_code} and \code{scr}. The station codes should match those in \code{met_in}. The \code{scr} values for each station can be numeric or character string BCG levels that represent qualitative rankings. 
#' 
#' BCG levels for each metric are based on the raw metric distributions for the a priori expert rankings of each site. Distributions for each metric at each level are recreated from normal distributions, where the mean and standard deviation of each distribution are based on maximum likelihoods estimates from the raw data. 
#' 
#' It is assumed that there is a 100\% chance a raw metric value can be assigned to one BCG level across the range of raw values. The probabilities across the range are based on scaled density functions of the raw metrics at each BCG level, where the density functions are scaled such that their sum is always equal to one.  See the plot from \code{\link{plot_fuzz}}. 
#' 
#' @return A two-element list named \code{met_in} and \code{pars}, where the former is the joined input data with BCG scores in \code{scr_in} and the latter is a \code{data.frame} with parameter (mean and standard deviation) estimates for each metric at each BCG level.
#' 
#' @import dplyr
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
#' # get levels
#' met_dist(met_in, scr_in)
#' } 
met_dist <- function(met_in, scr_in, n = 1000){

  # station code matches for metrics and scores
  mtch <- met_in$station_code %in% scr_in$station_code

  # sanity checks
  if(sum(mtch) == 0)
    stop('No station_code matches between metrics and scores')
  
  # join bcg scores with metric data
  met_in <- filter(met_in, mtch) %>% 
    left_join(scr_in,  by = 'station_code') %>% 
    gather('var', 'val', -station_code, -scr) 

  # get levels
  pars <- met_in %>% 
    group_by(var, scr) %>% 
    nest  %>% 
    mutate(

      ests = purrr::map(data, function(x){
        
        ests <- MASS::fitdistr(x$val, 'normal')$estimate
        data.frame(est = c('mu', 'sd'), val = ests)

      })
    ) %>% 
    select(-data) %>% 
    unnest %>% 
    spread(est, val)

  # output
  out <- list(met_in = met_in, pars = pars)
  return(out)
  
}
        
