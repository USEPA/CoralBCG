#' Get estimated BCG levels
#' 
#' Get estimated BCG levels from fuzzy membership functions
#'  
#' @param met_in input \code{data.frame} of coral metrics
#' @param scr_in input \code{data.frame} of site BCG scores, see details
#' @param all_out logical if probabilities for all BCG levels are returned for each metric, otherwise the single highest probability is returned.  
#'
#' @details The input \code{scr_in} data has two columns labelled \code{station_code} and \code{scr}. The station codes should match those in \code{met_in}. The \code{scr} values for each station can be numeric or character string BCG levels that represent qualitative rankings. 
#' 
#' BCG levels for each metric are based on the raw metric distributions for the a priori expert rankings of each site. Distributions for each metric at each level are recreated from normal distributions, where the mean and standard deviation of each distribution are based on maximum likelihoods estimates from the raw data. 
#' 
#' It is assumed that there is a 100\% chance a raw metric value can be assigned to one of several BCG levels across the range of raw values. The probabilities across the range are based on scaled density functions of the raw metrics at each BCG level, where the density functions are scaled such that their sum is always equal to one.  This creates a fuzzy membership function where any given value of a raw metric is assigned to a probability for each BCG level, where the sum of all probabilities is equal to one.  See the plot from \code{\link{plot_fuzz}}. 
#' 
#' @return A \code{data.frame} of estimated BCG scores for each metric.  Probabilities for all levels are returned if \code{all_out = TRUE}.
#' 
#' @import dplyr
#' 
#' @export
#'
#' @seealso \code{\link{plot_fuzz}}
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
#' met_levs(met_in, scr_in)
#' } 
met_levs <- function(met_in, scr_in, all_out = FALSE){

  # get curve distribution estimates for each metric, bcg score
  dists <- met_dist(met_in, scr_in)
  met_in <- dists$met_in
  pars <- dists$pars
  
  # unique bcg scores in data
  scrlev <- met_in$scr %>% 
    unique %>% 
    sort %>% 
    as.character
  
  # get probabilities of metric values for each distribution estimate and level
  prbs <- rename(met_in, 
      scr_act = scr
    ) %>% 
    left_join(pars, by = 'var') %>% 
    mutate(
      prb = dnorm(val, mu, sd), 
      prb = ifelse(is.infinite(prb), 0, prb)
    ) %>% 
    select(-mu, -sd) %>% 
    spread(scr, prb) %>% 
    mutate(
      rsums = rowSums(.[, scrlev])
    ) %>% 
    gather('scr', 'pr', -station_code, -scr_act, -var, -val, -rsums) %>% 
    mutate(pr = pr/rsums) %>% 
    select(-rsums) %>% 
    arrange(station_code, var, scr, pr)
  
  # return all if T
  if(all_out) return(prbs)
  
  # highest est
  out <- group_by(prbs, station_code, var) %>% 
    filter(pr == max(pr))
  
  return(out)

}
        
