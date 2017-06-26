#' Get probability distribution curves
#'
#' Get probability distribution curves from raw metrics for plotting
#' 
#' @param met_in input \code{data.frame} of coral metrics
#' @param scr_in input \code{data.frame} of site BCG scores, see details
#' @param n numeric indicating number of values across the range for each metric to predict the density curves
#' @param sdchg optional named list to manually change standard deviation estimates for each metric, see details
#'
#' @details The input \code{scr_in} data has two columns labelled \code{station_code} and \code{scr}. The station codes should match those in \code{met_in}. The \code{scr} values for each station can be numeric or character string BCG levels that represent qualitative rankings. 
#' 
#' The density curves are based on maximum-likelihood estimates of the mean and standard deviation for a normal curve corresponding to the raw metric data at each BCG level.
#' 
#' The distribution curves can be changed by manually entering standard deviation estimates for each metric and BCG score.  Ideally, standard deviation estimates should be derived from observations but these values could be manually changed to maximize accuracy of metric scores from expert assignments.  The standard deviation estimates can be manually changed using a named list for the element names correspond to one to many metrics and the elements for each is a numeric vector of the values to change (first is highest level, last is lowest).  See the examples. 
#' 
#' @return A two-element list named \code{met_in} and \code{crvs}, where the former is the joined input data with BCG scores in \code{scr_in} and the latter is estimated density curves from the raw distributions of each. 
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
#' # get curves
#' met_crvs(met_in, scr_in)
#' 
#' # get curves with manual sd
#' sdchg <- list(tot_rich = c(2, 2, 2, 2))
#' met_crvs(met_in, scr_in, sdchg = sdchg)
#' } 
met_crvs <- function(met_in, scr_in, n = 1000, sdchg = NULL){

  # get curve distribution estimates for each metric, bcg score
  dists <- met_dist(met_in, scr_in)
  met_in <- dists$met_in
  pars <- dists$pars
  
  # replace sd estimates with sdchg if provided
  if(!is.null(sdchg)){
    
    # format sdchg tocombine with pars
    scr <- pars$scr %>% 
      unique
    sdchg <- tibble::enframe(sdchg) %>% 
      mutate(
        value = purrr::map(value, 
          function(x) data.frame(scr = scr, sdchg = x)
          )
        ) %>% 
      tidyr::unnest(.) %>% 
      rename(var = name)

    # replace sd values in par with sdchg, by metric and scr
    pars <- left_join(pars, sdchg, by = c('var', 'scr')) %>% 
      mutate(sd = ifelse(!is.na(sdchg), sdchg, sd)) %>% 
      select(-sdchg)
    
  }
  
  # get density function estimates for bcg levels within metrics
  # return data.frame in long format of probabilities of each metric
  crvs <- met_in %>%  
    group_by(var) %>% 
    mutate(
      minv = min(val, na.rm = T), 
      maxv = max(val, na.rm = T)
      ) %>% 
    left_join(., pars, by = c('var', 'scr')) %>% 
    group_by(var, scr) %>% 
    nest %>% 
    mutate(
      
      ests = purrr::map(data, function(x){
        
        maxv <- unique(x$maxv)
        minv <- unique(x$minv)
        mu <- unique(x$mu)
        sd <- unique(x$sd)
        vals <- seq(minv, maxv, length = n)
        out <- dnorm(vals, mu, sd) %>% 
          data.frame(vals = vals, pr = .)
        
        return(out)
        
      })
    ) %>% 
    select(var, scr, ests) %>% 
    unnest %>% 
    mutate(pr = ifelse(is.infinite(pr), 0, pr))
  
  # output
  out <- list(met_in = met_in, crvs = crvs)
  return(out)
  
}
        

  