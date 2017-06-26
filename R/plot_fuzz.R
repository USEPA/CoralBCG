#' Plot fuzzy metrics
#'
#' Plot fuzzy metrics given based on expert BCG levels
#' 
#' @param met_in input \code{data.frame} of coral metrics
#' @param scr_in input \code{data.frame} of site BCG scores, see details
#' @param met chr string of metric to plot
#' @param cols chr string of palette for plot colors, from \code{\link[RColorBrewer]{brewer.pal}}
#' @param met_val numeric value of metric to show on plot
#' @param sdchg optional list passed to \code{\link{met_crvs}} for manually changing standard deviation estimates
#' @param widths numeric vector indicating widths of each of four plots, passed to \code{\link[gridExtra]{grid.arrange}}
#' 
#' @details The input \code{scr_in} data has two columns labelled \code{station_code} and \code{scr}. The station codes should match those in \code{met_in}. The \code{scr} values for each station can be numeric or character string BCG levels that represent qualitative rankings. 
#'
#' @return A \code{\link[ggplot2]{ggplot}} object
#' 
#' @seealso \code{\link{met_crvs}}, \code{\link{met_dist}}
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
#' # plot
#' plot_fuzz(met_in, scr_in, met = 'tot_rich')
#' 
#' # plot, manual change of standard deviation
#' sdchg <- list(tot_rich = c(2, 2, 2, 2))
#' plot_fuzz(met_in, scr_in, met = 'tot_rich', sdchg = sdchg)
#' }
plot_fuzz <- function(met_in, scr_in, met, cols = 'Paired', met_val = NULL, sdchg = NULL, widths = c(0.6, 1, 0.6, 0.6)){

  ## sanity checks
  if(!(met %in% names(met_in)))
    stop('met not found')

  if(!is.null(met_val)){
    rng <- range(met_in[, met], na.rm = TRUE)
    if(met_val > rng[2] | met_val < rng[1])
      stop('met_val not within metric range')
  }
  
  # get probabilites from metric distributions
  crvs <- met_crvs(met_in, scr_in, sdchg = sdchg)
  met_in <- crvs$met_in
  crvs<- crvs$crvs
  
  # unique bcg scores in data
  scrlev <- met_in$scr %>% 
    unique %>% 
    sort %>% 
    as.character
  
  ##  
  # datasets to plot
  
  # boxplot
  toplo1 <- filter(met_in, var %in% met)
  
  # density curves
  toplo2 <- filter(crvs, var %in% met)
  
  # scaled density curves
  toplo3 <- toplo2 %>% 
    spread(scr, pr) %>% 
    mutate(
      rsums = rowSums(.[, scrlev])
    ) %>% 
    gather('scr', 'pr', -var, -vals, -rsums) %>% 
    mutate(pr = pr/rsums) %>% 
    select(-rsums)
  
  # get pr estimates closest to met_val
  if(!is.null(met_val)){
    
    prbs <- group_by(toplo3, scr) %>% 
      mutate(mins = abs(met_val - vals)) %>% 
      filter(mins == min(mins)) %>% 
      ungroup %>% 
      select(scr, pr) %>% 
      mutate(
        scr = english::as.english(as.numeric(scr)),
        scr = as.character(scr),
        pr = round(pr, 2)
      ) %>% 
      unite('scr', scr, pr, sep = ': ') %>% 
      data.frame %>% 
      .$scr %>% 
      paste0(., collapse = ', ')
    
  } else {
    
    prbs <- NULL 
    
  }
  
  ##
  # plots 
  
  # ggplot theme
  mytheme <- theme_bw() + 
    theme(
      axis.title.y = element_blank(), 
      legend.position = 'none',
      axis.text.x = element_text(size = 8)
    )

  # boxplots of raw
  p1 <- ggplot(toplo1, aes(x = factor(scr), y = val, fill = factor(scr))) + 
    geom_boxplot(alpha = 0.5) + 
    mytheme +
    scale_y_continuous(expand = c(0, 0)) + 
    scale_x_discrete('BCG level') +
    ggtitle('') +
    scale_fill_brewer(palette = cols)
  
  # faceted density distribution
  p2 <- ggplot(toplo2, aes(x = vals, fill = factor(scr))) + 
    geom_ribbon(aes(ymin = 0, ymax = pr), alpha = 0.5) + 
    facet_wrap(~ scr, ncol = 4) +
    coord_flip() + 
    mytheme %+replace%
    theme(
      panel.grid.minor = element_blank()
      ) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous('Density', expand = c(0, 0)) +
    scale_fill_brewer(palette = cols)
  
  # filled density distribution
  p3 <- ggplot(toplo2, aes(x = vals, y = pr, fill = factor(scr))) + 
    geom_area(alpha = 0.5, position = 'fill') + 
    coord_flip() +
    mytheme +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous('Probability', expand = c(0, 0)) + 
    ggtitle('') +
    scale_fill_brewer(palette = cols)
  
  # scaled density distribution
  p4 <- ggplot(toplo3, aes(x = vals, group = factor(scr), fill = factor(scr))) + 
    geom_ribbon(aes(ymin = 0, ymax = pr), alpha = 0.5) + 
    coord_flip() + 
    mytheme +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous('Probability', expand = c(0, 0)) +
    scale_fill_brewer(palette = cols) + 
    ggtitle('')
  
  # add line for metric
  if(!is.null(met_val)){
    
    p1 <- p1 + geom_hline(yintercept = met_val, linetype = 'dashed')
    p2 <- p2 + geom_vline(xintercept = met_val, linetype = 'dashed')
    p3 <- p3 + geom_vline(xintercept = met_val, linetype = 'dashed')
    p4 <- p4 + geom_vline(xintercept = met_val, linetype = 'dashed')
    
  }
  
  # combine
  gridExtra::grid.arrange(p1,p2, p3, p4, ncol = 4, widths = widths, 
    left = met, bottom = prbs)
  
}