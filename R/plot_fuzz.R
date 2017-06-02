#' Plot fuzzy metrics
#'
#' Plot fuzzy metrics given based on expert BCG levels
#' 
#' @param met_in input \code{data.frame} of coral metrics
#' @param scr_in input \code{data.frame} of site BCG scores, see details
#' @param met chr string of metric to plot
#' @param cols chr string of palette for plot colors, from \code{\link[RColorBrewer]{brewer.pal}}
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
#' plot_fuzz(met_in, scr_in)
#' }
plot_fuzz <- function(met_in, scr_in, met, cols = 'Paired', widths = c(0.6, 1, 0.6, 0.6)){

  # sanity check
  if(!(met %in% names(met_in)))
    stop('met not found')
  
  # get probabilites from metric distributions
  crvs <- met_crvs(met_in, scr_in)
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
  
  # combine
  gridExtra::grid.arrange(p1,p2, p3, p4, ncol = 4, widths = widths, 
    left = met)
  
}