#' Data frame of coral demographic data
#'
#' Data frame of coral demographic data with minimal data requirements for estimating stony coral metrics
#' 
#' @format A data frame:
#' \describe{
#'   \item{\code{station_code}}{num station identifier}
#'   \item{\code{species_name}}{chr coral species name}
#'   \item{\code{ColonyID}}{num, coral colony id or individual}
#'   \item{\code{MaxDiam}}{num colony diameter, cm}
#'   \item{\code{PerpDiam}}{num perpendicular diameter cm}
#'   \item{\code{Height}}{num height cm}
#'   \item{\code{MortOld}}{num old mortality as a percentage of total colony size, 0-100}
#'   \item{\code{MortNew}}{num recent mortality as a percentage of total colony size, 0-100}
#'   \item{\code{Bleached}}{chr qualifer for bleaching, T total, P partially bleached, N no bleaching}
#'   \item{\code{Diseased}}{chr qualifier for disease, P present, A absent}
#' }   
"crl_dem"