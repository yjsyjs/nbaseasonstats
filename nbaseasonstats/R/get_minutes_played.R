#' Calculating Players' Total Minutes Played for a Given Year
#'
#' This function allows you to calculate the total minutes played for a given year for all players.
#' @param year The year (season) for which you are interested in knowing the players' total minutes played.
#' @keywords year, mp, minutes, played
#' @export
#' @examples
#' get_minutes_played()

get_minutes_played <- function(year){
  mp <- data %>%
    dplyr::filter(Year == year) %>%
    dplyr::select(c(Player, MP)) %>%
    dplyr::group_by(Player) %>%
    dplyr::summarise(sumMP = sum(MP))
  return(mp)
}
