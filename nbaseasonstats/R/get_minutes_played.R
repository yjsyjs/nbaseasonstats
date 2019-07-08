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
    tidyverse::filter(Year == year) %>%
    tidyverse::select(c(Player, MP)) %>%
    tidyverse::group_by(Player) %>%
    tidyverse::summarise(sumMP = sum(MP))
  return(mp)
}
