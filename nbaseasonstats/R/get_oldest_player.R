#' Finding the Oldest Player for a Given Year
#'
#' This function allows you to find the oldest player in a given year. For ties, it returns the first player in alphabetical order.
#' @param year The year (season) for which you are interested in knowing the oldest player.
#' @keywords year, age, player
#' @export
#' @examples
#' get_oldest_player()

get_oldest_player <- function(year){
  oldest <- data %>%
    dplyr::filter(Year == year) %>%
    dplyr::filter(Age == max(Age)) %>%
    dplyr::select(c(Player, Age)) %>%
    dplyr::group_by(Player) %>%
    dplyr::summarise(Age = mean(Age))
  return(oldest[1, ])
}
