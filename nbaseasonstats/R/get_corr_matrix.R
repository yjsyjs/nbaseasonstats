#' Correlation Matrix for All Numeric Variables in a Given Year
#'
#' This function allows you to output the correlation matrix for all numeric variables in a given year.
#' @param year The year (season) for which you are interested in getting a correlation matrix.
#' @keywords year, correlation, matrix
#' @export
#' @examples
#' get_corr_matrix()

# Function 3: input "Year", only include numeric variables for that year, output correlation matrix for all numeric variables in the data for that year
get_corr_matrix <- function(year){
  data_num <- purrr::keep(data, is.numeric)
  corr_matrix <- cor(data_num, use = "complete.obs")
  return(corr_matrix)
}
