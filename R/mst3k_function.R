#' Rowsdowser's Function (MST3K)
#'
#' This function allows you to pull random snippets of Mystery Science Theater 3000 quotations
#' @param none Defaults to three random consecutive quotes
#' @keywords rowsdower
#' @export
#' @examples
#' get_mst3k_quotes()

get_mst3k_quotes <- function(){
  library(rvest)
  library(dplyr)
  webpage <- read_html("https://en.wikiquote.org/wiki/Mystery_Science_Theater_3000")
  webpage
  results <- webpage %>% html_nodes(c("dl"))
  results
  bold_result <- results
  bold_result %>% html_nodes("b")
  quote_data <- xml_contents(bold_result) %>% html_text(trim = TRUE)
  quote_data2 <- quote_data[!quote_data %in% ""]
  quote_data3 <- as.data.frame(quote_data2)
  quote_data3$quote_data2 <- as.character(quote_data3$quote_data2)
  api_call <- quote_data3[sample(1:(nrow(quote_data3)-2), 1) + 0:2, ]
  #api_call <- quote_data3[index:(index + 3),]
  #api_call <- sample_n(quote_data3, 3)
  return(api_call)
}
get_mst3k_quotes()
