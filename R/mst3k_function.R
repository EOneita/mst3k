#' Rowsdowser's Function (MST3K)
#' 
#' This function allows you to pull random snippets of Mystery Science Theater 3000 quotations
#' @param id Choose a numeric id. Defaults to a random id
#' @keywords rowsdower
#' @export
#' @examples 
#' get_mst3k_quotes()

get_mst3k_quotes <- function(id = sample(1:8106, 1)){
  if(!(id%%1==0)){
    stop("Error: id must be an integer")
  }
  if(!(id >= 1 & id <= 8106)){
    stop("Error: id must be between 1 and 8106")
  }
  library(rvest)
  library(dplyr)
  message("The id you are using is ", as.character(id))
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
