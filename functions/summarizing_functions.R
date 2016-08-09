summarizer <- function(x) {
  # Returns a data frame with two columns: x, and
  # the percentage breakdown of the school by x
  
  return_df <- degrees %>% 
    mutate(total = sum(count)) %>% 
    group_by(degrees[[x]]) %>% 
    summarise(sum(count / total))
  names(return_df) <- c(x, "percent_of_total")
  return(return_df)  
}

detailed_summarizer <- function(x, y) {
  # Returns a data frame with three columns: x, y, and
  # the percentage breakdown of y by x
  
  return_df <- degrees %>% 
    group_by(degrees[[y]]) %>% 
    mutate(total = sum(count)) %>% 
    ungroup %>% 
    group_by(degrees[[x]], degrees[[y]]) %>% 
    summarise(sum(count / total)) %>% 
    ungroup
  names(return_df) <- c(x, y, str_c("percent_of_", y))
  return(return_df)
}

comparison_summarizer <- function(x, y){
  # Returns a data frame with four columns: x, y, the
  # percentage breakdown of y by x, and the percentage
  # breakdown of the school by x
  
  total_x <- summarizer(x)
  x_by_y <- detailed_summarizer(x, y)
  left_join(x_by_y, total_x)
}