summarizer <- function(df, x) {
  # Returns a data frame with two columns: x, and
  # the percentage breakdown of the school by x
  
  return_df <- df %>% 
    mutate(total = sum(count)) %>% 
    group_by(df[[x]]) %>% 
    summarise(sum(count / total))
  names(return_df) <- c(x, "percent_of_total")
  return(return_df)
}

detailed_summarizer <- function(df, x, y) {
  # Returns a data frame with three columns: x, y, and
  # the percentage breakdown of y by x
  
  return_df <- df %>% 
    group_by(df[[y]]) %>% 
    mutate(total = sum(count)) %>% 
    ungroup %>% 
    group_by(df[[x]], df[[y]]) %>% 
    summarise(sum(count / total)) %>% 
    ungroup
  names(return_df) <- c(x, y, str_c("percent_of_", y))
  return(return_df)
}

comparison_summarizer <- function(df, x, y){
  # Returns a data frame with four columns: x, y, the
  # percentage breakdown of y by x, and the percentage
  # breakdown of the school by x
  
  total_x <- summarizer(df, x)
  x_by_y <- detailed_summarizer(df, x, y)
  left_join(x_by_y, total_x)
}