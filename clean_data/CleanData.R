source("../functions/clean_df_gpa.R")
#test if binding correctly
#test = str_c(path, list.files(path, pattern = "gpa.csv.gz"))
#test = map_int(test, function(x) nrow(clean_df_gpa(x)))

path = "../raw_data/"
datasets = as.list(str_c(path, list.files(path, pattern = "gpa.csv.gz")))
dfs = lapply(datasets, clean_df_gpa)
gpa_data = do.call(rbind, dfs)

write.csv(gpa_data, file = gzfile(str_c('gpa_data.csv', '.gz')), row.names = F)

