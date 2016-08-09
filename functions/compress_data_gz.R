library(stringr)
library(purrr)
library(readr)

setwd("../raw_data/")
files = list.files()
files_to_compress = files[str_detect(files, "\\.csv$")]
walk(files_to_compress, function(x) write.csv(read_csv(x), file=gzfile(str_c(x, '.gz')), row.names = F))
file.remove(files_to_compress)
