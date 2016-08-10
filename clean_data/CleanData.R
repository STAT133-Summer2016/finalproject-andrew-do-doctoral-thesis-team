library(readr)
library(stringr)
library(dplyr)
library(tidyr)
source("../functions/clean_df_gpa.R")
#test if binding correctly
#test = str_c(path, list.files(path, pattern = "gpa.csv.gz"))
#test = map_int(test, function(x) nrow(clean_df_gpa(x)))

path = "../raw_data/"
datasets = as.list(str_c(path, list.files(path, pattern = "gpa.csv.gz")))
dfs = lapply(datasets, clean_df_gpa)
gpa_data = do.call(rbind, dfs)

write.csv(gpa_data, file = gzfile(str_c('gpa_data.csv', '.gz')), row.names = F)

ethnic_rollup_key <- read_csv("../raw_data/ethnic_rollup_key.csv.gz") %>% 
  select(8, 9) %>% 
  unique
colnames(ethnic_rollup_key) <- c("ethnic_category",
                                 "ethnicity")

degrees <- read_csv("../raw_data/degrees.csv.gz") %>% 
  filter(!is.na(`Semester Year Letter Cd Concat`) &
           `Degree Level Desc` == "Bachelor") %>% 
  select(year = `Semester Year Letter Cd Concat`,
         semester = `Semester Nm`,
         ethnicity = `Short Ethnic Desc`,
         gender = `Gender Desc`,
         residency = `Residency Status Desc`,
         college = `College/School`,
         major = Major,
         count = `Student Headcount`) %>% 
  mutate(year = str_extract(year, "[0-9]{4}") %>% as.integer,
         residency = str_extract(residency, "^[^\\s]+")) %>% 
  left_join(ethnic_rollup_key)

write.csv(degrees, file = gzfile(str_c('degree_data.csv', '.gz')), row.names = F)

grad_time <- read_csv("../raw_data/grad_time.csv.gz") %>% 
  select(c(-1, -5))
colnames(grad_time) <- c("year_semester",
                         "entry_status",
                         "time_to_grad",
                         "count",
                         "ethnicity",
                         "gender",
                         "residency",
                         "college")
grad_time <- grad_time %>% 
  separate(col = year_semester, into = c("year", "semester"), sep = " ") %>% 
  mutate(year = year %>% as.integer,
         residency = str_extract(residency, "^[^\\s]+"),
         time_to_grad = str_extract(time_to_grad, "[.0-9]+") %>% as.numeric) %>% 
  left_join(ethnic_rollup_key)

write.csv(grad_time, file = gzfile(str_c('grad_time_data.csv', '.gz')), row.names = F)
