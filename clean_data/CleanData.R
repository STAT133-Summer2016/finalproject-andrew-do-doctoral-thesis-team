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

multiple_fields_applied <- read_csv("MultipleFieldsApplied.csv")

multiple_fields_applied <- as.data.frame(multiple_fields_applied)

multiple_fields_admitted <- read_csv("MultipleFieldsAdmitted.csv")

names(multiple_fields_applied) <- c("residency", "ethnic_descr", 
                                    "major", "HSRank","headcount", "year", "admit")

names(multiple_fields_admitted) <- c("residency", "ethnic_descr", 
                                     "major", "HSRank", "headcount", "year", "admit") #subset with admit status


multiple_fields <- rbind(multiple_fields_applied, multiple_fields_admitted) %>%
  mutate(year = str_replace_all(year, "-[0-9]{2}", ""),  #Years for time plot
         year = as.integer(year),
         headcount = ifelse(is.na(headcount), 0, headcount),
         admit = str_replace_all(admit, "[()]", ""))

factor_admit <- factor(multiple_fields$admit, levels = c("Applied", "Admitted"),
                       labels = c("Applied", "Admitted"))

factor_residency <- factor(multiplefields$residency, 
                           levels = c("CA Resident", "Out of State Domestic",
                                      "International"),
                           labels = c("CA", "Out of State", "International"))

multiple_fields <- mutate(multiple_fields, admit = factor_admit,
                          residency = factor_residency)

asian <- c("Filipino", "Chinese", "Korean", 
           "Pacific Islander", "Japanese", "Vietnamese", 
           "South Asian", "Other Asian")

hispanic <- c("Mexican American/Chicano","Other Hispanic/Latino")


multiple_fields_percentages <- multiple_fields %>%
  select(-HSRank) %>%
  mutate(ethnic_descr = ifelse(ethnic_descr %in% asian, "Asian",
                               ethnic_descr),
         ethnic_descr = ifelse(ethnic_descr %in% hispanic, 
                               "Hispanic", ethnic_descr)) %>%
  filter(residency != "International") %>%
  data.frame()
write_csv(multiple_fields_percentages, "multiple_fields_percentages.csv")

american_studies <- multiple_fields_percentages %>%
  group_by(residency, major, year, admit) %>%
  mutate(majortotal = sum(headcount)) %>%
  group_by(residency, ethnic_descr, major, year, admit) %>%
  mutate(ethnic_count = sum(headcount),
         percentage = sum(headcount) / majortotal * 100) %>%
  group_by(residency, ethnic_descr, major, 
           year, admit, majortotal, ethnic_count) %>%
  distinct(percentage) %>%
  mutate(percentage = ifelse(is.nan(percentage), 0, percentage)) %>%
  ungroup() %>%
  filter(year >= 2000, major == "American Studies", 
         ethnic_descr != "Decline to State", residency == "CA", admit == "Applied") %>%
  arrange(residency, year)

write_csv(american_studies, "american_studies.csv")

ethnic_studies <- multiple_fields_percentages %>%
  group_by(residency, major, year, admit) %>%
  mutate(majortotal = sum(headcount)) %>%
  group_by(residency, ethnic_descr, major, year, admit) %>%
  mutate(ethnic_count = sum(headcount),
         percentage = sum(headcount) / majortotal * 100) %>%
  group_by(residency, ethnic_descr, major, 
           year, admit, majortotal, ethnic_count) %>%
  distinct(percentage) %>%
  mutate(percentage = ifelse(is.nan(percentage), 0, percentage)) %>%
  ungroup() %>%
  filter(year >= 2000, major == "Ethnic Studies", 
         ethnic_descr != "Decline to State", residency == "CA", admit == "Applied") %>%
  arrange(residency, year)

write_csv(ethnic_studies, "ethnic_studies.csv")

african_american_majors <- c("Afr Amer Stds-Humanities", "Afr Amer Stds-Social Sci",
                             "African American Studies")

afr_american_studies <- multiple_fields_percentages %>%
  mutate(major = ifelse(major %in% african_american_majors, 
                        "African American Studies", major)) %>%
  group_by(residency, major, year, admit) %>%
  mutate(majortotal = sum(headcount)) %>%
  group_by(residency, ethnic_descr, major, year, admit) %>%
  mutate(ethnic_count = sum(headcount),
         percentage = sum(headcount) / majortotal * 100) %>%
  group_by(residency, ethnic_descr, major, 
           year, admit, majortotal, ethnic_count) %>%
  distinct(percentage) %>%
  mutate(percentage = ifelse(is.nan(percentage), 0, percentage)) %>%
  ungroup() %>%
  filter(year >= 2000, major == "African American Studies", 
         ethnic_descr != "Decline to State", residency == "CA", admit == "Applied") %>%
  arrange(residency, year)

write_csv(afr_american_studies, "african_american_studies.csv")

asian_related_majors <- c("Asian Studies - China", "Asian Studies - Japan",
                          "Chinese Language", "Japanese Language", 
                          "Asian American Studies")

asian_studies <- multiple_fields_percentages %>%
  mutate(major = ifelse(major %in% asian_related_majors, 
                        "Asian Related Majors", major)) %>%
  group_by(residency, major, year, admit) %>%
  mutate(majortotal = sum(headcount)) %>%
  group_by(residency, ethnic_descr, major, year, admit) %>%
  mutate(ethnic_count = sum(headcount),
         percentage = sum(headcount) / majortotal * 100) %>%
  group_by(residency, ethnic_descr, major, 
           year, admit, majortotal, ethnic_count) %>%
  distinct(percentage) %>%
  mutate(percentage = ifelse(is.nan(percentage), 0, percentage)) %>%
  ungroup() %>%
  filter(year >= 2000, major == "Asian Related Majors", 
         ethnic_descr != "Decline to State", residency == "CA") %>%
  arrange(residency, year)

write_csv(asian_studies, "asian_studies.csv")

top_majors <- multiple_fields_percentages %>%
  group_by(residency, ethnic_descr, year, major, admit) %>%
  tally(headcount) %>%
  filter(major != "Letters & Sci Undeclared") %>%
  group_by(residency, ethnic_descr, admit, year) %>%
  filter(n == max(n)) %>%
  unique() %>%
  mutate(major = paste(major, sep = "", collapse = "/"))%>% 
  arrange(admit) %>%
  filter(year >= 2000, admit != "Admitted")
write_csv(top_majors, "top_majors.csv")

top_majors_2000 <- top_majors %>%
  filter(year == 2000) 

top_majors_2015 <- top_majors %>%
  filter(year == 2015) %>%
  unique()

top_majors_2005 <- top_majors %>%
  filter(year == 2005) 

top_majors_2010 <- filter(top_majors, year == 2010)

write_csv(top_majors_2000, "top_2000.csv")

write_csv(top_majors_2015, "top_2015.csv")

write_csv(top_majors_2010, "top_2010.csv")

write_csv(top_majors_2005, "top_2005.csv")
