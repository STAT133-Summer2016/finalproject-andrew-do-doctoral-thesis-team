library(readr)
library(dplyr)

clean_df_gpa = function(path) {
    df = read_csv("../raw_data/fall2010gpa.csv.gz") %>%
        select(-1)
    #fix col names
    names = c('grade_id', 'grade_count', 'dirty_col1', 'dirty_col2', 'ccn', 
              'instr_name', 'grade_pts', 'department', 'course_num', 'sec_num', 
              'letter_grade_subtype', 'grade_type', 'letter_grade', 'course_title')
    names(df) = names
    View(df)
    #fix missing grade points
    
}