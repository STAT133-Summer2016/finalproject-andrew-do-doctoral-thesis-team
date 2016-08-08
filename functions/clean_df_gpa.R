library(readr)
library(dplyr)

clean_df_gpa = function(path) {
    path = "~/Downloads/fall2015gpa.csv"
    df = read_csv(path)
    #fix col names
    names = c('grade_id', 'grade_count', 'ccn', 'dirty_col1', 'dirty_col2',
              'instr_name', 'grade_pts', 'department', 'course_num', 'sec_num', 
              'letter_grade_subtype', 'grade_type', 'letter_grade', 'course_title')
    names(df) = names
    
    #fix missing grade points
    
}