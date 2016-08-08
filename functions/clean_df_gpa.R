library(readr)
library(dplyr)
library(stringr)                                            

clean_df_gpa = function(dataset) {
    df = read_csv(dataset) %>%
        select(-1)
    #fix col names
    names = c('grade_id', 'grade_count', 'dirty_col1', 'dirty_col2', 'ccn', 
              'instr_name', 'grade_pts', 'department', 'course_num', 'sec_num', 
              'letter_grade_subtype', 'grade_type', 'letter_grade', 'course_title')
    names(df) = names
    
    #fix missing grade points
    grade_pt_mapping = c('A\\+$' = 4.3, 'A$' = 4.0, 'A-$' = 3.7,
                         'B\\+$' = 3.3, 'B$' = 3.0, 'B-$' = 2.7,
                         'C\\+$' = 2.3, 'C$' = 2.0, 'C-$' = 1.7,
                         'D\\+$' = 1.3, 'D$' = 1.0, 'D-$' = 0.7,
                         'F$' = 0)
    
    term = str_split(dataset, "gpa")[[1]][1]
    df = df %>%
        select(-1, -3, -4) %>%
        mutate(grade_pts = str_replace_all(letter_grade, grade_pt_mapping), 
               grade_pts = as.numeric(grade_pts), semester = str_sub(term, 1, 2),
               year = as.integer(str_sub(3, 6)))
    return(df)
}
