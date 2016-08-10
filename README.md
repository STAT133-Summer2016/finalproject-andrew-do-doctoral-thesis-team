Contributors:
Mason Chen, Ian Katz, Benny Chen, Dotun Lytton-Fatade

Cal Answers: https://calanswers-bi.berkeley.edu:9804/analytics/saw.dll?BIEEHome

In our project, we are going to use the data sets found on Cal Answers to investigate questions about undergraduates at UC Berkeley. Topics of the investigation include ethnic makeup of students and classes, trends in graduation time, and GPA trends.

How to Export Data (CSV, XML, Tab Delimited): http://calanswers.berkeley.edu/sites/default/files/quick_start_guide_09182015.pdf

Note: There is no script to download the data because the data is proprietary.

Folders in the repo:
raw_data - contains the compressed raw data files and codebook.txt
clean_data - contains the cleaning script and cleaned data files
eda - our exploration of the data
functions - contains functions used throughout the project
presentation - contains the pdf file of our presentation
paper - contains the Rmd and pdf files of our paper

Libraries used:
readr
stringr
dplyr
tidyr
purrr
ggplot2
lubridate

How to produce the final paper:
Open RStudio and knit the paper