Project II -ST558

Brief about the repo
This repo is built for the project II, ST558. This repo is built for direct connection with the R-studio, so that it would be readily available to the auther as well as to the users if any. And also to doucument the work securely for future reference. This repo contains the analysis about the online news popularity.

The list of R-packages used are:
tidyverse
knitr
caret
gbm
rattle
rmarkdown
ggplot2.
The analysis is done for each of the week day.

Analysis for Monday is available here
Analysis for Tuesday is available here
Analysis for Wednesday is available here
Analysis for Thursday is available here
Analysis for Friday is available here
Analysis for Saturday is available here
Analysis for Sunday is available here
Code used for Automation
#select the weekdays from the data
weekDays<- c("monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday")

params= lapply(weekDays, FUN = function(x){list(weekday=x)})
params
output_file<- paste0(weekDays, "Analysis.md")
output_file
#put the filenames into the dataframe
reports<- tibble(output_file, params)
reports
apply(reports, MARGIN = 1, 
      FUN = function(x){
        render(input="path\Project-II.Rmd", output_file = x[[1]], params =  x[[2]])
      })
