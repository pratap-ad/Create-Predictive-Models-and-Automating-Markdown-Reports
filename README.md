# Project-II-ST558
Project II -ST558

This repo is built for the project II, ST558 with direct connection with the R-studio, so that it would be readily 
available to the auther as well as to the users. This repo contains the analysis about the **online news popularity**. 

#The list of packages are: 
  + tidyverse 
  + knitr 
  + caret 
  + gbm
  + rattle 
  + gbm 
  + rmarkdown 
  + ggplot2.


The analysis is done for each of the week day.
1. [Monday's Analysis](https://github.com/pratap-ad/Project-II-ST558/blob/main/mondayAnalysis.md) 
2. [Tuesday's Analysis](https://github.com/pratap-ad/Project-II-ST558/blob/main/tuesdayAnalysis.md)
3. [Wednesday's Analysis](https://github.com/pratap-ad/Project-II-ST558/blob/main/wednesdayAnalysis.md)
4. [Thursday's Analysis](https://github.com/pratap-ad/Project-II-ST558/blob/main/thursdayAnalysis.md)
5. [Friday's Analysis](https://github.com/pratap-ad/Project-II-ST558/blob/main/fridayAnalysis.md)
6. [Saturday's Analysis](https://github.com/pratap-ad/Project-II-ST558/blob/main/saturdayAnalysis.md)
7. [Sunday's Analysis](https://github.com/pratap-ad/Project-II-ST558/blob/main/sundayAnalysis.md)


## Code used for Automation	
```{r }	   
#select the weekdays from the data	 
weekDays<- c("monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday")	    
params= lapply(weekDays, FUN = function(x){list(weekday=x)})	 
params
output_file<- paste0(weekDays, "Analysis.md") 
output_file
#put the filenames into the dataframe	
reports<- tibble(output_file, params)	
reports	
```	

``` {r}
apply(reports, MARGIN = 1,
      FUN = function(x){	 
        render(input="path\Project-II.Rmd", output_file = x[[1]], params =  x[[2]])
      })
