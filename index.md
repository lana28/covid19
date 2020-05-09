## Comparison Between COVID 19 Testing places and Safe Hospitals, and Drive-Through Testing Places in South Korea 

This is one of my project in my college, which deals with web scraping. As a graduate student who studies Computational Social Science, I would like to visulalize the hottest issue for now, COVID 19. I wanted to scrape information of testing places, safe hospitals for those who do not have respiratory diseases, and unprecedented drive-through testing places in South Korea. To visualize them, I used API of KAKAO map to acquire exact latitude and longitude of each place and used Google API to see distribution of testing places on a map. 

I start with scraping keyword information of each place since the government [website](https://www.mohw.go.kr/react/popup_200128.html) does not show the exact address (Instead, you can the addresses in another click-through webpage.). Due to the language of the wesite, it is not fully accurate enough to use `geocode` from package `ggmap`. Thus, I collected the exact coordinates of each place throughout keyword searching provided by KAKAO API.

Let's move on the practical part.

### Packages needed

```markdown
library(httr)
library(RCurl)
library(rvest)
library(XML)
library(ggplot2)
library(tidyverse)
library(ggmap)
library(jsonlite)
```
Call the packages that we need to begin. There are different ways to scrape data from the Internet,but I put up `httr` `RCurl` `rvest` `XML` just in case. The webpage has 4 different tabs, and the first of which is the address like the one above. Of those, I want to focus on the first one (safe hospitals), third one(normal testing places), and the last one (drive-through testing places). The reason why I chose the list of safe hospitals is that it was a nice touch to announce the available hospital list where other patients who do not have a respiratory disease could get medical service.


```markdown
# Information of safe hospitals as the first page 
# https://www.mohw.go.kr/react/popup_200128.html

# Information of testing places 
# https://www.mohw.go.kr/react/popup_200128_3.html

# Information of drive-through testing places only.
# https://www.mohw.go.kr/react/popup_200128_4.html

# I store the url pages into url vector.

url <- paste0("https://www.mohw.go.kr/react/popup_200128",c("","_3", "_4"),".html") 

```

```markdown
# Create an empty list

place <- list()

# Scrape source codes in HTML form

for(i in 1:length(url)){
  place[[i]] <- read_html(url[i], encoding = 'UTF-8') 
  # Don't forget to put "encoding = 'UTF-8'" as the site is written in Korean
  Sys.sleep(1)   
}

# Parse the codes in a tree form
parsedPlace <- lapply(place, function(x) htmlParse(x))
```
We can use `rvest` packagages to track down nodes, but as I am in the beginning of learning web scraping, I will just go one by one. After parsing the extracted source codes, I sort out information that I need by row using `xpathSApply` of package `XML`. It is important to chekc how the webpage is configured. As the index row of the table is included the first row vector after extracting, it is necessarty to subtract 1 to the entire number to go through loop later.    

```markdown
# The number of table rows
num <- c()
# Number of safe hospitals
num[1] <- length(xpathSApply(parsedPlace[[1]], "//tr"))
# Number of testing place
num[2] <- length(xpathSApply(parsedPlace[[2]], "//tr")) 
# Number of drive through testing
num[3] <- length(xpathSApply(parsedPlace[[3]], "//tr")) 

#### Since the number of table rows includes the index rows, it is needed to substract 1 to the numbers later on.
```
