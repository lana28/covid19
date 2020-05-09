## Comparison between COVID 19 Testing places and Safe Hospitals, and Drive-Through Testing Places in South Korea 

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

# I store the url pages into a url vector.

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

```markdown
node <- list()
# Extract elements by nodes(by row)
info_by_row <- function(page, nodeNum){

  # As the first table row is a list which has two clusters of information: one is information of indices and the other one is the first row 
  if(nodeNum == 1) { 
    node <- xpathSApply(page, paste0("//tr[",nodeNum,"]"))[[2]]
  } else {
    node <- xpathSApply(page, paste0("//tr[",nodeNum, "]"))[[1]]
  }
  #1st column: region, 2nd column: town, 3rd column: name of hospitals
  FirsttoThird <- paste0(".//td[",1:3,"]")  
  cells <-c()
  cells <- xpathSApply(node, FirsttoThird, xmlValue) 
  return(cells)
}


location <- df <- list()
for(i in 1:length(parsedPlace)) {
  location[[i]] <- lapply(1:(num[i]-1), function(x) info_by_row(page = parsedPlace[[i]], nodeNum = x))

}

# Bind them together in the data frame
# Thus, I store location information of safe hospitals and drive-through testings in the data frame.
for(j in 1:length(parsedPlace)){
    df[[j]] <- as.data.frame(do.call(rbind, location[[j]]))
    names(df[[j]]) <- c("Region", "Town", "Hospital Name")
    gsub("^\\s|\\s$", "", df[[j]]) 
    #remove space either beginning of the text or the end
}


```

```markdown
# To acquire the exact coordinate of each location, paste region, town, hospital names by row
 addr <-  c()  
catplace <- function(df) {
  for(i in 1:nrow(df)){

  addr[i] <- paste(df[i,1], df[i,2], df[i,3])}
  print(addr)
}

fulladdr <- lapply(df, function(x) catplace(x))
# -fulladdr- is the list of concatenated full address to search them as a one keyword 
# But, before going through search by these keywords, cleasing the unnecessary parts

fulladdr[[2]] <- gsub("*\\t.*", "", fulladdr[[2]]) #remove anything starts with '\t'

# I tried to extract latitude and longitude information of each location from Google using geocode, but the code did not return the exact coordinates as the information of hospital name is written in Korean.   

# Instead of Google, I used Kakao api to use map service which provides more friendly   and accurate information for Korean address. 
# As a result, I am able to get accurate information of longittude and latitude for     geocoding.

# Greate a function to apply the lists, fulladdr and df
addrCoord <- function(addr, df) {
  res <- coord <- list()
  coordx <- coordy <-c()
  for(i in 1:length(addr)){
  res[[i]] <- GET('https://dapi.kakao.com/v2/local/search/keyword.json',
                  # The given webpage provides keyword search 
             query = list(query = addr[i]),
            # The information of each hospital (addr) becomes a keyword to search                   exact coordinates
             add_headers(Authorization = Sys.getenv('KAKAO_MAP_API_KEY')),
             Sys.sleep(1))}

  
  for(j in 1:length(addr)){
   # As the given webpage is written in JSON, I extract text insides two tags using       jsonlite package
    coord[[j]] <- res[[j]] %>% 
      content(as = 'text') %>% 
      fromJSON()
   
    # If there is some missing data, assign NA to the element 
    if(length(coord[[j]]$documents)==0) {
      coordx[j] <- NA
      coordy[j] <- NA
    } else {
    # Otherwise, store longitude information to -coordx-, latitude to -coordy-
      coordx[j] <- coord[[j]]$documents$x[1]
      coordy[j] <- coord[[j]]$documents$y[1]
    }   
  }
  
  # Add new columns to the previously created data frame which contains hospital         information
  df$lon <- coordx
  df$lat <- coordy
  
  print(df)
}


# Store information in the different vectors.
# safehospital <- addrCoord(fulladdr[[1]], df[[1]])
# testingplace <- addrCoord(fulladdr[[2]], df[[2]])
# drivethrough <- addrCoord(fulladdr[[3]], df[[3]])


# It takes time to scrape all the information every time, so I save them in the vectors to recall later.
#save.image("safehospital")
#save.image('testingplace')
#save.image("drivethrough")

load("safehospital")
load("testingplace")
load("drivethrough")
table(is.na(safehospital$lat))
table(is.na(drivethrough))

# Since I bind the previous data frame before cleasing with coordinates, clean it again.
testingplace[[3]] <- gsub("*\\t.*", "", testingplace[[3]])



# Focus on testing locations in Seoul
testingS <- testingplace %>% 
  filter(Region == "서울")

table(is.na(testingS$lon)) # five missing coordinates

missing2 <- testingS[is.na(testingS$lon),] 


missing2 <- gsub("*\\([^\\)]+\\)", "", missing2) #Remove parenthsese 
missing2 <- as.data.frame(missing2)



miss <- catplace(missing2)
addrCoord(miss,missing2) 
# it returns NULL again, so just keep them as NA in testingS 



missing <- drivethrough[is.na(drivethrough$lon),] #25th row has missing values

# The name of hospital has somewhat confusing information in the parentheses.
# Remove the words in parentheses
missing[[3]] <-gsub("*\\([^\\)]+\\)", "", missing[[3]]) 


# Download the coordinate information again 
missingvalue <- GET('https://dapi.kakao.com/v2/local/search/keyword.json',
             query = list(query = paste(missing[[1]], missing[[2]], missing[[3]])),
             add_headers(Authorization = "KakaoAK 4f908a1b22ff7f3d0e645d78e5ee843c"))
value <- missingvalue %>% 
      content(as = 'text') %>% 
      fromJSON()
drivethrough[25,4] <- value$documents$x[1] #longitude
drivethrough[25,5] <- value$documents$y[1] #latitude

table(is.na(drivethrough)) # No NA anymore!
```
