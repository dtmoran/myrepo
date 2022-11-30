
library(rvest)
library(dplyr)
library(xml2)
library(magrittr)
library(stringr)

QS16_1 <- read_html("https://www.nice.org.uk/guidance/qs16/chapter/Quality-statement-1-Multidisciplinary-management")
QS16_1 %>%
    html_nodes("#qs16_outcome , #qs16_process , #qs16_structure , .title , #qs16_quality-statement") %>%
    html_text()


#extract quality statement

QS16_1 %>%
    html_nodes("#qs16_quality-statement") %>%
    html_text() %>%
    
    str_replace_all("\r\n", "") %>% # Remove carriage return control character
    str_trim() 


#extract structure 

QS16_1 %>%
    html_nodes("#qs16_structure") %>%
    html_text() %>%

str_replace_all("\r\n", "") %>% # Remove carriage return control character
    str_trim() 

#extract structure 2

QS16_1 %>%
    html_elements('div[title= "Quality measures"] div[title="Structure"] p') %>% 
    html_text2() %>% 
    
    str_replace_all("\r\n", "") %>% # Remove carriage return control character
    str_trim() 

#extract process 

QS16_1 %>%
    html_nodes("#qs16_process") %>%
    html_text() %>%
    
    str_replace_all("\r\n", "") %>% # Remove carriage return control character
    str_trim() 

#extract process 2 

QS16_1 %>%
    html_elements('div[title= "Quality measures"] div[title="Process"] p') %>% 
    html_text2() %>% 
    
    str_replace_all("\r\n", "") %>% # Remove carriage return control character
    str_trim() 


#extract outcome 2 

QS16_1 %>%
    html_elements('div[title= "Quality measures"] div[title="Outcome"] p') %>% 
    html_text2() %>% 
    
    str_replace_all("\r\n", "") %>% # Remove carriage return control character
    str_trim() 

