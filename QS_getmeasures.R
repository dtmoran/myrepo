# get_quality_standard function

library(tidyverse)
get_quality_standard <- function(id {
    page <- read_html(paste0 ("https://www.nice.org.uk/guidance/qs", id)) 
    name <- page %>% html_nodes("#content-start") %>%
        html_text()
     return(page %>% html_nodes("a") %>% html_attr("href") %>% 
               data.frame() %>% 
               filter(str_detect (.,"/chapter/Quality-statement-")) %>%
               mutate(qualitystandard=name,
                      qualitystandardid=id,
                      start_of_webaddress=("https://www.nice.org.uk") %>%  #added
                          full_web_add <- paste0("start_of_webaddress","1") %>%
               rename(qualitystatement=1)) 
            }



get_quality_standard(16) 


standards <- map_dfr(1:10, possibly (get_quality_standard, data.frame()))



# STR_C(start_of_webaddress,qualitystatement)
#get_quality_standard$full_web_add <- paste0(get_quality_standard$start_of_webaddress,get_quality_standard$qualitystatement))  

#standards <- dplyr::mutate(data_frame, start_of_webaddress="https://www.nice.org.uk")  #added not worked

#added below
#get_measures <- function(get_quality_standard) {
#    page <- read_html(paste0 ("https://www.nice.org.uk", qualitystatement)) 
#    name <- page %>% html_nodes("#qs16_outcome , #qs16_process , #qs16_structure , .title , #qs16_quality-statement") %>%
#        html_text()
#}





