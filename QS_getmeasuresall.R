# get_quality_standard function

library(tidyverse)
get_quality_standard <- function(id) {
    page <- read_html(paste0 ("https://www.nice.org.uk/guidance/qs", id)) 
    name <- page %>% html_nodes("#content-start") %>%
        html_text()
    return(page %>% html_nodes("a") %>% html_attr("href") %>% 
               data.frame() %>% 
               filter(str_detect (.,"/chapter/Quality-statement-")) %>%
               mutate(qualitystandard=name,
                      qualitystandardid=id) %>%
               rename(qualitystatement=1))
}


get_quality_standard(201) 


standards <- map_dfr(1:250, possibly (get_quality_standard, data.frame()))