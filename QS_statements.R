

# get_quality_statements function
get_quality_statement <- function(qs,id) {
message(qs)  
    page <- read_html(paste0 ("https://www.nice.org.uk",qs))
    
    statement <- page %>%
        html_nodes(paste0("#qs",id,"_quality-statement")) %>%
        html_text() %>%
        str_replace_all("\r\n", "") %>% # Remove carriage return control character
        str_trim()
    
    structure <- page %>%
        html_nodes(paste0("#qs",id,"_structure")) %>%
        html_text() %>%
        str_replace_all("\r\n", "") %>% # Remove carriage return control character
        str_trim() 
    
    process <- page %>%
        html_nodes(paste0("#qs",id,"_process")) %>%
        html_text() %>%
        str_replace_all("\r\n", "") %>% # Remove carriage return control character
        str_trim() 
    
    outcome <- page %>%
        html_nodes(paste0("#qs",id,"_outcome")) %>%
        html_text() %>%
        str_replace_all("\r\n", "") %>% # Remove carriage return control character
        str_trim() 
    
    data.frame(
        list(
            "qualitystatement" = qs,
            "statement" = ifelse(length(statement)==0,NA,statement),
            "structure" = ifelse(length(structure)==0,NA,structure),
            "process" = ifelse(length(process)==0,NA,process),
            "outcome" = ifelse(length(outcome)==0,NA,outcome)
        )
    )
}

get_quality_statement("/guidance/qs10/chapter/Quality-statement-4-Pulmonary-rehabilitation-for-stable-COPD-and-exercise-limitation",10)

statements <- map2_dfr(standards$qualitystatement, standards$qualitystandardid, get_quality_statement)

write_rds(statements, "data_statements.rds")
        