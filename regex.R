data_statements %>% 
    distinct(statement) %>%
    pull(statement)
    
mutate(QSID = paste0("QS",qualitystandardid," - ",qualitystandard)) %>%
    distinct(Statements) %>%
    pull(QSID)


#wrangle data_statements for quality statements
temp <- data_statements %>%
    mutate(standardid = str_extract(qualitystatement, "(?<=/guidance/qs)\\d+(?=/chapter)"),
           statementid = str_extract(qualitystatement, "(?<=-statement-)\\d+(?=-)"),
           statement_short = sub(".*chapter/Quality-statement-", "", qualitystatement),
           statement_tidy = str_remove(statement, "Quality statement"),
           statement_short = str_replace_all(statement_short,"-", " "),
           statement_short = sub('.', '', statement_short),
           statement_short = sub('.', '', statement_short),
           statement_short = str_trim(statement_short, side = "left"),
           statement_long = str_trim(statement_tidy, side = "left"),
           QSID2 = paste0("QS",standardid," - "),
           standard_statementid = paste0(standardid,".",statementid),
           QSID_statement = paste0(QSID2, statementid))


quality_statement <- temp[,c('standardid','standard_statementid','statement_short', 'statement_long' )]           

write_rds(quality_standards, file = "quality_statement.rds")


#wrangle data_statements for measures

measure <- data_statements %>%
    mutate(standardid = str_extract(qualitystatement, "(?<=/guidance/qs)\\d+(?=/chapter)"),
           statementid = str_extract(qualitystatement, "(?<=-statement-)\\d+(?=-)"),
           datasource_structure = sub(".*Data source:", "", structure),
           statement_short = sub(".*chapter/Quality-statement-", "", qualitystatement),
           statement_tidy = str_remove(statement, "Quality statement"),
           statement_short = str_replace_all(statement_short,"-", " "),
           statement_short = sub('.', '', statement_short),
           statement_short = sub('.', '', statement_short),
           statement_short = str_trim(statement_short, side = "left"),
           statement_long = str_trim(statement_tidy, side = "left"),
           QSID2 = paste0("QS",standardid," - "),
           standard_statementid = paste0(standardid,".",statementid),
           QSID_statement = paste0(QSID2, statementid))


