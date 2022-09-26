# extract NHSD smoking at time of delivery to map to QS22/5. Quarterly data extracted for FY2122 and FY2021


library(tidyverse)
library(magrittr)

smoking_data <- c("https://files.digital.nhs.uk/2C/EBBBB1/smok-time-del-eng-q4-2122.csv.csv",
                           "https://files.digital.nhs.uk/24/7C5D8E/smok-time-del-eng-q4-2021-v2.csv.csv"#,
                          # "https://files.digital.nhs.uk/9B/188BD6/smok-time-deli-eng-q4-1920.csv"
                  ) %>%
       map_dfr (~read.csv(., colClasses = c("Value"="numeric"),sep=";")) 




#As above, alternative method


smoking_data1 <- read.csv ("https://files.digital.nhs.uk/2C/EBBBB1/smok-time-del-eng-q4-2122.csv.csv"), 
                           colClasses = c("Value"="numeric"),sep=";")
smoking_data2 <- read.csv ("https://files.digital.nhs.uk/24/7C5D8E/smok-time-del-eng-q4-2021-v2.csv.csv", 
                           colClasses = c("Value"="numeric"),sep=";")
smoking_data3 <- read.csv ("https://files.digital.nhs.uk/9B/188BD6/smok-time-deli-eng-q4-1920.csv",
                           colClasses = c("Value"="numeric"),sep=";")

Smoking_DF <- bind_rows(smoking_data1,smoking_data2)

strsplit(Smoking_Data, ",",fixed=T)


# manual import- combine

Smoking_DF <- bind_rows(smok_time_del_2021,smok_time_del_2122) 

# Calculate outputs


output <- Smoking_DF %>% 
   # filter(OrgName=="England") %>%
    filter(OrgName %in% c("London","South East","South West","East of England",
                          "Midlands","North East and Yorkshire","North West")) %>%
    mutate(Value=as.integer(Value))%>%
    
    pivot_wider(names_from = Measure, values_from = Value) %>%
    
    mutate(percentage=`Women known to be smokers at time of delivery`/`Number of maternities`)

openxlsx::write.xlsx(output, "output.xlsx")


#group


output <- smoking_data %>% 
    # filter(OrgName=="England") %>%
    filter(OrgName %in% c("London","South East")) %>%
    mutate(Value=as.integer(Value))%>%
    group_by(FinancialYear, Quarter, Measure) %>%
    summarise(Value=sum(Value)) %>%
    
    pivot_wider(names_from = Measure, values_from = Value) %>%
    
    mutate(percentage=`Women known to be smokers at time of delivery`/`Number of maternities`)

#group2


output <- Smoking_DF %>% 
    filter(OrgName=="England") %>%
    #filter(OrgName %in% c("London","South East","South West","East of England",
    #                      "Midlands","North East and Yorkshire","North West")) %>%
    mutate(Value=as.integer(Value))%>%
    group_by(FinancialYear, Quarter, Measure) %>%
    summarise(Value=sum(Value)) %>%
    
    pivot_wider(names_from = Measure, values_from = Value) %>%
    
    
    mutate(percentage=`Women known to be smokers at time of delivery`/`Number of maternities`)


#chart line

#people by date
output_chart<-ggplot(data=output, aes(x=date, y="People (calculated)")) +
    geom_line(stat='identity',color="darkblue", fill="lightblue")

Abem_chart2

)




# manual import- combine

Smoking_DF2 <- bind_rows(smok_time_del_2021,smok_time_del_2122) 
 
    Smoking_DF2$YearQTR <- paste(Smoking_DF2$FinancialYear,Smoking_DF2$Quarter)
    
    Smoking_DF2

#group2
    
    
    output2 <- Smoking_DF2 %>% 
        #filter(OrgName=="England") %>%
        filter(OrgName %in% c("London","South East","South West","East of England",
                             "Midlands","North East and Yorkshire","North West")) %>%
        mutate(Value=as.integer(Value))%>%
        group_by(YearQTR,OrgName,Measure) %>%
        summarise(Value=sum(Value)) %>%
        
        pivot_wider(names_from = Measure, values_from = Value) %>%
        
        mutate(percentage=`Women known to be smokers at time of delivery`/`Number of maternities`)
    
    
    #chart line1

    output_chart1<-ggplot(data=output2, aes(x=OrgName, y=percentage)) +
        geom_line(stat='identity',color="darkblue", fill="lightblue")
    
        output_chart1
        
        
        
        
        #chart bar2
        
        output_chart2<-ggplot(data=output2, aes(x=YearQTR, y=percentage)) +
            geom_bar(stat='identity',color="darkblue", fill="lightblue") +
        facet_wrap(~OrgName) +
            labs(x="Year Quarter", y="Percentage", title="Proportion of Women known to be smokers at time of delivery")

        
        output_chart2
        

        # manual import- combine3
        
        Smoking_DF3 <- bind_rows(smok_time_del_2021,smok_time_del_2122) 
        
        Smoking_DF3$YearQTR <- paste(Smoking_DF2$FinancialYear,Smoking_DF2$Quarter)
        
        Smoking_DF3
        
        #group3
        
        
        output3 <- Smoking_DF3 %>% 
            filter(OrgName=="England") %>%
            #filter(OrgName %in% c("London","South East","South West","East of England",
            #                      "Midlands","North East and Yorkshire","North West")) %>%
            mutate(Value=as.integer(Value))%>%
            group_by(YearQTR,OrgName,Measure) %>%
            summarise(Value=sum(Value)) %>%
            
            pivot_wider(names_from = Measure, values_from = Value) %>%
            
            mutate(percentage=`Women known to be smokers at time of delivery`/`Number of maternities`)
        
        #chart bar3
        
        output_chart3<-ggplot(data=output3, aes(x=YearQTR, y=percentage)) +
            geom_bar(stat='identity',color="darkblue", fill="lightblue") +
            facet_wrap(~OrgName) +
            labs(x="Year Quarter", y="Percentage", title="Proportion of Women known to be smokers at time of delivery")
        
        
        output_chart3
        
        
        #chart line3
        
        output_line3<-ggplot(data=output3, aes(x=YearQTR, y=percentage)) +
            geom_line(stat='identity',color="darkblue", fill="lightblue") +
            facet_wrap(~OrgName) +
            labs(x="Year Quarter", y="Percentage", title="Proportion of Women known to be smokers at time of delivery")
        
        
        output_line3
        