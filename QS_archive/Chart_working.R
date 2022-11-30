Abemaciclib_data <- read.csv ("C:\\R_Projects\\myrepo\\Abemaciclib.csv")

#ADD by date
Abem_chart<-ggplot(data=Abemaciclib_data, aes(x=date, y=ADD)) +
    geom_bar(stat='identity',color="darkblue", fill="lightblue") + geom_hline(yintercept = 20000)


Abem_chart

#people by date
Abem_chart2<-ggplot(data=Abemaciclib_data, aes(x=date, y="People (calculated)")) +
    geom_line(stat='identity',color="darkblue", fill="lightblue")
    
    Abem_chart2
    
    
    
#People by year

Abemaciclib_data3 <- read.csv ("C:\\R_Projects\\myrepo\\Abemaciclib3.csv")
Abemaciclib_data3

Abem_chart3<-ggplot(data=Abemaciclib_data3, aes(x=Year, y=People)) +
    geom_bar(stat='identity',color="darkblue", fill="lightblue") + geom_hline(yintercept = 4059)

Abem_chart3

Abemaciclib_treatment_
library(reactable)


reactable(Abemaciclib_treatment_)

library(reactable)
Abemaciclib_table1<- read.csv ("C:/R_Projects/myrepo/Abemaciclib_table1.csv") %>%
    reactable(Abemaciclib_table1)


read.csv ("C:\\R_Projects\\myrepo\\Abemaciclib_table2.csv")
