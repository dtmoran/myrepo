smoking_data <- read.csv c("https://files.digital.nhs.uk/2C/EBBBB1/smok-time-del-eng-q4-2122.csv.csv",
                           "https://files.digital.nhs.uk/24/7C5D8E/smok-time-del-eng-q4-2021-v2.csv.csv",
                           "https://files.digital.nhs.uk/9B/188BD6/smok-time-deli-eng-q4-1920.csv")



c("https://files.digital.nhs.uk/2C/EBBBB1/smok-time-del-eng-q4-2122.csv.csv",
  "https://files.digital.nhs.uk/24/7C5D8E/smok-time-del-eng-q4-2021-v2.csv.csv",
  "https://files.digital.nhs.uk/9B/188BD6/smok-time-deli-eng-q4-1920.csv")


Smoking_DF <- map_dfr(smoking_data,data.frame())




smoking_data1 <- read.csv ("https://files.digital.nhs.uk/2C/EBBBB1/smok-time-del-eng-q4-2122.csv.csv")
smoking_data2 <- read.csv ("https://files.digital.nhs.uk/24/7C5D8E/smok-time-del-eng-q4-2021-v2.csv.csv")
smoking_data3 <- read.csv ("https://files.digital.nhs.uk/9B/188BD6/smok-time-deli-eng-q4-1920.csv")




Smoking_DF <- map_dfr(smoking_data1,smoking_data2,smoking_data3,data.frame())




smoking_data <- function(id) {
    read.csv c("https://files.digital.nhs.uk/2C/EBBBB1/smok-time-del-eng-q4-2122.csv.csv",
               "https://files.digital.nhs.uk/24/7C5D8E/smok-time-del-eng-q4-2021-v2.csv.csv",
               "https://files.digital.nhs.uk/9B/188BD6/smok-time-deli-eng-q4-1920.csv")  }
    

Smoking_DF <- map_dfr(smoking_data,data.frame())