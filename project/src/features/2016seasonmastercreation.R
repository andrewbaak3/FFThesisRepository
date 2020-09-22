library(data.table)

week1<-fread("./project/volume/data/external/external_data/weekly/2016/week1.csv")
week2<-fread("./project/volume/data/external/external_data/weekly/2016/week2.csv")
week3<-fread("./project/volume/data/external/external_data/weekly/2016/week3.csv")
week4<-fread("./project/volume/data/external/external_data/weekly/2016/week4.csv")
week5<-fread("./project/volume/data/external/external_data/weekly/2016/week5.csv")
week6<-fread("./project/volume/data/external/external_data/weekly/2016/week6.csv")
week7<-fread("./project/volume/data/external/external_data/weekly/2016/week7.csv")
week8<-fread("./project/volume/data/external/external_data/weekly/2016/week8.csv")
week9<-fread("./project/volume/data/external/external_data/weekly/2016/week9.csv")
week10<-fread("./project/volume/data/external/external_data/weekly/2016/week10.csv")
week11<-fread("./project/volume/data/external/external_data/weekly/2016/week11.csv")
week12<-fread("./project/volume/data/external/external_data/weekly/2016/week12.csv")
week13<-fread("./project/volume/data/external/external_data/weekly/2016/week13.csv")
week14<-fread("./project/volume/data/external/external_data/weekly/2016/week14.csv")
week15<-fread("./project/volume/data/external/external_data/weekly/2016/week15.csv")
week16<-fread("./project/volume/data/external/external_data/weekly/2016/week16.csv")
week17<-fread("./project/volume/data/external/external_data/weekly/2016/week17.csv")

week1<-data.table(week1)
week2<-data.table(week2)
week3<-data.table(week3)
week4<-data.table(week4)
week5<-data.table(week5)
week6<-data.table(week6)
week7<-data.table(week7)
week8<-data.table(week8)
week9<-data.table(week9)
week10<-data.table(week10)
week11<-data.table(week11)
week12<-data.table(week12)
week13<-data.table(week13)
week14<-data.table(week14)
week15<-data.table(week15)
week16<-data.table(week16)
week17<-data.table(week17)

week1$week <-1
week2$week <-2
week3$week <-3
week4$week <-4
week5$week <-5
week6$week <-6
week7$week <-7
week8$week <-8
week9$week <-9
week10$week <-10
week11$week <-11
week12$week <-12
week13$week <-13
week14$week <-14
week15$week <-15
week16$week <-16
week17$week <-17

Master2016<-rbind(week1,week2,week3,week4,week5,week6,week7,week8,week9,week10,week11,week12,week13,week14,
                  week15,week16,week17)


fwrite(Master2016, "./project/volume/data/interim/2016season.csv")
