library(frontier)
library(xlsx)


data<-read.xlsx("c:/users/lenovo/desktop/sfa/有EF的数据 时间段.xlsx",1)

data1<-read.xlsx("c:/users/lenovo/desktop/sfa/有EF的数据 收入水平.xlsx",1)

names(data)
s<-data1[,4:18]
names(s)
s1<-s[s$group==1,]
result <- sfa( log(OFDI) ~ log(GDPit) + log(GDPjt)+log(PGDPit)+
              log(PGDPjt)+TRADE+PICK+
                log(ECDI)+GOFE+COTR+INRE+ NS+TF+BF,
               data = s1 )
# R_q <- sfa( qualified ~  N_teacher + S_mean_fix+
#               S_mean_fund+student,
#             data = raw_q )

summary(result)
efficiencies(result)
