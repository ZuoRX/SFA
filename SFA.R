
#library(Benchmarking)
library(frontier)
library(xlsx)


#多投入多产出
raw<-read.csv("c:/users/lenovo/desktop/SFA/data/raw.csv")
names(raw)
raw1<-raw[,3:9]
result <- frontierTranslogRay( yNames = c( "N_teacher","S_mean_fix","S_mean_fund" ),
                                     xNames = c( "student", "m_score", 
                                                 "qualified","well"),
                                     data = raw1)
summary(result)
efficiencies(result)
coef(result)


#多投入单产出

#===评估合格率===#
raw_q<-raw[,3:7]
R_q <- sfa( log(qualified) ~ log(tms) + log(S_mean_fix)+
              log(S_mean_fund)+log(student),
              data = raw_q )
# R_q <- sfa( qualified ~  N_teacher + S_mean_fix+
#               S_mean_fund+student,
#             data = raw_q )

summary(R_q)
efficiencies(R_q)


#===评估良好率===#
raw_w<-raw[,c(3,4,5,6,8)]
R_w <- sfa( log(well) ~  log( tms) +log(S_mean_fix)+
              log(S_mean_fund)+log(student),
              data = raw_w )
# R_w <- sfa( well ~  N_teacher + S_mean_fix+      
#               S_mean_fund+student,
#             data = raw_w )

summary(R_w)
efficiencies(R_w)



#===评估优秀率===#
raw_e<-raw[,c(3,4,5,6,9)]
R_e <- sfa( log(excellent) ~ log( tms) + log(S_mean_fix)+
              log(S_mean_fund)+log(student),
              data = raw_e )

summary(R_e)
efficiencies(R_e)


#===良好与优秀的一个对比===#

raw1<-read.xlsx("c:/users/lenovo/desktop/SFA/data/raw1.xlsx",1)
names(raw1)
raw_we<-raw1[,3:7]

R_we <- sfa( log(w.e) ~ log(tms) + log(S_mean_fix)+
              log(S_mean_fund)+log(student),
            data = raw_we )

summary(R_we)
efficiencies(R_we)

