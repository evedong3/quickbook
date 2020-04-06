rm(list = ls())
library(dplyr)
library(ggplot2)
library(readr)
library(broom)
library(ggsci)
library(patchwork)
library(lubridate)
library(stringr)
library(tidyr)
p_theme <- theme(panel.background = element_blank(),
                 panel.grid.major.y = element_line(colour = "grey"),
                 plot.title = element_text(hjust = 0.5))
df <- read_csv("CW_Analytics_EDA_Take_Home_Data.csv")
#View(df)  #glance the data
#sum(is.na(df))   # the missiong value
#dim(df)    # the dimension

df<-df%>%mutate(transaction_date_time=mdy(transaction_date_time),  # convert into date
                Year =year (transaction_date_time),    # get the year
                Month = month(transaction_date_time),   #month (number)
                Days = day(transaction_date_time),    #day
                Months = days_in_month(transaction_date_time),   #how many days in the month
                Weeks = week(transaction_date_time),    # the No of week in the total year
                Wdays = wday(transaction_date_time),   #weekday number
                md = Month + Days/Months,
                wd = Weeks+Wdays/7,   
                state_=factor(state_,
                              labels = c("California","Florida","Georgia","New York","Texas")),
                # get the full State Name
                Month = month(transaction_date_time,label = T,abbr = T),#month factor
                Wdays = wday(transaction_date_time,label = TRUE,abbr = T),#weekday factor
                Weekdays=ifelse(Wdays=="Sun"|Wdays=="Sat","weekend","weekday"))%>%
  filter(Month!="Apr")  # we don't need april in 2019
#View(df)  glance the data 
######################################################################################
# check data quality
########################################################################################
p1<-df%>%ggplot(aes(x=txn_cnt,y=..density..))+
  geom_histogram(fill="#2276C6")+p_theme+
  labs(x="Daily number of transactions")+
  scale_x_log10()

p2<-df%>%ggplot(aes(x=charge_amt,y=..density..))+
  geom_histogram(fill="#2276C6")+p_theme+
  labs(x="Daily charge volume($)")+
  scale_x_log10()

p3<-df%>%ggplot(aes(age_ind))+
  geom_bar(stat="count",fill=c("#53A120","#EE7E33"),width = 0.5)+
  p_theme+labs(x="")

p4<-df%>%ggplot(aes(transaction_date_time,fill=factor(Year)))+
  geom_histogram(bins = 100)+
  scale_fill_manual(values = c("#53A120","#EE7E33"))+
  guides(fill=F)+p_theme+labs(x="Transaction date")

p5<-df%>%ggplot(aes(state_,fill=state_))+
  geom_bar()+coord_polar()+
  theme(panel.background = element_blank(),
        axis.text.x = element_blank())+
  labs(x="SMB’s state",y="Count")+
  scale_fill_d3(labels=c("California","Florida","Georgia","New York","Texas"),)

p6<-df%>%filter(str_detect(industry,"Service"))%>%
  mutate(industry=
           str_replace(industry,"Service Provider - ",""))%>%
  ggplot(aes(industry,fill=industry))+
  geom_bar(#fill="#2276C6"
  )+
  scale_fill_d3()+
  theme(panel.background = element_blank(),
        axis.text.x = element_blank())+
  labs(x="SMB’s industry",y="Count",fill="Service Provider")+
  coord_polar()


p7<-df%>%filter(str_detect(industry,"Goods"))%>%
  mutate(industry=
           str_replace(industry,"Goods Provider - ",""))%>%
  ggplot(aes(industry,fill=industry))+
  geom_bar(#fill="#2276C6"
  )+
  scale_fill_d3()+
  theme(panel.background = element_blank(),
        axis.text.x = element_blank())+
  labs(x="SMB’s industry",y="Count",fill="Goods Provider")+
  coord_polar()

(p1+p2)/(p3+p4)/(p5+p6+p7)

#############################################################################################
#############################################################################################
## descriptive analysis
#############################################################################################
df_1 <- df%>%group_by(Days,md,Month,Year,Weekdays,Wdays)%>%
  summarise(txn_cnt_m=sum(txn_cnt)/1000,
            charge_amt_m = sum(charge_amt)/1000000,Num=n())

# the charge volume
p1 <- df_1%>%ggplot(aes(x=md,y=txn_cnt_m,col=factor(Year)))+
  geom_point(aes(shape=Weekdays))+
  geom_line()+
  geom_point(aes(x=1.7,y=13),col="blue")+   # the time that discover the coronavirus
  geom_point(aes(x=3.42,y=15),col="red")+   # the day declare a state of emergency
  scale_color_manual(values = c("#53A120","#EE7E33"))+
  p_theme+
  scale_y_continuous(limits = c(0,40))+
  labs(col="",x="",y="",title="Daily number of transactions(K)",shape="")+
  scale_x_continuous(limits = c(1,3+23/31),breaks = c(1,2,3,4),
                     labels = c("Jan.","Feb.","Mar.","Apr."))+
  theme(legend.position = "bottom")

# the daily number transactions
p2 <- df_1%>%ggplot(aes(x=md,y=charge_amt_m,col=factor(Year)))+
  geom_line()+
  geom_point(aes(shape=Weekdays))+
  geom_point(aes(x=1.7,y=7.7),col="blue")+
  geom_point(aes(x=3.42,y=8.2),col="red")+
  scale_color_manual(values = c("#53A120","#EE7E33"))+
  p_theme+guides(col=F,shape=F)+
  labs(x="",y="",title="Daily charge volume(Million $)")+
  scale_x_continuous(limits = c(1,3+23/31),breaks = c(1,2,3,4),
                     labels = c("Jan.","Feb.","Mar.","Apr."))

p2/p1

#######################################################################################
# Summary by month
dff <- df%>%group_by(Year=factor(Year),Month=factor(Month))%>%
  summarise(txn_cnt_m=sum(txn_cnt)/1000,
            charge_amt = sum(charge_amt)/1000000,Num=n())%>%
  mutate(charge_amt=ifelse(Year=="2020"&Month=="3",charge_amt/24*31,charge_amt),
         txn_cnt_m=ifelse(Year=="2020"&Month=="3",txn_cnt_m/24*31,txn_cnt_m))

p1 <- ggplot(dff%>%filter(Month%in%c(1,2,3)),
             aes(x=Month,y=charge_amt,fill=Year))+
  geom_bar(stat = "identity",position = position_dodge(0.5),width = 0.5)+
  scale_fill_manual(values = c("#53A120","#EE7E33"))+
  guides(fill=F)+
  labs(y="",title = "Monthly Average Charge Volume(Million $)",x="")+
  p_theme+theme(plot.title = element_text(size=10))+
  scale_x_discrete(labels = c("Jan.","Feb.","Mar."))
p2<-ggplot(dff%>%filter(Month%in%c(1,2,3)),aes(x=Month,y=txn_cnt_m,fill=Year))+
  geom_bar(stat = "identity",position = position_dodge(0.5),width = 0.5)+
  scale_fill_manual(values = c("#53A120","#EE7E33"))+
  labs(y="",title = "Monthly Average Number of Transactions(K)",x="",fill="")+
  p_theme+theme(plot.title = element_text(size=10))+
  scale_x_discrete(labels = c("Jan.","Feb.","Mar."))
p1+p2

#######################################################################################
###state
df_3<-df%>%filter(Month%in%c(1,2,3))%>%group_by(Month,Year,state_)%>%
  summarise(txn_cnt_m=sum(txn_cnt)/1000,
            charge_amt_m = sum(charge_amt)/1000000)
df_3$Month<- factor(df_3$Month,labels = c("Jan.","Fbr.","Mar."))
pp1<-df_3%>%  
  ggplot(aes(x=state_,y=txn_cnt_m,fill=factor(Year)))+
  geom_bar(stat = "identity",position = "dodge")+
  labs(x="",y="",fill="",title = "Number of Transactions(K)")+
  scale_fill_manual(values = c("#53A120","#EE7E33"))+
  scale_x_discrete(labels=c("California","Florida","Georgia","New York","Texas"))+
  p_theme+guides(fill=F)+theme(plot.title = element_text(size=10))+
  facet_grid(Month~.)+
  theme(axis.text.x = element_text(angle = 30,hjust = 0.5,vjust = 0.5))
pp2<- df_3%>%  
  ggplot(aes(x=state_,y=charge_amt_m,fill=factor(Year)))+
  geom_bar(stat = "identity",position = "dodge")+
  labs(x="",y="",fill="",title = "Charge Volume(Million $)")+
  scale_fill_manual(values = c("#53A120","#EE7E33"))+
  scale_x_discrete(labels=c("California","Florida","Georgia","New York","Texas"))+
  p_theme+facet_grid(Month~.)+theme(plot.title = element_text(size=10))+
  theme(axis.text.x = element_text(angle = 30,hjust = 0.5,vjust = 0.5))
pp1+pp2


################################################################################
# industry
industry_mm <- str_split(df$industry," ",simplify = T)  #split between service and goods
df$industry_s<-industry_mm[,1]
dfs_1 <- df%>%group_by(industry_s,md,Year)%>%
  summarise(txn_cnt_m=sum(txn_cnt),
            charge_amt_m = sum(charge_amt),Num=n())
ps1 <- dfs_1%>%filter(industry_s=="Service")%>%
  ggplot(aes(x=md,y=txn_cnt_m,col=factor(Year)))+
  geom_line()+
  scale_color_d3()+p_theme+
  theme(plot.title = element_text(size=10))+
  labs(col="",x="",y="",title="Number of transactions in Service")+
  scale_x_continuous(limits = c(1,3+23/31),breaks = c(1,2,3,4),
                     labels = c("Jan.","Feb.","Mar.","Apr."))

ps2 <- dfs_1%>%filter(industry_s=="Service")%>%
  ggplot(aes(x=md,y=charge_amt_m,col=factor(Year)))+
  geom_line()+
  scale_color_d3()+p_theme+guides(col=F)+
  theme(plot.title = element_text(size=10))+
  labs(x="",y="",title="Charge volume($) in Service")+
  scale_x_continuous(limits = c(1,3+23/31),breaks = c(1,2,3,4),
                     labels = c("Jan.","Feb.","Mar.","Apr."))

ps3 <- dfs_1%>%filter(industry_s=="Service")%>%
  ggplot(aes(x=md,y=Num,col=factor(Year)))+
  geom_line()+
  scale_color_d3()+p_theme+guides(col=F)+
  labs(x="",y="",title="SMB’s industry number in Service")+
  theme(plot.title = element_text(size=10))+
  scale_x_continuous(limits = c(1,3+23/31),breaks = c(1,2,3,4),
                     labels = c("Jan.","Feb.","Mar.","Apr."))+
  scale_y_continuous(limits = c(90,105))

pg1 <- dfs_1%>%filter(industry_s=="Goods")%>%
  ggplot(aes(x=md,y=txn_cnt_m,col=factor(Year)))+
  geom_line()+
  guides(col=F)+
  scale_color_d3()+p_theme+
  theme(plot.title = element_text(size=10))+
  labs(col="",x="",y="",title="Number of transactions in Goods")+
  scale_x_continuous(limits = c(1,3+23/31),breaks = c(1,2,3,4),
                     labels = c("Jan.","Feb.","Mar.","Apr."))

pg2 <- dfs_1%>%filter(industry_s=="Goods")%>%
  ggplot(aes(x=md,y=charge_amt_m,col=factor(Year)))+
  geom_line()+
  scale_color_d3()+p_theme+guides(col=F)+
  theme(plot.title = element_text(size=10))+
  labs(x="",y="",title="Charge volume($) in Goods")+
  scale_x_continuous(limits = c(1,3+23/31),breaks = c(1,2,3,4),
                     labels = c("Jan.","Feb.","Mar.","Apr."))

pg3 <- dfs_1%>%filter(industry_s=="Goods")%>%
  ggplot(aes(x=md,y=Num,col=factor(Year)))+
  geom_line()+
  scale_color_d3()+p_theme+guides(col=F)+
  theme(plot.title = element_text(size=10))+
  labs(x="",y="",title="SMB’s industry number in Goods")+
  scale_x_continuous(limits = c(1,3+23/31),breaks = c(1,2,3,4),
                     labels = c("Jan.","Feb.","Mar.","Apr."))+
  scale_y_continuous(limits = c(75,95))

(pg1|ps1)/(pg2|ps2)/(pg3|ps3)

#####################################################
## each industry
df_2<-df%>%filter(Month%in%c(1,2,3))%>%group_by(Month,Year,industry)%>%
  summarise(txn_cnt_m=sum(txn_cnt)/1000,
            charge_amt_m = sum(charge_amt)/1000000)%>%
  mutate(charge_amt_m=ifelse(Year==2020&Month==3,charge_amt_m/0.8,charge_amt_m),
         txn_cnt_m=ifelse(Year==2020&Month==3,txn_cnt_m/0.8,txn_cnt_m))
df_2$Month<- factor(df_2$Month,labels = c("Jan.","Fbr.","Mar."))

pi1<-df_2%>%filter(str_detect(industry,"Service")&Month=="Mar.")%>%
  mutate(industry=str_replace(industry,"Service Provider -",""))%>%
  ggplot(aes(y=txn_cnt_m,x=reorder(industry,txn_cnt_m),fill=factor(Year)))+
  geom_bar(stat = "identity",position = "dodge")+
  labs(x="",y="",title = "Number of transactions (K) in Service",fill="")+
  p_theme+coord_flip()+
  scale_fill_manual(values = c("#53A120","#EE7E33"))+
  theme(axis.text = element_text(size=6),
        plot.title = element_text(size=10))+
  guides(fill=F)
pi2<-df_2%>%filter(str_detect(industry,"Goods")&Month=="Mar.")%>%
  mutate(industry=str_replace(industry,"Goods Provider -",""))%>%
  ggplot(aes(y=txn_cnt_m,x=reorder(industry,txn_cnt_m),fill=factor(Year)))+
  geom_bar(stat = "identity",position = "dodge")+
  labs(x="",y="",fill="",title = "Number of transactions (K) in Goods")+
  
  p_theme+coord_flip()+scale_fill_manual(values = c("#53A120","#EE7E33"))+
  theme(axis.text = element_text(size=6),
        plot.title = element_text(size=10))

pi3<-df_2%>%filter(str_detect(industry,"Service")&Month=="Mar.")%>%
  mutate(industry=str_replace(industry,"Service Provider -",""))%>%
  ggplot(aes(y=charge_amt_m,x=reorder(industry,charge_amt_m),fill=factor(Year)))+
  geom_bar(stat = "identity",position = "dodge")+
  labs(fill="",x="",y="",title = "Charge volume(Million $) in Service")+
  p_theme+coord_flip()+scale_fill_manual(values = c("#53A120","#EE7E33"))+
  theme(axis.text = element_text(size=6),
        plot.title = element_text(size=10))+
  guides(fill=F)
pi4<-df_2%>%filter(str_detect(industry,"Goods")&Month=="Mar.")%>%
  mutate(industry=str_replace(industry,"Goods Provider -",""))%>%
  ggplot(aes(y=charge_amt_m,x=reorder(industry,charge_amt_m),fill=factor(Year)))+
  geom_bar(stat = "identity",position = "dodge")+
  labs(fill="",x="",y="",title = "Charge volume(Million $) in Goods")+
  p_theme+coord_flip()+scale_fill_manual(values = c("#53A120","#EE7E33"))+
  theme(axis.text = element_text(size=6),
        plot.title = element_text(size=10))+
  guides(fill=F)

(pi1+pi2)/(pi3+pi4)

##############################################################################
#################### age
df_3<-df%>%filter(Month%in%c(1,2,3))%>%group_by(Month,age_ind,Year)%>%
  summarise(txn_cnt_m=sum(txn_cnt)/1000,
            charge_amt_m = sum(charge_amt)/1000000)%>%
  mutate(charge_amt_m=ifelse(Year==2020&Month==3,charge_amt_m/0.8,charge_amt_m),
         txn_cnt_m=ifelse(Year==2020&Month==3,txn_cnt_m/0.8,txn_cnt_m))
df_3$Month<- factor(df_3$Month,labels = c("Jan.","Fbr.","Mar."))
df_3<-df_3%>%filter(Month=="Mar.")

pai1<-df_3%>%
  ggplot(aes(y=txn_cnt_m,x=age_ind,fill=factor(Year)))+
  geom_bar(stat = "identity",position = "dodge")+
  labs(fill="",x="",y="",title = "Number of transactions (K)")+
  p_theme+scale_fill_manual(values = c("#53A120","#EE7E33"))+
  guides(fill=F)


pai2<-df_3 %>%
  ggplot(aes(y=charge_amt_m,x=age_ind,fill=factor(Year)))+
  geom_bar(stat = "identity",position = "dodge")+
  labs(fill="",x="",y="",title = "Charge volume(Million $)")+
  p_theme+scale_fill_manual(values = c("#53A120","#EE7E33"))

pai1+pai2

##################################################################################
#################################################################################
###################use regression analysis
get_regression_test<-function(df){
  df_1 <- df%>%
    group_by(Days,md,Month,Year,Weekdays,Wdays)%>%
    summarise(txn_cnt_m=sum(txn_cnt)/1000,
              charge_amt_m = sum(charge_amt)/1000000,Num=n())  #data mining
  df_lm_1 <- df_1 <- df_1%>%filter(md<=3+24/31)    #filtering 
  x<- df_lm_1%>%filter(Year==2019)%>%              #x
    .[,c(1,2,3,4,6,7,8,9,5)]%>%arrange(Month,Days)  
  
  y<- df_lm_1%>%filter(Year==2020)%>%
    .[,c(1,2,3,4,6,7,8,9,5)]%>%arrange(Month,Days)    # yy
  x_in <- x[2:59,7,drop=T]                     # train x
  y_in <- y[1:58,7,drop=T]                     #train y
  fit1 <- lm(y_in~x_in)
  volume_tab1 <- tidy(fit1)
  volume_tab2 <- glance(fit1)
  y_f <- fit1$fitted.values             # the fitting value
  y_p<- predict(fit1,newdata = data.frame(x_in=x[60:84,7,drop=T]))   #the predict data
  y_predict <- c(y_f,y_p)[1:nrow(y)]
  y$y_p <- y_predict
  y<- na.omit(y)
  y$y_p <- ifelse(y$md<(3+0/31),NA,y$y_p)
  volume_p<-y%>%.[,c(2,7,9,10)]%>%gather(key="Var",value = "Val",-md,-Weekdays)%>%
    ggplot(aes(x=md,y=Val,col=Var))+
    geom_line()+
    #geom_point(aes(shape=Weekdays))+
    geom_vline(xintercept = 3,linetype=2)+
    p_theme+labs(col="",x="",y="",
                 title = "Daily charge volume(Million $)",shape="")+
    scale_color_manual(values = c("#53A120","#EE7E33"),
                       labels=c("Actual","Predict"))+
    scale_x_continuous(limits = c(1,3+23/31),breaks = c(1,2,3,4),labels = c("Jan.","Feb.","Mar.","Apr."))
  yp<-y%>%filter(md>=3+12/31)
  volume_t_tab <- tidy(t.test(x=yp$charge_amt_m,y=yp$y_p,paired = T))
  
  
  x_in <- x[2:59,6,drop=T]
  y_in <- y[1:58,6,drop=T]
  fit1 <- lm(y_in~x_in)
  tran_tab1 <- tidy(fit1)
  tran_tab2 <- glance(fit1)
  y_f <- fit1$fitted.values
  y_p<- predict(fit1,newdata = data.frame(x_in=x[60:83,6,drop=T]))
  y_predict <- c(y_f,y_p)[1:nrow(y)]
  y$y_p <- y_predict
  y<- na.omit(y)
  y$y_p <- ifelse(y$md<(3+0/31),NA,y$y_p)
  tran_p<-y%>%.[,c(2,6,9,10)]%>%gather(key="Var",value = "Val",-md,-Weekdays)%>%
    ggplot(aes(x=md,y=Val,col=Var))+
    geom_line()+
    #geom_point(aes(shape=Weekdays))+
    geom_vline(xintercept = 3,linetype=2)+
    p_theme+labs(col="",x="",y="",
                 title = "Daily number of transactions(K)",shape="")+
    scale_color_manual(values = c("#53A120","#EE7E33"),
                       labels=c("Actual","Predict"))+
    scale_x_continuous(limits = c(1,3+23/31),breaks = c(1,2,3,4),labels = c("Jan.","Feb.","Mar.","Apr."))
  yp<-y%>%filter(md>=3+12/31)
  tran_t_tab<-tidy(t.test(x=yp$txn_cnt_m,y=yp$y_p,paired = T))
  return(list(volume_tab1,volume_tab2,volume_t_tab,
              tran_tab1,tran_tab2,tran_t_tab,
              volume_p,tran_p))
}

res <- df%>%get_regression_test
res

res_mm <-matrix(0,nrow = 19,ncol=8,
       dimnames = list(unique(df$industry),
                       c("charge_r","charge_p","charge_t","charge_tp",
                         "tran_r","tran_p","tran_t","tran_tp")))
industry_mm <- unique(df$industry)
dim(res_mm)
i<-1
for(i in 1:19){
  res <- df%>%filter(industry==industry_mm[i]) %>%get_regression_test
  res_mm[i,2]<-res[[1]][2,5,drop=T]
  res_mm[i,1]<-res[[2]][1,2,drop=T]
  res_mm[i,3]<-res[[3]][1,2,drop=T]
  res_mm[i,4]<-res[[3]][1,3,drop=T]
  res_mm[i,6]<-res[[4]][2,5,drop=T]
  res_mm[i,5]<-res[[5]][1,2,drop=T]
  res_mm[i,7]<-res[[6]][1,2,drop=T]
  res_mm[i,8]<-res[[6]][1,3,drop=T]
}
write.csv(res_mm,file = "industry_gression.csv",row.names =T)