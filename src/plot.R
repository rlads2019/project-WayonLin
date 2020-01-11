library(dplyr)
library(ggplot2)
all_data <- readr::read_csv('data/all.csv')
all_data

#將每個年度的資料按照轉系申請人數排序(好像不需要)
for (i in 99:108){
  transfer_rank_year <- all_data %>% filter(年度 == i) %>% mutate(轉系申請人數 = 轉系轉入申請志願1+轉系轉入申請志願2) %>% arrange(desc(轉系申請人數))
  transfer_rank_year <- transfer_rank_year %>% select(id,年度,代碼,學系,學院,轉系申請人數)
  if (i==99){
    transfer_rank <- transfer_rank_year
  }  else {
    transfer_rank <- rbind(transfer_rank,transfer_rank_year)
  }
}

# 99-108年申請轉系總數變化
transfer_total_year <- all_data %>% group_by(年度) %>% summarise(申請轉系總數=sum(轉系轉出申請,na.rm=T))
ggplot(data = transfer_total_year) +
  geom_line(mapping = aes(x = 年度, y = 申請轉系總數)) +
  labs(title = "99-108年申請轉系總數變化") +
  scale_y_continuous(limits = c(500,700)) +
  theme(axis.title.y=element_text(vjust=2),plot.title = element_text(hjust = 0.5))
ggsave(paste0("99-108年申請轉系總數變化(轉出).png"))

# 99-108年各系所申請轉入人數
ggplot(data =transfer_rank) +
  geom_bar(mapping = aes(x = 學系, y = 轉系申請人數),
           stat = "identity") +
  facet_wrap(vars(年度)) +
  labs(title = "99-108年各系所申請轉入人數", y= "申請轉入人數") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_text(vjust=2),
        plot.title = element_text(hjust = 0.5)) 
ggsave(paste0("99-108年各系所申請轉入人數.png"))

# 申請轉入人數前五名
for (i in 99:108)
{
transfer_rank_year <- all_data %>% filter(年度 == i) %>% mutate(轉系申請人數 = 轉系轉入申請志願1+轉系轉入申請志願2) %>% arrange(desc(轉系申請人數))
transfer_rank_year <- transfer_rank_year %>% select(id,年度,代碼,學系,學院,轉系申請人數)
transfer_rank_year <- transfer_rank_year[1:5,1:6]
ggplot(data =transfer_rank_year) +
  geom_bar(mapping = aes(x = reorder(學系, -轉系申請人數), y = 轉系申請人數),
            stat = "identity") +
  scale_y_continuous(limits = c(0,120)) +
  labs(title = paste0(i,"年申請轉入人數前五名"), x = "學系名稱", y= "申請轉入總人數") +
  theme(axis.title.x=element_text(vjust=-0.5),axis.title.y=element_text(vjust=1),plot.title = element_text(hjust = 0.5))
ggsave(paste0(i,".png"))

}

#計算競爭強度(申請轉入人數/名額)
all_data$轉系轉入核准 <- as.double(all_data$轉系轉入核准)
popular_rank <- all_data %>% 
                mutate(轉系申請人數 = 轉系轉入申請志願1+轉系轉入申請志願2) %>%
                filter(轉系轉入核准>0) %>%
                mutate(競爭強度 = 轉系申請人數/轉系轉入核准) %>%
                select(id,年度,代碼,學系,學院,轉系申請人數,競爭強度)

# 100-108競爭強度前五名
for (i in 99:108)
{
  popular_rank_year <- popular_rank %>% filter(年度 == i) %>% arrange(desc(競爭強度))
  popular_rank_year <- popular_rank_year[1:5,]
  ggplot(data =popular_rank_year) +
    geom_bar(mapping = aes(x = reorder(學系, -競爭強度), y = 競爭強度),
             stat = "identity") +
    scale_y_continuous(limits = c(0,30)) +
    labs(title = paste0(i,"年轉系競爭強度前五名"), x = "學系名稱", y= "競爭強度(申請人數/核准人數)") +
    theme(axis.title.x=element_text(vjust=-0.5),axis.title.y=element_text(vjust=1),plot.title = element_text(hjust = 0.5))
  ggsave(paste0(i,".png"))
  
}

# 99-108年申請雙主修總數變化
bisubject_total_year <- all_data %>% group_by(年度) %>% summarise(申請雙主總數=sum(雙主轉出申請,na.rm=T))
ggplot(data = bisubject_total_year) +
  geom_line(mapping = aes(x = 年度, y = 申請雙主總數)) +
  scale_y_continuous(limits = c(1000,1500)) +
  labs(title = "99-108年申請雙主總數變化") +
  theme(axis.title.y=element_text(vjust=2),plot.title = element_text(hjust = 0.5))
ggsave(paste0("99-108年申請雙主總數變化(轉出).png"))

# 申請此系為雙主修人數前五名
for (i in 99:108)
{
  bisubject_rank_year <- all_data %>% filter(年度 == i) %>%  select(id,年度,代碼,學系,學院,雙主轉入申請) %>% arrange(desc(雙主轉入申請))
  bisubject_rank_year <- bisubject_rank_year[1:5,]
  ggplot(data =bisubject_rank_year) +
    geom_bar(mapping = aes(x = reorder(學系, -雙主轉入申請), y = 雙主轉入申請),
             stat = "identity") +
    scale_y_continuous(limits = c(0,360)) +
    labs(title = paste0(i,"年申請雙主轉入人數前五名"), x = "學系名稱", y= "雙主轉入申請人數") +
    theme(axis.title.x=element_text(vjust=-0.5),axis.title.y=element_text(vjust=1),plot.title = element_text(hjust = 0.5))
  ggsave(paste0(i,".png"))
  
}
# 100-108雙主修競爭強度前五名
all_data$雙主轉入核准 <- as.double(all_data$雙主轉入核准)
all_data$雙主轉入申請 <- as.double(all_data$雙主轉入申請)
bisubject_popular_rank <- all_data %>% 
  filter(雙主轉入核准>0) %>%
  filter(雙主轉入申請>=0) %>%
  mutate(競爭強度 = 雙主轉入申請/雙主轉入核准) %>%
  select(id,年度,代碼,學系,學院,雙主轉入申請,競爭強度)
for (i in 99:108)
{
  popular_rank_year <- bisubject_popular_rank %>% filter(年度 == i) %>% arrange(desc(競爭強度))
  popular_rank_year <- popular_rank_year[1:5,]
  ggplot(data =popular_rank_year) +
    geom_bar(mapping = aes(x = reorder(學系, -競爭強度), y = 競爭強度),
             stat = "identity") +
    scale_y_continuous(limits = c(0,100)) +
    labs(title = paste0(i,"年雙主修競爭強度前五名"), x = "學系名稱", y= "競爭強度(申請人數/核准人數)") +
    theme(axis.title.x=element_text(vjust=-0.5),axis.title.y=element_text(vjust=1),plot.title = element_text(hjust = 0.5))
  ggsave(paste0(i,".png"))
  
}

# 99-108年申請輔系總數變化
auxsubject_total_year <- all_data %>% group_by(年度) %>% summarise(申請輔系總數=sum(輔系轉入申請,na.rm=T))
ggplot(data = auxsubject_total_year) +
  geom_line(mapping = aes(x = 年度, y = 申請輔系總數)) +
  scale_y_continuous(limits = c(2100,3000)) +
  labs(title = "99-108年申請輔系總數變化") +
  theme(axis.title.y=element_text(vjust=2),plot.title = element_text(hjust = 0.5))
ggsave(paste0("99-108年申請輔系總數變化(轉入).png"))

# 申請此系為輔系人數前五名
for (i in 99:108)
{
  auxsubject_rank_year <- all_data %>% filter(年度 == i) %>%  select(id,年度,代碼,學系,學院,輔系轉入申請) %>% arrange(desc(輔系轉入申請))
  auxsubject_rank_year <- auxsubject_rank_year[1:5,]
  ggplot(data =auxsubject_rank_year) +
    geom_bar(mapping = aes(x = reorder(學系, -輔系轉入申請), y = 輔系轉入申請),
             stat = "identity") +
    scale_y_continuous(limits = c(0,500)) +
    labs(title = paste0(i,"年申請輔系轉入人數前五名"), x = "學系名稱", y= "輔系轉入申請人數") +
    theme(axis.title.x=element_text(vjust=-0.5),axis.title.y=element_text(vjust=1),plot.title = element_text(hjust = 0.5))
  ggsave(paste0(i,".png"))
  
}

# 99-108輔系競爭強度前五名
all_data$輔系轉入核准 <- as.double(all_data$輔系轉入核准)
all_data$輔系轉入申請 <- as.double(all_data$輔系轉入申請)
auxsubject_popular_rank <- all_data %>% 
  filter(輔系轉入核准>0) %>%
  filter(輔系轉入申請>=0) %>%
  mutate(競爭強度 =輔系轉入申請/輔系轉入核准) %>%
  select(id,年度,代碼,學系,學院,輔系轉入申請,競爭強度)
for (i in 99:108)
{
  popular_rank_year <- auxsubject_popular_rank %>% filter(年度 == i) %>% arrange(desc(競爭強度))
  popular_rank_year <- popular_rank_year[1:5,]
  ggplot(data =popular_rank_year) +
    geom_bar(mapping = aes(x = reorder(學系, -競爭強度), y = 競爭強度),
             stat = "identity") +
    scale_y_continuous(limits = c(0,100)) +
    labs(title = paste0(i,"年輔系競爭強度前五名"), x = "學系名稱", y= "競爭強度(申請人數/核准人數)") +
    theme(axis.title.x=element_text(vjust=-0.5),axis.title.y=element_text(vjust=1),plot.title = element_text(hjust = 0.5))
  ggsave(paste0(i,".png"))
  
}

# 99-108年各系雙轉輔申請人數與核准人數變化
all_data$轉系轉入招收名額 <- as.double(all_data$轉系轉入招收名額)
all_data$雙主轉入招收名額 <- as.double(all_data$雙主轉入招收名額)
#all_data$輔系轉入招收名額 <- as.double(all_data$輔系轉入招收名額)
subject_name = "資訊工程學系"
cs_data <- all_data %>% filter(學系==subject_name) 
cs_change <- cs_data %>% mutate(雙轉輔申請 = 轉系轉入申請志願1+轉系轉入申請志願2 +雙主轉入申請+輔系轉入申請,
                                     雙轉輔核准=轉系轉入核准+雙主轉入核准+輔系轉入核准)
                                     #雙轉輔名額 = 轉系轉入招收名額+雙主轉入招收名額+輔系轉入招收名額)
ggplot(cs_change,aes(x=年度)) +
  geom_line(aes( y = 雙轉輔申請,colour="申請人數")) +
  geom_line( aes(y = 雙轉輔核准,colour="核准人數")) +
  #geom_line( aes(y = 雙轉輔名額,colour="招收人數")) +
  scale_colour_manual("",values=c("申請人數"="red","核准人數"="blue"))+
  labs(title = paste0("99-108年",subject_name,"雙轉輔申請人數與核准人數變化.png"),y="人數") +
  theme(axis.title.y=element_text(vjust=2),plot.title = element_text(hjust = 0.5))
ggsave(paste0("99-108年",subject_name,"雙轉輔申請人數與核准人數變化.png"))

# 99-108年各系雙轉輔申請人數與新生入學人數變化
subject_name = "資訊工程學系"
cs_data <- all_data %>% filter(學系==subject_name) 
cs_change <- cs_data %>% mutate(雙轉輔申請 = 轉系轉入申請志願1+轉系轉入申請志願2 +雙主轉入申請+輔系轉入申請,
                                     新生人數=考試分發+繁星計畫+個人申請)
ggplot(cs_change,aes(x=年度)) +
  geom_line(aes( y = 雙轉輔申請,colour="雙轉輔申請")) +
  geom_line( aes(y = 新生人數,colour="新生入學")) +
  scale_colour_manual("",values=c("雙轉輔申請"="red","新生入學"="blue"))+
  labs(title = paste0("99-108年",subject_name,"雙轉輔申請人數與新生入學人數變化.png"),y="人數") +
  theme(axis.title.y=element_text(vjust=2),plot.title = element_text(hjust = 0.5))
ggsave(paste0("99-108年",subject_name,"雙轉輔申請人數與新生入學人數變化.png"))

# 99-108年全校雙轉輔申請人數與核准人數變化
school_data <- all_data %>% mutate(雙轉輔申請 = 轉系轉入申請志願1+轉系轉入申請志願2 +雙主轉入申請+輔系轉入申請,
                                       雙轉輔核准=轉系轉入核准+雙主轉入核准+輔系轉入核准) %>%
                group_by(年度) %>% summarise(申請總數=sum(雙轉輔申請,na.rm= T),核准總數=sum(雙轉輔核准,na.rm=T))
ggplot(school_data,aes(x=年度)) +
  geom_line(aes( y = 申請總數,colour="申請人數")) +
  geom_line( aes(y = 核准總數,colour="核准人數")) +
  scale_colour_manual("",values=c("申請人數"="red","核准人數"="blue"))+
  labs(title = "全校雙轉輔申請人數與核准人數變化.png",y="人數") +
  theme(axis.title.y=element_text(vjust=2),plot.title = element_text(hjust = 0.5))
ggsave("全校雙轉輔申請人數與核准人數變化(轉入).png")

#全校轉系申請人數與競爭強度散布圖
all_data$轉系轉入核准 <- as.double(all_data$轉系轉入核准)
transfer_point <- all_data %>% 
  mutate(轉系申請人數 = 轉系轉入申請志願1+轉系轉入申請志願2) %>%
  filter(轉系轉入核准>0) %>%
  mutate(競爭強度 = 轉系申請人數/轉系轉入核准) %>%
  select(id,年度,代碼,學系,學院,轉系申請人數,競爭強度)
for (i in 104:104){
  transfer_point_year <- transfer_point %>% filter(年度==i)
  ggplot(data = transfer_point_year) +
    geom_point(mapping = aes(x = 轉系申請人數, y = 競爭強度, color = 學院),size=3) +
    labs(title = paste0(i,"年全校轉系轉入人數與競爭強度散布圖.png"))+
    theme(axis.title.y=element_text(vjust=2),plot.title = element_text(hjust = 0.5))
  ggsave(paste0(i,"年全校轉系轉入人數與競爭強度散布圖.png"))
}

ggplot(data =transfer_point) +
  geom_point(mapping = aes(x = 轉系申請人數, y = 競爭強度,color = 學院)) +
  facet_wrap(vars(年度))+
  labs(title = "全校轉系轉入人數與競爭強度散布圖")+
  theme(plot.title = element_text(hjust = 0.5))
ggsave("全校轉系轉入人數與競爭強度散布圖")

#全校雙主申請人數與競爭強度散布圖
all_data$雙主轉入核准 <- as.double(all_data$雙主轉入核准)
all_data$雙主轉入申請 <- as.double(all_data$雙主轉入申請)
bisubject_point <- all_data %>% 
  filter(雙主轉入核准>0) %>%
  filter(雙主轉入申請>=0) %>%
  mutate(競爭強度 = 雙主轉入申請/雙主轉入核准) %>%
  select(id,年度,代碼,學系,學院,雙主轉入申請,競爭強度)
ggplot(data =bisubject_point) +
  geom_point(mapping = aes(x = 雙主轉入申請, y = 競爭強度,color = 學院)) +
  facet_wrap(vars(年度))+
  labs(title = "全校雙主轉入人數與競爭強度散布圖")+
  theme(plot.title = element_text(hjust = 0.5))
ggsave("全校雙主轉入人數與競爭強度散布圖.png")

#全校輔系申請人數與競爭強度散布圖
all_data$輔系轉入核准 <- as.double(all_data$輔系轉入核准)
all_data$輔系轉入申請 <- as.double(all_data$輔系轉入申請)
auxsubject_point <- all_data %>% 
  filter(輔系轉入核准>0) %>%
  filter(輔系轉入申請>=0) %>%
  mutate(競爭強度 =輔系轉入申請/輔系轉入核准) %>%
  select(id,年度,代碼,學系,學院,輔系轉入申請,競爭強度)
ggplot(data =auxsubject_point) +
  geom_point(mapping = aes(x = 輔系轉入申請, y = 競爭強度,color = 學院)) +
  facet_wrap(vars(年度))+
  labs(title = "全校輔系轉入人數與競爭強度散布圖")+
  theme(plot.title = element_text(hjust = 0.5))
ggsave("全校輔系轉入人數與競爭強度散布圖.png")

#全校轉系申請人數箱形圖
all_data$年度 <- as.integer(all_data$年度)
transfer_box <- all_data %>% mutate(轉系申請人數 = 轉系轉入申請志願1+轉系轉入申請志願2) 
ggplot(transfer_box) +
  geom_boxplot(aes(x= 年度, y = 轉系申請人數, fill=年度,group = 年度))+
  labs(title = "全校轉系轉入人數箱型圖")+
  theme(plot.title = element_text(hjust = 0.5))
ggsave("全校轉系轉入人數箱型圖.png")

#全校雙主申請人數箱形圖
ggplot(all_data) +
  geom_boxplot(aes(x= 年度, y = 雙主轉入申請, fill=年度,group = 年度))+
  labs(title = "全校雙主轉入人數箱型圖")+
  theme(plot.title = element_text(hjust = 0.5))
ggsave("全校雙主轉入人數箱型圖.png")

#全校輔系申請人數箱形圖
ggplot(all_data) +
  geom_boxplot(aes(x= 年度, y = 輔系轉入申請, fill=年度,group = 年度))+
  labs(title = "全校輔系轉入人數箱型圖")+
  theme(plot.title = element_text(hjust = 0.5))
ggsave("全校輔系轉入人數箱型圖.png")

#觀察平均
all_data %>% group_by(年度) %>% mutate(轉系申請人數 = 轉系轉入申請志願1+轉系轉入申請志願2)%>% summarise(mean(轉系申請人數,na.rm=T))
all_data %>% group_by(年度) %>% summarise(mean(雙主轉入申請,na.rm=T))
all_data %>% group_by(年度) %>% summarise(mean(輔系轉入申請,na.rm=T))


# 99-108年申請雙轉府總數變化
three_change <- all_data %>%mutate(轉系申請人數 = 轉系轉入申請志願1+轉系轉入申請志願2) %>% group_by(年度) %>% summarise(申請轉系總數=sum(轉系申請人數,na.rm=T),申請雙主總數=sum(雙主轉入申請,na.rm=T),申請輔系總數=sum(輔系轉入申請,na.rm=T))
ggplot(data = three_change ,aes(x = 年度)) +
  geom_line( aes( y = 申請轉系總數,colour="轉系")) +
  geom_line( aes( y = 申請雙主總數,colour="雙主修")) +
  geom_line( aes( y = 申請輔系總數,colour="輔系")) +
  scale_colour_manual("",values=c("轉系"="red","雙主修"="blue","輔系" = "green"))+
  labs(title = "全校雙轉輔申請人數變化2.png",y="人數") +
  theme(axis.title.y=element_text(vjust=2),plot.title = element_text(hjust = 0.5))
ggsave("全校雙轉輔申請人數變化2.png")