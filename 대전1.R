##############################################
#
#               대전 공모전 
# 
##############################################


########################################################################################
# 1. 데이터 전처리 
########################################################################################
# 데이터 불러오기 
data=read.table('진짜최종데이터_마지막.csv',sep=",",head=T,encoding='utf-8')
str(data) # 변수정보
data_new=data # 본 데이터 복사 
data_new
str(data_new)


#############  EDA   ##############
# library
library(ggplot2)
#install.packages('ggExtra')
library(ggExtra)

# classic plot :
df1=subset(data_new, 구=='동구')
df2=subset(data_new, 구=='서구')
df3=subset(data_new, 구=='유성구')
df4=subset(data_new, 구=='대덕구')
df5=subset(data_new, 구=='중구')
ggplot(df1, aes(x=행정동, y=평균매출액,fill=행정동))+geom_bar(stat="identity")
ggplot(df2, aes(x=행정동, y=평균매출액,fill=행정동))+geom_bar(stat="identity")
ggplot(df3, aes(x=행정동, y=평균매출액,fill=행정동))+geom_bar(stat="identity")
ggplot(df4, aes(x=행정동, y=평균매출액,fill=행정동))+geom_bar(stat="identity")
ggplot(df5, aes(x=행정동, y=평균매출액,fill=행정동))+geom_bar(stat="identity")

ggplot(df1, aes(x=행정동, y=평점,fill=행정동))+geom_bar(stat="identity")
ggplot(df2, aes(x=행정동, y=평점,fill=행정동))+geom_bar(stat="identity")
ggplot(df3, aes(x=행정동, y=평점,fill=행정동))+geom_bar(stat="identity")
ggplot(df4, aes(x=행정동, y=평점,fill=행정동))+geom_bar(stat="identity")
ggplot(df5, aes(x=행정동, y=평점,fill=행정동))+geom_bar(stat="identity")+scale_y_continuous(limits = c(3, 5))



# 평점과 리뷰뷰제외한 모든 변수 정규화 
# MIN-MAX 정규화 
minmax=function(x){
  (x-min(x))/(max(x)-min(x))
}

data_new[,5:13]=apply(data_new[,5:13],MARGIN = 2, minmax) # margin 2=열별 계산


# 평점과 리뷰건수를 활용하여 평판이라는 변수 만듦
# 왜? 대부분 사람들이 평점을 3이상 줌
# 따라서 평점의 범위는 3~5사이인 경우가 대부분 
# 또한, 실제 평점이 유의한지 확인하기 위해서는 반드시 리뷰건수를 고려해야한다. 
# 예를 들어, A,B가게 모두 평점이 5로 동일하지만, A의 리뷰건수는 3, B의 리뷰건수는 200건 이라면
# B가게의 평점에 신뢰도가 상승한다. 
# 따라서 본 분석에서는 평점을 min-abs한후, 0값이 존재하지 않도록 +1더해 1.**으로 가중치를 만들었다. 
# 왜 min-abs냐? 
# MinMaxScaler특정값에 집중되어 있는 데이터가 그렇지 않은 데이터 분포보다 1표준편차에 의한 스케일 변화값이 커지게 된다. 한쪽으로 쏠림 현상이 있는 데이터 분포는 형태가 거의 유지된채 범위값이 조절되는 결과를 보인다.
# MaxAbsScaler의 경우, MinMaxScaler와 유사하나 음수와 양수값에 따른 대칭 분포를 유지하게 되는 특징이 있다
# 이 가중치를 리뷰건수에 곱하여, 평판이라는 새로운 변수를 생성하여 새로운 지표로 사용하고자 하였다. 

# 평판=리뷰건수*(1+maxabs(평점))
maxabs=function(x){
  x/max(x)
}
data_new[,3]=maxabs(data_new[,3])# 평점 정규화 
data_new$평판=data_new[,4]*(data_new[,3]+1) # 평판 변수 생성 
str(data_new) # 변수 정보 확인 
summary(data_new$평판) # 평판 변수의 기초통계량 확인  

# 앞서, 평판 변수의 값의 분포가 6~520으로 범위가 매우 컸으므로 
# 평판 변수를 4분위수에 따라 등급화하여 scale함
quantile(data_new$평판) # 4분위수 확인

for (i in 1:79){
  if (data_new[i,14]< 71.476) data_new[i,14]=0
  if ((data_new[i,14]>= 71.476)&(data_new[i,14]<95.882)) data_new[i,14]=1
  if ((data_new[i,14]>=95.882)&(data_new[i,14]<146.454)) data_new[i,14]=2
  if (data_new[i,14]>=146.454) data_new[i,14]=3
}
table(data_new$평판)
str(data_new)
cor(data_new$평판,data_new$평균매출액)  # 평판과 매출액의 상관관계 




########################################################################################
# 2. 1차 다중회귀 실시 : full model 
########################################################################################

# 다중회귀 : 모든 변수를 독립변수로 다중회귀 실시
# 결과: 유의수준을 0.1으로 설정했을때
# HO: B=0(회귀계수는 유의하지 않다)
# 귀무가설이 위와 같을 때 지하철역.수, 남4050대, 여2030대, 평판 총 4개의 독립변수가 유의했다. 
# 이때 수정된 R^2는 0.78로 높은편에 속했으며,
# 추정된 회귀식에 대한 p-value=2.2e-16로 귀무가설(Ho:b1=...=bn=0)을 기각하여, 추정된 회귀식은 유의했다. 
model=lm(평균매출액~., data=data_new[,5:14])
summary(model)
########################################################################################
# 3. 1차 다중공선성  
########################################################################################
# 유의한 독립변수를 채택하기 전에 다중공선성 확인을 실시하겠다. 
# https://blog.naver.com/PostView.nhn?blogId=sharp_kiss&logNo=221816968163&redirect=Dlog&widgetTypeCall=true&directAccess=false
# 결과 : full model에 넣은 모든 독립변수들의 vif 산출결과 10이상이면 다중공선성이 존재한다고 간주한다. 
# 이 때 유동인구들의 vif은 10 이상으로 매우 컸다. 
# 그러나, 유동인구 관련 변수를 제외한 나머지 독립변수들의 vif은 5이하로 다중공선성 문제가 발생하지 않았다. 
# install.packages('car')
library(car)
vif(model)

########################################################################################
# 4. 2차 다중회귀 실시 : selected model 
########################################################################################
# 앞선, 모형에서 다중공선성 문제가 발생함을 인지했다. 
# 따라서, 앞선 모형의 다중회귀분석 결과 회귀분석시, 변수선택방법인 
# '후진제거법을 통해 AIC가 가장 낮은 모형인 평판,지하철역.수, 2030대 선택
# 그 결과, 유의수준 a=0.1에서 유의한 회귀계수를 가진 독립변수는
# 지하철역.수, 여2030대, 평판이었다. 
# 이때, 수정된 R^2=0.77로 앞서 full model보다 0.01하락하지만, 추정된 회귀식의 p-value=2.2e-16로 유의했다. 

step(model,direction = "backward")

model2=lm(평균매출액~지하철역.수+X2030대+평판, data=data_new[,5:14])
summary(model2)
########################################################################################
# 5. 2차 다중공선성 : selected model 
########################################################################################
# 2차 다중회귀 모형의 다중공선성 확인 실시 
# selected model의 모든 독립변수들의 vif가 10이하로 다중공선성이 존재하지 않는다. 
vif(model2)


########################################################################################
# 8. 다중회귀 결과  
########################################################################################
# 다중 회귀 결과 selected model이 최종모형으로 선택되었다. 
# 이로써 y(종속변수)에 영향을 미치는 변수는 지하철역.수, 2030대, 평판 임을 알았다. 
# 따라서, 유의한 3개 변수를 활용하여 랜덤포레스트를 실시하여,
# 각 변수의 중요도를 살펴보고, 중요도를 가중치 취급한 후 
# y를 설명하는 신용평가점수를 생성하겠다. 

########################################################################################
# 9. 랜덤포레스트 
########################################################################################
# 앞선, 설명에 따라 랜덤포레스트를 실시한 결과
# 지하철역.수의 변수중요도는 0.351
# 2030대의 변수중요도는 1.308(매우 높은편=>영향력이 크다.)
# 평판의 변수중요도는  0.275
# 로 여 2030대의 변수중요도가 매우 큼을 알수 있다.
# install.packages('randomForest')
library(randomForest)
df=data_new[,c('지하철역.수','X2030대','평판','평균매출액')]
str(df)
set.seed(123)
forest_m = randomForest(평균매출액~지하철역.수+X2030대+평판, data=df)
forest_m$importance


########################################################################################
# 10. 신용평가점수  
########################################################################################
# 앞선, 랜덤포레스트의 변수 중요도를 가중치(weiht)취급하여 변수 생성 
# 신용평가점수=(0.351*지하철역.수)+(1.308*여2030대)+( 0.275*평판)
# 신용평가점수와 평균매출액 간의 상관계수는 0.72로 양의 강한 상관관계를 띈다. 
# 즉, 신용평가점수가 증가할때 평균매출액도 증가할 가능성이 매우 높다. 
# 이는 평균매출액을 추정할때 신용평가점수를 하나의 지표로 할 수 있을정도로 신뢰도가 높다고 볼 수 있다. 
df$신용평가점수=((0.352*df$지하철역.수)+(1.312*df$X2030대)+( 0.276*df$평판))
head(df)
cor(df$신용평가점수,df$평균매출액) 
str(df)

########################################################################################
# 11. 신용평가점수와 평균매출간의 관계 증명 - 비계층적 군집방법 
########################################################################################
# 앞서, 평균매출액과 신용평가점수 간에 강한 상관관계 있음을 알았다.
# 이 파트에서는, 비계층적 군집분석 방법 중 하나인 k군집방법을 실시하여
# 신용평가점수와, 지하철역.수, 여2030대, 평판등의 독립변수와 평균매출액간에 어떤 관계가 존재하는지 
# 군집분석을 실시하여, 군집대로 데이터를 나누고, 각 군집의 성격을 쉽게 파악할 수 있도록
# 그래프를 그려 확인해보겠다. 

######  클러스터링 ######
set.seed(123) # 랜덤 고정 

# 적절한 K개수 찾기 - 그래프 
# 팔꿈치 방법(The Elbow Method)으로 적절한 k개수를 찾은 결과 
# k=5에서 그래프가 팔꿈치 모양으로 꺽이는 지점임을 알 수 있다. 
# 따라서 군집개수를 5개로 설정하겠다. 
visual <- NULL
for(i in 2:10) 
{
  set.seed(123) 
  eval(parse(text=paste("result",i,"<- kmeans(df[,4],",i,");",sep=""))) 
  eval(parse(text=paste("visual[",i,"] <- result",i,"$tot.withinss",sep=""))) 
}
plot(visual[-1], type="l", ylab="", xlab="", main="cluster의 개수에 그룹내 제곱합") 
abline(v=3,col="red") # 선표시




# 군집개수 3개 설정 
set.seed(123)
str(df)
km3=kmeans(df[,4],3)  # 평균매출액 변수에 대해 군집분석

# 데이터 복사
kclust_data=df  
kclust_data$clust=km3$cluster # 군집 변수 추가 
table(kclust_data$clust) 
# 아래 코드인 r_mean의 결과를 토대로 평균매출액의 순위에 따라 군집 재배치 
kclust_data$clust1[kclust_data$clust==3]=1
kclust_data$clust1[kclust_data$clust==2]=3
kclust_data$clust1[kclust_data$clust==1]=2
table(kclust_data$clust1) 
# clust1변수가 최종 군집 변수 
kclust_data1=kclust_data[,c('지하철역.수','X2030대','평판','신용평가점수','평균매출액','clust1')]
str(kclust_data1)

## 군집특성 - 시각화 
# install.packages('fmsb')
library(fmsb) # 시각화를 위한 라이브러리 
str(kclust_data)
r_mean=aggregate(.~clust1,data=kclust_data1[,1:6],mean) # 그래프를 위한 작업 1 : 각 군집에 따른 변수들의 평균 (군집특성정보)
r_mean

r_max=as.data.frame(apply(kclust_data1[,1:5],2,max)) # 그래프를 위한 작업 2 :각 군집에 따른 변수들의 최대값 
r_min=as.data.frame(apply(kclust_data1[,1:5],2,min))# 그래프를 위한 작업 3 :각 군집에 따른 변수들의 최소값 
data=rbind(t(r_max),t(r_min),r_mean[,-1])# 그래프를 위한 작업 1~3에 해당되는 정보를 담은 데이터 

# 그래프 그리기 
par(mfrow=c(1,1))
#색깔설정
colors_border=c( rgb(0.8,0.2,0.5,0.9) ,rgb(0.2,0.5,0.5,0.9), rgb(0.9,0.6,0.1,0.9) )
colors_in=c( rgb(0.8,0.2,0.5,0.7) ,rgb(0.2,0.5,0.5,0.4), rgb(0.9,0.6,0.1,0.2) )
radarchart(df=data, # 첫번째행:최대값 두번째행:최소값, 그 다음으로 비교하고자 하는 행 넣기 
           axistype=1,
           #custom polygon
           pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
           #custom labels
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
           vlcex=0.8,
           title=c("신용평가점수와 평균매출간의 관계"))


legend("topleft", legend = c('신용불가','신용보통','신용안전'), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)



########################################################################################
# 12. 신용평가점수와 평균매출간의 관계 증명 - 비계층적 군집방법 결과 
########################################################################################
# 앞서, 설정한 군집별 특성이 신용평가점수와 평균매출액에 귀결되는 것을 보여준다. 




#############################################
# 13. 군집결과
# 군집결과에 따라 1=신용위험, 2=신용보통,3=신용안전
# 등급으로 할당하였다.
# 이 군집결과의 신뢰도 확인을 위해 
# 다시 랜덤포레스트를 실시
#############################################
# 군집변수를 신용등급변수로 변환 
library(dplyr)
kclust_data1=rename(kclust_data1,"신용등급"='clust1')
str(kclust_data1)

신용등급변수의 값을 1=>위험, 2=>보통, 3=>안전으로 할당 
kclust_data1$신용등급[kclust_data1$신용등급==1]='위험' 
kclust_data1$신용등급[kclust_data1$신용등급==2]='보통'
kclust_data1$신용등급[kclust_data1$신용등급==3]='안전' 
kclust_data1$신용등급=factor(kclust_data1$신용등급, levels=c('위험','보통','안전'))

library(randomForest)
rf=randomForest(신용등급 ~ ., data=kclust_data1)

# 데이터에 pred=랜덤포레스트결과 예측된 변수 추가 
kclust_data1$pred=rf$predicted
str(kclust_data1)

# 데이터의 오분류표 
tt=rf$confusion

# 정분류율 및 오분류율 계산 
sum(tt[row(tt) == col(tt)])/sum(tt)#정분류율
1-sum(tt[row(tt) == col(tt)])/sum(tt) #오분류율

# 시각화 
library(ggplot2)
dev.off()
ggplot(kclust_data1, aes(신용등급, pred, color = 신용등급))+
 geom_jitter(width = 0.2, height = 0.1, size=2) +
 labs(title="대전 79개 행정동 신용등급 예측결과",y="예측", x="실제")+
 theme_light() +
 theme(plot.title=element_text(size=20,hjust= 0.5))


########  잠깐!!!!!!!!! 데이터 저장 ########
# write.csv(kclust_data1,'신용등급.csv')











##############################################################################################################################      
#####################                                    구 지도 시각화                                    ###################
##############################################################################################################################



#install.packages("ggmap")
#install.packages("raster")
#install.packages("rgeos")
#install.packages("maptools")
#install.packages("rgdal")
library(ggmap)
library(ggplot2)
library(raster)
library(rgeos)
library(maptools)
library(rgdal)
library(ggplot2)

#####################  구 지도 시각화 ###################
data=read.table('진짜최종데이터_마지막.csv',sep=",",head=T,encoding='utf-8')
str(data) # 변수정보

# kclust_data1에 구와 행정동 변수 추가 
kclust_data1$구=data$구
kclust_data1$행정동=data$행정동
str(kclust_data1)

# 구별 평균 점수
ds=kclust_data1[,c(1,2,3,4,5,8)]
d_mean=aggregate(.~구,data=ds,mean) 
d_mean

# 대전 구의 id와 kclust_data1과 결합
gg=read.csv('대전구id.csv')
head(gg)
fdata=merge(d_mean,gg, by='구')
fdata$id=(fdata$id*10)
str(fdata)


# 일단 구 
map = readOGR("TL_SCCO_SIG.shp")
new_map = fortify(map, region = 'SIG_CD')
df_map_info = map@data  # 행정구역의 ID 찾기 
head(df_map_info)
# 119  30110                     Dong-gu              동구
# 120  30140                     Jung-gu              중구
# 121  30170                      Seo-gu              서구
# 122  30200                  Yuseong-gu            유성구
# 123  30230                  Daedeok-gu            대덕구
str(new_map)
new_map$id = as.numeric(new_map$id)
DD=subset(new_map, id>=30110&id<=30230)  # 대구 시에 해당하는 id만 추출
max(DD$id)

#df_map_info 
P_merge = merge(DD,fdata,  by='id')
str(P_merge)

############################################ 구별 평균매출액 ##############################
# *주의* 순서가 중요할 수 있음! 
gu_name=aggregate(.~구,data=P_merge,mean)
gu_name

ggplot() + geom_polygon(data = P_merge, aes(x = long,y = lat, group = group,fill =평균매출액), color = "#005666") + 
  scale_fill_gradient(low =  "#e5fafa",high = '#0b5b5a' , 
                      space = "Lab", guide = "colourbar") + 
  labs(fill = "평균매출액",title = "대전시 구별 평균 매출액") + theme_void() +
  theme(legend.position = c(.15, .85),plot.title=element_text(size=20,hjust= 0.5,face = 'bold')) +
  geom_text(data = gu_name, aes(x = long,y = lat,label = paste(구, sep = "\n")),size=7,fontface = 'bold',color='#292929')

############################################ 구별 신용평가점수  ##############################
ggplot() + geom_polygon(data = P_merge, aes(x = long,y = lat, group = group,fill =신용평가점수), color = "#eb5e42") + 
  scale_fill_gradient(low =  "#ffddd7",high = '#ff8f79' , 
                      space = "Lab", guide = "colourbar") + 
  labs(fill = "평균매출액",title = "대전시 구별 신용평가점수") + theme_void() +
  theme(legend.position = c(.15, .85),plot.title=element_text(size=20,hjust= 0.5,face = 'bold')) +
  geom_text(data = gu_name, aes(x = long,y = lat,label = paste(구, sep = "\n")),size=7,fontface = 'bold',color='#292929')






