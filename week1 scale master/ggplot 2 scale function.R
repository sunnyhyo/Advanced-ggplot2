# ggplot2 - 통계적인 데이터 변환이 필요할 때 변환을 수행
# ggplot 2 scale fuction 정복하기
# ggplot2는 scale을 변경함으로써 훨씬 더 가독성이 높은 그래프를 그릴 수 있다
#scale 함수군 = 지오메트릭과 데이터를 연결하는데 관련된 함수, 지오메트릭에 관련된 데이터의 스케일을 동적으로조정
library(ggplot2)
#1.scale_alpha= 지오메트릭에 알파채널 투명도 적용 연속형 스테일로 투명도의 범위를 조절
p<-ggplot(data=mtcars,aes(x=mpg, y=cyl,alpha=cyl))
p+geom_point(size=10)

p+scale_alpha(range=c(0.4,0.8))
#cyl 변수는 수치형 벡터이므로 factor, 이산형데이터로 저장

p<-ggplot(data=mtcars,aes(x=mpg,y=cyl,alpha=factor(cyl)))
p<-p+geom_point(size=10)
p+scale_alpha_discrete(range=c(0.4,0.8))

#2.sclae_*_brewer()함수들:colorbrewer.org에 있는 색상 패턴으로 채움 색상값을 지정
#colour와 fill의 차이점은? colour는 지오메트릭이 테두리난 점인 것의 색상을, fill은 원이나 막대의 내부와 같은 면적의 지오메트릭을 채울 색상
library(scales)
show_col(brewer_pal(pal="RdYlBu")(9))
p<-ggplot(data=diamonds,aes(price,carat,colour=clarity))
p<-p+geom_point()
p+scale_colour_brewer(type="seq",palette=5)
p+scale_colour_brewer(palette="Dark2")

p<-ggplot(data=diamonds,aes(price,fill=clarity))
p<-p+geom_histogram(binwidth = 500)
p+scale_fill_brewer(palette="YlGn")

#scale_*_gradient()함수들:continuous 함수들과 동일하게 연속형 데이터를 레벨화 하여 그라데이션 효과를 줌
dsub<-subset(diamonds,x>5&x<6&y>5&y<6)
p<-ggplot(data=dsub, aes(x,y,colour=z))
p<-p+geom_point()
p+scale_colour_gradient(limits=c(3,3.6),low="red",high="green")
#낮은 등급의 색상을 적색으로 지정하고, 높은 등급의 색상을 녹색으로 지정 수치값의 크기기
#colour의 인수를 z 로 선정하였으므로 z의 범위를 limit로 선정, 범위 밖은 회색.limit의 중요성
range(dsub$z)
boxplot(dsub$z)
#z변수의 이상치 문제로 인해 모두 낮은 등급이 책정됨. 
#그렇다면 범위 밖의 색깔지정방법은?
p<-ggplot(data=dsub,aes(x,fill=..count..))
p<-p+geom_histogram(binwidth = 0.02)
p+scale_fill_continuous(low="lightblue",high="blue",limits=c(200,700),na.value="white")
#그라디언트 효과를 2개=다이버징효과
dsub$diff<-with(dsub,sqrt(abs(x-y))*sign(x-y))
p<-ggplot(data=dsub,aes(x,y,colour=diff))
p<-p+geom_point()
p+ scale_color_gradient2(low="red",high="blue",mid="white",midpoint=0.15)
#diff의 값이 양을 가지면,x값큰경우, 파란색 기울기가 1인 사선이 mid

dsub<-data.frame(x=letters[1:5],y=c(-3,3,5,2,-2))
p<-ggplot(data=dsub,aes(x,y,fill=y))
p<-p+geom_bar(stat="identity")
p+scale_fill_gradient2()

#scale_*_identity()함수:스케일링 없이 지오메트릭에 사용할 값을 직접입력
colours<-c("red","green","blue","yellow","orange")
sizes<-c(1,2,3,4,5)+3
df<-data.frame(x=1:5,y=1:5)
p<-ggplot(data=df,aes(x,y,colour=colours,size=sizes))
p<-p+geom_point()
p<-p+p+scale_colour_identity()
p<-p+scale_size_identity()
p

#scale_linetype*() 이산형 변수의 선그래프 구분
library(reshape2)
library(plyr)
ecm<-melt(economics,id="date")
rescale01<-function(x)(x-min(x))/diff(range(x))
ecm<-ddply(ecm,"variable",transform,value=rescale01(value))
p<-ggplot(data=ecm,aes(date,value,group=variable,linetype=variable,colour=variable))
p<-p+geom_line()
p+scale_linetype_discrete()

#scale_*_date()함수 시계열 그래프에서 축의 범위 지정가능
p<-ggplot(data=economics,aes(x=date,y=psavert))
p+geom_path()
#개인의 가계 저축률을 그래프로
#2000년 이후의 데이터만?
p<-ggplot(data=economics,aes(x=date,y=psavert))
p+geom_path()
p+scale_x_date("21 century",limits=c(as.Date("2000-01-01"),max=(economics$date)))

#scale_*_discrete()함수:매핑된 축의 라벨을 바꾸거나 벙위를 조절하기 위해서 사용
p<-ggplot(data=subset(diamonds,carat>1),aes(x=cut,y=clarity,colour=carat))
p<-p+geom_jitter()
p+scale_x_discrete("Cutting",labels=paste("Grade",1:5))

p<-ggplot(data=subset(diamonds,carat>1),aes(x=cut,y=clarity,colour=carat))
p<-p+geom_jitter()
p+scale_x_discrete("Cutting",labels=paste("Grade",1:5),limits=c("Fair","Good","Very Good"))
#SCALE*_continuous 함수:x축과 y축의 매핑된 데이터가 표현되는 범위를 지정
#scale*_reverse함수
p<-ggplot(data=diamonds,aes(x=carat,y=price))
p<-p+geom_point()
p<-p+scale_x_reverse()
p+scale_y_reverse()
