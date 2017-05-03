## T1
y<-c(1,rep(0,2),rep(1,3),0,1,1)
# Method 1, loop, 15min
findRuns1 <- function(x,tag,k) {
       n <- length(x)
       runs <- vector(length=n)
       count <- 0
       for (i in 1:(n-k+1)) {
              if (all(x[i:(i+k-1)]==tag)) {
                     count <- count + 1
                     runs[count] <- i
              }
       }
       if (count > 0) {
              runs <- runs[1:count]
       } else runs <- NULL
       return(runs)
}
x<- c(1,0,0,1,1,1,0,1,1)
findRuns1(x,tag=1,2)
findRuns <- function(x,tag,k) {  
       n <- length(x)  
       runs <- vector()  
       count <- 0  
       for (i in 1:(n-k+1)) {    
              if (all(x[i:(i+k-1)]==tag)) {      
                     count <- count + 1      
                     runs[count] <- i    }  
       }  
       if (count > 0) {    runs <- runs[1:count]  } else runs <- NULL  return(runs)}
# Method 2, vector, 25 min 
findNum2<-function(y,x,l) {
       a<-which(y==x)
       b<-which(diff(y,1,l-1)==0)
       return(a[a%in%b])
       }
findNum(y,1,2)

## T2 每一行记录，每一列属性，分类变量
raw <- read.delim("data/weather.txt",check.names = F, na.strings = ".")
library(reshape2)
data<-melt(raw,id=c("year", "month", "element"), 
           variable.name = "day", na.rm = TRUE)
df<-dcast(data,year+month+day~element, value.var = "value")
day<-as.Date(paste0(df$year,"-",df$month,"-",df$day))
result<-data.frame(tdiff = df$tmax-df$tmin, row.names = day)
# no x[,c(1,3,5)]  use names

## T3
library(hflights)
str(hflights)
table(hflights$Year)
data<-hflights[,c("UniqueCarrier","Month", "ArrDelay")]
sdf<- function(x) {
       return(cbind(data$Year,data$UniqueCarrier,data$Month,quantile(x,probs=seq(0,1,0.1),na.rm=T)))}
result<-tapply(data$ArrDelay,list(data$Year,data$UniqueCarrier, data$Month),
               sdf)
do.call(rbind,result)

library(dplyr)
library(hflights)
rs<-na.omit(hflights) %>% 
       select(UniqueCarrier,Year,Month,ArrDelay) %>%
       group_by(UniqueCarrier,Year,Month) %>%
       # mutate compute each row, summarize give one raw
       mutate(quan=findInterval(ArrDelay,quantile(ArrDelay,seq(0,1,0.1)),rightmost.closed = TRUE)) %>% 
       group_by(UniqueCarrier,Year,Month,quan) %>%
       summarise(avgQuan=mean(ArrDelay,na.rm=T))

lst[i]<-list(tapply(temp$ArrDelay,temp$Month,
                    function(x) {return(quantile(x,probs=seq(0,1,0.1)))})) 


out <- aggregate(hflights$ArrDelay,by=list(hflights$UniqueCarrier,hflights$Year,hflights$Month), FUN=quantile,probs=seq(0,1,0.1),na.rm=T)
##by tapply aggregate

dat <- data.table(hflights)
dat <- na.omit(dat[,.(Year,Month,UniqueCarrier,ArrDelay)])
dat_qua <- dat[,quan:=findInterval(ArrDelay,quantile(ArrDelay,seq(0,1,0.1)),rightmost.closed = TRUE),
               by=.(UniqueCarrier,Year,Month)]
dat_qua <- setorder(dat_qua,UniqueCarrier,Year,Month,quan)
dat_delay<- dat_qua[,.(meandelay=mean(ArrDelay)),by=.(UniqueCarrier,Year,Month,quan)]
head(dat_delay)

x <- 2:18
v <- c(5, 10, 15) # create two bins [5,10) and [10,15)
cbind(x, findInterval(x, v))


## ex 01, 1) find overlap, 2) traverse
ds<-data.frame(id=seq(10,80,by=10),
               anest=c("baker","baker",rep("dow",6)),
               start=c("08:00","09:00","09:00","08:00","10:00","12:30","13:30","18:00"),
               end=c("11:00","13:00","15:30","13:30","11:30","13:30","14:30","19:00"))

ds<-data.frame(id=seq(10,80,by=10),
               anest=c("baker","baker",rep("dow",6)),
               start=c(8,9,9,8,10,12.5,13.5,18),
               end=c(11,13,15.5,13.5,11.5,13.5,14.5,19))

#Method 01: matrix, apply sum->max overlap intervals, optimal
#Method 02: min scaler, 0,1 sum, no traverse, for speed: no square
rs<-Map(function(s,e)) # select items
c(rep(0,s),rep(1,e-s),rep(0,2400-e)),start, end)
Reduce('+',rs)
#Method 03: arrange start
#Method 00: Rcpp

#Method 1: Discrete/Scaled time periods, use median of the scale as notchs, then check the occurences of the notchs
library(dplyr)
dsc<-ds %>%
       filter(anest=="dow") %>%
       select(-anest)
notch<-data.frame()
for (i in 1:nrow(dsc)) {
       nch<-cbind(id=dsc$id[i],mark=seq(dsc[i,"start"]+0.25,dsc[i,"end"]-0.25,0.25))
       notch<-rbind(notch,nch)
}
t<-table(notch$mark)
count<-max(t)
n<-as.numeric(names(t)[which(t==count)])
periods<-data.frame()
for (i in 1:length(n)) {
       periods<-rbind(periods,notch[which(notch$mark==n[i]),"id"])
}
periods<-distinct(periods)
names(periods)<-paste0("id",1:count)
## count, periods

我用了两种方法，第一种方法类似老师的第二种，但使用的不是0,1矩阵，
而是将区间离散化再用table()加which.max()定位

第二种方法类似于讨论的1,3。
先写一个函数识别两个区间的重叠区间，
再依据如果ab的重叠区间与bc的重叠区间重叠、则abc重叠，
先两两遍历输出所有重叠区间的数据框，再以重叠区间输入两两遍历，
从1开始往后循环，到没有重叠区间break
这样如果重叠最大数较大，计算量则较大

在识别两个区间的是否重叠时，为了提取区间重叠的部分，我识别了两种不同的重叠方式
包含和交集
在讨论的方法3中可以尝试识别并记录重叠的类型，排除包含和交集混合的情况

刚刚又想到，识别[i,i+1]是否为0，为零就在第i列-1

# fisher-adf
# 

# Method 2：any continuous intervals
# function overlap: find the overlap of two intervals: df1 & df2
overlap<-function (df1,df2) {
       library(dplyr)
       df<-rbind(df1,df2)
       df<-arrange(df,start)
       if (df[1,"end"] > df[2,"start"]) { # whether they have overlap
              if (df[1,"end"]>=df[2,"end"]) {
                     interval<-df[2,c("start","end")] # overlap type 1
              } else {
                     interval<-data.frame(start=df[2,"start"],end=df[1,"end"])
                     # overlap typr 2
              }
       } else {interval<- NULL}
       return(interval)
}
library(dplyr)
dsc<-ds %>%
       filter(anest=="dow") %>%
       select(-anest) # select obs. with the asset name
# function findLaps: solve the problem in question
findLaps<-function (dsc) {
       library(dplyr)
       m<-nrow(dsc)
       dsod<-dsc
       count<-1
       for (k in 1:m) { # at least 1, at most m intervals are overlapping
              n<-nrow(dsod)
              result<-NULL
              for (i in 1:(n-1)) {
                     for (j in (i+1):n) {
                            int<-overlap(dsod[i,],dsod[j,])
                            if (is.null(int)) {result<-result} else {
                                   id<-dplyr::select(dsod[c(i,j),],-c(end,start))
                                   id<-unique(as.vector(as.matrix(id)))
                                   lap<-cbind(matrix(id,nrow = 1),int)
                                   result<-unique(rbind(result,lap))
                            }
                     }
              }
              count<-count+1
              if (is.null(result)) {
                     count<-count-1
                     break
              } else {dsod<-result}
       }
       return(list(count=count,periods=dsod))
}
findLaps(dsc)


df <- dsc %>%
       arrange (start)
n<-nrow(df)
mat<-matrix(rep(0,n^2),nrow = n)
for (i in 1:n) {
       for (j in i:n) {
              mat[j,i]<-df$end[i] > df$start[j]
       }
}
sm<-colSums(mat)
count<-max(sm)
st<-which(sm==count)
for (i in st) {
       set<-which(mat[,st[i]]==T)
       brk <- NULL
       for (j in (st[i]+1):(st[i]+max(set)-1)) {
              a<-which(mat[j:max(set),j]==0)
              if (length(a)!=0) {
                     brk<-rbind(brk,mat[(a+j-1),j])
              } else {brk <- brk}
       }
}

set1<-set[2]
mat[1:4,1:4]
mat[2:5,2:5]

options(warn =-1)

sm<-outer(a,a,Fun="paste",sep=",")
sm<-c(sm)
sm<-paste0(sm,collapse=",")
sm<-strplit(sm,",")

rep(1:3,3)
rep(1:3,each=3)

expand.grid(x=(1,2,5),y=c(1,2,5))

# 不要卡在两两重叠，不符合要求就跳出来


x<-c(1,2,3)
n<-length(x)
x[-1]-x[-n]

which(x>3)

library(zoo)
library(xts)





# rename in scope
lookup<-c(m="Male",f="Female",u=NA)
unname(lookup[x])

grades<-c()
info<-data.frame(grade=3:1,
                 desc=c("E","G","P"),
                 fail=c(F,F,T))
a<-c(1:5)
b<-c(3,5)
a%in%b
b%in%a
# 中缀函数
match()
join()

rep(1:nrow(df),df$n)
df[]

# <-, <<-
subset()
methods(subset)
getAnywhere(subset









