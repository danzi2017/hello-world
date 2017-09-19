##test
library(plyr)
library(ggplot2)
library(xts)
library(stringr)
ETFPE<-read.csv("/Users/danzi/ETFPEnew.csv",header = TRUE,sep = ",")
Price<-read.csv("/Users/danzi/ETFPricenew.csv",header = TRUE,sep = ",")

ETFPE$日期<-paste0(substr(ETFPE$日期,1,4) ,"-",substr(ETFPE$日期,5,6),"-",substr(ETFPE$日期,7,8))
ETFPE$日期<-as.Date(ETFPE$日期)
ETFPE<-na.omit(ETFPE) 
colnames(ETFPE)<-c("日期","指数代码","指数名称","PE","PB","PE中位","PB中位")



Price$时间<-paste0(substr(Price$时间,1,4) ,"-",substr(Price$时间,5,6),"-",substr(Price$时间,7,8))
Price$时间<-as.Date(Price$时间)
Price<-Price[Price$X %in% c("中小300","创业板","300ETF","500ETF"),]
colnames(Price)<-c("日期","ETF代码","ETF名称","单位净值","复权单位净值")

Price$指数<-""
Price[Price$ETF名称=="500ETF",]$指数<-"中证500"
Price[Price$ETF名称=="300ETF",]$指数<-"沪深300"
Price[Price$ETF名称=="中小300",]$指数<-"中小300"
Price[Price$ETF名称=="创业板",]$指数<-"创业板指"


ddply(ETFPE,c("指数代码","指数名称"),function(df) result=c(summary(df$PE),length(df$日期)))
ddply(Price,c("ETF代码","ETF名称"),function(df) 
  result=c(summary(df$复权单位净值),length(df$日期)))



ggplot(ETFPE, aes(x = PE, fill = 指数名称)) +
  geom_density()+
  theme(text = element_text(family = 'Hei'))+ #支持中文并设置字体（Mac）
  ggtitle("主要宽基指数PE分布")+  #标题
  xlab("")+  #设置X轴说明
  ylab("") + #设置Y轴说明
  facet_grid(指数名称 ~ .) #按照Gender属性分面，此时必须设置上面的fill=Gender


ggplot(Price, aes(x = 复权单位净值 , fill = ETF名称)) +
  geom_density()+
  theme(text = element_text(family = 'Hei'))+ #支持中文并设置字体（Mac）
  ggtitle("主要宽基ETF净值分布")+  #标题
  xlab("")+  #设置X轴说明
  ylab("") + #设置Y轴说明
  facet_grid(ETF名称 ~ .) #按照Gender属性分面，此时必须设置上面的fill=Gender
                  
# 
# plot(ETFPE[ETFPE$指数名称=="中证500",c("PE")],type="l")
# plot(Price[Price$ETF名称=="500ETF",c("复权单位净值")],type="l")
#d<-diff(data.ts)
#plot(d,type="l")


#合并数据集
datamerge<-merge(Price,ETFPE,by.x=c("指数","日期"),by.y=c("指数名称","日期"))
datamerge<-na.omit(datamerge)  #对不上时间的不要

# 
# head(data)
# str(data)
# summary(data$日期)



#指数PE和ETF净值相关性分析
databasinf<-ddply(datamerge,c("指数","指数代码","ETF代码","ETF名称"),function(df) 
  result=c(
           cor(df$PE,df$复权单位净值),
           cor(df$PB,df$复权单位净值),
           quantile(df$PE,0.2),
           quantile(df$PE,0.4),
           quantile(df$PE,0.6),
           quantile(df$PE,0.8),
           quantile(df$PB,0.2),
           quantile(df$PB,0.4),
           quantile(df$PB,0.6),
           quantile(df$PB,0.8)))
colnames(databasinf)<-c("指数","指数代码","ETF代码","ETF名称",
                        "PE与净值相关性","PB与净值相关性",
                        "20%PE","40%PE","60%PE","80%PE",
                        "20%PB","40%PB","60%PB","80%PB")
databasinf

#相关性画一个散点图看看
ggplot(datamerge, aes(x =复权单位净值,y = PE)) +
  geom_point()+
  theme(text = element_text(family = 'Hei'))+ #支持中文并设置字体（Mac）
  ggtitle("主要宽基ETF净值和PE关系")+  #标题
  xlab("复权单位净值")+  #设置X轴说明
  ylab("PE") + #设置Y轴说明
  facet_grid(指数 ~ .) #按照Gender属性分面，此时必须设置上面的fill=Gender



#做一个回归看一下
##中证500
zz500<-datamerge[datamerge$指数=="中证500",c("日期","PE","PB","复权单位净值")]
modzz500<-lm(复权单位净值~PE-1,data=zz500) 
summary(modzz500)
#复权单位净值=0.0397643*PE

##中小300
zx500<-datamerge[datamerge$指数=="中小300",c("日期","PE","PB","复权单位净值")]
modzx500<-lm(复权单位净值~PE-1,data=zx500) 
summary(modzx500)
#复权单位净值=0.028879 *PE


##沪深300
hs300<-datamerge[datamerge$指数=="沪深300",c("日期","PE","PB","复权单位净值")]
modhs300<-lm(复权单位净值~PE-1,data=hs300) 
summary(modhs300)
#复权单位净值=0.1018714*PE

##创业板指(用PB)
cyb<-datamerge[datamerge$指数=="创业板指",c("日期","PE","PB","复权单位净值")]
modcyb<-lm(复权单位净值~PB-1,data=cyb) 
summary(modcyb)
#复权单位净值=0.31*PB

#结论：
##中证500: 复权单位净值=0.0397643*PE
##中小300:复权单位净值=0.028879 *PE
##沪深300:复权单位净值=0.1018714*PE
##创业板指(用PB):复权单位净值=0.31*PB

#######下面是时间序列分析#############################

ddply(datamerge,c("指数","指数代码","ETF代码","ETF名称"),
      function(df) result=c(min(df$日期)))

zz500ts<-ts(datamerge[datamerge$指数=="中证500",c("复权单位净值")],start=c(2011,9),frequency = 12)
zx500ts<-ts(datamerge[datamerge$指数=="中小300",c("复权单位净值")],start=c(2011,6),frequency = 12)
hs300ts<-ts(datamerge[datamerge$指数=="沪深300",c("复权单位净值")],start=c(2012,5),frequency = 12)
cybts<-ts(datamerge[datamerge$指数=="创业板指",c("复权单位净值")],start=c(2011,9),frequency = 12)


ret_simple<-diff(zx500ts)/lag(zx500ts,k=-1)*100
ret_cont<-diff(log(data.ts))*100

quantile(ret_simple,probs = 0.20)
tail(ret_simple)
tail(ret_cont)
hist(ret_simple)
quantile(ret_simple)

plot(ret_simple,type="l")
plot(data.ts,plot.type = "single",col=1:2)



