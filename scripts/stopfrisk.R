library(data.table)
options(scipen=999)
library(plotly)
library(ggplot2)


setwd("~/Data_Products/morejustnyc/data/stop_question_frisk")

y11=fread('2011.csv')
y12=fread('2012.csv')
y13=fread('2013.csv')
y14=fread('2014.csv')
y15=fread('sqf-2015.csv')
y16=fread('sqf-2016.csv')
y17=fread('sqf-2017.csv')
y18=fread('sqf-2018.csv')

#names(y11)
#names(y12)
#names(y13)
#names(y14)
#names(y15)
#names(y16)
#names(y17)
#names(y18)

y11r=y11[,82]
y12r=y12[,82]
y13r=y13[,82]
y14r=y14[,82]
y15r=y15[,82]
y16r=y16[,82]
y17r=y17[,66]
y18r=y18[,66]

unique(y11r)
y11r[which(y11r$race=="A"),1]<-'Asian'
y11r[which(y11r$race=="B"),1]<-'Zblack'
y11r[which(y11r$race=="Q"),1]<-'Xhispanic'
y11r[which(y11r$race=="W"),1]<-'White'
y11r[which(y11r$race=="P"),1]<-"Xhispanic"
y11r[which(y11r$race=="Z"),1]<-'Other'
y11r[which(y11r$race=="U"),1]<-'Other'
y11r[which(y11r$race=="I"),1]<-"Amerind"

y12r[which(y12r$race=="A"),1]<-'Asian'
y12r[which(y12r$race=="B"),1]<-'Zblack'
y12r[which(y12r$race=="Q"),1]<-'Xhispanic'
y12r[which(y12r$race=="W"),1]<-'White'
y12r[which(y12r$race=="P"),1]<-"Xhispanic"
y12r[which(y12r$race=="Z"),1]<-'Other'
y12r[which(y12r$race=="U"),1]<-'Other'
y12r[which(y12r$race=="I"),1]<-"Amerind"

y13r[which(y13r$race=="A"),1]<-'Asian'
y13r[which(y13r$race=="B"),1]<-'Zblack'
y13r[which(y13r$race=="Q"),1]<-'Xhispanic'
y13r[which(y13r$race=="W"),1]<-'White'
y13r[which(y13r$race=="P"),1]<-"Xhispanic"
y13r[which(y13r$race=="Z"),1]<-'Other'
y13r[which(y13r$race=="U"),1]<-'Other'
y13r[which(y13r$race=="I"),1]<-"Amerind"

y14r[which(y14r$race=="A"),1]<-'Asian'
y14r[which(y14r$race=="B"),1]<-'Zblack'
y14r[which(y14r$race=="Q"),1]<-'Xhispanic'
y14r[which(y14r$race=="W"),1]<-'White'
y14r[which(y14r$race=="P"),1]<-"Xhispanic"
y14r[which(y14r$race=="Z"),1]<-'Other'
y14r[which(y14r$race=="U"),1]<-'Other'
y14r[which(y14r$race=="I"),1]<-"Amerind"

y15r[which(y15r$race=="A"),1]<-'Asian'
y15r[which(y15r$race=="B"),1]<-'Zblack'
y15r[which(y15r$race=="Q"),1]<-'Xhispanic'
y15r[which(y15r$race=="W"),1]<-'White'
y15r[which(y15r$race=="P"),1]<-"Xhispanic"
y15r[which(y15r$race=="Z"),1]<-'Other'
y15r[which(y15r$race=="U"),1]<-'Other'
y15r[which(y15r$race=="I"),1]<-"Amerind"

y16r[which(y16r$race=="A"),1]<-'Asian'
y16r[which(y16r$race=="B"),1]<-'Zblack'
y16r[which(y16r$race=="Q"),1]<-'Xhispanic'
y16r[which(y16r$race=="W"),1]<-'White'
y16r[which(y16r$race=="P"),1]<-"Xhispanic"
y16r[which(y16r$race=="Z"),1]<-'Other'
y16r[which(y16r$race=="U"),1]<-'Other'
y16r[which(y16r$race=="I"),1]<-"Amerind"
y16r[which(y16r$race==""),1]<-"Other"

unique(y17r)
length(which(y17r$SUSPECT_RACE_DESCRIPTION=='MALE'))
y17r=y17r[-which(y17r$SUSPECT_RACE_DESCRIPTION=='MALE'),]

y17r[which(y17r$SUSPECT_RACE_DESCRIPTION=='BLACK HISPANIC'),1]<-'XHISPANIC'
y17r[which(y17r$SUSPECT_RACE_DESCRIPTION=='WHITE HISPANIC'),1]<-'XHISPANIC'
y17r[which(y17r$SUSPECT_RACE_DESCRIPTION=='ASIAN/PAC.ISL'),1]<-'ASIAN'
y17r[which(y17r$SUSPECT_RACE_DESCRIPTION=='(null)'),1]<-'OTHER'
y17r[which(y17r$SUSPECT_RACE_DESCRIPTION=='AMER IND'),1]<-'AMERIND'
y17r[which(y17r$SUSPECT_RACE_DESCRIPTION=='BLACK'),1]<-'ZBLACK'

unique(y18r)
y18r[which(y18r$SUSPECT_RACE_DESCRIPTION=='BLACK HISPANIC'),1]<-'XHISPANIC'
y18r[which(y18r$SUSPECT_RACE_DESCRIPTION=='WHITE HISPANIC'),1]<-'XHISPANIC'
y18r[which(y18r$SUSPECT_RACE_DESCRIPTION=='ASIAN / PACIFIC ISLANDER'),1]<-'ASIAN'
y18r[which(y18r$SUSPECT_RACE_DESCRIPTION=='(null)'),1]<-'OTHER'
y18r[which(y18r$SUSPECT_RACE_DESCRIPTION=='AMERICAN INDIAN/ALASKAN NATIVE'),1]<-'AMERIND'
y18r[which(y18r$SUSPECT_RACE_DESCRIPTION=='BLACK'),1]<-'ZBLACK'

#to make proper case
y17r$SUSPECT_RACE_DESCRIPTION=tools::toTitleCase(tolower(y17r$SUSPECT_RACE_DESCRIPTION))
y18r$SUSPECT_RACE_DESCRIPTION=tools::toTitleCase(tolower(y18r$SUSPECT_RACE_DESCRIPTION))

names(y17r)<-"race"
names(y18r)<-"race"

y11r$year=rep(2011,nrow(y11r))
y12r$year=rep(2012,nrow(y12r))
y13r$year=rep(2013,nrow(y13r))
y14r$year=rep(2014,nrow(y14r))
y15r$year=rep(2015,nrow(y15r))
y16r$year=rep(2016,nrow(y16r))
y17r$year=rep(2017,nrow(y17r))
y18r$year=rep(2018,nrow(y18r))


#getting dataset for visual
x=rbind(y11r,y12r,y13r,y14r,y15r,y16r,y17r,y18r)
x=setorderv(x,cols = "race")
names(x)
unique((x$race))

?order()

t=table(x$race,x$year)
prop.table(t, 2)*100
t2=as.matrix(prop.table(t, 2)*100)
#save sqf file

t2=cbind(t,row.names(t))
colnames(t)[9]<-'race'
t2
write.csv(t2,'sqf.csv', row.names = FALSE)

#visual
#stacked percent plot

#council categorical color palette
council_pal<- c("#A07952","#D05D4E","#12B886","#BE4BDB", "#F59F00", "#228AE6", "#82C91E")


# Make a stacked barplot--> it will be in %!
par(mar = c(6,5,4,9))
barplot(prop.table(t, 2)*100, col=council_pal , border="white", xlab="Year", ylab="Percent", xlim = c(0,9), 
        las=2, main = 'Stop and Frisk Racial Breakdown', cex.axis = 0.8, cex.names = 0.8, family = "Open Sans",
        legend.text = c("Amerind", "Asian", "Other", "White", "Hispanic", "Black"), 
        args.legend = list(x ='right', bty='n', inset=c(-0.45,0), xpd = TRUE))
mtext('Source: NYPD Stop, Question and Frisk Data', side=1, line=4.5, at=9, cex = 0.7)


#make line graph
par(mar = c(7,6,5,10))
plot(colSums(t), type = 'l', bty='n', ylab = "", xlab = "", xaxt="n", yaxt='n', col=NA,
     main = "Stop & Frisk Incidents",family = "Open Sans", cex.main=2 )
title(ylab="Incidents", xlab="Year", line=4)
axis(1,at=1:8,labels = c('2011','2012', '2013', '2014', '2015', '2016', '2017', '2018'),col = NA, col.ticks = 0.4, las=2, hadj = 0.75, title="Year")
axis(2, at=seq(0,800000,100000), las=2, col = NA, col.ticks = 0.4,line=0.000005, cex.axis=0.9, hadj = 0.75, 
     labels=formatC(seq(0,800000,100000), big.mark = ",", format = "d"))
#Add the grey lines
abline(v=seq(0,90,1) , h=seq(0,700000,50000) , col="light grey" , lwd=0.6)
abline(h=0, col="gray", lwd=2)
lines(colSums(t), col='#2F56A6', lwd=7)

#add legend
legend("right", legend = c("Total Incidents"), col = c('#2F56A6'), lty = 1, lwd = 4,
       bty = "n", cex = 1, horiz = F , inset = c(-0.55,0), xpd=TRUE)

#Total incidents in 2011 vs 2018
mtext(paste(formatC(colSums(t)[1], big.mark = ",", format = "d"),"(2011)"), side=1, line = -19.5, at=2.5)
mtext(paste(formatC(colSums(t)[8], big.mark = ",", format = "d"),"(2018)"),  side=1, line = -2, at=8.5)


#interactive, ggplot version
plot_ly(colSums(t) , type="scatter", mode="markers+lines")

df=as.data.frame(c(colSums(t)))
colnames(df)<-('Total Incidents')
df


#plotly
library(plotly)
library(MASS)

covmat <- matrix(c(0.8, 0.4, 0.3, 0.8), nrow = 2, byrow = T)
df <- mvrnorm(n = 10000, c(0,0), Sigma = covmat)
df <- as.data.frame(df)

colnames(df) <- c("x", "y")
p <- plot_ly(df, x = ~x, y = ~y, alpha = 0.3) %>%
  add_markers(marker = list(line = list(color = "black", width = 1))) %>%
  layout(
    title = "Drop down menus - Plot type",
    xaxis = list(domain = c(0.1, 1)),
    yaxis = list(title = "y"),
    updatemenus = list(
      list(
        y = 0.8,
        buttons = list(
          
          list(method = "restyle",
               args = list("type", "scatter"),
               label = "Scatter"),
          
          list(method = "restyle",
               args = list("type", "histogram2d"),
               label = "2D Histogram")))
    ))
plot_ly(labels = t2$race, values = t2$2018) %>%
  add_pie(hole = 0.6) %>%
  layout(title = "Donut charts using Plotly",  showlegend = F,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

t2=data.table(t2)
t(prop.table(t, 2)*100)





