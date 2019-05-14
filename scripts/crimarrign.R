library(data.table)
options(scipen=999)

setwd("~/Data_Analysis/NYPD_Summons_Arrests")

crim=read.csv('crim_arraignments_2018_top10.csv')
crim$Percent=crim$Total_Arraignments/206000*100

#create graphics
  
  par( xpd=NA, mar=c(4,10,2,2))
  
  crim1=sort(crim$LongDesc, decreasing = FALSE)
  
  #top ten last 7 days
  cols1=c(rep('#2F56A6',6), '#F59F00', rep('#2F56A6',3))
  b1=barplot(sort(crim$Total_Arraignments, decreasing = FALSE),las=2, col=cols1 ,horiz=TRUE, 
             xlim = c(0,35000), border = NA, xaxt='n', names.arg = crim[order(crim$Total_Arraignments, decreasing = FALSE),3])
  #title("4th: Driving with Suspended License", cex=1)
  ##topten not found in the other highlighted in blue
  axis(1,at=seq(0,35000,5000),col = 'lightgrey', col.ticks = 0.6, las=2, hadj = 0.75)
  
  text(b1, crim[order(crim$Total_Arraignments, decreasing = FALSE),2], 
       labels=crim[order(crim$Total_Arraignments, decreasing = FALSE),3], col="black")
 

  
  