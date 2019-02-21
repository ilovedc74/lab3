# IMPORT AND COLNAMES-------------------

setwd("~/Documents/Panc/Research/Research skills/lab3")
dat = read.csv("lab3_BM.csv")

# install.packages("data.table")
library(data.table)
dat = as.data.table(dat)

dat = dat[0:30, 0:9]

View(dat)
summary(dat)
colnames(dat)
colnames(dat) = c('ID','Sex', 'Age', 'GMSI', 'Gp', 
                  'T1Score', 'T1Diff', 'T2Score', 'T2Diff')
hist(dat$T1Score)

# TODO: How to do this more clean in codes?
# show means
d = dat[Gp == 1, T1G1mean := mean(T1Score),]
d = dat[Gp == 2, T1G2mean := mean(T2Score),]
d = dat[Gp == 1, T2G1mean := mean(T2Score),]
d = dat[Gp == 2, T2G2mean := mean(T2Score),]
View(d)

dat = dat[, FScore := ifelse(Gp==1,T1Score, T2Score),]
dat = dat[, SScore := ifelse(Gp==1,T2Score, T1Score),]
dat = dat[, mdiff := FScore-SScore,]

library(reshape2)
dat.col = melt(dat, id.vars = c("ID", "Sex","Age","GMSI" ,    "Gp"      
                                ,"T1Score",  "T1Diff" ,  "T2Score"  ,"T2Diff" ,  "T1G1mean"
                                ,"T1G2mean" ,"T2G1mean" ,"T2G2mean", "FScore",   "SScore"  
                                ,"mdiff"), measure.vars = c("FScore","SScore"))

colnames(dat.col)
# NORMALITY CHECK-------------------

# Anscombe-Glynn kurtosis test
anscombe.test(dat$T1Score)
# D'Agostino skewness test
agostino.test(dat$GMSI)



# TODO: For loop in R Dataframe



# TODO: How to do many Boxplot at once?
boxplot(c('T1Score','T2Score', 'T1Diff', 'T2Diff'), data=dat, main="test scores")


# Main analysis--------------

# TODO: Nonparametric Repeated Measure analysis theory check
# Example: https://rdrr.io/cran/nparLD/man/print.nparLD.html
# paper: Nonparametric Analysis of Ordered Categorical Data in Designs with Longitudinal Observations and Small Sample Sizes
install.packages("nparLD")
library(nparLD)

# TODO: Figure out Why does it work.....
ss = nparLD(dat.col$T1Score ~ Gp, data = dat, subject="ID")
print(ss)



# Data Wrangling, important for repeated Measure analysis

install.packages("Hmisc", dependencies = T)
library('Hmisc')
dat.cor = rcorr(as.matrix(dat))
# dat.goldDiff = melt(dat, id.vars = c("ID", "Sex","Age","GMSI","Gp"), measure.vars = c("T1Diff","T2Diff"))
rcorr(dat.col$GMSI,dat.col$T1Diff)
rcorr(dat.col$GMSI,dat.col$T2Diff)

# Main demographic table

dat[, lapply(.SD, mean, na.rm=TRUE) ,by = c("Sex","Gp")]
dat[, lapply(.SD, sd, na.rm=TRUE) ,by = c("Sex","Gp")]

dat[, lapply(.SD, mean, na.rm=TRUE),by = Gp]
dat[, lapply(.SD, sd, na.rm=TRUE),by = Gp]

#  Demographic and Main effect

install.packages("gglot2")
library(ggplot2)

ggplot(dat.col, aes(x = GMSI, y = value, color = variable)) + 
  geom_point(na.rm = TRUE) + 
  geom_smooth(method = "lm", se=FALSE) +
  scale_color_manual(values=c("#999999", "#E69F00"))+
  theme(
    panel.background = element_rect(fill = "white", colour = "grey50"),
    plot.title = element_text(size = rel(1.5)),
    axis.title.x = element_text(size = rel(1.5)),
    axis.title.y = element_text(size = rel(1.5), angle = 90))+
  labs(
    title = "Difference of Fast/Slow Scores by GMSI",
    x = "Musicality",
    y = "Fast/Slow music background Test Scores"
  )
                    