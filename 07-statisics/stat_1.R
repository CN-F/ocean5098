xaringanExtra::use_clipboard()

library (psych)
library (tidyverse)
library(gridExtra)
library(car)
library(ggpubr)
library(rstatix)


softies<-read.csv("C:\\ocean5098\\07-statisics\\sf_res.csv", sep=',', header=T)
str(softies)
softies$sample_no<-NULL
softies$species<-as.factor(softies$species)
softies$depth<-as.factor(softies$depth)
str(softies)

summary(softies)

psych::describe(softies[,-c(1:2)])

psych::describe (softies[,-c(1:2)]~species)
psych::describeBy (softies[,-c(1:2)],softies$species)

# One variables
prop.table (table(softies$species))

# two variables
table(softies$depth, softies$species)
prop.table (table(softies$depth, softies$species))

# three variables, with nicer formatting after adding a random variables
softies_2<-softies
softies_2$site<-rep(c("A", "B"),10)
ftable(softies_2$depth, softies_2$species, softies_2$site)

ratio.plot=softies[,c('species','ratio')]
s=ggplot(data=ratio.plot, aes(x=species, y=ratio, fill=species))
ratio_main_title = expression(paste("Ratio chlorophylls", italic(" c/a")))
ratio_y_title = expression(paste("ratio", italic(' c/a'))) 
s + geom_violin(trim = FALSE)+
  geom_jitter(shape=16, position=position_jitter(0.1))+
  labs(title= ratio_main_title, x="Species", y=ratio_y_title)+
  theme_bw()+
  theme(legend.position='none')+
  theme(axis.text.x= element_text(face="italic"))+
  theme(plot.title= element_text(hjust="0.5"))+
  scale_y_continuous(limits=c(0.2,0.6))

 
mean(softies$ratio)
lit <- softies$species == 'Litophyton sp.'
mean(softies$ratio[lit])

aggregate(softies$ratio,list (species=softies$species),FUN="median")
tapply(softies$ratio,softies$species, FUN="median")

# plot1 <-ggplot(iris, aes(x=Species, y=Sepal.Length)) +
#   geom_boxplot()
# 
# plot2 <-ggplot(iris, aes(x=Species, y=Sepal.Width)) +
#   geom_boxplot()
# 
# plot3 <-ggplot(iris, aes(x=Species, y=Petal.Length)) +
#   geom_boxplot()
# 
# plot4 <-ggplot(iris, aes(x=Species, y=Petal.Width)) +
#   geom_boxplot()
# 
# grid.arrange(plot1, plot2,plot3, plot4, ncol=2)
# describeBy (iris, iris$Species)
# 
# iris %>% group_by(Species) %>% summarise(across(c(1:4), length))
# aggregate(iris[,1:4],by=list(iris$Species), median)
# tapply(iris$Sepal.Length , iris$Species, mean)

students<-read.table('https://www.dipintothereef.com/uploads/3/7/3/5/37359245/students.txt',header=T, sep="\t", dec='.')
# HO: no significant relationship between height and shoesize
# H1: significant relationship between height and shoesize
x1<-students$height
y1<-students$shoesize
s1<-students[,1:2] # a matrix
# Pearson correlation
# cor(x1,y1)
# cor(s1)
cor.test(x1,y1)

# HO: no significant relationship between the amounts of chl a and c
# H1: significant relationship between the amounts of chl a and c
x2<-softies$chla.afdw
y2<-softies$chlc.afdw
s2<-softies[,c(12,14)] # a matrix
# Pearson correlation
# cor(x,y)
# cor(s)
cor.test(x2,y2)

ggplot(softies, aes(x = chla.afdw, y = chlc.afdw)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")

w<-(1:100)
z<-exp(w)
cor.test(w,z,method='spearman')
# cor.test(w,z,method='pearson') 

#Cast 240 times a die. We counted occurrence of 1,2,3,4,5,6
die<-data.frame(obs=c(55,44,35,45,31,30), row.names=c('Cast1','Cast2','Cast3','Cast4','Cast5','Cast6'))
chisq.test(die)

obs <- c(750, 50, 200)
exp <- c(0.60, 0.35, 0.05)
chisq.test (x=obs, p=exp)

F <- matrix(nrow=4,ncol=2,data=c(33,14, 8,18,31,25,14,12))
chisq.test(F) # alternative see `fisher.test`

# One sample
t.test (softies$ratio, mu=0.4)
# Two sample (with equal variances)
t.test (softies$ratio~softies$species, var.equal = TRUE)
# Two sample (with unequal variances) 
t.test (softies$chla.afdw~softies$species)
# Two sample paired t.test
# ?sleep
sleep2 <- reshape(sleep, direction = "wide", idvar = "ID", timevar = "group")
t.test (sleep2$extra.1,sleep2$extra.2,paired=T )
# same as: t.test(Pair(extra.1, extra.2) ~ 1, data = sleep2)

## set <- iris[iris$Species == "setosa", ]$Sepal.Length
## ver <- iris[iris$Species == "versicolor", ]$Sepal.Length
## boxplot(set, ver)
## setver <- t.test(set, ver, var.equal = TRUE, paired = FALSE)

## One-sample test.
## Hollander & Wolfe (1973), 29f.
## Hamilton depression scale factor measurements in 9 patients with
##  mixed anxiety and depression, taken at the first (x) and second
##  (y) visit after initiation of a therapy (administration of a
##  tranquilizer).
x <- c(1.83,  0.50,  1.62,  2.48, 1.68, 1.88, 1.55, 3.06, 1.30)
y <- c(0.878, 0.647, 0.598, 2.05, 1.06, 1.29, 1.06, 3.14, 1.29)
depression <- data.frame(first = x, second = y, change = y - x)
wilcox.test(change ~ 1, data = depression) # Formula interface to one-sample
wilcox.test(Pair(first, second) ~ 1, data = depression) # Formula interface to paired tests

## Two-sample test.
### Ozone
boxplot(Ozone ~ Month, data = airquality)
wilcox.test(Ozone ~ Month, data = airquality, subset = Month %in% c(5, 8))
### wilcox_test in package coin for exact, asymptotic and Monte Carlo conditional p-values, including in the presence of ties. 

## Softies
boxplot(prot.afdw~species, data=softies)
shapiro.test(softies$prot.afdw) # data are not normal
qqnorm(softies$prot.afdw) 
qqline(softies$prot.afdw)
wilcox.test (softies$prot.afdw~softies$species) 

kruskal.test(Ozone ~ Month, data = airquality)
# see also from rstatix
res.kruskal <- airquality %>% kruskal_test(Ozone ~ Month)
res.kruskal

# Pairwise comparisons
oz <- airquality %>% 
  dunn_test(Ozone ~ Month, p.adjust.method = "bonferroni") 
oz

oz <- oz %>% add_xy_position(x = "Month")
ggboxplot(airquality, x = "Month", y = "Ozone") +
  stat_pvalue_manual(oz, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.kruskal, detailed = TRUE),
    caption = get_pwc_label(oz)
    )

## # Test of variance: we test HO: homogeneous, H1:heterogeneous
## fligner.test (softies$ratio ~ softies$species)
## fligner.test (softies$chla.afdw ~ softies$species)

## tg<-ToothGrowth
## tg$dose<-factor(tg$dose)
## boxplot(len~dose*supp, data=tg)
## # also work with: boxplot(len ~ interaction (dose,supp), data=tg)
## # or: plot(len ~ interaction (dose,supp), data=tg)
## bartlett.test(len~interaction (supp,dose),data=ToothGrowth) # sensitivity non-normality +++
## leveneTest(len~interaction (supp,dose),data=ToothGrowth) # sensitivity non-normality ++
## fligner.test(len~interaction (supp,dose),data=ToothGrowth) # sensitivity non-normality +
