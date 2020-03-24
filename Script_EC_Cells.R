df1 <- read.csv("x1.csv", sep = ";", dec=",")
df10 <- read.csv("x10.csv", sep = ";", dec=",")
head(df10, n = 30)

#Filter data df1
df1_Fir <- subset(df1, (Type == 'Firmicutes') & (CT < 28.58))
df1_Fir['Abundance'] <- 1/(df1_Fir$CT)*100
head(df1_Fir)
df1_Bact <- subset(df1, (Type == 'Bacteroides') & (CT < 30.01))
df1_Bact['Abundance'] <- 1/(df1_Bact$CT)*100
head(df1_Bact)
#Filter data df10
df10_Fir <- subset(df10, (Type == 'Firmicutes') & (CT < 29.48))
df10_Fir['Abundance'] <- 1/(df10_Fir$CT)*100
head(df10_Fir)
df10_Bact <- subset(df10, (Type == 'Bacteroides') & (CT < 23.1))
df10_Bact['Abundance'] <- 1/(df10_Bact$CT)*100
head(df10_Bact)

#Получаем базовые статистики
library(psych)
library(dplyr)
library(plyr)
df1_Fir_stat <- ddply(df1_Fir, ~Group, function(x) quantile(x$Abundance, prob = c(.25, .5, .75)))
df1_Bact_stat <- ddply(df1_Bact, ~Group, function(x) quantile(x$Abundance, prob = c(.25, .5, .75)))

df10_Fir_stat <- ddply(df10_Fir, ~Group, function(x) quantile(x$Abundance, prob = c(.25, .5, .75)))
df10_Bact_stat <- ddply(df10_Bact, ~Group, function(x) quantile(x$Abundance, prob = c(.25, .5, .75)))

#ONE-WAY ANOVA
fit_df1_Fir <- aov(Abundance~ Group, data = df1_Fir)
summary(fit_df1_Fir)
fit_df1_Bact <- aov(Abundance~ Group, data = df1_Bact)
summary(fit_df1_Bact)

fit_df10_Fir <- aov(Abundance ~ Group, data = df10_Fir)
summary(fit_df10_Fir)
fit_df10_Bact <- aov(Abundance~ Group, data = df10_Bact)
summary(fit_df10_Bact)

#Kruskal-test
library(vcd)
kruskal.test(Abundance~ Group, data = df1_Fir)
kruskal.test(Abundance~ Group, data = df1_Bact)
kruskal.test(Abundance~ Group, data = df10_Fir)
kruskal.test(Abundance~ Group, data = df10_Bact)


#Множественные сравнения по Tukey
TukeyHSD(fit_df1_Fir)
TukeyHSD(fit_df1_Bact)
TukeyHSD(fit_df10_Fir)
TukeyHSD(fit_df10_Bact)

#Строим боксплот
library(ggplot2)
ggplot(data = df1_Fir, aes(x=Group, y = Abundance)) + geom_boxplot()
ggplot(data = df1_Bact, aes(x=Group, y = Abundance)) + geom_boxplot()
ggplot(data = df10_Fir, aes(x=Group, y = Abundance)) + geom_boxplot()
ggplot(data = df10_Bact, aes(x=Group, y = Abundance)) + geom_boxplot()
