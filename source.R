rm(list=ls())

data_2016 <- read.csv("2016_Data.csv")
data_2017 <- read.csv("2017_Data.csv")
data_2018 <- read.csv("2018_Data.csv")
data_2019 <- read.csv("2019_Data.csv")
data_2020 <- read.csv("2020_Data.csv")

# One-way Anova
# DTHD
T1 <- data_2016$DTHD
T2 <- data_2017$DTHD
T3 <- data_2018$DTHD
T4 <- data_2019$DTHD
T5 <- data_2020$DTHD

which(is.na(T1))
which(is.na(T2))
which(is.na(T3))
which(is.na(T4))
which(is.na(T5))
T2 = na.omit(T2)
T3 = na.omit(T3)
T5 = na.omit(T5)

x = c(T1, T2, T3, T4, T5)
Treatments=c(rep("2016",1200),rep("2017",1319),rep("2018",1318),rep("2019",1200), rep("2020",1318))
data = data.frame(x, Treatments)
av = aov(x~Treatments)
summary(av)


# DAYSMT
T1 <- data_2016$DAYSMT
T2 <- data_2017$DAYSMT
T3 <- data_2018$DAYSMT
T4 <- data_2019$DAYSMT
T5 <- data_2020$DAYSMT

which(is.na(T1))
which(is.na(T2))
which(is.na(T3))
which(is.na(T4))
which(is.na(T5))
T2 = na.omit(T2)
T3 = na.omit(T3)
T5 = na.omit(T5)

x = c(T1, T2, T3, T4, T5)
Treatments=c(rep("2016",1200),rep("2017",1319),rep("2018",1318),rep("2019",1200), rep("2020",1318))
data = data.frame(x, Treatments)
av = aov(x~Treatments)
summary(av)


# Plaint Height
T1 <- data_2016$PH
T2 <- data_2017$PH
T3 <- data_2018$PH
T4 <- data_2019$PH
T5 <- data_2020$PH

which(is.na(T1))
which(is.na(T2))
which(is.na(T3))
which(is.na(T4))
which(is.na(T5))
T2 = na.omit(T2)
T3 = na.omit(T3)
T5 = na.omit(T5)

x = c(T1, T2, T3, T4, T5)
Treatments=c(rep("2016",1200),rep("2017",1319),rep("2018",1318),rep("2019",1200), rep("2020",1318))
data = data.frame(x, Treatments)
av = aov(x~Treatments)
summary(av)


# SN
T1 <- data_2016$SN
T2 <- data_2017$SN
T3 <- data_2018$SN
T4 <- data_2019$SN
T5 <- data_2020$SN

which(is.na(T1))
which(is.na(T2))
which(is.na(T3))
which(is.na(T4))
which(is.na(T5))
T2 = na.omit(T2)
T3 = na.omit(T3)
T5 = na.omit(T5)

x = c(T1, T2, T3, T4, T5)
Treatments=c(rep("2016",1200),rep("2017",1319),rep("2018",1318),rep("2019",1200), rep("2020",1318))
data = data.frame(x, Treatments)
av = aov(x~Treatments)
summary(av)


# SPKLNG
T1 <- data_2016$SPKLNG
T2 <- data_2017$SPKLNG
T3 <- data_2018$SPKLNG
T4 <- data_2019$SPKLNG

which(is.na(T1))
which(is.na(T2))
which(is.na(T3))
which(is.na(T4))
T2 = na.omit(T2)
T3 = na.omit(T3)

x = c(T1, T2, T3, T4)
Treatments=c(rep("2016",1200),rep("2017",1319),rep("2018",1318),rep("2019",1200))
data = data.frame(x, Treatments)
av = aov(x~Treatments)
summary(av)


# SPLN
T1 <- data_2016$SPLN
T2 <- data_2017$SPLN
T3 <- data_2018$SPLN
T4 <- data_2019$SPLN
T5 <- data_2020$SPLN

which(is.na(T1))
which(is.na(T2))
which(is.na(T3))
which(is.na(T4))
which(is.na(T5))
T1 = na.omit(T1)
T2 = na.omit(T2)
T3 = na.omit(T3)
T4 = na.omit(T4)
T5 = na.omit(T5)

x = c(T1, T2, T3, T4, T5)
Treatments=c(rep("2016",1200),rep("2017",1319),rep("2018",1318),rep("2019",1200), rep("2020",1318))
data = data.frame(x, Treatments)
av = aov(x~Treatments)
summary(av)


# GRNSPK 
T1 <- data_2016$GRNSPK
T2 <- data_2017$GRNSPK
T3 <- data_2018$GRNSPK
T4 <- data_2019$GRNSPK
T5 <- data_2020$GRNSPK

which(is.na(T1))
which(is.na(T2))
which(is.na(T3))
which(is.na(T4))
which(is.na(T5))
T1 = na.omit(T1)
T2 = na.omit(T2)
T3 = na.omit(T3)
T4 = na.omit(T4)
T5 = na.omit(T5)

x = c(T1, T2, T3, T4, T5)
Treatments=c(rep("2016",1200),rep("2017",1319),rep("2018",1318),rep("2019",1200), rep("2020",1318))
data = data.frame(x, Treatments)
av = aov(x~Treatments)
summary(av)


# TGW
T1 <- data_2016$TGW
T2 <- data_2017$TGW
T3 <- data_2018$TGW
T4 <- data_2019$TGW
T5 <- data_2020$TGW

which(is.na(T1))
which(is.na(T2))
which(is.na(T3))
which(is.na(T4))
which(is.na(T5))
T1 = na.omit(T1)
T2 = na.omit(T2)
T3 = na.omit(T3)
T4 = na.omit(T4)
T5 = na.omit(T5)

x = c(T1, T2, T3, T4, T5)
Treatments=c(rep("2016",1200),rep("2017",1319),rep("2018",1318),rep("2019",1200), rep("2020",1318))
data = data.frame(x, Treatments)
av = aov(x~Treatments)
summary(av)



# Tukey HSD
# GRYLD
Grain_yield_2016 <- data_2016$GRYLD
Grain_yield_2017 <- data_2017$GRYLD
Grain_yield_2018 <- data_2018$GRYLD
Grain_yield_2019 <- data_2019$GRYLD
Grain_yield_2020 <- data_2020$GRYLD

which(is.na(Grain_yield_2016))
which(is.na(Grain_yield_2017))
which(is.na(Grain_yield_2018))
which(is.na(Grain_yield_2019))
which(is.na(Grain_yield_2020))
Grain_yield_2017 = na.omit(Grain_yield_2017)
Grain_yield_2018 = na.omit(Grain_yield_2018)
Grain_yield_2020 = na.omit(Grain_yield_2020)
x = c(Grain_yield_2016, Grain_yield_2017, Grain_yield_2018, Grain_yield_2019, Grain_yield_2020)
Treatments=c(rep("2016",1200),rep("2017",1319),rep("2018",1319),rep("2019",1200), rep("2020",1318))
data = data.frame(x, Treatments)
av = aov(x~Treatments)
TukeyHSD(av,conf.level = 0.90)


hist(Grain_yield_2016, main = 'Seasons from 15-16', xlab="Sản lượng ngũ cốc (tính bằng tấn) trên mỗi ha", breaks=30, prob = TRUE)
lines(x = density(Grain_yield_2016), col = "red", lwd = 2)
hist(Grain_yield_2017, main = 'Seasons from 16-17', xlab="Sản lượng ngũ cốc (tính bằng tấn) trên mỗi ha", breaks=30, prob = TRUE)
lines(x = density(Grain_yield_2017), col = "red", lwd = 2)
hist(Grain_yield_2018, main = 'Seasons from 17-18', xlab="Sản lượng ngũ cốc (tính bằng tấn) trên mỗi ha", breaks=30, prob = TRUE)
lines(x = density(Grain_yield_2018), col = "red", lwd = 2)
hist(Grain_yield_2019, main = 'Seasons from 18-19', xlab="Sản lượng ngũ cốc (tính bằng tấn) trên mỗi ha", breaks=30, prob = TRUE)
lines(x = density(Grain_yield_2019), col = "red", lwd = 2)
hist(Grain_yield_2020, main = 'Seasons from 19-20', xlab="Sản lượng ngũ cốc (tính bằng tấn) trên mỗi ha", breaks=30, prob = TRUE)
lines(x = density(Grain_yield_2020), col = "red", lwd = 2)



# Hệ số tương quan
data_2016$CT_Avg <- rowMeans(data_2016[7:14], na.rm=TRUE)
data_2016$NDVI_Avg <- rowMeans(data_2016[15:23], na.rm=TRUE)
data_2017$CT_Avg <- rowMeans(data_2017[7:20], na.rm=TRUE)
data_2017$NDVI_Avg <- rowMeans(data_2017[21:34], na.rm=TRUE)
data_2018$CT_Avg <- rowMeans(data_2018[7:18], na.rm=TRUE)
data_2018$NDVI_Avg <- rowMeans(data_2018[19:30], na.rm=TRUE)
data_2019$CT_Avg <- rowMeans(data_2019[7:19], na.rm=TRUE)
data_2019$NDVI_Avg <- rowMeans(data_2019[20:32], na.rm=TRUE)
data_2020$CT_Avg <- rowMeans(data_2020[7:21], na.rm=TRUE)
data_2020$NDVI_Avg <- rowMeans(data_2020[22:36], na.rm=TRUE)

# CT_Avg
cor(data_2016$CT_Avg, data_2016$GRYLD)
cor(data_2017$CT_Avg, data_2017$GRYLD, use = 'complete.obs')
cor(data_2018$CT_Avg, data_2018$GRYLD, use = 'complete.obs')
cor(data_2019$CT_Avg, data_2019$GRYLD, use = 'complete.obs')
cor(data_2020$CT_Avg, data_2020$GRYLD, use = 'complete.obs')

# NDVI_Avg
cor(data_2016$NDVI_Avg, data_2016$GRYLD)
cor(data_2017$NDVI_Avg, data_2017$GRYLD, use = 'complete.obs')
cor(data_2018$NDVI_Avg, data_2018$GRYLD, use = 'complete.obs')
cor(data_2019$NDVI_Avg, data_2019$GRYLD, use = 'complete.obs')
cor(data_2020$NDVI_Avg, data_2020$GRYLD, use = 'complete.obs')

# DTHD
cor(data_2016$DTHD, data_2016$GRYLD)
cor(data_2017$DTHD, data_2017$GRYLD, use = 'complete.obs')
cor(data_2018$DTHD, data_2018$GRYLD, use = 'complete.obs')
cor(data_2019$DTHD, data_2019$GRYLD, use = 'complete.obs')
cor(data_2020$DTHD, data_2020$GRYLD, use = 'complete.obs')

# DAYSMT
cor(data_2016$DAYSMT, data_2016$GRYLD)
cor(data_2017$DAYSMT, data_2017$GRYLD, use = 'complete.obs')
cor(data_2018$DAYSMT, data_2018$GRYLD, use = 'complete.obs')
cor(data_2019$DAYSMT, data_2019$GRYLD, use = 'complete.obs')
cor(data_2020$DAYSMT, data_2020$GRYLD, use = 'complete.obs')

# PH
cor(data_2016$PH, data_2016$GRYLD)
cor(data_2017$PH, data_2017$GRYLD, use = 'complete.obs')
cor(data_2018$PH, data_2018$GRYLD, use = 'complete.obs')
cor(data_2019$PH, data_2019$GRYLD, use = 'complete.obs')
cor(data_2020$PH, data_2020$GRYLD, use = 'complete.obs')

# SN
cor(data_2016$SN, data_2016$GRYLD)
cor(data_2017$SN, data_2017$GRYLD, use = 'complete.obs')
cor(data_2018$SN, data_2018$GRYLD, use = 'complete.obs')
cor(data_2019$SN, data_2019$GRYLD, use = 'complete.obs')
cor(data_2020$SN, data_2020$GRYLD, use = 'complete.obs')

# SPKLNG
cor(data_2016$SPKLNG, data_2016$GRYLD)
cor(data_2017$SPKLNG, data_2017$GRYLD, use = 'complete.obs')
cor(data_2018$SPKLNG, data_2018$GRYLD, use = 'complete.obs')
cor(data_2019$SPKLNG, data_2019$GRYLD, use = 'complete.obs')

# SPLN
cor(data_2016$SPLN, data_2016$GRYLD)
cor(data_2017$SPLN, data_2017$GRYLD, use = 'complete.obs')
cor(data_2018$SPLN, data_2018$GRYLD, use = 'complete.obs')
cor(data_2019$SPLN, data_2019$GRYLD, use = 'complete.obs')
cor(data_2020$SPLN, data_2020$GRYLD, use = 'complete.obs')

# GRNSPK
cor(data_2016$GRNSPK, data_2016$GRYLD)
cor(data_2017$GRNSPK, data_2017$GRYLD, use = 'complete.obs')
cor(data_2018$GRNSPK, data_2018$GRYLD, use = 'complete.obs')
cor(data_2019$GRNSPK, data_2019$GRYLD, use = 'complete.obs')
cor(data_2020$GRNSPK, data_2020$GRYLD, use = 'complete.obs')

# TGW
cor(data_2016$TGW, data_2016$GRYLD)
cor(data_2017$TGW, data_2017$GRYLD, use = 'complete.obs')
cor(data_2018$TGW, data_2018$GRYLD, use = 'complete.obs')
cor(data_2019$TGW, data_2019$GRYLD, use = 'complete.obs')
cor(data_2020$TGW, data_2020$GRYLD, use = 'complete.obs')