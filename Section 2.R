#read data TA-IPandFFF
library("foreign")
d = read.spss("TA-IPandFFF.sav", to.data.frame=TRUE)

d$Enrgy[544:648]
d$RF[544:648]
d$HML[544:648]
d$SMB[544:648]
HML = d$HML[544:648]
SMB = d$SMB[544:648]
EnrgyRF = d$Enrgy[544:648] - d$RF[544:648]
MktRF = d$Mkt[544:648] - d$RF[544:648]
ls = lm(EnrgyRF ~ MktRF + HML + SMB)
summary(ls)

abs(qt(0.95, 101))
anova(ls)
