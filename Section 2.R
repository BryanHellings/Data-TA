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
ls1 = lm(EnrgyRF ~ MktRF)
ls2 = lm(EnrgyRF ~ MktRF + HML + SMB)
summary(ls)

abs(qt(0.95, 101))
anova(ls)
qf(.90, df1=3, df2=101)
energy = d$Enrgy[544:648]
RF = d$RF[544:648]
Mkt = d$Mkt[544:648]
data.frame(Mkt*0.015, HML*0.025)
summary(ls1)
summary(ls2)
anova(ls1, ls2)
anova(ls2)
qf(.90, df1=3, df2=101)
data.frame(Mkt=0.015, HML=0.025)
mean(data.frame(Mkt*0.015))

hist(ls$residuals, freq=FALSE)
curve(dnorm(x, mean=0, sd=sd(ls$residuals)), col="red", add=TRUE)

plot(1:NROW(energy), ls2$residuals)
abline(lm(1:NROW(energy) ~ ls2$residuals), col="blue")
summary(lm(MktRF ~ SMB + HML, d))
