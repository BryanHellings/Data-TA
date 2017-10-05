#read data TA-IPandFFF
library("foreign")
d = read.spss("TA-IPandFFF.sav", to.data.frame=TRUE)

d$Enrgy[544:648]
d$RF[544:648]
EnrgyRF = d$Enrgy[544:648] - d$RF[544:648]
MktRF = d$Mkt[544:648] - d$RF[544:648]
plot(MktRF, EnrgyRF)
abline(lm(MktRF ~ EnrgyRF), col="red")
ls = lm(MktRF ~ EnrgyRF)
summary(ls)

t.test(x=MktRF, mu=0.82057, conf=0.90)

plot(MktRF, ls$residuals)
abline(lm(MktRF ~ ls$residuals), col="blue")
summary(ls$residuals)

hist(ls$residuals, freq=FALSE)
curve(dnorm(x, mean=0, sd=sd(ls$residuals)), col="dark.green", add=TRUE)

help(pt)
