x = c(125,136,110,150,80,76,96,105,130,115)
y = c(34,38,32,28,42,45,44,50,53,43)
lm(y~x+0)
summary(lm(y~x+0))

#Multiple R-squared:  0.9012 >> R**2
مدل  بدون  عرضاز مبدا  

summary(lm(y~x+0))$ r.squared
==========
  ((round(rnorm(100,50,10),0))%%6)==0
a=choose.files()

==================
  f <- function(x, measure = NA){
    switch(measure,
           Mean = mean(x),
           Median = median(x),
           SD = sd(x))
          Plot= plot(x)
  }



repeat{
  y <- rnorm(1)
  if (y > 2) break 
  cat(y, "\n")
  plot(y)
}