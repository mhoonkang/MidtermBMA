library(devtools)
library(roxygen2)
current.code <- as.package("BmaPack")
load_all(current.code)
document(current.code)
install(pkg=current.code, local=TRUE) 

# check help documents
?fitBMA
?BmaPack

# check function
x <- matrix(rnorm(100,0,1),20,5)
y <- rnorm(20,1,3)
g <-3
fitBMA(x,y,g)
a <- fitBMA(x,y,g)
summary(a)
plot(a)

# check structure of bma class and summary.bma class
str(a)
b <- summary(a)
str(b)