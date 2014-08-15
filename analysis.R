x <- c(88,5,12,13)
x <- c(x[1:3],168,x[4]) # insert 168 before the 13
x

x <- c(1,2,4)
length(x)

first1 <- function(x) {
	for (i in 1:length(x)) {
	if (x[i] == 1) break # break out of loop
	}
	return(i)
}

x <- c()
x
length(x)
1:length(x)

m # matrix
m + 10:13

z <- 3 # No declaration needed
y[1] <- 5 # will not work if y not defined
y <- vector(length=2)
y[1] <- 5
y[2] <- 12

x <- c(1,5) # not constrained by mode
x
x <- "abc"

c(1,2,4) + c(6,0,9,20,22)
c(1,2,4,1,2) + c(6,0,9,20,22)

m # matrix
m+c(1,2)

2+3
"+"(2,3)

x <- c(1,2,4)
x + c(5,0,-1)

x * c(5,0,-1)
x / c(5,4,-1)
x %% c(5,4,-1)

y <- c(1.2,3.9,0.4,0.12)
y[c(1,3)] # extract elements 1 and 3 of y
y[2:3]
v <- 3:4
y[v]

x <- c(4,2,17,5)
y <- x[c(1,1,3)]
y

z <- c(5,12,13)
z[-1] # exclude element 1
z[-1:-2] # exclude elements 1 through 2

z <- c(5,12,13)
z[1:(length(z)-1)]
z[-length(z)]

5:8
5:1
i <- 2
1:i-1 # this means (1:i) - 1, not 1:(i-1)
1:(i-1)

seq(from=12,to=30,by=3)
seq(from=1.1,to=2,length=10)
for (i in 1:length(x)) # can be problem
for (i in seq(x))
x <- c(5,12,13)
x
seq(x)
x <- NULL
x
NULL
seq(x)

x <- rep(8,4)
x
rep(c(5,12,13),3)
rep(1:3,2)
rep(c(5,12,13),each=2)

x <- 1:10
any(x > 8)
any(x > 88)
all(x > 88)
all(x > 0)

findruns <- function(x,k) {
	n <- length(x)
	runs <- NULL
	for (i in 1:(n-k+1)) {
	if (all(x[i:(i+k-1)]==1)) runs <- c(runs,i)
	}
	return(runs)
}

y <- c(1,0,0,1,1,1,0,1,1)
findruns(y,3)
findruns(y,2)
findruns(y,6)

findruns1 <- function(x,k) {
	n <- length(x)
	runs <- vector(length=n)
	count <- 0
	for (i in 1:(n-k+1)) {
	if (all(x[i:(i+k-1)]==1)) {
		count <- count + 1
		runs[count] <- i
		}
	}
	if (count > 0) {
		runs <- runs[1:count]
	} else runs <- NULL
	return(runs)
}

preda <- function(x,k) {
	n <- length(x)
	k2 <- k/2
	# the vector pred will contain our predicted values
	pred <- vector(length=n-k)
	for (i in 1:(n-k)) {
	if (sum(x[i:(i+(k-1))]) >= k2) pred[i] <- 1 else pred[i] <- 0
	}
	return(mean(abs(pred-x[(k+1):n])))
}

predb <- function(x,k) {
	n <- length(x)
	k2 <- k/2
	pred <- vector(length=n-k)
	sm <- sum(x[1:k])
	if (sm >= k2) pred[1] <- 1 else pred[1] <- 0
	if (n-k >= 2) {
		for (i in 2:(n-k)) {
			sm <- sm + x[i+k-1] - x[i-1]
			if (sm >= k2) pred[i] <- 1 else pred[i] <- 0
		}
	}
	return(mean(abs(pred-x[(k+1):n])))
}

u <- c(5,2,8)
v <- c(1,3,9)
u > v

w <- function(x) return(x+1)
w(u)
y <- c(1.2,3.9,0.4)
z <- round(y)
z
y <- c(12,5,13)
y+4
'+'(y,4)

f <- function(x,c) return((x+c)^2)
f(1:3,0)
f(1:3,1)
f(1:3,1:3)

f <- function(x,c) {
	if (length(c) != 1) stop("vector c not allowed")
	return((x+c)^2)
}

x <- c(88,NA,12,168,13)
x
mean(x)
mean(x,na.rm=T)
x <- c(88,NULL,12,168,13)
mean(x)

z <- NULL
for (i in 1:10) if (i %%2 == 0) z <- c(z,i)
z

z <- NA
for (i in 1:10) if (i %%2 == 0) z <- c(z,i)
z

u <- NULL
length(u)
v <- NA
length(v)

z <- c(5,2,-3,8)
w <- z[z*z > 8]
w
z[c(TRUE,FALSE,TRUE,TRUE)]

x <- c(1,3,8,2,20)
x[x > 3] <- 0
x

x <- c(6,1:3,NA,12)
x
x[x > 5]
subset(x,x > 5)

z <- c(5,2,-3,8)
which(z*z > 8)

first1 <- function(x) {
	for (i in 1:length(x)) {
	if (x[i] == 1) break # break out of loop
	}
	return(i)
}

first1a <- function(x) return(which(x == 1)[1])

ifelse(b,u,v)
x <- 1:10
y <- ifelse(x %% 2 == 0,5,12) # %% is the mod operator
y

x <- c(5,2,9,12)
ifelse(x > 6,2*x,3*x)

findud <- function(v) {
	vud <- v[-1] - v[-length(v)]
	return(ifelse(vud > 0,1,-1))
}

udcorr <- function(x,y) {
	ud <- lapply(list(x,y),findud)
	return(mean(ud[[1]] == ud[[2]]))
}

x <- c(5,12,13,3,6,0,1,15,16,8,88)
y <- c(4,2,3,23,6,10,11,12,6,3,2)
udcorr(x,y)

x <- 1:3
y <- c(1,3,4)
x == y

x <- 1:3
y <- c(1,3,4)
x == y
all(x == y)

identical(x,y)

x <- c(1,2,4)
names(x)
names(x) <- c("a","b","ab")
names(x)
x
x["b"]












