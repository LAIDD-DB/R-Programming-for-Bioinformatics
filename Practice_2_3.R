x = 1
y = 2
z = 1+2
z = x+y
x = 11
x + y = z
x+y -> z
z <- x+y

x=5
y=2

x/y
x*y
x+y
x-y

print("hello")
str="hello"
print(str)

v = c(1,3)
str= c("Hi","HS")
btype= factor( str )
btype= factor( c("A","B" , "O","AB","O","AB","A") )

a=TRUE
a=T
a=FALSE
a=F
x=Inf
y=Inf

class(x)
class(str)
class(btype)

a= c(1,3,5, NA)
is.na(a)
bc=as.character(btype)
bc
class(bc)
as.numeric(bc)
as.matrix(bc)
mbc = as.matrix(bc)
class(mbc)

a = T
b = F
a & b
a | b

a = c(T,F,T,T)
b = c(T,T,F,T)
a & b
a | b
a && b
a || b
a = 1:7
a = 7:1
a= vector(length=7) 

y= c(1,3,4)
y= c(y, 5,8,7)
seq(1,10,2)
seq(0,1,0.1)
seq(1,10,length.out=11)
seq(1,10,by=0.9)
rep(c(1,2,3), times=2 )
rep(c(1,2,3), each=2 )
rep(c(1,2,3), each=3 )

x = c(2,5,8,9,10)
x= seq(1,1400, by=2)
length(x)


x = c(1,2,3,4)
y = c(5,6,7,8)
z = c(1,2)
w = c(5,6,7)

y+z
y+w
x+w

x > 3
x < 2
x == 3
x != 3

all( x > 3  )
any( x > 3 )
x= seq(1,1400, by=2)
head(x)
tail(x)


x = seq(0,2000,by=15)
length(x)
y= seq(0,2000, by=8)
length(y)

intersect(x,y)
setdiff(x,y)
setdiff(y,x)
union(x,y)
setequal(x,y)

x = c(1,2,3)
y= c(3,2,1)
identicla(x,y)
setequal(x,y)


x = array( 1:5, c(2,4,3) )
x = array( 1:5, c(2,4) )
x = matrix( 1:8, nrow=2, ncol=4)
x = matrix( 1:8, nrow=2, ncol=4, byrow=T)

x = matrix(1:4, nr=2, nc=2)
y = matrix(5:8, nr=2, nc=2)
x+ y
x-y
x*y
x %*% y
t(x)
solve(x)
det(x)


m=matrix(1:30, nr=6, nc=5)
apply(m, 1, max)
apply(m, 2, max)

dim(m)
sample(m,3)
sample(m)
sample(10)

name=c("철수","춘향","길동")
age=c(22,20,25)
gender=factor(c("M","F","M"))
btype=factor(c("A","O","B"))

pp=data.frame(name, age, gender, btype)

pp[,4]
pp$btype
pp$name
pp[1,3]
pp[pp$name == "춘향",4]
pp[pp$name == "춘향", c("btype")]
pp[which(pp$name == "춘향"), c("btype")]
which(pp$name == "춘향")
which(c(T,F,F,F,T))
pp
pp$btype
pp[,"btype"]
pp[, c("age","btype")]
pp[, c("btype","age")]

max(women$weight)
mean(women$weight)
min(women$weight)
sd(women$weight)


with( women, sd(weight) )
with( women, sd(height) )

subset(women, height >70)
subset(women,  height >70, select=c(weight))
women[ women$height>70 , c("weight")]



np=data.frame(day=1:6, no=c(50,60,55,52,65,58) )
ll=list(a=pp, b=np)

ll$a$name
ll[[1]]
ll[1]
ll$a
ll[["a"]]

lapply(ll,dim)
lapply(ll,nrow)
lapply(ll, ncol)

sapply(ll,nrow)

data()

