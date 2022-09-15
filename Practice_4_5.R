stu = read.table(file="E:/Rstudy/students.txt")
stu = read.table(file="E:/Rstudy/students.txt", header=T, sep="\t")
stu2 = read.table(file="E:/Rstudy/students2.txt", header=T, sep=" ")
stu3 = read.csv(file="E:/Rstudy/students.csv",header=T)
stu4 = read.table(file="E:/Rstudy/students.csv", header=T, sep=",")
stu5 = read.csv(file="E:/Rstudy/students.txt",header=T, sep="\t")

write.table(stu, file="E:/Rstudy/output.txt")
write.table(stu, file="E:/Rstudy/output.txt", quote=F)
write.table(stu, file="E:/Rstudy/output.txt", quote=F, row.names=F )
write.csv(stu, file="E:/Rstudy/output.csv", quote=F, row.names=F )

save(stu,stu2,stu3, file="E:/Rstudy/output.Rdata" )


load(file="E:/Rstudy/output.Rdata" )


test = c(15, 20, 30, NA, 45)
test[1]

test<40
test[ test<40 ]
which(test<40)
test[which(test<40) ]

test[test%%3 == 0 ]
test[test%%3 != 0 ]

test[which(test%%3 == 0) ]
test[which(test%%3 != 0) ]

is.na(test)
test[!is.na(test)]

test[ test%%3 == 0 & !is.na(test)  ]
test[ test%%3 == 0 | !is.na(test)  ]


stu[2,3]
stu[,"math"]
stu$math

stu[stu$name == "이상훈", "math" ]
stu[stu$name == "이상훈", colnames(stu)=="math" ]

stu[ stu$korean >= 90 & stu$english >=90, 1]
stu[ stu$korean >= 90 & stu$english >=90, "name"]
stu[ stu$korean >= 90 & stu$english >=90, colnames(stu)== "name"]

stu[ stu$korean >= 90 & stu$english >=90, colnames(stu)== "math"]
stu[ stu$korean >= 90 & stu$english >=90,  "math"]


x= 10
if(x<100) print("Olleh~")
if(x>100) print("Hmm..")

x= 150


if(x>100) print("Hmm..")

x=10
if(x>100){
  print("Hmm")
}else{
  print("Olleh~")
}

if(x>100){
  print("Hmm")
}else if(x>1000){
  print("Ha~!!")
}else{
  print("Olleh~")
}

x=150

x=10
ifelse( x>100 , "High", "low" )


stu[2,3] = -100
stu[4,4] = -2

stu$english = ifelse(stu$english < 0, NA, stu$english )
stu$math = ifelse(stu$math < 0, NA, stu$math)


for(i in c(1,3,5) ){
  print(i)
}


for(i in 1:10 ){
  print(i)
}

v = c()
for( i in 1:10){
  v[i]= i+1
  
}

m = matrix(NA, nr=10, nc=5 )
for(i in 1:10 ){
    
  for(j in 1:5){
    
    m[i,j] = i+j
  }
  
} 


m = matrix(NA, nr=10, nc=5 )
for(i in 1:10 ){
  
  for(j in 1:5){
    
    if(i==j){
      m[i,j] = NA
    }else{
      m[i,j] = i+j
    }
  }
  
}
m = matrix(NA, nr=10, nc=5 )
for(i in 1:10 ){
  
  if( i == 5) next
   
  for(j in 1:5){
    m[i,j] = i+j
  }
  
} 




stu$english = ifelse(stu$english < 0, NA, stu$english )
stu$math = ifelse(stu$math < 0, NA, stu$math)

fillmean=function(v){
  
  if( any( is.na(v) )  ){
    
    resv = v
    resv[is.na(resv)]= mean(resv[!is.na(resv)])
    
  }else{
    resv=v
  }
  
  return(resv)
  
}

stu$english = fillmean( stu$english )
stu$math = fillmean( stu$math )



fillmin=function(v){
  
  if( any( is.na(v) )  ){
    
    resv = v
    resv[is.na(resv)]= min(resv[!is.na(resv)])
    
  }else{
    resv=v
  }
  
  return(resv)
  
}
stu[3,4]=NA
fillmin(stu$math)
stu$math=fillmin(stu$math)
mean(airquality$Ozone, na.rm=T)

newaq = na.omit(airquality)


name=c("철수","춘향","길동","해승","예지","세라")
age=c(22,20,25,30, 29,26)
gender=factor(c("M","F","M","F","K","F"))
btype=factor(c("A","O","B","AB","A","C"))

pp=data.frame(name, age, gender, btype)

pp[pp$gender =="K",]
pp[pp$gender =="K","gender"] = NA

bt = c("A","B","AB","O")
pp$btype == "C"
pp$btype %in% bt

pp[!(pp$btype %in% bt), "btype" ] = NA



oz=airquality$Ozone
boxplot(oz)
bp = boxplot(oz)
bp$stats

airquality[oz > bp$stats[5,] ,]
airquality[oz > bp$stats[5,] & !is.na(oz),]

airquality$Ozone = ifelse( oz > bp$stats[5,] & !is.na(oz) , NA , oz )

table( is.na(airquality$Ozone) )

mean(oz, na.rm=T)
mean(airquality$Ozone, na.rm=T)

naq=na.omit(airquality)
