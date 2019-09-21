library(data.table)
library(ggplot2)
library(magrittr)
options(datatable.print.class = TRUE)

# %>% :pipe 

student <- fread("data/student-data.csv")

summary(student)
student[,.N,male]
student[,.N,age][order(age)]

student<-student[beer!=2.11212e+11| is.na(beer)]

convertMHeightToCm<-function(student){
  student[height>=1.3 & height<=2.1,height:=height*100]
}


clearUnreliableHeight<-function(student){
  student[height<100|height>230,height:=NA]
}

clearUnreliableFood<-function(student){
  student[food<100|food>10^6,food:=NA]
}

clearUnreliableBeer<-function(student){
  student[beer<0|beer>100,beer:=NA]
}

convertMHeightToCm(student)
clearUnreliableHeight(student)
clearUnreliableFood(student)
clearUnreliableBeer(student)

student[order(-beer)]

summary(student)



student[,television:=as.numeric(television)]

ggplot(student,aes(height,weight))+geom_point()+facet_wrap(~as.factor(male))

long_data<-melt(student,measure.vars = c("food","beer","television"))
ggplot(long_data,aes(value))+geom_histogram(bins=10)+facet_wrap(~variable, scales = "free_x")

#miert jo a magrittr
c(1,2,3) %>% mean() %>%sqrt()
sqrt(mean(c(1,2,3)))
# a ketto ugyanazt adja vissza, de nehez esetben pipeos megoldas konnyebben olvashato

melt(student,measure.vars = c("food","beer","television")) %>% .[male==0]%>%
  ggplot(aes(value))+geom_histogram(bins=10)+facet_wrap(~variable, scales = "free_x")

# .[male] resznel a . a korabbrol erkezettet jeloli (pipeolas elott)



sales<-fread("data/sales_sample.csv")
summary(sales)
install.packages("fasttime")
library(fasttime)
sales[,purchase_date:=as.Date(fastPOSIXct(purchase_date))]
#contact_id order id, sales_amount max, quantity max

ggplot(sales[sales_amount<1000],aes(sales_amount))+geom_histogram()
sum(sales$sales_amount<1000) #2503618
sum(sales$sales_amount<1000) # 938
 
ggplot(sales,aes(quantity))+geom_histogram()

sales[,daily_sum:=sum(sales_amount),by=purchase_date]
sales
summary(sales)
ggplot(sales,aes(daily_sum))+geom_histogram()
sales[,daily_quantity:=sum(quantity),by=purchase_date]
ggplot(sales,aes(daily_quantity))+geom_histogram()

sales[, .N, by = customer_lifecycle_status]

