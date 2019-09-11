library(data.table)
data_file <- 'data/sales_sample.csv'
sales <- fread(data_file)

sales_sample

ggplot(sales_sample)+geom_boxplot(mapping = aes(x=customer_lifecycle_status,y=sales_amount,col=customer_lifecycle_status))
