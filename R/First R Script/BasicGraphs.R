#set directory path
setwd("/Users/nikhilahuja/Documents/MGSC_Courses/MGSC_310/Assignment1/dataestate@nahuja")

#read from file
real_estate_data <- read.csv("dataestate@nahuja.csv")

#print data thingy
str(real_estate_data)

#set x and y + labels
real_estate_data$population_category <- cut(real_estate_data$population, breaks = c(0, 10000, 20000, 30000, 40000, 50000), labels = c("0-10k", "10k-20k", "20k-30k", "30k-40k", "40k-50k"))

# make box plot
boxplot(sale_amount ~ population_category, data = real_estate_data,
        main = "House Sale Amount Compared to Population",
        xlab = "Population", 
        ylab = "Final Sale Price",
        col = "blue",  
        outline = FALSE)  #making true gets rid of box

#install ggplot2
install.packages("ggplot2")

#make the months numeric
real_estate_data$month <- factor(real_estate_data$month, levels = 1:12)

# Count the number of sales for each month
sales_counts <- table(real_estate_data$month)

# Create a bar graph using ggplot2
library(ggplot2)

#make bar graph
ggplot(data = data.frame(Month = names(sales_counts), Sales = as.vector(sales_counts)), aes(x = Month, y = Sales)) +
  geom_bar(stat = "identity", fill = "white", color = "blue") +
  labs(title = "Monthly Sales Count", x = "Month", y = "Number of Sales") +
  scale_x_discrete(limits = as.character(1:12)) # had to add this to keep x axis in order



#old-------------------------------------
# Creates bar graph
barplot(sales_counts, 
        main = "Monthly Sales Count",
        xlab = "Month",
        ylab = "Number of Sales",
        col = "blue",
        ylim = c(0, max_count + 10000)) #extends y axis
#create scatter plot
#gets rid of scientific notation
options(scipen = 1)
point_size <- 0.5  # Adjust the size as needed
point_color <- "green"  # Change to your desired color

plot(real_estate_data$population, real_estate_data$sale_amount, 
     main = "Population vs. Sale Amount",
     xlab = "Population", 
     ylab = "Sale Amount",
     cex = point_size,
     col = point_color)

