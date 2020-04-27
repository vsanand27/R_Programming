install.packages("tidyverse")
install.packages ("jsonlite")
demo_table <- read.csv(file = 'demo.csv', check.names = F, stringsAsFactors = F)

library(jsonlite)
?fromJSON()

demo_table2 <- fromJSON(txt = 'demo2.JSON')

x <- c(3,3,2,2,5,5,8,8,9)
x[3]

## 3 row and column Year
## 1st Way
demo_table[3,"Year"]
## 2nd Way
demo_table[3,3]
## 3r way to refer is $
demo_table$"Vehicle_Class"[2]
filter_table <-demo_table2[demo_table2$price >10000,]

?subset()

filter_table2 <- subset(demo_table2, price > 10000 & drive == "4wd" & "clean" %in% title_status) #filter by price and drivetrain

?sample()

sample(c("cow", "deer", "pig", "chicken", "duck", "sheep", "dog"), 4)

demo_table[sample(1:nrow(demo_table), 3),]

library(tidyverse)
?mutate()

demo_table <- demo_table %>% mutate(Mileage_per_Year=Total_Miles/(2020-Year),IsActive=TRUE) #add columns to original data frame

## Group by and Summarize data
summarize_demo <- demo_table2 %>% group_by(condition) %>% summarize(Mean_Mileage=mean(odometer)) #create summary table

summarize_demo <- demo_table2 %>% group_by(condition) %>% summarize(Mean_Mileage=mean(odometer),Maximum_Price=max(price),Num_Vehicles=n()) #create summary table with multiple columns

?gather()
demo_table3 <- read.csv('demo3.csv',check.names = F,stringsAsFactors = F)


long_table <- gather(demo_table3,key="Metric",value="Score",buying_price:popularity)
long_table <- demo_table3 %>% gather(key="Metric",value="Score",buying_price:popularity)

?spread()

wide_table <- long_table %>% spread(key="Metric",value="Score")

## Compare tables using all.equal fuction
all.equal(demo_table3,wide_table)

?ggplot()
head(mpg)

plt <- ggplot(mpg,aes(x=class)) #import dataset into ggplot2
plt + geom_bar() #plot a bar plot  & it uses count to display


?geom_bar()

mpg_summary <- mpg %>% group_by(manufacturer) %>% summarize(Vehicle_Count=n()) #create summary table
plt <- ggplot(mpg_summary,aes(x=manufacturer,y=Vehicle_Count)) #import dataset into ggplot2
plt + geom_col() #plot a bar plot

# formatting graph - titles X & Y axis utilzing xlab & ylab
plt + geom_col() + xlab("Manufacturing Company") + ylab("Number of Vehicles in Dataset")+ #plot bar plot with labels
# Rotating x axis label at 45 degree angel
theme(axis.text.x=element_text(angle=45,hjust=1)) #rotate the x-axis label 45 degrees

# Create Summary table - line Chart
mpg_summary2 <- subset(mpg,manufacturer=="toyota") %>% group_by(cyl) %>% summarize(Mean_Hwy=mean(hwy)) #create summary table
plt <- ggplot(mpg_summary2, aes(x=cyl,y=Mean_Hwy)) #import dataset into ggplot2
plt + geom_line()
# defining X axis with limit since there is no 8 cylender vehicle and scale_y_contineous
plt + geom_line() + scale_x_discrete(limits=c(4,6,8)) + scale_y_continuous(breaks = c(15:30)) #add line plot with labels

# Scatter plot chart x- independent variable y - dependent to obtain relationship
plt <- ggplot(mpg,aes(x=displ,y=cty)) #import dataset into ggplot2
plt + geom_point() + xlab("Engine Size (L)") + ylab("City Fuel-Efficiency (MPG)") #add scatter plot with labels

# Add color to Scatterplot
plt <- ggplot(mpg,aes(x=displ,y=cty,color=class)) #import dataset into ggplot2
plt + geom_point() + labs(x="Engine Size (L)", y="City Fuel-Efficiency (MPG)", color="Vehicle Class") #add scatter plot with labels

# Add Shape to Scatterplot
plt <- ggplot(mpg,aes(x=displ,y=cty,color=class,shape=drv)) #import dataset into ggplot2
plt + geom_point() + labs(x="Engine Size (L)", y="City Fuel-Efficiency (MPG)", color="Vehicle Class",shape="Type of Drive") #add scatter plot with multiple aesthetics


# Add size to Scatterplot
plt <- ggplot(mpg,aes(x=displ,y=cty,color=class,shape=drv,size=cyl)) #import dataset into ggplot2
plt + geom_point() + labs(x="Engine Size (L)", y="City Fuel-Efficiency (MPG)", color="Vehicle Class",shape="Type of Drive", size ="No of Cylenders") #add scatter plot with multiple aesthetics

#---------------------
# 15.3.5 Add new boxplot chart
# define y value for boxplot

plt <- ggplot(mpg,aes(y=hwy)) #import dataset into ggplot2
plt + geom_boxplot() #add boxplot

plt <- ggplot(mpg,aes(x=manufacturer,y=hwy)) #import dataset into ggplot2
plt + geom_boxplot() + theme(axis.text.x=element_text(angle=45,hjust=1)) #add boxplot and rotate x-axis labels 45 degrees


# add color
plt <- ggplot(mpg,aes(x=manufacturer,y=hwy, color=class)) #import dataset into ggplot2
plt + geom_boxplot() + theme(axis.text.x=element_text(angle=45,hjust=1))+labs(x="Manufacturers", y="Highway Fuel-Efficiency (MPG)", color="Vehicle Class") #add boxplot and rotate x-axis labels 45 degrees

#----------------------------------
# 15.3.6 - Headmap plots
# Note factor - created no of years not year value
mpg_summary3 <- mpg %>% group_by(class,year) %>% summarize(Mean_Hwy=mean(hwy)) #create summary table
plt <- ggplot(mpg_summary3, aes(x=class,y=factor(year),fill=Mean_Hwy))
plt + geom_tile() + labs(x="Vehicle Class",y="Vehicle Year",fill="Mean Highway (MPG)") #create heatmap with labels 


# removed factor
mpg_summary3 <- mpg %>% group_by(class,year) %>% summarize(Mean_Hwy=mean(hwy)) #create summary table
plt <- ggplot(mpg_summary3, aes(x=class,y=year,fill=Mean_Hwy))
plt + geom_tile() + labs(x="Vehicle Class",y="Vehicle Year",fill="Mean Highway (MPG)") #create heatmap with labels 

# Average highway fuel efficince by vehicle
mpg_summary4 <- mpg %>% group_by(model,year) %>% summarize(Mean_Hwy=mean(hwy)) #create summary table
plt <- ggplot(mpg_summary4, aes(x=model,y=factor(year),fill=Mean_Hwy)) #import dataset into ggplot2
plt + geom_tile() + labs(x="Model",y="Vehicle Year",fill="Mean Highway (MPG)") + theme(axis.text.x = element_text(angle=90,hjust=1,vjust=.5)) #rotate x-axis labels 90 degrees

#---------------------------------------
# Layering plot with same variable or different variables

#1 Box Plot and Scatter Plot mix

plt <- ggplot(mpg,aes(x=manufacturer,y=hwy, color=class)) #import dataset into ggplot2
plt + geom_boxplot() + #add boxplot
theme(axis.text.x=element_text(angle=45,hjust=1)) + labs(x="Manufacturer",y="Highway (MPG)", color="class")+
geom_point() #overlay scatter plot on top

# Mapping different plots with different variables
# Step 1 scatter graph

mpg_summary5 <- mpg %>% group_by(class) %>% summarize(Mean_Engine=mean(displ)) #create summary table
plt <- ggplot(mpg_summary5,aes(x=class,y=Mean_Engine)) #import dataset into ggplot2
plt + geom_point(size=4) + labs(x="Vehicle Class",y="Mean Engine Size") #add scatter plot

# Step 2 standard deviation
mpg_summary6 <- mpg %>% group_by(class) %>% summarize(Mean_Engine=mean(displ),SD_Engine=sd(displ))
plt <- ggplot(mpg_summary6,aes(x=class,y=Mean_Engine)) #import dataset into ggplot2
plt + geom_point(size=4) + labs(x="Vehicle Class",y="Mean Engine Size") + #add scatter plot with labels
geom_errorbar(aes(ymin=Mean_Engine-SD_Engine,ymax=Mean_Engine+SD_Engine)) #overlay with error bars

# Faceting - is divider inbetween graph;  what if city and highway was in long format - how to visualize long data in single plot
# combine 2 columns into 1 with value 
mpg_long <- mpg %>% gather(key="MPG_Type",value="Rating",c(cty,hwy)) #convert to long format
head(mpg_long)

plt <- ggplot(mpg_long,aes(x=manufacturer,y=Rating,color=MPG_Type)) #import dataset into ggplot2
plt + geom_boxplot() + theme(axis.text.x=element_text(angle=45,hjust=1)) #add boxplot with labels rotated 45 degrees

?facet_wrap()
plt <- ggplot(mpg_long,aes(x=manufacturer,y=Rating,color=MPG_Type)) #import dataset into ggplot2
plt + geom_boxplot() + facet_wrap(vars(MPG_Type)) + #create multiple boxplots, one for each MPG type
theme(axis.text.x=element_text(angle=45,hjust=1),legend.position = "none") + xlab("Manufacturer") #rotate x-axis labels

head(mtcars)
mtcars1 <- mtcars
ggplot(mtcars,aes(x=wt))+ geom_density() # visualize distribution

# for normality - shapiro-Wilk test is performed
?shapiro.test()
shapiro.test(mtcars$wt)

#-------------------------------------
## Sampling and Testing 
# Sample() or Sample_n()functions(Two dimenstion)

? sample_n()

population_table <- read.csv("used_car_data.csv",check.names=F,stringsAsFactors = F )# Import used car data
plt <- ggplot(population_table,aes(x=log10(Miles_Driven))) #import dataset into ggplot2
plt + geom_density() #visualize distribution using density plot

# utilizing sample data now from population and normalize it utiling log10
sample_table <- population_table %>% sample_n(50) #randomly sample 50 data points
plt <- ggplot(sample_table,aes(x=log10(Miles_Driven))) #import dataset into ggplot2
plt + geom_density() #visualize distribution using density plot

## t test - one sample

?t.test()
t.test(log10(sample_table$Miles_Driven),mu=mean(log10(population_table$Miles_Driven))) #compare sample versus population means

# t 0 test two sided - two sample test

sample_table <- population_table %>% sample_n(50) #generate 50 randomly sampled data points
sample_table2 <- population_table %>% sample_n(50) #generate another 50 randomly sampled data points


t.test(log10(sample_table$Miles_Driven),log10(sample_table2$Miles_Driven)) #compare means of two samples

mpg_data <- read.csv('mpg_modified.csv') #import dataset
mpg_1999 <- mpg_data %>% filter(year==1999) #select only data points where the year is 1999
mpg_2008 <- mpg_data %>% filter(year==2008) #select only data points where the year is 2008


# paired t-test to determine if there is a statistical difference in overall highway fuel efficiency between vehicles manufactured in 1999 versus 2008.
t.test(mpg_1999$hwy,mpg_2008$hwy,paired = T) #compare the mean difference between two samples

# Analysis of Variance test - under one and two independent variable vs. one dependent variable. 

?aov()
# Stat difference in horsepower (dep) vs engine type (independent)
mtcars_filt <- mtcars[,c("hp","cyl")] #filter columns from mtcars dataset
mtcars_filt$cyl <- factor(mtcars_filt$cyl) #convert numeric column to factor

aov(hp ~ cyl,data=mtcars_filt) #compare means across multiple levels

# Since this does not provide you with pvalue, use summary function()

summary(aov(hp ~ cyl,data=mtcars_filt))

#----------------------------------------------------------------

# Correlations
?cor()

head (mtcars)

plt <- ggplot(mtcars,aes(x=hp,y=qsec)) #import dataset into ggplot2
plt + geom_point() #create scatter plot

cor(mtcars$hp,mtcars$qsec) # calc coiffient 

used_cars <- read.csv('used_car_data.csv',stringsAsFactors = F) #read in dataset
head(used_cars)

plt <-ggplot(used_cars,aes(x=Miles_Driven,y=Selling_Price))
plt+geom_point()# scatter plot

cor(used_cars$Miles_Driven,used_cars$Selling_Price) #calculate correlation coefficient

#Muliple Variables in matrix and correlations
used_matrix <- as.matrix(used_cars[,c("Selling_Price","Present_Price","Miles_Driven")]) #convert data frame into numeric matrix
cor(used_matrix)

#--------------------------------------------------------------------
# Linear regression
# y (dependent var)= m(slope)*x (independent var)+ intercept

?lm()

lm(qsec ~ hp,mtcars) #create linear model

summary(lm(qsec~hp,mtcars)) #summarize linear model

model <- lm(qsec ~ hp,mtcars) #create linear model
yvals <- model$coefficients['hp']*mtcars$hp + model$coefficients['(Intercept)'] #determine y-axis values from linear model

plt <- ggplot(mtcars,aes(x=hp,y=qsec)) #import dataset into ggplot2
plt + geom_point() + geom_line(aes(y=yvals), color = "red") #plot scatter and linear model

#----------------------------------------------------------------
# Multiple linear regression 

lm(qsec ~ mpg + disp + drat + wt + hp,data=mtcars) #generate multiple linear regression model

summary(lm(qsec ~ mpg + disp + drat + wt + hp,data=mtcars)) #generate summary statistics

#----------------------------------------------------------------------
# Chi-squared test
# The chi-squared test is used to compare the distribution of frequencies across two groups and tests the following hypotheses:

# table function produces frequecy 
tbl <- table(mpg$class,mpg$year) #generate contingency table

chisq.test(tbl)# compare categorial 






