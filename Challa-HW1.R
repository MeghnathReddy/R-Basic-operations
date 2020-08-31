#Meghnath Reddy Challa-Homework-1

require(plyr) #package to work with data 
#Question 1
#a)create a vector and assign it to x
x <- c(3,12,6,-5,0,8,15,1,-10,7)

#b) create a sequence and assign it to y
y <- seq(min(x),max(x),length=10)

#c) basic stat function on x and y
cat("The sum of number of x: ", sum(x),'\n',"Mean of x:",mean(x),'\n',
    "Standard Deviation of x: ",sd(x),'\n',"variance of x: ",var(x),'\n',"Mean absolute deviation of x: ",mad(x,na.rm = TRUE)
    ,'\n',"Quartile of x: ",quantile(x),'\n',"Quintile of x: ",quantile(x,probs = seq(0,1,0.2)))

cat("The sum of number of y: ", sum(y),'\n',"Mean of y:",mean(y),'\n',
    "Standard Deviation of y: ",sd(y),'\n',"Mean absolute deviation of y: ",mad(y,na.rm = TRUE)
    ,'\n',"variance of y: ",var(y),'\n','\n',"Quartile of y: ",quantile(y),'\n',"Quintile of y: ",quantile(y,probs = seq(0,1,0.2)))

#d) sample with replacement
z <- sample(x,size = 7,replace = TRUE)
z
#e) find difference between means
t.test(x,y)

#f) sorted t-test
t.test(order(x),y,paired = TRUE)

#g) logical vector
a <-x>=0  #boolean value
a
b <-x[x<0] #to display negative number
b

#h) remove negative numbers
x<-x[a!=FALSE]
x
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#Question 2)
#a)display value of rows whose value is NULL
col1 <- c(1,2,3,NA,5)
col2 <- c(4,5,6,89,101)
col3 <- c(45,NA,66,121,201)
col4 <- c(14,NA,13,NA,27)
X <- rbind (col1,col2,col3,col4)
# list rows of data that have missing values
X[!complete.cases(X),]

#b) i) replace 99 with 'NA'
y <- c(3,12,99,99,7,99,21)
y[y==99]<-NA
y
#ii) number of NA 
sum(is.na(y))
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

#Question3)
#Using R: Introductory data exploration
#a)read csv
college <- read.csv('college.csv',stringsAsFactors=TRUE)

#b)remove 1st column
rownames (college) <- college [,1]
View (college )
college <- college [,-1]
View (college )

#c)different operations
#i)summary
summary(college)

#ii)help function
?pairs 
pairs(college[,1:10])

#iii)plot function "private vs outstate"
plot(college$Outstate~college$Private, main = "Outstate tution fee based on University type", xlab =
       "Private University", ylab = "Out-of-state Tuition")

#iV)Top10perc
Elite <- rep ("No", nrow(college )) #replace all the row values as "NO" by using rep function
Elite [college$Top10perc >50] <- "Yes" #where Top10perc is greater than 50 replace it by "Yes"
Elite <- as.factor (Elite) #convert a column into factor column i.e Levels-"Yes" and "No"
college <- data.frame(college ,Elite) #add the new column created as Elite to our original dataset "college"

#v)count of Elite university
summary(college) #they are 78 Elite universities

#vi)plot function "outstae vs elite"
plot(college$Outstate~college$Elite, main = "Outstate tution fee based on whether University is Elite or not", xlab =
       "Private University", ylab = "Elite University") 

#vii)histogram
?hist
par(mfrow=c(2,2))
hist(college$PhD,main="Count of PhD students",xlab="PhD students",ylab="Frequency")
hist(college$Books,main="Frequency of Book cost",xlab="Book cost")
hist(college$Enroll,main="Number of students enrolled",xlab="Students enrolled")
hist(college$perc.alumni,main="Percentage of alumni donated",xlab="Alumni donated")

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Question 4 : Manipulating data in data frames
#a)load data set
?baseball
data(baseball)

#b)clean data
baseball$sf[baseball$year<1954]<-0 #replace with 0
baseball$hbp[is.na(baseball$hbp)]<-0
clean_data<-subset(baseball,baseball$ab>=50)

#c)base percentage
clean_data$obp<-(clean_data$h+clean_data$bb+clean_data$hbp)/(clean_data$ab+clean_data$bb+clean_data$hbp+clean_data$sf)
clean_data$obp

#d)sorted order
sorted<-clean_data[order(clean_data$obp),]
cat("Top 5 records",'\n',"Years: ",sorted$year[1:5],'\n',"Player Name: ",sorted$id[1:5])

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#Question5 Aggregate function
?aggregate
#a)load data set
data("quakes")

#b)plot function
par(mfrow=c(2,2))
plot(quakes$depth,quakes$mag,main = "Magnitude vs Depth",xlab = "Magnitude",ylab = "Depth")

#c)aggregate implementation
quakeAvgDepth <- aggregate(quakes$depth, by = list(quakes$mag), FUN = mean) 

#d)rename
quakeAvgDepth <- rename(quakeAvgDepth,c(Group.1="Earthquake_Magnitude"))
quakeAvgDepth <- rename(quakeAvgDepth,c(x="Average_depth_recorded"))

#e)plot
plot(quakeAvgDepth$Average_depth_recorded,quakeAvgDepth$Earthquake_Magnitude,
     main = "Magnitude vs Avg_depth",xlab="Magnitude",ylab = "Avg_depth" )

#f)insight
#Observation 1)Earthquakes with the highest magnitude are found at the lowest depth.
#Observation 2)Lesser magnitude earthquakes are seen at the highest depth typically these results match the real-world scenario
#when such low magnitude earthquakes occur they might not cause any major damages to lives or financial impact
#sometimes they might go unnoticed by the people ex. In California state.
