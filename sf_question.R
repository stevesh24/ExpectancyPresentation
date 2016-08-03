home <- "C:/Users/ibshi/Desktop/data incubator/SF property/"
setwd(home)
filename <- "Historic_Secured_Property_Tax_Rolls.csv"
filename <- paste(home,filename,sep="")
sf_data <- read.csv(filename, header = TRUE)


#q1
t <- summary(sf_data$Property.Class.Code)
max_perc <- max(t)/(sum(t))
print(paste("max perc = ",as.character(max_perc)))

sf_sorted <- sf_data[order(sf_data[,5], sf_data[,1]), ]
sf_sorted <- subset(sf_sorted,!(is.na(sf_sorted$Closed.Roll.Fiscal.Year)))
                   
uni_block <- unique(sf_sorted$Block.and.Lot.Number)
sf_2014 <- subset(sf_sorted, sf_sorted$Closed.Roll.Fiscal.Year == 2014)
sf_2013 <- subset(sf_sorted, sf_sorted$Closed.Roll.Fiscal.Year == 2013)
sf_2012 <- subset(sf_sorted, sf_sorted$Closed.Roll.Fiscal.Year == 2012)
sf_2011 <- subset(sf_sorted, sf_sorted$Closed.Roll.Fiscal.Year == 2011)
sf_2010 <- subset(sf_sorted, sf_sorted$Closed.Roll.Fiscal.Year == 2010)
sf_2009 <- subset(sf_sorted, sf_sorted$Closed.Roll.Fiscal.Year == 2009)
sf_2008 <- subset(sf_sorted, sf_sorted$Closed.Roll.Fiscal.Year == 2008)
sf_2007 <- subset(sf_sorted, sf_sorted$Closed.Roll.Fiscal.Year == 2007)

# a check to see if number of unique block and lot numbers equals number of rows (no duplication)
t <- nrow(sf_2007)
t1 <- length(unique(sf_2007$Block.and.Lot.Number))
print(paste(as.character(2007),as.character(t),as.character(t1)))

t <- nrow(sf_2008)
t1 <- length(unique(sf_2008$Block.and.Lot.Number))
print(paste(as.character(2008),as.character(t),as.character(t1)))

t <- nrow(sf_2009)
t1 <- length(unique(sf_2009$Block.and.Lot.Number))
print(paste(as.character(2009),as.character(t),as.character(t1)))

t <- nrow(sf_2010)
t1 <- length(unique(sf_2010$Block.and.Lot.Number))
print(paste(as.character(2010),as.character(t),as.character(t1)))

t <- nrow(sf_2011)
t1 <- length(unique(sf_2011$Block.and.Lot.Number))
print(paste(as.character(2011),as.character(t),as.character(t1)))

t <- nrow(sf_2012)
t1 <- length(unique(sf_2012$Block.and.Lot.Number))
print(paste(as.character(2012),as.character(t),as.character(t1)))

t <- nrow(sf_2013)
t1 <- length(unique(sf_2013$Block.and.Lot.Number))
print(paste(as.character(2013),as.character(t),as.character(t1)))

t <- nrow(sf_2014)
t1 <- length(unique(sf_2014$Block.and.Lot.Number))
print(paste(as.character(2014),as.character(t),as.character(t1)))

# creating data by last assessment (Q5, Q8)
sf_final <- sf_2007

t <- unique(sf_2008$Block.and.Lot.Number)
sf_final <- sf_final[!(sf_final$Block.and.Lot.Number %in% t), ]
sf_final <- rbind(sf_final,sf_2008)

t <- unique(sf_2009$Block.and.Lot.Number)
sf_final <- sf_final[!(sf_final$Block.and.Lot.Number %in% t), ]
sf_final <- rbind(sf_final,sf_2009)

t <- unique(sf_2010$Block.and.Lot.Number)
sf_final <- sf_final[!(sf_final$Block.and.Lot.Number %in% t), ]
sf_final <- rbind(sf_final,sf_2010)

t <- unique(sf_2011$Block.and.Lot.Number)
sf_final <- sf_final[!(sf_final$Block.and.Lot.Number %in% t), ]
sf_final <- rbind(sf_final,sf_2011)

t <- unique(sf_2012$Block.and.Lot.Number)
sf_final <- sf_final[!(sf_final$Block.and.Lot.Number %in% t), ]
sf_final <- rbind(sf_final,sf_2012)

t <- unique(sf_2013$Block.and.Lot.Number)
sf_final <- sf_final[!(sf_final$Block.and.Lot.Number %in% t), ]
sf_final <- rbind(sf_final,sf_2013)

t <- unique(sf_2014$Block.and.Lot.Number)
sf_final <- sf_final[!(sf_final$Block.and.Lot.Number %in% t), ]
sf_final <- rbind(sf_final,sf_2014)

# Q2
# Calculate the average improvement value (using only non-zero assessments) 
# in each neighborhood. What is the difference between the greatest and least average values?

#q2 <- subset(sf_final,sf_final$Closed.Roll.Assessed.Improvement.Value > 0)
q2 <- subset(sf_sorted,sf_sorted$Closed.Roll.Assessed.Improvement.Value > 0)
a2 <- tapply(q2$Closed.Roll.Assessed.Improvement.Value, q2$Neighborhood.Code, mean)
a2 <- as.numeric(a2)
a2 <- subset(a2,!is.na(a2))
print(paste("difference in means",as.character(max(a2)-min(a2))))

#Q4
#q4 <- subset(sf_final,(sf_final$Number.of.Units > 0 & sf_final$Number.of.Bedrooms > 0))
q4 <- subset(sf_sorted,(sf_sorted$Number.of.Units > 0 & sf_sorted$Number.of.Bedrooms > 0))
a4_unit <- tapply(q4$Number.of.Units, q4$Zipcode.of.Parcel, mean)
a4_unit <- as.numeric(a4_unit)
a4_unit <- subset(a4_unit,!is.na(a4_unit))
a4_bedrooms <- tapply(q4$Number.of.Bedrooms, q4$Zipcode.of.Parcel, mean)
a4_bedrooms <- as.numeric(a4_bedrooms)
a4_bedrooms <- subset(a4_bedrooms,!is.na(a4_bedrooms))
a4_ratio <- a4_bedrooms/a4_unit
print(paste("max ratio of bed to unit",as.character(max(a4_ratio))))

# Q5
q5 <- subset(sf_final,sf_final$Closed.Roll.Assessed.Improvement.Value > 0)
print(paste("median assessed improvement value",as.character(median(q5$Closed.Roll.Assessed.Improvement.Value))))

# Q6

# What is the yearly growth rate of Land Values over the years covered by this data? 
# Take a simplistic model: the value is given by P=P0ertP=P0ert, where tt is measured in years. 
# (A more complete model would give each property its own base price P(i)0P0(i).) Estimate rr over all assessments 
# with a non-zero land value. (Hint: Consider using linear regression and logarithms.)

q6 <- subset(sf_2008,sf_2008$Closed.Roll.Assessed.Land.Value > 0)
q6 <- rbind(q6,subset(sf_2009,sf_2009$Closed.Roll.Assessed.Land.Value > 0))
q6 <- rbind(q6,subset(sf_2010,sf_2010$Closed.Roll.Assessed.Land.Value > 0))
q6 <- rbind(q6,subset(sf_2011,sf_2011$Closed.Roll.Assessed.Land.Value > 0))
q6 <- rbind(q6,subset(sf_2012,sf_2012$Closed.Roll.Assessed.Land.Value > 0))
q6 <- rbind(q6,subset(sf_2013,sf_2013$Closed.Roll.Assessed.Land.Value > 0))
q6 <- rbind(q6,subset(sf_2014,sf_2014$Closed.Roll.Assessed.Land.Value > 0))

q6$time <- q6$Closed.Roll.Fiscal.Year - 2007
q6$logP <- log10(q6$Closed.Roll.Fiscal.Year)

# q6a <- subset(sf_2007$Closed.Roll.Assessed.Land.Value,sf_2007$Closed.Roll.Assessed.Land.Value > 0)
# P0 <- mean(q6a)
# logP0 <- log10(P0)

fitq6 <- lm(logP ~ time, data = q6) 
print(paste("r = ",as.character(fitq6$coefficients[2])))

# Q7
sf_early <- sf_2014

t <- unique(sf_2013$Block.and.Lot.Number)
sf_early <- sf_early[!(sf_early$Block.and.Lot.Number %in% t), ]
sf_early <- rbind(sf_early,sf_2013)

t <- unique(sf_2012$Block.and.Lot.Number)
sf_early <- sf_early[!(sf_early$Block.and.Lot.Number %in% t), ]
sf_early <- rbind(sf_early,sf_2012)

t <- unique(sf_2011$Block.and.Lot.Number)
sf_early <- sf_early[!(sf_early$Block.and.Lot.Number %in% t), ]
sf_early <- rbind(sf_early,sf_2011)

t <- unique(sf_2010$Block.and.Lot.Number)
sf_early <- sf_early[!(sf_early$Block.and.Lot.Number %in% t), ]
sf_early <- rbind(sf_early,sf_2010)

t <- unique(sf_2009$Block.and.Lot.Number)
sf_early <- sf_early[!(sf_early$Block.and.Lot.Number %in% t), ]
sf_early <- rbind(sf_early,sf_2009)

t <- unique(sf_2008$Block.and.Lot.Number)
sf_early <- sf_early[!(sf_early$Block.and.Lot.Number %in% t), ]
sf_early <- rbind(sf_early,sf_2008)

t <- unique(sf_2007$Block.and.Lot.Number)
sf_early <- sf_early[!(sf_early$Block.and.Lot.Number %in% t), ]
sf_early <- rbind(sf_early,sf_2007)

a7_1 <- mean(subset(sf_early$Number.of.Units,sf_early$Year.Property.Built >= 1950 ))
a7_2 <- mean(subset(sf_early$Number.of.Units,sf_early$Year.Property.Built < 1950 ))  

print(paste("difference in units, (before 1950) - (after and equal 1950) = ",as.character(a7_2 - a7_1)))

# Q8
# Estimate how built-up each zip code is by comparing the total property area to the total lot area. 
# What is the largest ratio of property area to surface area of all zip codes?

q8 <- sf_final
a8_property <- tapply(q8$Property.Area.in.Square.Feet, q8$Zipcode.of.Parcel, mean)
a8_property <- as.numeric(a8_property)
a8_property <- subset(a8_property,!is.na(a8_property))
a8_lot <- tapply(q8$Lot.Area, q8$Zipcode.of.Parcel, mean)
a8_lot <- as.numeric(a8_lot)
a8_lot <- subset(a8_lot,!is.na(a8_lot))
a8_ratio <- a8_property/a8_lot
print(paste("largest ratio of property area to surface area",as.character(max(a8_ratio))))

