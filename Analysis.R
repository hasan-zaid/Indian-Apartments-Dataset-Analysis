# Hasan Akram Abdullah Zaid
# TP066635

# Load necessary libraries
install.packages(c("Rfast", "ggplot2", "tidyr", "plotrix", "dplyr", "ggpubr"))
packages = c("ggplot2", "tidyr", "dplyr", "Rfast", "plotrix", "ggpubr")
lapply(packages, require, character.only = TRUE)


# Read the file
home_rent = read.table("C:\\Users\\hasan\\Documents\\Assignments\\PFDA\\House_Rent_Dataset.csv",
                       header = TRUE, sep = ",")


# Data transformation
# This converts the datatype of the column from character to Date
date_posted = as.Date(home_rent$Posted.On, "%m/%d/%Y")

# Removing the old column to be replaced with the new one that has the correct datatype
home_rent$Posted.On = NULL
homes = cbind(date_posted, home_rent)



# Data Cleaning
# Remove the missing values
houses = drop_na(homes)
floors = strsplit(houses$Floor, split = " out of ")
floor.level = c()
max.floor = c()
{
  for (i in 1:length(floors))
  {
    if(floors[[i]][1] == "Ground")
    {
      floor.level = append(floor.level, "0")
      
    }
    else if(floors[[i]][1] == "Upper Basement")
    {
      floor.level = append(floor.level, "-1")
      
    }
    else if(floors[[i]][1] == "Lower Basement")
    {
      floor.level = append(floor.level, "-2")
    }
    else
    {
      floor.level = append(floor.level, floors[[i]][1])
    }
    
    max.floor = append(max.floor, floors[[i]][2])
  }
}

houses$Floor = NULL
semi.final = cbind(as.integer(floor.level), houses)
Final_Data = cbind(as.integer(max.floor), semi.final)

# Renames the headers of the dataframe
names(Final_Data) = c("Max_Floor", "Floor_Level","Date_Posted", "Bedrooms", "Rent", "Size", "Area_Type",
                     "District", "City", "Furnishing_Status", "Prefered_Tenant", "Bathrooms", "Point_of_Contact")



# Data Exploration
summary(Final_Data)

# Meta data bout the categorical fields
factor(Final_Data$Area_Type)
factor(Final_Data$District)
factor(Final_Data$City)
factor(Final_Data$Furnishing_Status)
factor(Final_Data$Prefered_Tenant)
factor(Final_Data$Point_of_Contact)

# Meta data about continous fields
summary(Final_Data$Max_Floor)
summary(Final_Data$Floor_Level)
summary(Final_Data$Date_Posted)
summary(Final_Data$Bedrooms)
summary(Final_Data$Rent)
summary(Final_Data$Size)
summary(Final_Data$Bathrooms)


str(Final_Data)

# Viewing the data as one piece
View(Final_Data)
head(Final_Data)
tail(Final_Data)



# Questoin 1

# Analysis 1:1
# grouping the number of houses for each tenant type preferred
tenant_types = Final_Data%>%group_by(Prefered_Tenant)%>%
  summarise(Available_Houses = length(Prefered_Tenant))

tenant_types

pie(tenant_types$Available_Houses, tenant_types$Prefered_Tenant, radius = 1,
    col = c("Green", "Blue", "Orange"),
    clockwise = TRUE, main = "Houses Availability per Tenant Type")



# Analysis 1:2
# creates a data.frame for excluding irrelevant data to bachelors 
bachelors = Final_Data[Final_Data$Prefered_Tenant == "Bachelors", ]

ggplot(bachelors, aes(x=Bedrooms))+ geom_histogram(colour="White",aes(fill=..count..)) + 
  scale_fill_gradient("count", low = "green", high= "red") +
  labs(title='Number of Bedrooms Preferred by Bachelors', x='Number of Bedrooms',y='Number of Preferences') +
  scale_x_continuous(breaks= c(1:6))



# Analysis 1:3
Average_Rent_per_City = bachelors%>%group_by(City)%>%summarise(Avg_rent=mean(Rent))

Average_Rent_per_City

ggplot(Average_Rent_per_City, aes(City, Avg_rent, fill = Avg_rent)) +
  geom_bar(stat='Identity') +
  labs(title='Average Rent by City', x='City',y='Average Rent')



# Analysis 1:4
most.preferred.city = bachelors%>%group_by(City)%>%
  summarise(tenants = length(Prefered_Tenant))

most.preferred.city

ggplot(most.preferred.city, aes(x = tenants, y = City, color = factor(City))) + geom_boxplot() +
  labs(title='Preferred Cities By Bachelors', x='Number of Bachelors',y='City')



# Analysis 1:5
preferred.type.of.contact = bachelors%>%group_by(Point_of_Contact)%>%
  summarise(Count_Points=length(Point_of_Contact))

preferred.type.of.contact

ggplot(preferred.type.of.contact, aes(Point_of_Contact, Count_Points)) +
  geom_bar(stat='Identity', color="blue",fill= "#ADD8E6") +
  labs(title='Preferred Point of Contact By Bachelors', x='Point of Contact',y='Points Count')



# Analysis 1:6
furnishing_preferance_by_city = bachelors%>%group_by(City, Furnishing_Status)%>%
  summarise(Furnishing = length(Furnishing_Status))

furnishing_preferance_by_city

ggplot(furnishing_preferance_by_city, aes(x = " ", y = Furnishing, group = Furnishing_Status, fill = factor(Furnishing_Status))) +
  geom_bar(width = 1, stat = "Identity") +
  coord_polar("y", start = 0) +
  facet_grid(.~ City) + theme_void() +
  labs(title = "Most Preferred Furnishing Status in Each City")



# Analysis 1:7
preferred.area.type.per.city = bachelors%>%group_by(City, Area_Type)%>%
  summarise(area.types=length(Area_Type))

preferred.area.type.per.city

ggplot(preferred.area.type.per.city, aes(x=area.types, y=Area_Type, color = factor(Area_Type))) +
  geom_point() + 
  labs(title='Preferred Area Type by Bachelors in All Cities', x='Point of Contact',y='Points Count')

preferred.area.type.per.city%>%
  mutate(city.area = paste(City, Area_Type))%>%
  ggplot(aes(x = area.types, y = city.area, color = factor(Area_Type), fill = factor(Area_Type))) +
  geom_bar(stat='Identity') +
  labs(title='Preferred Area Type per City', x='Number of Available Houses',y='City/Area Type')



# Analysis 1:8
districts_count = bachelors%>%group_by(City, District)%>%
  summarise(highest_district=length(District))%>%top_n(3)

districts_count


ggplot(districts_count, aes(x = " ", y = highest_district, group = District, fill = factor(District))) +
  geom_bar(width = 1, stat = "Identity") +
  coord_polar("y", start = 0) +
  facet_grid(.~ City) + theme_void() +
  labs(title = "Most Preferred District in Each City")



# Analysis 1:9
size_per_bedroom = bachelors%>%group_by(Bedrooms)%>%summarise(Avg_size=mean(Size))

size_per_bedroom

ggplot(bachelors, aes(x= Bedrooms, y= Size)) + geom_point(aes(shape = factor(Bedrooms), color = factor(Bedrooms)))+ 
  facet_wrap(~Bedrooms) +
  labs(title='Preferred Size per Number of Bedrooms', x='Number of Bedrooms',y='Size') +
  scale_x_continuous(breaks= c(1:6))

plot(x = size_per_bedroom$Bedrooms, xlab = "Bedrooms",
     y = size_per_bedroom$Avg_size, ylab = "Average Size",
     type = "o", main = "Average House Size per Bedroom Count", col = "Brown")


# Analysis 1:10
preferrd.interior.per.floor.level = bachelors%>%mutate(interior = paste(Area_Type, Furnishing_Status))%>%
  group_by(Floor_Level, interior)%>%
  summarise(Available_Units= length(interior))%>%top_n(1)

View(preferrd.interior.per.floor.level)

bachelors%>%
  mutate(interior = paste(Area_Type, Furnishing_Status))%>%
  ggplot(aes(x = Floor_Level, y = interior)) +
  geom_point(aes(shape = factor(interior))) +
  labs(title='Preferred House Interior per Floor Level', x='Floor Level',y='House Interior') +
  scale_x_continuous(breaks= c(-2,0,5,10,15,20,25,30,35,40,45,50,55,60))



# Analysis 1:11
preferred.bathrooms.per.bedroom = bachelors%>%group_by(Bedrooms)%>%
  summarise(Avg_Bathrooms = round(mean(Bathrooms)))

preferred.bathrooms.per.bedroom

ggplot(preferred.bathrooms.per.bedroom, aes(x= Bedrooms, y= Avg_Bathrooms)) + 
  geom_point(aes(color=factor(Bedrooms))) +
  scale_x_continuous(breaks= c(1:6)) +
  labs(title='Preferred Number of Bathrooms per Bedroom Count', y='Average Number of Bathrooms')



# Analysis 1:12
summary(bachelors$Date_Posted)

ggplot(bachelors, aes(x = Date_Posted)) +
  geom_histogram(colour="Brown", binwidth=0.2) +
  labs(title='Dates of Posts About Houses Preferred by Bachelors', x='Date of Posting',y='Number of Postings') +
  scale_x_date(breaks = as.Date(c("2022-04-1", "2022-04-1", "2022-04-30", "2022-05-10",
                          "2022-05-20", "2022-05-30", "2022-05-30", "2022-06-10",
                          "2022-06-20", "2022-06-30", "2022-07-10")))



# Question 2

# Analysis 2:1
average.rent = Final_Data%>%group_by(City)%>%summarise(Avg_Rent = mean(Rent))

average.rent

ggplot(average.rent, aes(City, Avg_Rent, color = factor(City))) +
  geom_boxplot() +
  labs(title = "Average House Rent per City", y = "Average Rent")



# Analysis 2:2
average.rent.per.furnish = Final_Data%>%group_by(Furnishing_Status)%>%
  summarise(Average = mean(Rent))

average.rent.per.furnish

ggplot(average.rent.per.furnish, aes(Furnishing_Status, Average, color = factor(Furnishing_Status),
                                     fill = factor(Furnishing_Status))) +
  geom_bar(stat='Identity') +
  labs(title = "Averate Rent per Furnishing Status", x = "Furnishing Status", y = "Average Rent")



# Analysis 2:3
max.rent.per.floor = Final_Data%>%group_by(Floor_Level)%>%
  summarise(Max = max(Rent))%>%arrange(Max)

View(max.rent.per.floor)


ggplot(max.rent.per.floor, aes(Floor_Level, Max, color = "Brown")) +
  geom_bar(stat = "Identity")



# Analysis 2:4
# For Mumbai
# A data frame to filter out all the variables that can affect the accuracy of the analysis
filtered = Final_Data[(Final_Data$Furnishing_Status == "Semi-Furnished") &
                        (Final_Data$Bedrooms == 2) & (Final_Data$City == "Mumbai") & (Final_Data$Bathrooms == 2), ]

average_rent_per_date = filtered%>%group_by(Date_Posted)%>%
  summarise(Avg_Rent = mean(Rent))

View(average_rent_per_date)

ggplot(average_rent_per_date, aes(Date_Posted, Avg_Rent)) +
  geom_line(color = '#0c7091') +
  labs(title = "Rent Changes Through Time in Mumbai", x = "Date Posted", y = "Average Rent")

# For other cities
filtered_2 = Final_Data[(Final_Data$Furnishing_Status == "Semi-Furnished") &
                        (Final_Data$Bedrooms == 2) & (Final_Data$City != "B") & (Final_Data$Bathrooms == 2), ]

average_rent_per_date_2 = filtered_2%>%group_by(Date_Posted)%>%
  summarise(Avg_Rent = mean(Rent))

View(average_rent_per_date_2)

ggplot(average_rent_per_date_2, aes(Date_Posted, Avg_Rent)) +
  geom_line(color = '#0c7091') +
  labs(title = "Rent Changes Through Time in Other Cities", x = "Date Posted", y = "Average Rent")



# Analysis 2:5
average.rent.per.district = Final_Data%>%group_by(City, District)%>%
  summarise(Avg_Rental = mean(Rent))%>%top_n(3)

average.rent.per.district

ggplot(average.rent.per.district, aes(District, Avg_Rental, color = factor(City), fill = factor(City), group = City)) +
  geom_bar(stat = "Identity") + facet_wrap(~District) +
  labs(title = "Most Expensive Districts in Each City", y = "Average Rent") +
  scale_x_discrete(labels = c())



# Analysis 2:6
avg.rent.per.contact = filtered_2%>%group_by(Point_of_Contact)%>%
  summarise(Avg = mean(Rent))

avg.rent.per.contact

ggplot(avg.rent.per.contact, aes(Point_of_Contact, Avg, color = factor(Point_of_Contact), fill = factor(Point_of_Contact))) +
  geom_bar(stat = "Identity") +
  labs(title = "Average Rent based on Point of Contact", x = "Point of Contact", y = "Average Rent")
  



# Analysis 2:7
# this variable holds the filtered data for more accurate analysis about rent
filtered = Final_Data[(Final_Data$Furnishing_Status == "Semi-Furnished") &
                        (Final_Data$Bedrooms == 2) & (Final_Data$Bathrooms == 2), ]

avg.rent.per.max.floor = filtered%>%group_by(Max_Floor)%>%
  summarise(average = mean(Rent))

View(avg.rent.per.max.floor)

ggplot(avg.rent.per.max.floor, aes(Max_Floor, average)) +
  geom_line(col = "Blue") +
  labs(title = "Average Rent per Building Height in India", x = "Max Floor", y = "Average Rent")



# Analysis 2:8
filtered%>%group_by(Prefered_Tenant)%>%summarise(avg = mean(Rent))
  
ggplot(filtered, aes(Prefered_Tenant, Rent, color = "#451452")) +
  geom_boxplot() + labs(title = "Average Rent per Tenant Type", x = "Preferred Tenant")



# Analysis 2:9
Final_Data%>%group_by(Area_Type)%>%summarise(avg_size = mean(Size), avg_Rent = mean(Rent))

ggplot(Final_Data, aes(x = Size, y = Rent, group = Area_Type, color = factor(Area_Type))) +
  geom_line(size = 1) +
  labs(title='Average Rent per House Interior', x='Size',y='Average Rent')



# Analysis 2:10
filtered%>%group_by(City, Size)%>%summarise(avg = mean(Rent))%>%top_n(3)

ggplot(filtered, aes(Size, Rent, group = City, color = factor(City))) +
  geom_line() + facet_wrap(~City) +
  labs(title = "Price per Square Feet in Each City")




# Question 3

# Analysis 3:1
bedroom.preference.by.tenant = Final_Data%>%group_by(Prefered_Tenant)%>%
  summarise(avg = mean(Bedrooms))

bedroom.preference.by.tenant

ggplot(bedroom.preference.by.tenant, aes(Prefered_Tenant, avg, color = factor(Prefered_Tenant), fill = factor(Prefered_Tenant))) +
  geom_bar(stat = "Identity") + facet_wrap(~Prefered_Tenant) +
  labs(title = "Bedrooms Preferance per Tenant Type", y = "Average Number of Rooms", x = "Preferred Tenant")




# Analysis 3:2
floor.bedroom.preference.by.tenant = Final_Data%>%group_by(Prefered_Tenant)%>%
  summarise(avg_floor = mean(Floor_Level), avg_bedroom = mean(Bedrooms))

floor.bedroom.preference.by.tenant

ggplot(floor.bedroom.preference.by.tenant, aes(avg_bedroom, avg_floor, group = Prefered_Tenant, color = factor(Prefered_Tenant),
                                               fill = factor(Prefered_Tenant))) + geom_bar(stat = "Identity") +
  labs(title = "Bedrooms to Floor Level Preferance Ratio per Tenant Type", y = "Average Floor Level", x = "Average Number Bedrooms")



# Analysis 3:3
size.to.bedroom.avg.ratio = Final_Data%>%group_by(Bedrooms)%>%
  summarise(avg.size = mean(Size))

size.to.bedroom.avg.ratio

ggplot(Final_Data, aes(Bedrooms, Size, group = City, color = factor(City))) +
  geom_line() + facet_wrap(~City) +
  labs(title = "Size to Number of Bedrooms Ratio in Each City")



# Analysis 3:4
contact.preferance = Final_Data%>%group_by(Prefered_Tenant, Point_of_Contact)%>%
  summarise(avg.contact = length(Point_of_Contact))

contact.preferance

ggplot(Final_Data, aes(Point_of_Contact, Prefered_Tenant, color = factor(Prefered_Tenant), fill = factor(Prefered_Tenant))) + 
  geom_histogram(stat = "Identity") + facet_wrap(~Prefered_Tenant) +
  scale_y_discrete(labels = c()) +
  labs(title = "Most Preferred Point of Contact per Tenant Type", y = "Preferences Count")



# Analysis 3:5
bathroom.per.bedroom = Final_Data%>%group_by(Bedrooms)%>%summarise(avg = mean(Bathrooms))

bathroom.per.bedroom


plot(bathroom.per.bedroom$avg, bathroom.per.bedroom$Bedrooms, type = "o", col = "Blue",
     xlab = "Average Number of Bathrooms", ylab = "Number of Bedrooms", main = "Average Number of Bathrooms per Number of Bedrooms")



# Analysis 3:6
size.to.area.type =  Final_Data%>%group_by(Area_Type)%>%
  summarise(common.size = mean(Size), smallest.size = min(Size), largest.size = max(Size))

size.to.area.type

ggplot(size.to.area.type, aes(common.size, largest.size, fill = factor(Area_Type))) +
  geom_bar(stat = "Identity") +
  labs(title = "Average and Largest Size Based on Area Type", x = "Average Size", y = "Largest Available Size")



# Analysis 3:7
size.to.furnishing =  Final_Data%>%group_by(Furnishing_Status)%>%
  summarise(common.size = mean(Size), smallest.size = min(Size), largest.size = max(Size))

size.to.furnishing

ggplot(size.to.furnishing, aes(common.size, largest.size, fill = factor(Furnishing_Status))) +
  geom_bar(stat = "Identity") +
  labs(title = "Average and Largest Size Based on Furnishing Status", x = "Average Size", y = "Largest Available Size")



# Analysis 3:8
Final_Data%>%group_by(Point_of_Contact)%>%
  summarise(avg.size = mean(Size), avg.rent = mean(Rent))

ggplot(Final_Data, aes(Size, Rent, color = factor(Point_of_Contact))) + geom_boxplot() +
  labs(title = "Rent to Size Ratio Based on Point of Contact")



# Analysis 3:9
Final_Data%>%group_by(City, Point_of_Contact)%>%
  summarise(contact = length(Point_of_Contact))

ggplot(Final_Data, aes(Point_of_Contact, fill = factor(City))) + geom_histogram(stat = "Count") +
  facet_wrap(~City) +
  labs(title = "Most Preferred Point of Contact in Each City", x = "Point of Contact", y = "Count")



# Analysis 3:10
rent.per.tenant.type = Final_Data%>%group_by(Prefered_Tenant, City)%>%
  summarise(avg.rent = mean(Rent))

rent.per.tenant.type

ggplot(rent.per.tenant.type, aes(City, avg.rent, color = factor(Prefered_Tenant))) +
  geom_point(stat = "identity", aes(shape = factor(Prefered_Tenant))) +
  labs(title = "Average Rent per Tenant Type in Each City", y = "Average Rent")




# Question 4

# Analysis 4:1
most.common.bathroom.count = Final_Data%>%group_by(City, Bathrooms)%>%
  summarise(common = length(Bathrooms))%>%top_n(3)

most.common.bathroom.count

ggplot(Final_Data, aes(Bathrooms, fill = factor(City))) + geom_bar() + facet_wrap(~City) +
  labs(title = "Most Common Number of Bathrooms in Each City")



# Analysis 4:2
most.common.tenant.type = Final_Data%>%group_by(City, Prefered_Tenant)%>%
  summarise(common = length(Prefered_Tenant))

most.common.tenant.type
`
ggplot(most.common.tenant.type, aes(x = " ", y = common, group = Prefered_Tenant, fill = factor(Prefered_Tenant))) +
  geom_bar(width = 1, stat = "Identity") +
  coord_polar("y", start = 0) +
  facet_grid(.~ City) + theme_void() +
  labs(title = "Most Common Tenant Type in Each City")`



# Analysis 4:3
most.common.furnishing = Final_Data%>%group_by(City, Furnishing_Status)%>%
  summarise(common = length(Furnishing_Status))

most.common.furnishing

ggplot(most.common.furnishing, aes(x = " ", y = common, group = Furnishing_Status, fill = factor(Furnishing_Status))) +
  geom_bar(width = 1, stat = "Identity") +
  coord_polar("y", start = 0) +
  facet_grid(.~ City) + theme_void() +
  labs(title = "Most Common Furnishing Status in Each City")



# Analysis 4:4
most.common.size = Final_Data%>%group_by(City, Size)%>%
  summarise(common = length(Size))%>%top_n(3)

most.common.size

ggplot(most.common.size, aes(x = Size, y = common, group = City, fill = factor(City))) +
  geom_bar(stat = "Identity") +
  facet_grid(.~ City) +
  labs(title = "Most Common Size in Each City", y = "Commonality")



# Analysis 4:5
most.common.district = Final_Data%>%group_by(City, District)%>%
  summarise(common = length(District))%>%top_n(2)

most.common.district

ggplot(most.common.district, aes(x = " ", y = common, group = District, fill = factor(District))) +
  geom_bar(width = 1, stat = "Identity") +
  coord_polar("y", start = 0) +
  facet_grid(.~ City) + theme_void() +
  labs(title = "Most Common District in Each City")



# Analysis 4:6
most.common.area = Final_Data%>%group_by(City, Area_Type)%>%
  summarise(common = length(Area_Type))

most.common.area

ggplot(most.common.area, aes(x = " ", y = common, group = Area_Type, fill = factor(Area_Type))) +
  geom_bar(width = 1, stat = "Identity") +
  coord_polar("y", start = 0) +
  facet_grid(.~ City) + theme_void() +
  labs(title = "Most Common Area Type in Each City")



# Analysis 4:7
most.common.bedroom.count = Final_Data%>%group_by(City, Bedrooms)%>%
  summarise(common = length(Bedrooms))%>%top_n(3)

most.common.bedroom.count

ggplot(Final_Data, aes(Bedrooms, fill = factor(City))) + geom_bar() + facet_wrap(~City) +
  labs(title = "Most Common Number of Bedrooms in Each City")



# Analysis 4:8
most.common.date = Final_Data%>%group_by(City, Date_Posted)%>%
  summarise(common = length(Date_Posted))%>%top_n(3)

most.common.date

ggplot(most.common.date, aes(x = Date_Posted, y = common, group = City, fill = factor(City))) +
  geom_bar(stat = "Identity") +
  facet_grid(.~ City) +
  labs(title = "Most Common Time for New House Offers in Each City", y = "Commonality")



# Analysis 4:9
most.common.floor = Final_Data%>%group_by(City, Floor_Level)%>%
  summarise(common = length(Floor_Level))%>%top_n(3)

most.common.floor

ggplot(most.common.floor, aes(x = Floor_Level, y = common, group = City, fill = factor(City))) +
  geom_bar(stat = "Identity") +
  facet_grid(.~ City) +
  labs(title = "Most Common Floor Level in Each City", y = "Commonality")








