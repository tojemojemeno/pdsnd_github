library(tidyverse)
ny = read.csv('new-york-city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')

head(ny)
head(wash)
head(chi)

# question 1 --------------------------------------------------------------
#How many times were the most popular start stations used?


#dataframes for start stations and their counts in each city in decreasing order
t_wash=data.frame(count=sort(table(wash$Start.Station), decreasing =TRUE))
t_chi=data.frame(count=sort(table(chi$Start.Station), decreasing =TRUE))
t_ny=data.frame(count=sort(table(ny$Start.Station), decreasing =TRUE))

#top 10 start stations in each city
t1_wash=head(t_wash,10)
t1_chi=head(t_chi,10)
t1_ny=head(t_ny,10)

#visualization
ggplot(aes(x=count.Var1, y=count.Freq), data=t1_wash)+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x= element_text(angle=10))+
  labs(x="Stations", 
       y="Count",
       title="Top 10 start stations in Washington")
ggplot(aes(x=count.Var1, y=count.Freq), data= t1_chi)+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x= element_text(angle=10))+
  labs(x="Stations", 
       y="Count",
       title="Top 10 start stations in Chicago")
ggplot(aes(x=count.Var1, y=count.Freq), data=t1_ny)+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x= element_text(angle=10))+
  labs(x="Stations", 
       y="Count",
       title="Top 10 start stations in NYC")

#summary
summary(t1_wash)
summary(t1_chi)
summary(t1_ny)

#The top 10 start stations for Washington lie within the range of 823(Eastern Market Metro/Pennsylvania Ave & 7th St SE) to 1700(Columbus Circle/Union Station). For Chicago the range lies within 81(Franklin St & Monroe St)and 210(Streeter Dr & Grand Ave). For NYC it is 288(Greenwich Ave & 8 Ave) and 592(Pershing Square North).

# question 2 --------------------------------------------------------------
#What is the average year of birth for each gender?

# how many people of each gender are there?
table(chi$Gender)
table(ny$Gender)
#summary by gender
by(chi$Birth.Year, chi$Gender, summary)
by(ny$Birth.Year, ny$Gender, summary)

#visualization
ggplot(aes(y=Birth.Year), data= ny)+
  geom_boxplot()+
  facet_wrap(~Gender)+
  labs(title="Year of birth by gender NYC",
       y="Year of birth")
ggplot(aes(y=Birth.Year), data= chi)+
  geom_boxplot()+
  facet_wrap(~Gender)+
  labs(title="Year of birth by gender Chicago",
       y="Year of birth")

#The data is not symmetric, so median is the better way to count the average. In Chicago the median year of birth is 1986 for women and 1983 for men. In NYC 1982 is for women and 1980 for men. In Chicago, people who didn't choose their gender also didn't mention their year of birth (except for one observation). In NYC, however, there is a whole group of people with its own average of 1979, who didn't select the binary gender options, but mentioned their year of birth.

# question 3 --------------------------------------------------------------
#What is the most common day of week?

# converting date to weekday
ny$day =weekdays(as.Date(ny$Start.Time))
wash$day=weekdays(as.Date(wash$Start.Time))
chi$day=weekdays(as.Date(chi$Start.Time))

#creating dataframe with weekdays and frequency
ny_day=data.frame(table(ny$day))
wash_day=data.frame(table(wash$day))
chi_day=data.frame(table(chi$day))

#visualization
ny_day %>% 
  mutate(Var1=fct_relevel(Var1,"Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", 
                          "Friday", "Saturday")) %>% 
  ggplot(aes(x=Var1, y=Freq))+
  geom_col()+
  labs(x="Weekday",
       y="Count",
       title="The most common day of week NYC")
wash_day %>% 
  mutate(Var1=fct_relevel(Var1,"Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", 
                          "Friday", "Saturday")) %>% 
  ggplot(aes(x=Var1, y=Freq))+
  geom_col()+
  labs(x="Weekday",
       y="Count",
       title="The most common day of week Washington")
chi_day %>% 
  mutate(Var1=fct_relevel(Var1,"Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", 
                          "Friday", "Saturday")) %>% 
  ggplot(aes(x=Var1, y=Freq))+
  geom_col()+
  labs(x="Weekday",
       y="Count",
       title="The most common day of week Chicago")

ny_day
wash_day
chi_day

#The most common day of week is Wednesday for NYC and Washington, and Monday for Chicago.