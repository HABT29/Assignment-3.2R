# Assignment 3.2R.1
# Henry Triebel 

#Q3.2.R.1

remind_me <- function(){
  return("buy soymilk, eggs, tofu, oats and blueberrys")
}

remind_me()

cheat <- function(exercise){
  if(exercise == "1"){
    print("grades <- c()
while (length(grades) < 63) {
  samplegrades <- rnorm(1, 7.5, sd = 1)
  if (samplegrades > 0 & samplegrades < 10) {
    grades <- c(grades, samplegrades)
  }
}
x11()
hist(grades)")
    
  }
  if(exercise == "2"){
    print("schipdata <-
  read.table(
    'C:/Users/henry/OneDrive/Desktop/Programming/week3/schipoldata.txt',
    header = TRUE,
    sep = ','
  )
schipdata$TMAX

plot(schipdata$DATE, schipdata$TMAX)")
    
  }
  if(exercise == "3"){
    print("install.packages('titanic')
library(titanic)

survdata <- titanic_train

sex <- factor(survdata$Sex)
survival <- factor(survdata$Survived)
df <- data.frame(sex, survival)


x11()
ggplot(df, aes(fill = survival, x = sex, y = frequency(sex))) +
  geom_bar(position = 'stack', stat = 'identity') +
  labs(y = 'count', x = 'Sex', fill = 'How did it go?') +
  scale_fill_discrete(labels = c('dead', 'alive'))")
    
  }
}

cheat("3")
