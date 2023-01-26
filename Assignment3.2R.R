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

#Q3.2.R.2
install.packages("RColorBrewer")


make_art <- function(seed = 123){
  library(ggplot2)
  library(RColorBrewer)
  library(patchwork)
  set.seed(seed)
  x1 <- rnorm(50, 20, 10)
  y1 <- rnorm(50, 20, 10) 
  xend1 <- x1 + rnorm(50, 20, 10)
  yend1 <- y1 + rnorm(50, 20, 10)
  size1 <- rnorm(10, 5, 10) 
  color1 <- runif(50)
  data1 <- data.frame(x1, y1, xend1, yend1, color1, size1)
  pcol <- c("darkblue", "blue", "lightblue", "purple", "pink")
  rseq <- seq(1:3)
  rnum <- sample(rseq, 1)
  artplot1 <-
    ggplot(data = data1, aes(
      x = x1,
      y = y1,
      xend = xend1,
      yend = yend1,
      size = size1, 
      colour = color1,
    )) +
    geom_segment(show.legend = FALSE) +
    coord_polar() +
    scale_y_continuous(expand = c(0, -10)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_size(range = c(1, 5)) +
    scale_color_gradient(low=sample(pcol, size = 1, replace = FALSE),
                         high=sample(pcol, size = 1, replace = FALSE))+
    theme_void()
  artplot2 <-
    ggplot(data = data1, aes(
      x = x1,
      y = y1,
      xend = xend1,
      yend = yend1,
      size = size1, 
      colour = color1,
    )) +
    geom_segment(show.legend = FALSE) +
    coord_polar() +
    scale_y_continuous(expand = c(0, -10)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_size(range = c(1, 5)) +
    scale_color_gradient(low=sample(pcol, size = 1, replace = FALSE),
                         high=sample(pcol, size = 1, replace = FALSE))+
    theme_void()
  artplot3 <-
    ggplot(data = data1, aes(
      x = x1,
      y = y1,
      xend = xend1,
      yend = yend1,
      size = size1, 
      colour = color1,
    )) +
    geom_segment(show.legend = FALSE) +
    coord_polar() +
    scale_y_continuous(expand = c(0, -10)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_size(range = c(1, 5)) +
    scale_color_gradient(low=sample(pcol, size = 1, replace = FALSE),
                         high=sample(pcol, size = 1, replace = FALSE))+
    theme_void()
  if(rnum == 1){
    (artplot1 | artplot2 | artplot3) /
      artplot2
  }
  if(rnum == 2){
    artplot1 /
      (artplot2 | artplot3) /
      artplot1
  } else {
    artplot2 /
      (artplot1 | artplot2 | artplot3) 
  }
}
  
make_art(1)


