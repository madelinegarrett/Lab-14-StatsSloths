# Lab-14-StatsSloths

---
title: "Lab 4: StatisticalSloths"
output: html_document
---
Madeline Garrett, Kevin Luth, Zandy Boone, Katie Stewart
```{r{r chunk_name, include = FALSE}
knitr::opts_chunk$set(echo = FALSE)
```

```{r chunk_name, include= FALSE}
library(tidyverse)
babies <- read_csv("https://raw.githubusercontent.com/ervance1/Sp2018-Data-Science-repo/master/babies2a.dat")
babies <- rename(babies, bwtoz = `bwt/oz`) #renaming `bwt/oz` because the "/" sign requires special care
babies <- na.omit(babies)
```

# Does Smoking Lead to Babies Being Born Prematurely? 
* This question is interesting becuase it can help to better inform mothers about the risk that smoking can have on their unborn children, specifically if smoking can lead to a premature birth which can be extremely dangerous for children. This is important because knowing this may help to advise mothers to help avoid giving birth to children early, something that can affect the baby's well-being. 

```{r}
ggplot(data = babies) + 
  geom_bar(mapping = aes(x = Premature, fill =as.factor(number) ), position = "fill")+
  ggtitle("Does Smoking Lead to Babies Being Born Prematurely?")+
  xlab("Not Premature (0)                        Premature (1)")+
  guides(fill=guide_legend(title="Cigarettes Smoked Per Day"))
  
  
```


### Conclusion
* This data shows that there are more babies being born prematurely when the mother smoked at least one cigarette a day. However when we ran the table of how many women smoked and babies being born prematurely we saw that only 4 more babies were born prematurely to women who smoked. This leads us to conclude that there is a minor correlation between smoking and babies being born prematurely 

### Recommendations
*  Our recommendation is to tell mothers that if they smoke that there is a risk that there is a chance that their child will be born prematurely. We also think that more data must be gathered in order to more accurately answer this question. However when it comes to the life of their child, most mothers would rather noe take risks which is why we think it would be wise to advise them of the possible dangers of smoking while pregnant. We also recognize that there are confounding variables such as smoking in the past and mothers age.


# Does Smoking Lead to Babies Being Born with Low Birth Weights?
* This question is interesting becuase it can help to better inform mothers about the risk that smoking can have on their unborn children, specifically if smoking can lead to low birth weights which can be very troubling for new born babies. This is important because knowing this may help to advise mothers to help avoid giving birth to children with dangerously low birth weights that could affect their health. 

```{r}
ggplot(data = babies) + 
  geom_smooth(mapping = aes(x=gestation, y=bwtoz, color=as.factor(smoke)), se=FALSE) + 
  xlab("Gestation Period Length") + 
  ylab("Birth Weight (oz)")+ 
  ggtitle("Does Smoking Lead to Babies Being Born with Low Birth Weights?")
```



### Conclusion
* The data shows that generally, the babies of mothers who smoke have lower birth weights than of those who do not for almost all gestational ages. This means that the data does support the Surgeon General's second assertion that newborns of mothers who smoke have smaller birth weights at every gestational age. 

### Recommendation
* Our recomendation is to inform mothers and soon-to-be mothers that if they make the decision to smoke during their pregnancy their baby is more likely to have a lower birth weight regardless of the gestation period that the mother and baby are in. It would be useful to collect more data and research why women who smoke tend to have babies with higher birth weights after the 300th day of gestation. Overall, mothers and expecting mothers should not smoke while they are pregnant in order to better their chances of having a healthy baby. There are potential confounding variables to these results such as the condition of the mother during the pregnancy and birth.

# Preliminary Question: Which variables are similar between smokers and nonsmokers? Which are different?
* Whether one smokes or not does not appear to have an effect on their marital status or the number of previous pregnancies (parity) they've had. There are similar numbers of both smokers and non smokers for the different values of these variables. When it comes to variables like being premature or birth weight, smoking does appear to affect their status. Our two graphs that answer the questions above show this. Babies of smokers tend to have lower birth weights and are premature more frequently. Another variable that smoking appears to affect is education level. For both males and females, there are more people with higher education levels among those who do not smoke than those who do not.
```{r}
ggplot(data = babies) + 
  geom_jitter(mapping = aes(x=smoke, y=marital, color = as.factor(smoke)))
 ggplot(data = babies) + 
  geom_jitter(mapping = aes(x=smoke, y=parity, color = as.factor(smoke)))
 ggplot(data = babies) + 
  geom_jitter(mapping = aes(x=smoke, y=ded, color = as.factor(smoke)))
 ggplot(data = babies) + 
  geom_jitter(mapping = aes(x=smoke, y=med, color = as.factor(smoke)))
```


# Individual Sub Questions 

Madeline's Plot

* Question: Does the education of the mother affect the  height of the mother?  
```{r}
ggplot(data = babies, mapping = aes(x= mht, fill = as.factor(med))) +
  geom_histogram(bins=15) +
  facet_grid(~med)+
  ggtitle("Does the education of the mother affect the height of the mother?")+
  xlab("Height of Mother")+
  guides(fill=guide_legend(title="Mother's Education"))

```


Madeline's Findings: Most mothers had only graduated from high school and nothing else. I also saw that there was a slighlty higher amount of tall children in people who were high school graduates and some college and also in college graduate.


Kevin's Plot

* Question: Does the number of previous pregnancies had by someone affect their baby's birth weight?
```{r}
ggplot(data = babies) + 
  geom_smooth(mapping = aes(x=parity, y=bwtoz), fill="green", size=2, level=0.85) + 
  xlab("# of All Previous Pregnancies") + 
  ylab("Birth Weight (oz)")
```


Kevin's Findings:

* The babies of those who have had a higher number of previous pregnancies tend to have lower birth weights.


Katie's Plot:

* Question: Does the mother's pre-pregnancy weight affect whether her baby will be born prematurely?
```{r}
ggplot(data = babies)+
  geom_point(mapping = aes(x = as.factor(Premature), y = mpregwt), color = "blue")+
  xlab("Premature")+
  ylab("Mother's Pre-Pregnancy Weight")
```


Katie's Findings:

* A mother's pre-pregnancy weight does not seem to directly effect whether or not her baby will be born prematurely. However, by looking at the graph you can see that mother's who gave birth to premature babies were typically less than 150 pounds.


Zandy's Plot:

* Question: Does a dad's weight impact the weight of the weight of the newborn? 
```{r}
ggplot(data = babies) +
  geom_jitter(mapping = aes(x = dwt, y = bwtoz), color = "green") +
  geom_smooth(mapping = aes(x = dwt, y = bwtoz), color = "blue")
```
Zandy's Findings:

* A  dad's weight does not seem to directly affect the birth weight of his baby with only a weak positive correlation of 0.14. The data also shows though, that most babies are born with the dad being about 150-200 lbs. and that those babies have overall higher birth weights mostly due to more data being concentrated in that weight range of 150-200 lbs.


# Team Summary:

* I, Kevin Luth, created a plot showing the relationship between the number of previous pregnancies (parity) and birth weights. The data showed that child birth weights start to decrease as you get into higher numbers of previous pregnancies. I used the geom_smooth function and changed the color and width of the standard error interval to make it stand out on the gray background and to not have it overshadow the actual line. In addition, I changed the thickness of the line to make it more prominent on the page and in the error interval. I changed the x and y labels as well to more clearly display what they represent.

* I, Katie Stewart, created a plot that shows the relationship between a mother's pre-pregnancy weight and whether her baby was born prematurely. The data showed that mothers who gave birth to premature babies typically had pre-pregnancy weights below 150 pounds. I used the geom_point function and changed the color of the plotted points. I also changed the labels on the x and y axis to make it easier to understand. 

* I, Zandy Boone, created a plot showing the relationship between a dad's weight in pounds and his baby's birth weight in ounces. The data showed that there was a very weak correlation between the weigh of the dad and the weight of the baby with a correlation of 0.14. The data also shows though, that most babies are born with the dad being about 150-200 lbs. and that those babies have overall higher birth weights mostly due to more data being concentrated in that weight range of 150-200 lbs. I used a geom_ jitter function and a geom_smooth function with the dad's weight in pounds,  on the x-axis and the birth weights of the babies in ounces, on the y-axis for both graphs. I changed the colors of the line and the color of the points to make the line and points stand out more and to give the graph a more professional look.

*  I, Madeline Garrett, worked with Zandy to create a plot that answered the question of whether smoking affected premature birth. We created a bar plot using the geom_bar. I also made a graph of  mothers education versus mothers height. Most mothers had only graduated from high school and nothing else. I also saw that there was a slighlty higher amount of tall children in people who were high school graduates and some college and also in college graduate. I  think  this is interesting because it has been seen in other studies that more CEOs tend to be taller in height. I  thought that this related to this study well.
