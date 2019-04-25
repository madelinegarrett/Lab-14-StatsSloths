# Lab-14-StatsSloths

---
title: "Lab 4: StatisticalSloths"
output: html_document
---
Madeline Garrett, Kevin Luth, Zandy Boone, Katie Stewart

```{r chunk_name, include= FALSE}
library(tidyverse)
babies <- read_csv("https://raw.githubusercontent.com/ervance1/Sp2018-Data-Science-repo/master/babies2a.dat")
babies <- rename(babies, bwtoz = `bwt/oz`) #renaming `bwt/oz` because the "/" sign requires special care
babies <- na.omit(babies)
?babies

```

# Does Smoking Lead to Babies Being Born Prematurely?
* This question is interesting becuase it can help to better inform mothers about the risk that smoking can have on their unborn children, specifically if smoking can lead to a premature birth which can be extremely dangerous for children. This is important because knowing this may help to advise mothers to help avoid giving birth to children early, something that can affect the baby's well-being. 


```{r}
momSmoke <- babies %>%
  filter(smoke == 1)
mean(momSmoke[["Premature"]])

nonSmoke <- babies %>%
  filter(smoke == 0)
mean(nonSmoke[["Premature"]])

0.2211982-0.1527778
# Mean difference is 0.0684204
```


```{r, include = FALSE}
perm_mean <- function(perms = 1000, values, n1)
{
  ## Variables ##
  # perms: The number of permutations 
  # values (num): 
  # n1 (int): Size of group 1
  ###############
  
  # Step 1:
  # Create vector of zeroes of length "perms" to store
  # permuted mean differnces
  means <- vector(mode = "double", length = perms)
  
  # Loop throught number of permutations
  for (i in c(1:perms))
  {
    # Step 2
    # Shuffle them using smample
    # Randomly separate vector "values" into disjoint 
    # groups of size "n1" and "length(values) - n1" respectively
    # group_one <- sample(values, n1)
    # group_two <- sample(values, length(values)-n1)
    sampled <- sample(values)
    group_one <- sampled[1:n1]
    group_two <- sampled[n1:length(values)]
    
    # Step 3:
    # Compute the sample means for the two groups from
    g1mean <- mean(group_one)
    g2mean <- mean(group_two)
    
    # Step 4: 
    # Compute the difference in sample means, store the
    # value in the vector from step 1
    diff <- g1mean - g2mean
    means[i] <- diff
  }
  # Step 5:
  # Return new updated vector, created in step 1
  means
}
```


```{r}
preBabies <- babies %>%
  filter(smoke == "1" | smoke == "0")
mean_vals <- perm_mean(1000, babies$Premature, 240)
mean_data <- tibble(mean_vals)
```

```{r}
ggplot(data = mean_data) +
  geom_histogram(mapping = aes(x = mean_vals), color = "lightblue", binwidth = .01)+ 
   geom_vline(xintercept =0.0684204, col=c("darkblue")) +
  ggtitle("Distribution of Mean Differences For Moms that Smoked vs Non Smokers")
```

### Conclusion
* This data shows that there are more babies being born prematurely when the mother smoked at least one cigarette a day. However when we ran the table of how many women smoked and babies being born prematurely we saw that only 4 more babies were born prematurely to women who smoked. This leads us to conclude that there is a minor correlation between smoking and babies being born prematurely 

### Recommendations
*  Our recommendation is to tell mothers that if they smoke that there is a risk that there is a chance that their child will be born prematurely. We also think that more data must be gathered in order to more accurately answer this question. However when it comes to the life of their child, most mothers would rather noe take risks which is why we think it would be wise to advise them of the possible dangers of smoking while pregnant. We also recognize that there are confounding variables such as smoking in the past and mothers age.



# Team Summary:

* I, Kevin Luth, created a plot showing the relationship between the number of previous pregnancies (parity) and birth weights. The data showed that child birth weights start to decrease as you get into higher numbers of previous pregnancies. I used the geom_smooth function and changed the color and width of the standard error interval to make it stand out on the gray background and to not have it overshadow the actual line. In addition, I changed the thickness of the line to make it more prominent on the page and in the error interval. I changed the x and y labels as well to more clearly display what they represent.

* I, Katie Stewart, created a plot that shows the relationship between a mother's pre-pregnancy weight and whether her baby was born prematurely. The data showed that mothers who gave birth to premature babies typically had pre-pregnancy weights below 150 pounds. I used the geom_point function and changed the color of the plotted points. I also changed the labels on the x and y axis to make it easier to understand. 

* I, Zandy Boone, created a plot showing the relationship between a dad's weight in pounds and his baby's birth weight in ounces. The data showed that there was a very weak correlation between the weigh of the dad and the weight of the baby with a correlation of 0.14. The data also shows though, that most babies are born with the dad being about 150-200 lbs. and that those babies have overall higher birth weights mostly due to more data being concentrated in that weight range of 150-200 lbs. I used a geom_ jitter function and a geom_smooth function with the dad's weight in pounds,  on the x-axis and the birth weights of the babies in ounces, on the y-axis for both graphs. I changed the colors of the line and the color of the points to make the line and points stand out more and to give the graph a more professional look.

*  I, Madeline Garrett, worked with Zandy to create a plot that answered the question of whether smoking affected premature birth. We created a bar plot using the geom_bar. I also made a graph of  mothers education versus mothers height. Most mothers had only graduated from high school and nothing else. I also saw that there was a slighlty higher amount of tall children in people who were high school graduates and some college and also in college graduate. I  think  this is interesting because it has been seen in other studies that more CEOs tend to be taller in height. I  thought that this related to this study well.
