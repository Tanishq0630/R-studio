44+56
c(21, 30, 24, 35, 26, 37, 25, 29, 25, 28, 22, 45, 23, 25, 28, 19, 23, 27, 35, 34, 37, 39)
age = c(21, 30, 24, 35, 26, 37, 25, 29, 25, 28, 22, 45, 23, 25, 28, 19, 23, 27, 35, 34, 37, 39)
age
class(age)
colors <- c('blue','yellow','red','green','green','red','blue','blue','yellow','yellow','green','yellow','green','green','green','yellow','red','green','red','green','blue','yellow') 
colors
class(colors)
above25 = age > 25
above25 
class(above25)
mixed <- c(1:5, "puppy", "kitty") 
mixed
class(mixed)
test = as.numeric(mixed)
test
class(as.numeric(mixed))
class(test)
class(as.numeric(mixed))
age 
colors
list=list(age, colors)
list
class(list)
help(sum)
?sum
sum(age, FALSE)
sum(... = age, na.rm = FALSE)
sum (age)
?t.test
table(colors)
col_freq = table(colors)
col_freq
pie(col_freq, # these are the frequencies for each slice of the pie
    main = "Pie Chart of Preferred Colors", # main allows us to add a title to our plot
    labels = c('blue', 'green', 'red', 'yellow') # labels allows us to add more descriptive names for each of our categories)
)
barplot(col_freq,
        main = "Bar Chart of Preferred Colors", 
        ylab = "Frequency", # ylab allows us to add a label to the y-axis
        names.arg = c('blue', 'green', 'red', 'yellow'), # names.arg allows us to change category names just like we used labels for the pie chart
        col = c(4, 3, 2, 7) # col allows us to change the colors of the bars
        # each number is associated with a different color
        # for example, 4 = blue and 3 = green 
        # you could also have done something like col = c(2:5)
)
col_height = t(height_col)
col_height; height_col
barplot(col_height, 
        col = c(4,3,2,7),
        main = 'Segmented Bar Graph of Preferred Color',
        ylab = 'Relative Frequency (%)',
        beside = FALSE,   # to get the segmented bar graph, we can just get rid of beside = TRUE, or just set it to FALSE (beside = TRUE is the default)
        legend.text = rownames(col_height),
        horiz = T,
        xlim = c(0, 1.6)
)
hist(age,  
     main = 'Histogram of Age',
     xlab = 'Age (in years)'
)
salary <- c(12.4, 41.3, 29.8, 62.8, 38.5, 79, 33.9, 30.5, 40, 29.6, 17.7, 109.1, 24.4, 33.3, 37.3, 20.1, 22.9, 30.4, 80.5, 56.5, 90.8, 101.7) 
plot(salary, age)
