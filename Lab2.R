#Section A
#1. What percent of test-takers obtained a score of 450 or lower?
pnorm(450, mean= 496, sd=114)
#2. A student who got a score of 580 is at what percentile?
pnorm(580, mean=496, sd=114)
#3. What score was at the third quartile of the distribution?
qnorm(0.75, mean=496, sd=114)
#4. How high a score was needed to be at the 80th percentile?
qnorm(0.80, mean=496, sd=114)
#5. To be in the top 5% of scores, a student would need to have at least what score?
qnorm(0.05, mean=496, sd=114, lower.tail = FALSE)
#Section B
#1. Generate a sample of size 100 from the uniform distribution between 1 and 10, inclusive.
set.seed(100)
S1 <- runif(100, 1, 10)
#2. Display the first quartile of the theoretical uniform distribution.
qunif(0.25, min=1, max=10)
#3. Display the first quartile of the sample you generated.
quantile(S1, 0.25)
#4. Create a histogram of the sample you generated.
hist(S1)
#5. Generate a second sample of size 1000.
set.seed(100)
S3 <- runif(1000, 1, 10)
#6. Display the first quartile of your second sample.
quantile(S3, 0.25)
#7. Create a histogram of your second sample.
hist(S3)
#8. Generate a third sample of size 10,000.
set.seed(100)
S4 <- runif(10000, 1, 10)
#9. Display the first quartile of your third sample.
quantile(S4, 0.25)
#10. Create a histogram of your third sample.
hist(S4)
#11. Add some comments to the end of your script to answer the following questions:
#a. How did the first quartile of your 3 samples compare to each other and to the first quartile of the distribution from which they were drawn?
#the larger the sample, the closer it is to the theorical uniform distribution
#b. How did the histograms of your 3 samples compare?
#The bigger the sample, the more similar the frequency of different numbers are, meaning that the actual top lines of the histograms come closer to each other, looking more like the theorical uniform distribution
#c. Review (or look up) the "Law of Large Numbers". What does this law have to do with the tasks you have completed above?
#Proofs the point that the larger the sample is the frequency of the numbers and the difference between becomes more and more similar