# MY455

install.packages("psych")
install.packages("GPArotation")
install.packages("summarytools")

library(psych)
library(GPArotation)
library(summarytools)

catsat <- read.csv("C:/Users/emmaw/OneDrive/MASTERS/MY455/Data for seminars/Week 3/CATSAT.csv")

catsat <- data.frame(ID=seq(nrow(catsat)),catsat) # Add an ID variable, in case useful later

names(catsat)
tail(catsat)
items <- c("cvs", "cnvs", "cra", "csa", "se", "sm", "ss")

# Summary statistics
dfSummary(catsat[,items], graph.col = F)
# Correlation matrix
round(cor(catsat[,items],use="complete.obs"),4)

#Calculate principal component scores
catsat.res <- princomp(~cvs+cnvs+cra+csa+se+sm+ss,data=catsat, cor = TRUE, scores = TRUE, na.action=na.exclude)
summary(catsat.res)
#Summary function only produces standard deviations, not variance (eigenvalues), so square these SDs
catsat.res$sdev^2

#Plot eigenvalues of the correlation matrix
screeplot(catsat.res,type='l',main="")

#From eigenvalues we can see that components 1 and 2 are greater than 0.7 which is our cut-off.
#From screeplot we can confirm this as, the "elbow" is at component 2
#Lastly, from "proportion of Variance", component 1 explains 66% of the variance, with component 2 explaining another 12%. Together they explain ~75% of the variation which is a good amount of variance (we usually look for between 50 and 75%).

#Calculate factor loadings
catsat.weights <- loadings(catsat.res)

#Loadings smaller than 0.0001 are suppressed
print(catsat.weights,cutoff=0,digits=4)

#Factor 1 has similar loadings with a positive sign on all 7 of the items suggesting it can be interpreted as something like "general intelligence"
#Factor 2 has high magnitude positive loadings on items csa and se (spelling and English), and high magnitude negative loads on items cnvs, sm and ss (non-verbal, maths and science). Therefore, this factor can be interpreted as a measure of non-mathematical intelligence.

sqrt.lambda <- catsat.res$sdev
print(t(t(catsat.weights)*sqrt.lambda),cutoff = 0,digits=4)
cbind(catsat[,items], round(catsat.res$score[,1:2],3))

#1) whether the tests taken at the same age are highly correlated 
#We could interpret component 3 as a performance in CATs (taken at age 12), compared to SATs (taken at age 14). This factor explains 7.5% of the variance, but the eigenvalue is 0.53 which is below the usual cut-off of 0.7, so I would suggest that tests taken at the same age are not highly correlated.

#2) whether verbal tests tend to be more highly correlated with each other than with non-verbal tests
#As shown about, component 2 represents this, and we conclude from the eigen values and screeplot that there is significant correlation within verbal tests and within non-verbal tests.

#Remove NA values in items columns or receive error
catsat <- catsat[complete.cases(catsat[,items]),]
# scale=T bases the PCA on the correlation matrix
(catsat.PC.cor = prcomp(catsat[,items], scale=TRUE))
(catsat.PC.cov = prcomp(catsat[,items], scale=FALSE))

biplot(catsat.PC.cov)
biplot(catsat.PC.cor) 
#Can see from covariance plot that csa (CATs spelling test) dominates. The correlation plot better reveals the relationships between the different items. Should use correlation because the scales of the variables are quite different (CATs vs. SATs).
cov <- round(cov(catsat[,items],use="complete.obs"),4)
cor <- round(cor(catsat[,items],use="complete.obs"),4)
dfSummary(cov, graph.col = F)
dfSummary(cor, graph.col = F)
#Can see from summary statistics that none of the none of the values for any of the observations on the items is less than 0.4 or greater than 1, as by using correlation matrix instead of covariance matrix, they have been standardised.

print(catsat.fa2 <- fa(catsat[,items], nfactors=2, fm="ml", rotate="oblimin")) # 2 factor, oblique rotation
#First factor relates to lack of pure mathetmatical ability (maths and non-verbal reasoning are very low magnitude, with non-verbal reasoning being negative)
#Second factor relates to lack of reading/writing ability (reading and spelling are the only two items with negative factor loadings)
#Estimated correlation is 0.76 between the two factors (from fa() function)

#Obtain factor scores for these two factors.
d.tmp <-catsat.fa2$scores
#Convert to dataframe so can use summary(), sample mean for ML1 = 0, and the same for ML2. This is because in using the correlation matrix factor scores have been standardised to 0.
as.data.frame(d.tmp)
summary(d.tmp)

#Add to original dataset
catsat.complete <- cbind(catsat, d.tmp)

#What are the sample means of these scores, separatel for boys and girls?
#For 1st factor:
catsat.complete %>%
+     group_by(gender) %>%
+     summarize(mean_ML1 = mean(ML1))
#Mean for boys = 0.0556, for girls = -0.0608

#For 2nd factor:
catsat.complete %>% group_by(gender) %>% summarize(mean_ML2 = mean(ML2))
#Mean for boys = -0.0366, for girls = 0.04

#For each of the two factors, carry out a two-sample t-test which compares the means of the factor scores between boys and girls. What do you conclude from these tests?

t.test(ML1~gender, data=catsat.complete,var.equal=T)
#p-value is 0.53, so despite the difference in means, there is insufficient evidence to reject the null hypothesis that there is no difference in the mean factor scores of boys and girls for factor 1.

t.test(ML2~gender, data=catsat.complete,var.equal=T)
#p-value is 0.3371, so despite the difference in means, there is insufficient evidence to reject the null hypothesis that there is no difference in the mean factor scores of boys and girls for factor 2.


