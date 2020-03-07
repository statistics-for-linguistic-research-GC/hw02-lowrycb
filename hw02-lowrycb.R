prep <- 501
dobj <- 1859
n <- prep + dobj

p <- dobj / n
odds.ratio <- p / (1-p)
odds.ratio

binom.test(dobj, n, p = 0.5, alternative = "two.sided")

num_dobj <- c(0:n)
proportion <- sapply(x, dbinom, n, .5)

sum(prop)

df = data.frame(cbind(x, prop))
 
library(ggplot2)

p = ggplot(df, aes(x=x, y=prop)) +
  geom_bar(stat="identity") + 
  xlim(460, 1900) +
  geom_vline(xintercept = 1859, color = "red") +
  ggtitle("Proportion of double object datives given null hypothesis, p = 0.5") 
plot(p)


1900 - n/2

n/2-720

remove(df, dobj, n, odds.ratio, p, prep, prop, x)

# -------------------------

df <- read.table("~/Dropbox/CUNY/Stats/hw/hw02-lowrycb/PTB.tsv", header = TRUE, sep = "\t", comment.char = "")

df$Stanford.corr <- df$Stanford.tag == df$gold.tag
df$NLP4J.corr <- df$NLP4J.tag == df$gold.tag

stanford_wins <- sum(df$Stanford.corr & !df$NLP4J.corr)
stanford_wins

NLP4J_wins <- sum(df$NLP4J.corr & !df$Stanford.corr)
NLP4J_wins

n = stanford_wins + NLP4J_wins

binom.test(NLP4J_wins, n, p = .5)


