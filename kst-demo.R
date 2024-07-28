# https://cran.r-project.org/web/packages/pks/pks.pdf
if (!require(pks)) install.packages("pks", dependencies = TRUE)
if (!require(relations)) install.packages("relations", dependencies = TRUE)
if (!require(Rgraphviz)) install.packages("Rgraphviz", dependencies = TRUE)

library(pks)
library(relations)
library(Rgraphviz)

data(chess)
# Rows are students, responses to items (binary) are columns
ita(chess$R) # find (locally) optimal threshold L
i <- ita(chess$R, L = 6, makeK = TRUE)
identical(sort(as.pattern(i$K)),
          sort(as.pattern(chess$dst1)))
# Found precedence relation
plot(relations::as.relation(i$I))


# Fitting basic local independence model (BLIM)
data(DoignonFalmagne7)
K <- DoignonFalmagne7$K # knowledge structure
N.R <- DoignonFalmagne7$N.R # frequencies of response patterns
m <- blim(K, N.R, method = "ML")
m
BIC(m)

# Generating item matrix
sf <- read.table(header = TRUE, text = "
item s t u
e 1 1 0
e 1 0 1
f 0 0 1
g 1 0 0
g 0 1 0
h 0 1 0
")
delineate(sf)

data(endm)
endm$K # true knowledge structure
endm$K2 # misspecified knowledge structure
endm$N.R # response patterns

## Generate data from BLIM based on K
blim0 <- list(
  P.K = setNames(c(.1, .15, .15, .2, .2, .1, .1), as.pattern(endm$K)),
  beta = rep(.1, 4),
  eta = rep(.1, 4),
  K = endm$K,
  ntotal = 200)
class(blim0) <- "blim"
simulate(blim0)
## Fit BLIM based on K2
blim1 <- blim(endm$K2, endm$N.R, "MD")




