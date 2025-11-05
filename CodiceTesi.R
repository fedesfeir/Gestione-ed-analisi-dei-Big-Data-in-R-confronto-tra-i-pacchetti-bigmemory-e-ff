library(bigmemory)
load(my_data.Rdata)
bigData <- as.big.matrix(my_data)
#divisione dataset
dataD <- data[c(1:5,56:58),]
dataS <- data[c(6:55,59:200),]
#Caricare separatamente in as.big.matrix()
bigDataS <- as.big.matrix(dataS, type="short")
bigDataD <- as.big.matrix(dataD, type="double")
colNames <- read.table(file="colNamesD.txt", h=FALSE)
colNames2 <- as.vector(as.matrix(colNames))
bigDataD <- read.big.matrix("dataD.csv", type="double", col.names =
                              colNames2, sep=";")

#Utilizzo del pacchetto bigmemory
a <- bigDataD[bigDataD[,"ID"]==10025889,]
a <- bigDataD[mwhich(x=bigDataD, cols=2, vals=list(10025889),
                     comps=’eq’),]
#confronto biglm e lm
set.seed(12345)
n <- 1e6
p <- 10
beta <- seq(-1, 1, length.out = p)^5
x1 <- matrix(rnorm(n * p), nrow = n, ncol = p)
x1[, p] <- 2 * x1[, 1] + rnorm(n, sd = 0.1)
x1[, p - 1] <- 2 - x1[, 2] + rnorm(n, sd = 0.5)
y1 <- 1 + x1 %*% beta + rnorm(n)
x2 <- matrix(rnorm(100 * p), nrow = 100, ncol = p)
y2 <- 1 + x2 %*% beta + rnorm(100)
bigData1 <- data.frame("resp" = y1, "pred" = x1)
bigData2 <- data.frame("resp" = y2, "pred" = x2)

biglm::biglm(formula = resp ~ ., data = bigData1)
biglm::biglm(formula = y ~ x)

f <- formula(paste("resp ~", paste(names(bigData1)[-1], collapse = " + ")))
biglmMod <- biglm::biglm(formula = f, data = bigData1)

print(object.size(biglmMod), units = "KB")
print(object.size(lmMod), units = "MB")

# Dimensione della variabile risposta
print(object.size(rnorm(1e6)) * 1e2, units = "GB")
## 0.7 Gb
# Dimensione dei predittori
print(object.size(rnorm(1e6)) * 1e2 * 10, units = "GB")
## 7.5 Gb


## Caso studio
library(bigmemory)
library(biglm)
library(biganalytics)
library(bigtabulate)
mortgages <- read.big.matrix("allstates.txt", sep = "\t", header = TRUE,
                             type = "double", backingfile = "allstates.bin", descriptor = "allstates.desc")

xdesc <- dget("allstates -clean.desc")
mortgages <- attach.big.matrix(xdesc)
dim(mortgages)
head(mortgages)[,1:7]

mean(mortgages[,"income"])
# 100.3027
median(mortgages[,"income"])
# 76

mod1 <- biglm.big.matrix(high.rate ~ sex, data = mortgages)
summary(mod1)

## ff
install.packages("ff")
library(ff)
vet_ff <- ff(1:1e7)
head(vet_ff)
summary(vet_ff)

matrice <- ff(matrix(1:1e6, nrow=1000, ncol=1000))

df_ff <- ffdf(x = ff(1:1e6), y = ff(rnorm(1:1e6)))
summary(df_ff)
head(df_ff$x)

subset_ff <- df_ff[df_ff$x > 500000, ]
summary(subset_ff)

flush(vet_ff)
finalizer(vet_ff) <- "delete"

vettore <- ff(1:1e7)
sum_vett <- 0
chunk_size <- 1e6 # Dimensione del chunk
n <- length(vettore)
for (i in seq(1, n, by = chunk_size)) {
  chunk <- vettore[i:min(i + chunk_size - 1, n)]
  sum_vett <- sum_vett + sum(chunk)}
sum_vett


##Dataset simulato 
set.seed(123)
n_rows <- 5e7
n_cols <- 10
data <- matrix(runif(n_rows * n_cols), nrow = n_rows, ncol = n_cols)

system.time({
  big_data <- read.big.matrix("large_dataset.csv", header = TRUE, type = "double")
})

system.time({
  col_sums <- biganalytics::colsum(big_data)
  col_means <- biganalytics::colmean(big_data) })

system.time({
  ff_data <- read.csv.ffdf(file = "large_dataset.csv", header = TRUE,
                           VERBOSE = TRUE)})

col_sums <- numeric(ncol(ff_data))
col_means <- numeric(ncol(ff_data))
n_rows <- nrow(ff_data)
chunk_size <- 100000 # Dimensione del chunk
system.time({
  for (start in seq(1, n_rows, by = chunk_size)) {
    end <- min(start + chunk_size - 1, n_rows)
    chunk <- ff_data[start:end, ]
    # Somme
    col_sums <- col_sums + apply(chunk, 2, sum, na.rm = TRUE)
    # Medie
    col_means <- col_means + apply(chunk, 2, mean, na.rm = TRUE) *
      (end - start + 1)}
  col_means <- col_means / n_rows # Calcolo della media finale })
  
  
  
#Analisi comparativa in termini di utilizzo della memoria e tempo di esecuzione 
set.seed(81216)
chunksize <- 10000
n <- 1.5e7
p <- 9
b <- runif(p)
names(b) <- paste0("x", 1:p) 
  
data <- data.frame(y=rnorm(n), check.rows=FALSE)
for ( nm in names(b) ) {
  xi <- rnorm(n)
  data[[nm]] <- xi
  data[["y"]] <- data[["y"]] + xi * b[nm]}

fm <- as.formula(paste0("y ~ ", paste0(names(b), collapse=" + ")))
lm.prof <- list()
lm.prof[["base"]] <- profmem({
  base.out <- lm(fm, data=data)})
rm(base.out)
gc()
print(lm.prof[["base"]])

library(bigmemory)
library(biganalytics)
backingfile <- "lm-ex.bin"
backingpath <- tempdir()
descriptorfile <- "lm-ex.desc"

data.bm <- filebacked.big.matrix(nrow=n, ncol=p + 1,
                                 backingfile=backingfile,
                                 backingpath=backingpath,
                                 descriptorfile=descriptorfile,
                                 dimnames=list(NULL, c("y", names(b))),
                                 type="double")
for ( nm in names(data))
  data.bm[,nm] <- data[[nm]]
rm(data)
gc()
lm.prof[["bigmemory"]] <- profmem({
  bm.out <- biglm.big.matrix(fm, data=data.bm, chunksize=chunksize)})
rm(bm.out)
gc()
print(lm.prof[["bigmemory"]])


library(ff)
library(ffbase)
data.ff <- ff(filename=paste0(backingpath, "/", backingfile),
              vmode="double", dim=c(n, p + 1),
              dimnames=list(NULL, c("y", names(b))))
data.ff <- as.ffdf(data.ff)
lm.prof[["ff"]] <- profmem({
  ff.out <- biglm(fm, data=data.ff, chunksize=chunksize) })
rm(ff.out)
gc()
print(lm.prof[["ff"]])
  
  