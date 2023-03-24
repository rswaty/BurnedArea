# Load the installed package
#install.packages("caret")
library(caret)

#library(devtools)
#devtools::install_url("https://cran.r-project.org/src/contrib/caret_6.0-78.tar.gz")


# Initialization of Sample factors
# of predicted and actual values
pred_values <- factor(c(TRUE,FALSE,
                        FALSE,TRUE,FALSE,TRUE,FALSE))
actual_values<- factor(c(FALSE,FALSE,
                         TRUE,TRUE,FALSE,TRUE,TRUE))

# Confusion Matrix
cf <- caret::confusionMatrix(data=pred_values,
                             reference=actual_values)
print(cf)


# Visualizing Confusion Matrix
(as.table(cf),color=c("green","red"),main = "Confusion Matrix")

cf



# Elements are arranged sequentially by row.
M <- matrix(c(3:14), nrow = 4, byrow = TRUE)
print(M)

# Elements are arranged sequentially by column.
N <- matrix(c(3:14), nrow = 4, byrow = FALSE)
print(N)

# Define the column and row names.
rownames = c("row1", "row2", "row3", "row4")
colnames = c("col1", "col2", "col3")

P <- matrix(c(3:14), nrow = 4, byrow = TRUE, dimnames = list(rownames, colnames))
print(P)