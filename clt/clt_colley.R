library(scales)

clt_colley <- read_csv("clt_colley.csv")

clt_colley <- clt_colley %>% 
  mutate(W = rowSums(. == "W") + rowSums(. == "WL") + 
           rowSums(. == "LW") + rowSums(. == "WW")*2 + rowSums(. == "WWW")*3,
         L = rowSums(. == "L") + rowSums(. == "WL") + 
           rowSums(. == "LW") + rowSums(. == "LL")*2 + rowSums(. == "LLL")*3,
         t = W + L,
         Percent = percent(W/t),
         Record = paste(W, L, sep="-"),
         C = t + 2,
         b = 1 + (W - L)/2)

# Create Colley matrix
clt_df <- as.data.frame(ifelse(clt_colley==0, 0, 
                                 ifelse(clt_colley=="X", 3, 
                                        -apply(clt_colley,2,nchar)
                                 ) 
))
clt_df <- select(clt_df, Scott:Bobby)

# Inverse of Colley matrix
clt_invmat <- solve(clt_df, tol = 1e-20)

# Get rating by multiplying inverse Colley matrix by b
clt_colley$Rating <- round(as.numeric(clt_invmat %*% clt_colley$b), 4)

# Arrange teams by rating & assign ranking
clt_colley <- arrange(clt_colley, desc(Rating))
clt_colley$Rank <- 1:nrow(clt_colley)

# Create new dataframe of rankings
clt_rankings <- select(clt_colley, one_of(c("Team", "Record", "Percent", "Rating"))) #can add Rank
clt_rankings
