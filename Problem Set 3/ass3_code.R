library(matrixStats)

#### Single Price ####
# Reading the data
data <- read.csv("WTP_Data_v4.csv", header = T)

N=nrow(data)
# Computing maximum WTP for each client across time slots and entering it as a 
# new column (Class 4)
for (i in 1:N){
  data$maxWTP[i]=max(data[i,5])
}
# Using maximum WTP as the upper bound for the price search
maxprice=max(data$maxWTP)

# Defining empty array variables we will be introducing
demand_singleprice=rep(NA,maxprice)
revenue_singleprice=rep(NA,maxprice)

# Calculating demand and revenue at different price levels
for (p in 1:maxprice){
  demand_singleprice[p]=sum(data$maxWTP>=p)
  revenue_singleprice[p]=(p*demand_singleprice[p])*12/3
}  

#### Current Revenue Model ####
# Basic revenue calculation based on the demand for current model at 1000.
avail_rooms <- 200
price <- 1000 # per month price
fill_rate <- demand_singleprice[1000]/N
revenue_base <- (avail_rooms*price*fill_rate)*12 # Calculating yearly revenue



#### Identifying the Optimal Price ####
revenueBest=max(revenue_singleprice)
priceBest=which(revenue_singleprice == revenueBest)
demandBest=demand_singleprice[priceBest]
print(paste("If a single price is charged across all classes, total revenue is maximized when price =", priceBest))



#### Introducing 5 Classes ####
# Introducing empty arrays for demand, surplus and price
surplus_class1 <- rep(0,N)
surplus_class2 <- rep(0,N)
surplus_class3 <- rep(0,N)
surplus_class4 <- rep(0,N)
surplus_class5 <- rep(0,N)

demand_class1 <- rep(0,N)
demand_class2 <- rep(0,N)
demand_class3 <- rep(0,N)
demand_class4 <- rep(0,N)
demand_class5 <- rep(0,N)

price1 <- rep(0,N)
price2 <- rep(0,N)
price3 <- rep(0,N)
price4 <- rep(0,N)
price5 <- rep(0,N)

index=1
# Defining prices to consider for the demand calculation
for (class1Price in seq(400,1400,100)) {
  for (class2Price in seq(400,1400,100)) {
    for (class3Price in seq(400,1400,100)) {
      for (class4Price in seq(400,1400,100)) {
        for (class5Price in seq(400,1400,100)) {
          for (i in 1:N) {
            # Calculating the surplus at various price levels
            surplus_class1[i]=max(data$Class.1[i]-class1Price)
            surplus_class2[i]=max(data$Class.2[i]-class2Price)
            surplus_class3[i]=max(data$Class.3[i]-class3Price)
            surplus_class4[i]=max(data$Class.4[i]-class4Price)
            surplus_class5[i]=max(data$Class.5[i]-class5Price)
          }
          # Recording the prices at the different levels
          price1[index] <- class1Price
          price2[index] <- class2Price
          price3[index] <- class3Price
          price4[index] <- class4Price
          price5[index] <- class5Price
          
          # Calculating demand based on surplus at different price levels
          demand_class1[index]=sum((surplus_class1>surplus_class2 & surplus_class1>surplus_class3 & surplus_class1>surplus_class4 & surplus_class1>surplus_class5)*(surplus_class1>=0))
          demand_class2[index]=sum((surplus_class2>surplus_class1 & surplus_class2>surplus_class3 & surplus_class2>surplus_class4 & surplus_class2>surplus_class5)*(surplus_class2>=0))
          demand_class3[index]=sum((surplus_class3>surplus_class2 & surplus_class3>surplus_class1 & surplus_class3>surplus_class4 & surplus_class3>surplus_class5)*(surplus_class3>=0))
          demand_class4[index]=sum((surplus_class4>surplus_class2 & surplus_class4>surplus_class3 & surplus_class4>surplus_class1 & surplus_class4>surplus_class5)*(surplus_class4>=0))
          demand_class5[index]=sum((surplus_class5>surplus_class2 & surplus_class5>surplus_class3 & surplus_class5>surplus_class4 & surplus_class5>surplus_class1)*(surplus_class5>=0))
          index=index+1
        }
      }
    }
  }
}
# Creating a data frame to visualise demand at various price levels
demand <- data.frame(cbind(price1, price2, price3, price4, price5, demand_class1, demand_class2, demand_class3, demand_class4, demand_class5))
# Calculating revenue based on the demand at all possible price levels
# The (12/3) is the scaling of the revenue to yearly terms and to 200 respondents
demand$revenue <- (demand_class1*price1 + demand_class2*price2 + demand_class3*price3 + demand_class4*price4 + demand_class5*price5)*12/3

# Identifying the best prices
bestPrices <- which(demand$revenue==max(demand$revenue))
prices <- demand[bestPrices,1:5]

#### Utilities and Attractions ####
# Computing average WTP across the classes
avgWTPs<-colMeans(data[2:6])
sdWTPs<- colSds(as.matrix(data[2:6]))
varWTPs<- sdWTPs^2
avgvarWTPs<- mean(varWTPs)

# Defining parameters for the assortment optimization
util <- avgWTPs
mu <- sqrt(5*avgvarWTPs)/pi
v0 <- 10
index=1

# Creating an empty matrix to record outputs
results_assortment <- matrix(ncol = 11)
priceLabels=c(paste0("price", 1:5))
productLabels=c(paste0("Prod", 1:5))
colnames(results_assortment)<- c(productLabels,"Revenue", priceLabels)

# For the best prices identified from the consumer surplus demand model, we
# consider all possible combinations of assortments. For each assortment, we
# compute purchase probabilities and record the revenue
for (x in 1:length(bestPrices)) {
  attr_price <- as.vector(t(demand[bestPrices[x],1:5]))
  attractions <- exp((util-attr_price)/mu)
  probPurchase<- rep(NA,5)
  Revenue=rep(NA,2^5)
  assortmenttable=data.frame(matrix(nrow=2^5,ncol = 12))
  indicatorLabels=c(paste0("Prod", 1:5))
  purchaseProbLabels=c(paste0("pBuy", 1:5))
  colnames(assortmenttable)=c("N",indicatorLabels,purchaseProbLabels, "Revenue")
  for (i1 in 0:1){
    for (i2 in 0:1){
      for (i3 in 0:1){
        for (i4 in 0:1){
          for (i5 in 0:1){
            # Collecting all assortment indicators in a single array
            i=c(i1,i2,i3,i4,i5)
            # Computing the denominator for the purchase probabilities
            Denominator=sum(attractions*i)+v0
            # Obtaining the purchase probabilities
            for (j in 1:5){
              probPurchase[j]=attractions[j]*i[j]/Denominator
            }
            # Revenue from this assortment
            Revenue[index]=(sum(probPurchase*attr_price))*12/3
            assortmenttable[index,1]=sum(i)
            assortmenttable[index,2:6]=i
            assortmenttable[index,7:11]=round(probPurchase,3)
            assortmenttable[index,12]=round(Revenue[index],3)
            index=index+1
          }
        }
      }
    }
  }
  # Creating a new matrix to view the assortment and revenue at the price combinations
  assortmenttable2<-assortmenttable[order(assortmenttable$N),]
  assortmenttable2 <- na.omit(assortmenttable2)
  # Subsetting based on a specified size constraint (2 classes offered in our model)
  prod_offer <- assortmenttable2[assortmenttable2$N==2,]
  # Finding the best combination of two classes that maximises revenue
  optimalassortmentind <- which(prod_offer$Revenue==max(prod_offer$Revenue))
  # Subsetting the assortments that maximise revenue
  optimaloffer <- prod_offer[optimalassortmentind,c(2:6,12)]
  optimaloffer <- cbind(optimaloffer, (prices[x,]))
  results_assortment <- rbind(results_assortment, optimaloffer)
  results_assortment <- na.omit(results_assortment)
}
results_assortment



#### Peak Price - Class 4 ####
data1 <- read.csv("WTP_Class4_peak_nonpeak.csv", header = T)
# Identifying the maximum demand between peak and non-peak
data1$maxWTP=apply(data1[,2:3],1,max)

# The maximum WTP in data, we can use this as the upper bound for our price search.
maxprice=max(data1$maxWTP)

# Setting baseprice for non-peak period
basePrice = 900

# Creating empty lists for results
demandNonPeak = rep(0,maxprice-basePrice)
demandPeak = rep(0,maxprice-basePrice)
revenue = rep(0,maxprice-basePrice)
surplusNonPeak = rep(0,N)
surplusPeak<-matrix(0,N,maxprice)

# For each respondent we will obtain their maximum WTP and maximum Surplus among the nonpeak period
for (i in 1:N){
  surplusNonPeak[i]=data1[i,2]-basePrice
}

# Calculating the surplus for all respondents
for (p in basePrice:maxprice){
  for (i in 1:N){
    surplusPeak[i,p]=data1[i,3]-p
  }
}

# Calculating the demands for high and low peak and revenue
for (p in basePrice:maxprice){
  demandNonPeak[p]=sum((surplusNonPeak>surplusPeak[,p])*(surplusNonPeak>=0))
  demandPeak[p]=sum((surplusPeak[,p]>=surplusNonPeak)*(surplusPeak[,p]>=0))
  # The revenue calculation considers the demand distribution (which was equal) from the consumer 
  # surplus model for classes 4 & 5. Here, the output is the total revenue.
  revenue[p]=((basePrice*demandNonPeak[p])+(p*demandPeak[p]))*(12/3)/2 + (100*900)*12
}

##### Plotting Revenue vs Peak Period Price
xaxis=1:maxprice
# Plotting revenue for different peak prices
plot(xaxis,revenue/1000, pch = 16, type="s",col="blue", las=1, xaxt="n", cex.axis = 0.8, tcl  = -0.2,
     xlab="Price for Peak Period",ylab="Revenue in thousands", main = "Optimal peak period price for class 4")
# Setting x labels
xticks <- seq(0, maxprice, by=1)
axis(side = 1, at = xticks, cex.axis = 0.8, tcl  = -0.2)
# Plotting best price and obtained revenue
revenueBest=max(revenue[basePrice:maxprice])/1000
priceBest=min(which(revenue/1000 == revenueBest))
# Plot the lines of the revenue results
# The optimal peak period price
text(1400, 200, paste("Optimal peak price:",priceBest), cex = 0.8, tcl  = -0.2)
lines(c(priceBest,priceBest),c(0, revenueBest),lty=2)
# The revenue
text(260, 2000, paste("Revenue:",round(revenueBest),2), cex = 0.8, tcl  = -0.2, col = "red")
lines(c(0,priceBest),c(revenueBest, revenueBest),lty=2, col="red")

# Print the results
print(paste("When low peak periods have a base price of £900, the optimal price for the high peak slot is: £", priceBest, sep=""))
print(paste("When low peak periods have a base price of £900 and a high peak price of £", priceBest,  " the total revenue for Class 4 is: £", round(revenueBest*1000), sep=""))



#### Setting protection levels ####
# Inputting results to identify best protection level for class 4
mL=round((demandNonPeak[priceBest])*avail_rooms/N)      # Mean Demand for Low-Fare, Poisson
mH=round((demandPeak[priceBest])*avail_rooms/N)         # Mean Demand for High-Fare, Poisson
pL=basePrice           # Price for Low-Fare
pH=priceBest          # Price for Low-Fare
capacity=avail_rooms/2    # Capacity 
ExpRevenue=rep(0,capacity+1)
# Looping through capacity levels and recording revenue at each level
for (i in 1:(capacity+1)){
  protect=i-1
  availforLowFare=capacity-protect;
  ExpRevenue[i]=0;
  for(dL in 0:200){
    soldLowFare=min(availforLowFare,dL)
    remainforHighFare=capacity-soldLowFare
    for(dH in 0:200){
      soldHighFare=min(remainforHighFare,dH)
      # Calculating revenue
      RevenueThisIter=pL*soldLowFare+pH*soldHighFare
      ExpRevenue[i]=ExpRevenue[i]+
        RevenueThisIter*dpois(dL,mL)*dpois(dH,mH)
    }
  }
}
# Identifying the optimal protection level
Protectindexbest = which(ExpRevenue == max(ExpRevenue))
ProtectBest=Protectindexbest-1
OptimalExpRevenue=max(ExpRevenue)
print(paste("The Optimal Protection Level for High-Fare Demand:", ProtectBest))