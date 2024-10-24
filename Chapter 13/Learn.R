cbc.df <- read.csv2("cvc-df.csv", stringsAsFactors = T)[,-1]
cbc.df$seat = as.factor(cbc.df$seat)
cbc.df$price = as.factor(cbc.df$price)
head(cbc.df)

summary(cbc.df)

xtabs(choice ~ price , data =cbc.df)
xtabs(choice ~ cargo , data =cbc.df)

library(mlogit)
cbc.mlogit = mlogit.data(data = cbc.df, choice='choice', shape='long',
                         varying = c("carpool", 'seat', 'cargo', 'eng','price'), 
                         alt.levels = paste('pos', 1:3),
                         id.var = "resp.id")
m1 = mlogit(choice ~ 0 + seat + cargo + eng + price , data = cbc.mlogit)
summary(m1)

m2 = mlogit(choice ~ 1 + seat + cargo + eng + price, data=cbc.mlogit) 
summary(m2)

lrtest(m1,m2)

m3 = mlogit(choice ~ 0 + seat + cargo + eng + as.numeric(as.character(price)),
            data = cbc.mlogit)
summary(m3)

lrtest(m1,m3)

#willingness to pay
coef(m1)["cargo3ft"]/(-coef(m1)["price"]/1000)

#choice share
predict.mn1 = function(model, data) {
  #function to predict shares from a multinomial logit mode
  #model: mlogit object returned by mlogit
  #data: a data frame containing all the set of designs for which you want to
  #predict shares. Same format as the data used to estimate model.
  data.model = model.matrix(update(model$formula, 0 ~ .), data = data)[,-1]
  utility = data.model %*% model$coef
  share = exp(utility) / sum(exp(utility))
  cbind(share, data)
}

attrib <- list (seat = c("6", "7", "8"),
                cargo = c("2ft", "3ft"),
                eng = c("gas", "hyb", "elec"),
                price = c("30", "35", "40"))
(new.data = expand.grid(attrib)[c(8,1,3,41,49,26), ])

predict.mn1(m1, new.data) #probabilities of attributes combinations

#sensitivity plot 
sensitivity.mnl = function(model, attrib, base.data, competitor.data){
  # Function for creating data for a share-sensitivity chart
  # model : mlogit object
  # attrib : list of vectors with attribute levels
  # base.data : data frame containing baseline design
  # competitor.date: data frame containing design of competitive set
  data = rbind(base.data, competitor.data)
  base.share = predict.mn1(model, data)[1,1]
  share = NULL
  for (a in seq_along(attrib)){
    for (i in attrib[[a]]) {
      data[1,] = base.data
      data[1,a] = i
      share = c(share, predict.mn1(model, data)[1,1])
    }
  }
  data.frame(level=unlist(attrib), share=share, increase=share-base.share)
}
base.data = expand.grid(attrib)[c(8), ]
competitor.data = expand.grid(attrib)[c(1,3,41,49,26),]
(tradeoff = sensitivity.mnl(m1, attrib, base.data, competitor.data))

# mixed logit model
m1.rpar = rep("n", length = length(m1$coefficients))
names(m1.rpar) = names(m1$coefficients)
m1.rpar

m1.hier = mlogit(choice ~ 0 + seat + eng + cargo + price,
                 data = cbc.mlogit,
                 panel = T, rpar = m1.rpar, correlation = F)
summary(m1.hier)
stdev(m1.hier)

# with correlation
m2.hier = mlogit(choice ~ 0 + seat + eng + cargo + price, 
                 data = cbc.mlogit, 
                 panel = T, rpar = m1.rpar, correlation = T)
summary(m2.hier)
cov2cor(cov.mlogit(m2.hier))

predict.hier.mnl = function(model, data, nresp=1000) {
  # Function to predict shares of a hierarchical mnl
  # model: mlogit object
  # data: a data frame containing the set of design
  # This code assumes all model parameters are random
  data.model = model.matrix(update(model$formula, 0 ~ .), data = data)[,-1]
  coef.Sigma = cov.mlogit(model)
  coef.mu = model$coef[1:dim(coef.Sigma)[1]]
  draws = mvrnorm(n=nresp, coef.mu, coef.Sigma)
  shares = matrix(NA, nrow = nresp, ncol = nrow(data))
  for (i in 1:nresp) {
    utility = data.model %*% draws[i,]
    share = exp(utility)/sum(exp(utility))
    shares[i,] = share
  }
  cbind (colMeans(shares), data )
}
predict.hier.mnl(m2.hier,new.data)

# Bayesian model
choice = rep(0, nrow (cbc.df))
choice[cbc.df[,"alt"]==1] = cbc.df[cbc.df[,"choice"]==1,"alt"]
head(choice)

cbc.coded = model.matrix(~ seat + eng + cargo + price , data = cbc.df)
cbc.coded = cbc.coded[, -1] #
choicemodelr.data = cbind(cbc.df[,1:3], cbc.coded , choice)
head(choicemodelr.data)

carpool = cbc.df$carpool[cbc.df$ques==1 & cbc.df$alt==1] == "yes"
carpool = as.numeric(carpool)
choicemodelr.demos = as.matrix(carpool, nrow=length(carpool))
str(choicemodelr.demos)

library(ChoiceModelR)
hb.post = choicemodelr(data = choicemodelr.data, xcoding=rep(1,7),
                       demos=choicemodelr.demos, 
                       mcmc = list(R=20000, use=10000),
                       options=list(save=TRUE), directory = "G:/Code/R")
names(hb.post)
hb.post$compdraw[[567]]$mu
hb.post$deltadraw[567,]
hb.post$compdraw[[567]]$rooti

crossprod(hb.post$compdraw[[567]]$rooti)
head(hb.post $ betadraw[, ,567]) # part worth for each person

str(hb.post$betadraw)

beta.post.mean = apply(hb.post$betadraw, 1:2, mean )
head(beta.post.mean)

beta.post.q05 = apply(hb.post$betadraw, 1:2, quantile, probs=c(0.05))
beta.post.q95 = apply(hb.post$betadraw, 1:2, quantile, probs=c(0.95))
rbind(q05 = beta.post.q05[1,], mean=beta.post.mean[1,], q95=beta.post.q95[1,])

predict.hb.mnl = function(betadraws, data){
  # function to predict shares from a hierarchical multinomial logit model
  # model: mlogit object
  # data: a data frame containing the set of design
  data.model = model.matrix(~ seat + eng + cargo + price, data=data)
  data.model = data.model[,1]
  nresp = dim(betadraws)[1]
  ndraws = dim(hb.post$betadraw)[3]
  shares = array(dim=c(nresp, nrow(data), ndraws))
  for (d in 1:ndraws){
    for (i in 1:nresp){
      utility = data.model %*% betadraws[1,,d]
      shares[1,,d] = exp(utility) / sum(exp(utility))
    }
  }
  shares.agg = apply(shares, 2:3, mean)
  cbind(share=apply(shares.agg, 1, mean),
        pct = t(apply(shares.agg, 1, quantile, probs=c(0.05, 0.95))),
        data)

}
predict.hb.mnl(hb.post$betadraw, new.data)
