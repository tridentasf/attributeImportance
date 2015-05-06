
getAttributeImportance <- function(attrs, chips) {
st <- data.frame(attrs, chips)
names(st)<-c("attributes","chips")

#format data into [comp1, comp2, chips1, chips2]
attr_split<-strsplit(as.character(st$attributes), ",")
chip_split<-strsplit(as.character(st$chips), ",")
max_col=max(unlist(lapply(attr_split,length),recursive=TRUE))

add_list <- function (x,max_col){
  u_l <- unlist(x)
  l<-length(unlist(x))
  pad_col = max_col - l
  r_l <- c(u_l, rep("NA",pad_col))
  return(r_l)
}

attrList<-lapply(attr_split,add_list,max_col)
chipList<-lapply(chip_split,add_list,max_col)

attr_matrix<-data.frame(matrix(unlist(attrList,recursive=TRUE),nrow=NROW(st),byrow=T),stringsAsFactors=FALSE)
anchor_matrix<-data.frame(matrix(0,nrow=NROW(st),byrow=T),stringsAsFactors=FALSE)
chip_matrix<-data.frame(matrix(unlist(chipList,recursive=TRUE),nrow=NROW(st),byrow=T),stringsAsFactors=FALSE)

df<-cbind(attr_matrix,chip_matrix,anchor_matrix,anchor_matrix)
names(df)[1] <- "comp1"
names(df)[2] <- "comp2"
names(df)[3] <- "chips1"
names(df)[4] <- "chips2"
names(df)[5] <- "anch1"
names(df)[6] <- "anch2"

#change 11->10.5 and 0->0.5
df$chips1[df$chips1=="11"]<-"10.5";
df$chips2[df$chips2=="11"]<-"10.5";
df$chips1[df$chips1=="0"]<-"0.5";
df$chips2[df$chips2=="0"]<-"0.5";
df <- transform(df, chips1=as.numeric(chips1))
df <- transform(df, chips2=as.numeric(chips2))
df <- transform(df, anch1=as.numeric(anch1))
df <- transform(df, anch2=as.numeric(anch2))


#comparison 1 dataframe
comp1df = data.frame(df$comp1,df$chips1,stringsAsFactors=FALSE);
names(comp1df)[1] <- "attr"
names(comp1df)[2] <- "chips"

#comparison 2 dataframe
comp2df = data.frame(df$comp2,df$chips2,stringsAsFactors=FALSE);
names(comp2df)[1] <- "attr"
names(comp2df)[2] <- "chips"

#merge comps and make sure chips is numeric
attrdf <- rbind(comp1df, comp2df)
attrdf <- transform(attrdf, chips=as.numeric(chips))

#sum chips per attribute and sort
aggrdf <- aggregate(attrdf$chips, by=list(attr=attrdf$attr), FUN=sum)
names(aggrdf)[2] <- "totalchips"
sorteddf <- aggrdf[with(aggrdf, order(-totalchips)),]

#replace anchor attribute with log(100)=2
df$anch1[df$comp1==sorteddf[1,1]]<-2
df$anch2[df$comp2==sorteddf[1,1]]<-2

#calculate Y log(chips1)-log(chips2)
ydf = data.frame(data.frame(log(df$chips1, 10)) - data.frame(log(df$chips2, 10)))
df <- cbind(df, ydf);
names(df)[7] <- "y"
df$y[df$anch1==2]<-df$y[df$anch1==2]-2
df$y[df$anch2==2]<-df$y[df$anch2==2]+2

#form X data frame
xdf1 = data.frame(df$comp1, df$comp1, df$comp1, df$comp1, df$comp1, stringsAsFactors=FALSE);
names(xdf1) <- sorteddf[2:nrow(sorteddf),1]
xdf1[1][xdf1[1]!=names(xdf1)[1]]<-0;
xdf1[2][xdf1[2]!=names(xdf1)[2]]<-0;
xdf1[3][xdf1[3]!=names(xdf1)[3]]<-0;
xdf1[4][xdf1[4]!=names(xdf1)[4]]<-0;
xdf1[5][xdf1[5]!=names(xdf1)[5]]<-0;
xdf1[1][xdf1[1]!=0]<-1;
xdf1[2][xdf1[2]!=0]<-1;
xdf1[3][xdf1[3]!=0]<-1;
xdf1[4][xdf1[4]!=0]<-1;
xdf1[5][xdf1[5]!=0]<-1;
xdf1=sapply(xdf1, as.numeric)
xdf2 = data.frame(df$comp2, df$comp2, df$comp2, df$comp2, df$comp2, stringsAsFactors=FALSE);
names(xdf2) <- sorteddf[2:nrow(sorteddf),1]
xdf2[1][xdf2[1]!=names(xdf2)[1]]<-0;
xdf2[2][xdf2[2]!=names(xdf2)[2]]<-0;
xdf2[3][xdf2[3]!=names(xdf2)[3]]<-0;
xdf2[4][xdf2[4]!=names(xdf2)[4]]<-0;
xdf2[5][xdf2[5]!=names(xdf2)[5]]<-0;
xdf2[1][xdf2[1]!=0]<--1;
xdf2[2][xdf2[2]!=0]<--1;
xdf2[3][xdf2[3]!=0]<--1;
xdf2[4][xdf2[4]!=0]<--1;
xdf2[5][xdf2[5]!=0]<--1;
xdf2=sapply(xdf2, as.numeric)
xdf=xdf1+xdf2

#form regression data frame and regress
regdata <- data.frame(xdf, df$y)
xvars <- names(regdata)
names(xvars) <- c("x1","x2","x3","x4","x5","y")
names(regdata) <- c("x1","x2","x3","x4","x5","y")
fit <- lm(formula = y ~ -1+x1+x2+x3+x4+x5, data=regdata)
coef <- coefficients(fit)
names(coef) <- xvars[1:5]
coef$missingattr<-2
names(coef)[6] <- sorteddf[1,1]
coefdf <- 10^(data.frame(coef, stringsAsFactors=FALSE))

#final attribute importance
attrImportance <- data.frame(t(coefdf / sum(coefdf) * 100))
names(attrImportance)[1] <- "importance"

return(attrImportance)
}