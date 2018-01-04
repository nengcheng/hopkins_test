
#load dataset Problem 3
jones<-read.table("jones_cleaned.csv", header = T, sep = ",")
data_dist<-dist(jones)
#using "complete link algorithm"
h_clust<-hclust(data_dist, method = "complete")
plot(h_clust, hang=-1)
group.6<-cutree(h_clust, k=6, h=NULL)
print(group.6)  # index IDs of each group
table(group.6)  # statistics of number of IDs in each cluster

#print out all group ID for each clusters
s1<-subset(jones, group.6==1)
print(s1)
write.csv(s1,file = "subset1.csv")  # save the cluster 1 for SSE calculation
s2<-subset(jones, group.6==2)
s3<-subset(jones, group.6==3)
s4<-subset(jones, group.6==4)
s5<-subset(jones, group.6==5)
s6<-subset(jones, group.6==6)
## do the same for the rest of clusters
# write.csv(s2,file = "subset2.csv")
#write.csv(s3,file = "subset3.csv")
#write.csv(s4,file = "subset4.csv")
#write.csv(s5,file = "subset5.csv")
#write.csv(s6,file = "subset6.csv")


####8**<<Centroid algorthm >> **

h_clust2<-hclust(data_dist, method = "centroid")
plot(h_clust, hang=-1)
group.6<-cutree(h_clust2, k=6, h=NULL)
print(group.6)  # index IDs of each group
table(group.6)  # statistics of number of IDs in each cluster

#print out all group ID for each clusters
s1<-subset(jones, group.6==1)
print(s1)
s2<-subset(jones, group.6==2)
s3<-subset(jones, group.6==3)
s4<-subset(jones, group.6==4)
s5<-subset(jones, group.6==5)
s6<-subset(jones, group.6==6)
#write.csv(s1,file = "subset_c1.csv") 
#write.csv(s2,file = "subset_c2.csv")
#write.csv(s3,file = "subset_c3.csv")
#write.csv(s4,file = "subset_c4.csv")
#write.csv(s5,file = "subset_c5.csv")
#write.csv(s6,file = "subset_c6.csv")

###<Hopkins statistics>###

jones<-read.table("jones_cleaned.csv", header = T, sep = ",")

Hopkins<-function(data){
  n=round(0.05*nrow(data))   # sample 5% of the dataset
  sample<-round(runif(n, 0, nrow(data)))
  q<-matrix(0, ncol=ncol(data), nrow=n) #generating a space n matrix
  for (i in 1:ncol(data)){
  q[,i]<-runif(n,min = min(data), max=max(data))} #generating simulated data with similar variation
  p<-as.matrix(data[sample, ])   # sample n point from the real data
  q.dist<-rep(0,nrow(data))
  p.dist=0;
  X=c()
  Y=c()
  for (i in 1:n) { 
    q.dist[1]<-dist(rbind(q[i,],data[1,])) 
    Xi<-dist(rbind(p[i,], data[1,]))       # caculate its closest point distances for X and Y set
    for(j in 2:nrow(data)){
      q.dist[j]<-dist(rbind(q[i,], data[j,]))  
      if (sum(abs(p[i,]-data[j,]!=0))) {
        p.dist<-dist(rbind(p[i,], data[j,]))
        if(p.dist<Xi)
          Xi<-p.dist;
      }
    }
    Y[i]<-min(q.dist) # sum the closest distances
    X[i]<-Xi;
  }
  return(sum(Y)/(sum(X)+sum(Y)))  # Hopkins statistics caluclation
}
#test the dataset for 10 times
Hopkins(jones)


