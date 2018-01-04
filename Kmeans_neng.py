import numpy as np
import pandas as pd
from pandas import DataFrame
mydata=pd.read_csv('jones_cleaned.csv')
np.set_printoptions(suppress =True) #set threshold=np.nan, if print all instances
#compute the initial centroid of the data
def center(mydata):
    return np.mean(mydata, 0)
#define sse and its calculation
def sse(mydata):
    u = center(mydata)
    return np.sum(np.linalg.norm(np.square(mydata - u), 2, 1)) # calculate sse
#define k-means clutering algorithm, K:number of clusters, maxium iteratations=10
class K_means:
    def __init__(self, mydata=None, k=2, output=True):
        #most convergence happened in first few iterations
        if mydata is not None:
            self.fit(mydata, k, output)
    def fit(self, mydata, k=2, output=True):
        self.mydata = np.matrix(mydata)
        self.k = k
# generate one random initial centroids
        for inits in range(1):
            indices = np.random.choice(len(mydata), k, replace=False)
            u = self.mydata[indices, :]
            t = 0
            old_sse = np.inf
            while True: #for-loop  distributing the points
                t = t + 1
                C = [None] * k
                for x in self.mydata:
                    #calculate closest euclidean disance
                    j = np.argmin(np.linalg.norm(x - u, 2, 1))
                    C[j] = x if C[j] is None else np.vstack((C[j], x))
                for j in range(k):
                    u[j] = center(C[j])  # Centroids are updated
                    subset=np.array(C[j])
                    print(subset)
                    print("\n Print mean of this cluster:",j,"\n",u[j],"\n")
                    sse_cls=sse(C[j])
                    print("SSE for this cluster: ", round(sse_cls,2),"\n")
                if t >= 10:  #stop when reach maxinum iterations
                    break
                new_sse = np.sum([sse(C[j]) for j in range(k)]) #total sse
                vary = old_sse - new_sse
                for mark in range(10):   #print the last iteration results
                    if t < mark:
                        t = mark
                if output:
                    print("Total_SSE: ", new_sse, "\n") #print final SSE and iterations used
                if vary < 0.01:  #stop when change of change of sse is small
                    break
                else:
                    old_sse = new_sse
        return self
print("SSE of each cluster with K=2:\n\n")
c=K_means(mydata, k=2, output=True)
print("\n\n")
print("SSE of each cluster with K=3:\n\n")
c=K_means(mydata, k=3, output=True)
print("\n\n")
print("Then set the value of k=3,4, 5, 6 ,7, Done!")
