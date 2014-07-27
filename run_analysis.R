run_analysis<-function() {
#   function to find the mean values of all columns tagged as mean or std
#   there is an expected directory layout
#   with the activity_labels.txt, features.txt to be in the top level
#   and two directories with test and train with the data in those directories
#
#   read in the tope level information
    
    a<-read.table("activity_labels.txt")
    activity<-a$V2
    currentDir<-getwd();
    colNam<-read.table("features.txt")
    
#
#   read int the data in the test directory
#
    print("Reading test data")
    
    setwd("test")
    t<-read.table("y_test.txt")
    person<-read.table("subject_test.txt")
    ct<-activity[t$V1]
    data<-read.table("X_test.txt",col.names=colNam$V2)
    descol<-sort(c(grep("mean",names(data),ignore.case=TRUE),grep("std",names(data),ignore.case=TRUE)))
    subd1<-data[,descol]
    subd1$activity<-ct
    subd1$person<-person$V1

#
#   read the data in the train data
#
    print("Reading train data")
    
    setwd(currentDir)
    setwd("train")
    t<-read.table("y_train.txt")
    person<-read.table("subject_train.txt")
    ct<-activity[t$V1]
    data<-read.table("X_train.txt",col.names=colNam$V2)
    descol<-sort(c(grep("mean",names(data),ignore.case=TRUE),grep("std",names(data),ignore.case=TRUE)))
    subd2<-data[,descol]
    subd2$activity<-ct
    subd2$person<-person$V1

# merge the data and then find the mean of the selected data

    print("Merging data")
    
    setwd(currentDir)
    data<-rbind(subd1,subd2)
    tidy<-data.frame()
    person<-vector("numeric")
    activ<-vector("character")
    for(i in 1:30) {
        for(j in 1:length(activity)) {
            suball<-data[data$person==i & data$activity==activity[j],]
            if(nrow(suball) > 0 ) {
                mval<-vector("numeric")
                for(k in 1:(ncol(subd2)-2)) {
                    val<-mean(suball[,k])
                    mval<-c(mval,val)
                }
                person<-c(person,i)
                activ<-c(activ,as.character(activity[j]))
                tidy<-rbind(tidy,mval)
            }
        }
    }
    tidy$activity<-activ
    tidy$person<-person
    colnames(tidy)<-names(subd2)
    tidy
}
