dictConn<-function(option){
	user<-"user"
	password<-"DictMammalia"
	db<-"DictMammalia-01"
	collection<-"dict"

	if(option == 1){
		cluster1<-"dictmammalia-01-fceex.mongodb.net/test"
		conn<-collectionConnection(user, password, cluster1, db, collection)	
	}

	else if(option == 2){
		cluster2<-"dictmammalia-01-qifzg.mongodb.net/test"
		conn<-collectionConnection(user, password, cluster2, db, collection)
	}
	
	else if(option == 3){
		cluster3<-"dictmammalia-01-gwjzt.mongodb.net/test"
		conn<-collectionConnection(user, password, cluster3, db, collection)	
	} 

	return(conn)
}

#cluster4<-"dictmammalia-01-xd0zm.mongodb.net/test"
#conn<-mongo(db = bioDist, collections = collection, user = password, cluster = cluster4)

#cluster5<-"dictmammalia-01-yjjys.mongodb.net/test"
#conn<-mongo(db = bioDist, collections = collection, user = password, cluster = cluster5)

#cluster6<-"dictmammalia-06-ugibv.mongodb.net/test"
#conn<-mongo(db = bioDist, collections = collection, user = password, cluster = cluster6)

