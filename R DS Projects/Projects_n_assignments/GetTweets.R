library(twitteR)
library(ROAuth)

# Download "cacert.pem" file
download.file(url="http://curl.haxx.se/ca/cacert.pem",destfile="cacert.pem")


#create an object "cred" that will save the authenticated object that we can use for later sessions
cred <- OAuthFactory$new(consumerKey='RIg6X6k8qBXXtKogJLd7cIR9g',
                         consumerSecret='5gxcD17QPKC0dlR5aFXzgsUBRYaOvJEZuIllfYngsLWpSw9O3U',
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL='https://api.twitter.com/oauth/authorize')

# Executing the next step generates an output --> To enable the connection, please direct your web browser to: <hyperlink> . Note:  You only need to do this part once
cred$handshake(cainfo="cacert.pem")

#save for later use for Windows
save(cred, file="twitter authentication.Rdata")

load("twitter authentication.Rdata")
#registerTwitterOAuth(cred)
setup_twitter_oauth(CUSTOMER_KEY, CUSTOMER_SECRET, ACCESS_TOKEN, ACCESS_secret, credentials_file="twitter authentication.Rdata")

search.string <- "#datascientistjobs"
no.of.tweets <- 100

tweets <- searchTwitter(search.string, n=no.of.tweets, cainfo="cacert.pem", lang="en")


#Example 2

packages <- c("twitteR","ROAuth")#"openssl","base64enc"
### checking if packages are already installed and installing if not
check.install.load.Package<-function(package_name){
        if(!package_name%in%installed.packages()){
                install.packages(package_name)
        }
        library(package_name,character.only = TRUE)
}
for(package in packages){
        check.install.load.Package(package)
}


api_key = "RIg6X6k8qBXXtKogJLd7cIR9g" # your api_key
api_secret = "5gxcD17QPKC0dlR5aFXzgsUBRYaOvJEZuIllfYngsLWpSw9O3U" # your api_secret 
access_token = "31064688-UvyXDHG9Yty7wnPB8BrPrytJiGMdVxvZZgwc5fZvl" # your access_token 
access_token_secret = "QfyuMxTBuRHzg0l9t2g95934jmtOsDqw3Q227913n3bpU" # your access_token_sceret 
credential<-OAuthFactory$new(consumerKey='RIg6X6k8qBXXtKogJLd7cIR9g',
                             consumerSecret='5gxcD17QPKC0dlR5aFXzgsUBRYaOvJEZuIllfYngsLWpSw9O3U',
                             requestURL="https://api.twitter.com/oauth/request_token",
                             accessURL="https://api.twitter.com/oauth/access_token",
                             authURL="https://api.twitter.com/oauth/authorize")

credential$handshake()

setup_twitter_oauth(api_key,api_secret,access_token,
                    access_token_secret)



search.string <- "#oviyasweetz"
no.of.tweets <- 60

RohingyaTerrorReality.Tweets <- searchTwitter(search.string, n=no.of.tweets,lang="en")



df <- do.call("rbind", lapply(RohingyaTerrorReality.Tweets, as.data.frame))
View(df)
tweets