library(ZillowR)
library(XML)

set_zillow_web_service_id('X1-ZWz184cjk2qcqz_2m43a')

GetMonthlyPayments(price = 300000L)

sampleProperty <- GetUpdatedPropertyDetails(zpid = 48749425, zws_id = "X1-ZWz184cjk2qcqz_2m43a")

xmlData <- xmlToList(sampleProperty$response)

seattleLink <- 'http://www.zillow.com/webservice/GetRegionChildren.htm?zws-id=<X1-ZWz184cjk2qcqz_2m43a>&state=wa&city=seattle&childtype=neighborhood'

seattleData <- xmlParse(seattleLink)
?xmlParse

XML::xmlRoot(XML::xmlTreeParse(seattleLink))
