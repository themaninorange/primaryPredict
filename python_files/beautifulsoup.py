
wikiurl = "https://en.wikipedia.org/wiki/Wikipedia"
foxurl = "http://www.foxnews.com/politics/2016/02/19/mourners-pay-respects-to-late-justice-scalia-at-supreme-court.html"
cnnurl = "http://www.cnn.com/2016/02/18/politics/republican-town-hall-highlights/index.html"

from urllib.request import urlopen
from bs4 import BeautifulSoup
html= urlopen()
print(html.read())

wiki=BeautifulSoup(html.read(wikiurl))
len(str(wiki))

cnnhtml=urlopen(cnnurl)
cnn=BeautifulSoup(cnnhtml.read())
len(str(cnn))

foxhtml = BeautifulSoup(requests.get(foxurl).text)
print(foxhtml)
len(str(foxhtml))

str(foxhtml)

foxhtml.
    

