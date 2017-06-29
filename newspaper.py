import newspaper
from newspaper import Article



url1 = 'http://www.politico.com/story/2016/02/peter-welch-endorses-bernie-sanders-219499'
article1 = Article(url1)
article1.download()
article1.html
article1.parse()
article1.publish_date
article1.authors
article1.text

url2 = 'http://www.foxnews.com/politics/2016/02/19/fight-over-cruzs-ballot-eligibility-moves-to-courtrooms.html'
article2 = Article(url2)
article2.download()
article2.html
article2.parse()
article2.publish_date
article2.authors
article2.text

url3 = 'http://www.nytimes.com/2016/02/20/us/politics/a-donald-trump-victory-could-clash-with-south-carolinas-self-image.html?ref=politics'
article3 = Article(url3)
article3.download()
article3.html
article3.parse()
article3.authors
article3.text

cnn_paper = newspaper.build(u'http://cnn.com')
for article in cnn_paper.articles:
    print(article.url)

for category in cnn_paper.category_urls():
    print(category)



r = urllib.urlopen('http://www.politico.com/story/2016/02/peter-welch-endorses-bernie-sanders-219499')
soup = BeautifulSoup(r)
print type(soup)
print soup.prettify()
from IPython.display import Image
import IPython.display as ipdu

Image('http://www.politico.com/story/2016/02/peter-welch-endorses-bernie-sanders-219499')

ipd.HTML('<iframe src=http://www.politico.com/story/2016/02/peter-welch-endorses-bernie-sanders-219499 width=700 height=500></iframe>')


letters = soup.find_all(text=True)
print letters

date = soup.find_all