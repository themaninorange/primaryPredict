from lxml import html
import requests

#example from the webpage (but really boring)
page = requests.get('http://econpy.pythonanywhere.com/ex/001.html')
tree = html.fromstring(page.content)

buyers = tree.xpath('//div[@title="buyer-name"]/text()')
print(buyers)


#Now trying it with fox news

page1=requests.get('http://www.foxnews.com/politics/2016/02/18/republicans-have-weapon-to-stop-obama-recess-appointment.html')
tree1=html.fromstring(page1.content)


para=tree1.xpath('//div[@class="article-text"]/text()')


doc = html.parse('http://java.sun.com').getroot()
for link in doc.cssselect('div.pad a'):
    print( '%s: %s' % (link.text_content(), link.get('href')))
