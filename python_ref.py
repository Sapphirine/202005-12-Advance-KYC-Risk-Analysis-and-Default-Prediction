from textblob import TextBlob
import pandas as pd
import numpy as np
import re
import fnmatch
from bs4 import BeautifulSoup
import requests

class Analysis:
    def __init__(self, term):
        self.term = term
        self.title_sentiment = 0
        self.title_subjectivity = 0
        self.arti_sentiment = 0
        self.arti_subjectivity = 0
        self.url = f'https://news.search.yahoo.com/search?q={self.term}'
        self.index = 5
    
    def run(self):
        response = requests.get(self.url)
        soup = BeautifulSoup(response.text, 'html.parser')
        results = soup.find_all('div', class_='dd NewsArticle')
        for news_item in results:
            title = news_item.find('h4').text
            blob = TextBlob(title)
            self.title_sentiment += blob.sentiment.polarity
            self.title_subjectivity += blob.sentiment.subjectivity
            link = news_item.find('a').get('href')
            if fnmatch.fnmatch(link,'*finance.yahoo*'):
                self.index = 1
            if self.index == 1 & bool(link and link.strip()):
                arti = BeautifulSoup(requests.get(link).text,'html.parser')
                #print(arti)
                more_item = arti.find('div',class_='canvas-body')
                result = more_item.find('p')
                #result = arti.find('div',class_='canvas-body').find('p')
                #print(more_item)
                topline = TextBlob(result.get_text())
                self.arti_sentiment = topline.sentiment.polarity
                self.arti_subjectivity = topline.sentiment.subjectivity
                self.index +=1
        if len(results)!=0:
            d = {'title_sentiment':round(self.title_sentiment/ len(results),3), 'title_subjectivity':round(self.title_subjectivity/ len(results),3), 'arti_sentiment':round(self.arti_sentiment,3), 'arti_subjectivity':round(self.arti_subjectivity,3)}
        else :
            d= {'title_sentiment':0, 'title_subjectivity':0, 'arti_sentiment':0, 'arti_subjectivity':0}
        df = pd.DataFrame(data=d,index = [0])
        return(df)
        

def testMethod(name):
  a = Analysis(name)
  r1 = a.run()
  return(r1)
  
def balabala(name):
  return(name)

class headline:
    def __init__(self, term):
        self.term = term
        self.url = f'https://news.search.yahoo.com/search?q={self.term}'
        self.index = 5
    
    def run(self):
        response = requests.get(self.url)
        soup = BeautifulSoup(response.text, 'html.parser')
        results = soup.find_all('div', class_='dd NewsArticle')
        for news_item in results:
            title = news_item.find('h4').text
            blob = TextBlob(title)
            link = news_item.find('a').get('href')
            time = news_item.find('span', class_='fc-2nd').text
            # time = time.encode('utf-8').replace('·', '').strip()
            if fnmatch.fnmatch(link,'*finance.yahoo*'):
                self.index = 1
            if self.index == 1 & bool(link and link.strip()):
                arti = BeautifulSoup(requests.get(link).text,'html.parser')
                more_item = arti.find('div',class_='canvas-body')
                result = more_item.find('p')
                #result = arti.find('div',class_='canvas-body').find('p')
                #print(more_item)
                topline = TextBlob(result.get_text())
                self.index +=1
        return("{} {} {}".format(title, time, link))

def topline(name):
  b = headline(name)
  r2 = b.run()
  return(r2)
