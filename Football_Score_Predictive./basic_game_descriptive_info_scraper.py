import pandas as pd
import numpy as np
import os
import requests
from selenium import webdriver
from selenium.webdriver.common.keys import Keys
import nltk
from nltk.stem import PorterStemmer
import spacy
from collections import Counter
import en_core_web_sm
nlp = en_core_web_sm.load()


from nltk import word_tokenize, pos_tag, ne_chunk
from nltk.chunk import conlltags2tree, tree2conlltags

import ssl

try:
    _create_unverified_https_context = ssl._create_unverified_context
except AttributeError:
    pass
else:
    ssl._create_default_https_context = _create_unverified_https_context

nltk.download('punkt')
nltk.download('averaged_perceptron_tagger')
nltk.download('maxent_ne_chunker')
nltk.download('words')



def logo():
    n_drives = len(driver.find_elements_by_xpath("//a[@role='button']"))
    for drive in range(n_drives):
        driver.find_elements_by_xpath("//a[@role='button']")[drive].click()
        UNC = driver.find_elements_by_xpath("//img[@src='https://a.espncdn.com/combiner/i?img=/i/teamlogos/ncaa/500/153.png&h=100&w=100']")
        print(len(UNC))
        Pitt = driver.find_elements_by_xpath("//img[@src='https://a.espncdn.com/combiner/i?img=/i/teamlogos/ncaa/500/221.png&h=100&w=100']")
        print(len(Pitt))
        OSU = driver.find_elements_by_xpath("//img[@src='https://a.espncdn.com/combiner/i?img=/i/teamlogos/ncaa/500/153.png&h=100&w=100']")
        print(len(OSU))
        TCU = driver.find_elements_by_xpath("//img[@src='https://a.espncdn.com/combiner/i?img=/i/teamlogos/ncaa/500/221.png&h=100&w=100']")
        print(len(TCU))
        NCSU = driver.find_elements_by_xpath("//img[@src='https://a.espncdn.com/combiner/i?img=/i/teamlogos/ncaa/500/153.png&h=100&w=100']")
        print(len(NCSU))
        WF = driver.find_elements_by_xpath("//img[@src='https://a.espncdn.com/combiner/i?img=/i/teamlogos/ncaa/500/221.png&h=100&w=100']")
        print(len(WF))
        if UNC:
            print('UNC')
        elif Pitt:
            print('Pitt')
        elif OSU:
            print('OSU')
        elif TCU:
            print('TCU')
        elif NCSU:
            print('NCSU')
        elif WF:
            print('WF')


#UNC Pitt  
espn_url = " https://www.espn.com/college-football/scoreboard/_/year/2021/seasontype/2/week/11"

driver = webdriver.Firefox(executable_path=r'/Users/mattcarolinerigby/Downloads/geckodriver')
driver.get(espn_url)     
driver.find_element_by_xpath("//a[@href='/college-football/game/_/gameId/401282683']").click()
driver.find_element_by_name('&lpos=ncf:game:post:subnav:play-by-play').click()
n_drives = len(driver.find_elements_by_xpath("//a[@role='button']"))
for drive in range(n_drives):
    driver.find_elements_by_xpath("//a[@role='button']")[drive].click()
    #a = driver.find_element_by_class_name("post-play").text
    a = [el.text for el in driver.find_elements_by_class_name("post-play")]
    #print(a)
    #data_holder['Play'].append(a)
    #pd.DataFrame(data_holder)

df = pd.DataFrame(a,columns=['Play'])
df

#OSU TCU
espn_url = " https://www.espn.com/college-football/scoreboard/_/year/2021/seasontype/2/week/11"

driver = webdriver.Firefox(executable_path=r'/Users/mattcarolinerigby/Downloads/geckodriver')
driver.get(espn_url)
driver.find_element_by_xpath("//a[@href='/college-football/game/_/gameId/401287940']").click()
driver.find_element_by_name('&lpos=ncf:game:post:subnav:play-by-play').click()
n_drives = len(driver.find_elements_by_xpath("//a[@role='button']"))
for drive in range(n_drives):
    driver.find_elements_by_xpath("//a[@role='button']")[drive].click()
    #a = driver.find_element_by_class_name("post-play").text
    a = [el.text for el in driver.find_elements_by_class_name("post-play")]
    #print(a)
    #data_holder['Play'].append(a)
    #pd.DataFrame(data_holder)

df2 = pd.DataFrame(a,columns=['Play'])
df2

#NCSU WF
espn_url = " https://www.espn.com/college-football/scoreboard/_/year/2021/seasontype/2/week/11"

driver = webdriver.Firefox(executable_path=r'/Users/mattcarolinerigby/Downloads/geckodriver')
driver.get(espn_url)     
driver.find_element_by_xpath("//a[@href='/college-football/game/_/gameId/401282690']").click()
driver.find_element_by_name('&lpos=ncf:game:post:subnav:play-by-play').click()
n_drives = len(driver.find_elements_by_xpath("//a[@role='button']"))
for drive in range(n_drives):
    driver.find_elements_by_xpath("//a[@role='button']")[drive].click()
    #a = driver.find_element_by_class_name("post-play").text
    a = [el.text for el in driver.find_elements_by_class_name("post-play")]
    #print(a)
    #data_holder['Play'].append(a)
    #pd.DataFrame(data_holder)

df3 = pd.DataFrame(a,columns=['Play'])
df3

#All Games Combined
master_df = pd.concat([df, df2, df3])
master_df.to_csv('master_df.csv')

#Entity Recognition
tokens = nlp(''.join(str(master_df.Play.tolist())))

items = [x.text for x in tokens.ents]
Counter(items).most_common(20)

person_list = []
for ent in tokens.ents:
    if ent.label_ == 'PERSON':
        person_list.append(ent.text)
        
person_counts = Counter(person_list).most_common(20)
df_person = pd.DataFrame(person_counts, columns =['text', 'count'])
df_person
