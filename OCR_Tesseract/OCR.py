#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Jul 12 22:22:46 2020

@author: virajraje
"""

#Objective of this is to perform OCR by first looking for the keyword or fixed label on the form and then perform OCR only
#in the region surrounding the keyword. For Eg: if we want to read the value of "NAme" entered on the form, we first search for the fixed lable "Name". 
#Once the location of the keyword"name" is identified, perform OCR in the region surrounding it to fet ch the name.
#This will improve accuracy as we concentrate only on a region within document to perform OCR

import pytesseract
from pytesseract import Output
import pandas as pd
from PIL import Image 
import matplotlib.pyplot as plt
import cv2
import numpy as np

pytesseract.pytesseract.tesseract_cmd = r'/usr/local/Cellar/tesseract/4.1.1/bin/tesseract'


#This function just displays the image
def displayimage(img):
  plt.figure(figsize=(15, 15))
  plt.axis('off')
  plt.imshow(cv2.cvtColor(img, cv2.COLOR_BGR2RGB))
  plt.show()
 
#This function searches the text within the image and draws a broder around it    
def findstring(imgname,strname):
  
  img = cv2.imread(imgname)

  d = pytesseract.image_to_data(img, lang='eng', output_type=Output.DICT)

  cols = ['left','top','width','height','text']
  df = pd.DataFrame(columns = cols )
  df = df.astype(dtype=dict(zip(cols,[int,int,int,int,str]) ))

  for i in range(len(d['level'])):
      df.loc[i] = [d['left'][i], d['top'][i], d['width'][i], d['height'][i],d['text'][i] ]

  a = df.loc[df['text'].str.contains(strname,case=False)].reset_index()
  for i in range(len(a)):
      (x, y, w, h) = (a['left'][i], a['top'][i], a['width'][i], a['height'][i])
      img = cv2.rectangle(np.array(img), (x, y), (x + w, y + h), (0, 255, 0), 2)
  print('Counts : ' + str(len(a)))
  displayimage(img)


#This function First locates the keyword passed to the method. 
#Once the keyword is found in the scanned document, it draws a border around it and then does OCS just in that area.
#This can be used to improve the accuracy of OCR in the scenario where we are reading a data entry form. 
#We are aware of the labels and mainly want to search for the data entered next to the label. 
#Eg: LAble is "NAme" and then the value next to it is theactualname that this function will return
def OCRSurrounding(imgname,strname, left, top, width, height):
  
  img = cv2.imread(imgname)

  d = pytesseract.image_to_data(img, lang='eng', output_type=Output.DICT)

  cols = ['left','top','width','height','text']
  df = pd.DataFrame(columns = cols )
  df = df.astype(dtype=dict(zip(cols,[int,int,int,int,str]) ))

  for i in range(len(d['level'])):
      df.loc[i] = [d['left'][i], d['top'][i], d['width'][i], d['height'][i],d['text'][i] ]

  a = df.loc[df['text'].str.contains(strname,case=False)].reset_index()
  for i in range(len(a)):
      (x, y, w, h) = (a['left'][i]-left, a['top'][i]-top, a['width'][i]+width, a['height'][i]+height)
      img = cv2.rectangle(np.array(img), (x, y), (x + w, y + h), (0, 255, 0), 2)
      crop_img = img[y: (y+h), x:(x+w)]
      displayimage(crop_img)

      config = ('-l eng --oem 1 --psm 3')
  # pytessercat
      text = pytesseract.image_to_string(crop_img)
      print(text)
 
  print('Counts : ' + str(len(a)))
  displayimage(img)
  #crop_img = img[x: (x+w), y:(y+100)]
  #crop_img = img[y: (y+h), x:(x+w)]
  #displayimage(crop_img)

  #config = ('-l eng --oem 1 --psm 3')
  # pytessercat
  #text = pytesseract.image_to_string(crop_img)
  #print(text)


  
imagename = "/Users/virajraje/Desktop/Screen Shot 2020-07-12 at 10.22.10 PM.png"
img = cv2.imread(imagename)
displayimage(img)
findstring(imagename,'Insured')
OCRSurrounding(imagename,'Annuitant/Insured', 10, 10,200,50)
OCRSurrounding(imagename,'birth', 50, 10, 200, 50)
OCRSurrounding(imagename,'Principal', 10, 10, 200, 50)

findstring(imagename,'Annuitant/Insured')
OCRSurrounding(imagename,'Annuitant/Insured', 10, 10,200,50)

  
im = cv2.imread('/Users/virajraje/Downloads/a38637a3863727.jpeg')
im1 = '/Users/virajraje/Downloads/a38637a3863727.jpeg'
im2 = '/Users/virajraje/Downloads/example_01.png'
# configurations

pytesseract.pytesseract.tesseract_cmd = r'/usr/local/Cellar/tesseract/4.1.1/bin/tesseract'

config = ('-l eng --oem 1 --psm 3')
# pytessercat
text = pytesseract.image_to_string(im, config=config)
# print text
text = text.split('\n')
text






##############################################################################################
#Demo
imagename = "/Users/virajraje/Desktop/Screen Shot 2020-07-12 at 10.22.10 PM.png"
img = cv2.imread(imagename)
displayimage(img)

#OCR of Full Image
config = ('-l eng --oem 1 --psm 3')
# pytessercat
text = pytesseract.image_to_string(imagename, config=config)
# print text
text = text.split('\n')
text

###########


findstring(imagename,'Insured')

OCRSurrounding(imagename,'Annuitant/Insured', 10, 10,200,50)

###################


  
  
  
  