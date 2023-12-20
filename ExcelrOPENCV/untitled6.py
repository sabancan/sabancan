# -*- coding: utf-8 -*-
"""
Created on Mon Apr 20 13:36:02 2020

@author: Mukesh
"""


#grayscale -> blur -> threshold(focus on imp area) ->edge and gradient dectector -> drawing contour

import numpy as np
from __future__ import print_function
import cv2 #opencv contains the image processing functions

image = cv2.imread("C:/Users/Mukesh/Desktop/videos/coins.jfif")


gray = cv2.cvtColor(image, cv2.COLOR_BGR2GRAY) 
blurred = cv2.bilateralFilter(gray, 5, 10, 10)  #blurring the grayscale image.
cv2.imshow("Image", blurred)
cv2.waitKey(0)


edged = cv2.Canny(blurred, 30, 150) #threshold
cv2.imshow("Edges", edged)
cv2.waitKey(0)

contours, hierarchy = cv2.findContours(edged.copy(), cv2.RETR_EXTERNAL,cv2.CHAIN_APPROX_SIMPLE)

print("I count {} coins or edges in this image".format(len(contours)))

coins = image.copy()
cv2.drawContours(coins, contours, -1, (0, 255, 0), 2) #-1 indicating that we need all contours
cv2.imshow("Coins", coins)
cv2.waitKey(0)