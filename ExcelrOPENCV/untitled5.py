# -*- coding: utf-8 -*-
"""
Created on Mon Apr 20 12:35:57 2020

@author: Mukesh
"""

import numpy as np
from __future__ import print_function
import cv2 #opencv contains the image processing functions

image = cv2.imread("C:/Users/Mukesh/Desktop/videos/lena.png")

#Simple thresholding of an image

image = cv2.cvtColor(image, cv2.COLOR_BGR2GRAY)  #converting image to grayscale image
#Applying Gaussian blurring helps remove some of the high frequency edges in the image that we are not concerned with.
blurred = cv2.GaussianBlur(image, (5, 5), 0)

cv2.imshow("Image", image)
cv2.waitKey(0)

#simple threshold
(T, thresh) = cv2.threshold(blurred, , 255, cv2.THRESH_BINARY) #(blurred image, min,max of pixel, threshold type)
cv2.imshow("Threshold Binary", thresh)
cv2.waitKey(0)

#(T, threshInv) = cv2.threshold(blurred, 155, 255, cv2.THRESH_BINARY_INV)
#cv2.imshow("Threshold Binary", thresh)
#cv2.waitKey(0)



#Adaptive Thresholding 
image = cv2.cvtColor(image, cv2.COLOR_BGR2GRAY)
blurred = cv2.GaussianBlur(image, (5, 5), 0)
cv2.imshow("Image", image)

#(blur image, max value, optimal T number, threshold type, neighbours, guassinan mean-3 for fine tuning of threshold)
thresh = cv2.adaptiveThreshold(blurred, 255,cv2.ADAPTIVE_THRESH_MEAN_C, cv2.THRESH_BINARY_INV, 11, 4)
cv2.imshow("Mean Thresh", thresh)
cv2.waitKey(0)

thresh = cv2.adaptiveThreshold(blurred, 255,cv2.ADAPTIVE_THRESH_GAUSSIAN_C, cv2.THRESH_BINARY_INV, 15, 3)
#(blur image, max value, optimal T number, threshold type, neighbours, guassinan mean-3 for fine tuning of threshold)
cv2.imshow("Gaussian Thresh", thresh)
cv2.waitKey(0)



#Gradient and Edge Detection

image = cv2.cvtColor(image, cv2.COLOR_BGR2GRAY)
image = cv2.GaussianBlur(image, (5, 5), 0)
cv2.imshow("Blurred", image)
cv2.waitKey(0)

canny = cv2.Canny(image, 30, 100) #(image, threshold1, threshold 2)
cv2.imshow("Canny", canny)
cv2.waitKey(0)



##########################################################################################

image = cv2.cvtColor(image, cv2.COLOR_BGR2GRAY)
image = cv2.GaussianBlur(image, (5, 5), 0)
cv2.imshow("Original", image)
cv2.waitKey(0)

lap = cv2.Laplacian(image, cv2.CV_64F) #laplician method for calculating gradient
lap = np.uint8(np.absolute(lap))
cv2.imshow("Laplacian", lap)
cv2.waitKey(0)

sobelX = cv2.Sobel(image, cv2.CV_64F, 1, 0)
sobelY = cv2.Sobel(image, cv2.CV_64F, 0, 1)

sobelX = np.uint8(np.absolute(sobelX))
sobelY = np.uint8(np.absolute(sobelY))

sobelCombined = cv2.bitwise_or(sobelX, sobelY)

cv2.imshow("Sobel X", sobelX)
cv2.imshow("Sobel Y", sobelY)
cv2.imshow("Sobel Combined", sobelCombined)
cv2.waitKey(0)
########################################################################################