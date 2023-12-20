# -*- coding: utf-8 -*-
"""
Created on Mon Apr 20 10:28:39 2020

@author: Mukesh
"""

from __future__ import print_function
import cv2 #opencv contains the image processing functions


image = cv2.imread("C:/Users/Mukesh/Desktop/videos/lena.png")
print("width: {} pixels".format(image.shape[1]))
print("height: {} pixels".format(image.shape[0]))
print("channels: {}".format(image.shape[2])) #colours
cv2.imshow("Image", image)
cv2.waitKey(0)


#translation
import imutils #module for image rotation, resizing, cropping etc (basic functions for image processing)
import numpy as np

M = np.float32([[1, 0, -25], [0, 1, -50]])  #shift image right and down
shifted = cv2.warpAffine(image, M, (image.shape[1], image.shape[0])) #(image,matrix,w,h)
cv2.imshow("Shifted Down and Right", shifted)
cv2.waitKey(0)


M = np.float32([[1, 0, -50], [0, 1, -90]])
shifted = cv2.warpAffine(image, M, (image.shape[1], image.shape[0]))
cv2.imshow("Shifted Up and Left", shifted)
cv2.waitKey(0)


#Rotation of an image
(h, w) = image.shape[:2]
center = (w // 2, h // 2)  #at the center
  

M = cv2.getRotationMatrix2D(center, 45, 1.0)  #(point,angle,dimension of image)
rotated = cv2.warpAffine(image, M, (w, h))
cv2.imshow("Rotated by 45 Degrees", rotated)
cv2.waitKey(0)

M = cv2.getRotationMatrix2D(center, 90, 1.0)  
rotated = cv2.warpAffine(image, M, (w, h))
cv2.imshow("Rotated by -90 Degrees", rotated)
cv2.waitKey(0)

################################################################################
def rotate(image, angle, center = None, scale = 1.0):
 (h, w) = image.shape[:2]
 
 if center is None:
     center = (w // 2, h // 2) #middle value as center
 M = cv2.getRotationMatrix2D(center, angle, scale)
 rotated = cv2.warpAffine(image, M, (w, h))
 return rotated
rotated = imutils.rotate(image, 180)
cv2.imshow("Rotated by 180 Degrees", rotated)
cv2.waitKey(0)

#################################################################################
#Fliping the image
flipped = cv2.flip(image, -1) #1-horizontal fliping, -1-vertical

cv2.imshow("Flipped Horizontally", flipped)
cv2.waitKey(0)
#1- filiping image horizontally
#-1- vertical fliping 


#cropping the image
cropped = image[30:120 , 240:335] #image (column(y) , rows(x))
cv2.imshow("lena", cropped)
cv2.waitKey(0)



#smoothning and bluring 
#Average by using cv2.blur  #by using avearge
#using np.hstack to get images in slide window
blurred = np.hstack([cv2.blur(image, (3, 3)),cv2.blur(image, (5, 5)),cv2.blur(image, (7, 7))]) #average
cv2.imshow("Averaged", blurred)
cv2.waitKey(0)

#Gausian bluring   #by using weighted mean of neighbors pixels #N(0,1)  #1,2,3,4,5
blurred = np.hstack([cv2.GaussianBlur(image, (3, 3), 0),cv2.GaussianBlur(image, (5, 5), 0),cv2.GaussianBlur(image, (7, 7), 0)])
cv2.imshow("Gaussian", blurred)
cv2.waitKey(0)

#Median Bluring #helps to reduce noise in image
blurred = np.hstack([cv2.medianBlur(image, 3),cv2.medianBlur(image, 5),cv2.medianBlur(image, 7)])
cv2.imshow("Median", blurred)
cv2.waitKey(0)

#Bilateral Bluring #reduce and also to save the edges
#(image,diameter of pixel neighbor, color strength from other pixel,space from center pixel )
blurred = np.hstack([cv2.bilateralFilter(image, 5, 21, 21),cv2.bilateralFilter(image, 7, 31, 31),cv2.bilateralFilter(image, 9, 41, 41)])
cv2.imshow("Bilateral", blurred)
cv2.waitKey(0)
