# -*- coding: utf-8 -*-
"""
Created on Mon Apr 20 07:41:00 2020

@author: Mukesh
"""
#Importing the required packages
from __future__ import print_function

import cv2 #opencv contains the image processing functions

image = cv2.imread("C:/Users/Mukesh/Desktop/videos/lena.png")
print("width: {} pixels".format(image.shape[1]))
print("height: {} pixels".format(image.shape[0]))
print("channels: {}".format(image.shape[2])) #colours

cv2.imshow("Image", image)
cv2.waitKey(0)


cv2.imwrite("newimage.jpg", image)



# Manipulating one  pixels
(b, g, r) = image[0, 0]  #BGR
print("Pixel at (0, 0) - Red: {}, Green: {}, Blue: {}".format(r,g, b)) #printing values in wanted format

image[0, 0] = (0, 0, 255)
(b, g, r) = image[0, 0]
print("Pixel at (0, 0) - Red: {}, Green: {}, Blue: {}".format(r,g, b))

#maipulating group of pixels
corner = image[0:100, 0:100] # storing the pixels into an variable
cv2.imshow("Corner", corner)
cv2.waitKey(0)

image[0:100, 0:100] = (0, 255, 0) #green
cv2.imshow("Updated", image)
cv2.waitKey(0)


# drwaing lines , rectangles in the image

green = (0, 255, 0)  #(B,G,R)
cv2.line(image, (0, 0), (512, 512), green,3) #(image,y,x,colour,density of colour)
cv2.imshow("green line", image)
cv2.waitKey(0)

red = (0, 0, 255)
cv2.line(image, (300, 0), (0, 300), red, 6)
cv2.imshow("red line",image)
cv2.waitKey(0)


cv2.rectangle(image, (150, 300), (200, 225), red, 5)
cv2.imshow("red_image", image)
cv2.waitKey(0)

blue = (255, 0, 0)
cv2.rectangle(image, (200, 50), (225, 125), blue, -1)
cv2.imshow("blue_rectangle", image)
cv2.waitKey(0)




