def add_square(img_f,sq_size,x,y,xmax,ymax):
    colour_sq = 255
    begx = int(round(x - ((sq_size-1)/2)))
    begy = int(round(y - ((sq_size-1)/2)))
    if (begx < 0):
        begx = 0
    if (begy < 0):
        begy = 0

    endx = int(round(x + ((sq_size-1)/2)))
    endy = int(round(y + ((sq_size-1)/2)))
    if (endx > xmax):
        endx = xmax
    if (endy > ymax):
        endy = ymax

    for i in range(begx,endx):
        for j in range(begy,endy):
            img_f[i,j] = colour_sq

    return img_f

def gauss(distance, sd):
    xvalue = distance/sd
    yvalue = math.exp(-1.000*xvalue**2/2.000)/math.sqrt(2.000*math.pi)
    return yvalue
    
import math
import numpy as np
#import re
import string

xsize = 1280
ysize = 1024

xsize_big = 2202
ysize_big = 1645

big_id0 = 28
big_id1 = 65

n_part = 319
n_img = 150    

pick_x = np.zeros((n_part+1,n_img+1),  dtype=np.int_) # +1 to match id's and img's starting at 1
pick_y = np.zeros((n_part+1,n_img+1),  dtype=np.int_)
   
pick_big_x = np.zeros((n_part+1,2),  dtype=np.int_)
pick_big_y = np.zeros((n_part+1,2),  dtype=np.int_)

#heat = np.zeros((xsize,ysize), dtype=np.float64)
#heat_big = np.zeros((xsize_big,ysize_big), dtype=np.float64)
    
#sum_heat = np.zeros((xsize,ysize), dtype=np.float64)
#sum_heat_big = np.zeros((xsize_big,ysize_big), dtype=np.float64)

#image = np.zeros((xsize,ysize), dtype=np.uint8)
#image_big = np.zeros((xsize_big,ysize_big), dtype=np.uint8)

#image_heat = np.zeros((xsize,ysize), dtype=np.uint8)
#image_heat_big = np.zeros((xsize_big,ysize_big), dtype=np.uint8)

img_dir = 'C:\Users\ibshi\Desktop\data incubator\eleanor\heat maps\images'

infile = open('C:\Users\ibshi\Desktop\data incubator\interview\heat map data.txt','r')

for temp in infile:
    if (temp.strip('\n') ==''):
        continue
    temp2 = string.split(temp)
    part = int(temp2[0])
    img_no = int(temp2[1])
    x = int(temp2[2])
    y = int(temp2[3])
    
    if (img_no <> big_id0 and img_no <> big_id1):

        if ((x == 0 and y == 0) or pick_x[part,img_no] <> 0 or pick_y[part,img_no] <> 0):
            continue # do nothing
        else:
            pick_x[part,img_no] = x
            pick_y[part,img_no] = y

    else:

        if (img_no == big_id0): 
            big_index = 0 
        else:
            big_index = 1

        if ((x == 0 and y == 0) or (pick_big_x[part,big_index] <> 0 or pick_big_y[part,big_index] <> 0)):
            continue #do nothing
        else:
            pick_big_x[part,big_index] = x
            pick_big_y[part,big_index] = y
		
infile.close() 

b_img = 1
e_img = 3
#e_img = n_img
from scipy import misc
squaresize = 11

do_squares = 1

if (do_squares):
    
    for i in range(b_img,e_img):
        i_str = str(i)
        img_file = img_dir + "\img" + i_str + '.jpg'
        img = misc.imread(img_file,mode = 'L')
        for j in range(1,n_part):
            if (i <> big_id0 and i <> big_id1):
                if (pick_x[j,i] <> 0 or pick_y[j,i] <> 0):
                    img = add_square(img,squaresize,ysize-pick_y[j,i],pick_x[j,i],ysize,xsize)
            else:
                if (i == big_id0):
                    big_index = 0 
                else:
                    big_index = 1
                if (pick_big_x[j,big_index] <> 0 or pick_big_y[j,big_index] <> 0):
                    img = add_square(img,squaresize,ysize_big-pick_big_y[j,big_index],pick_big_x[j,big_index],ysize_big,xsize_big)
        out_img_file = "C:\Users\ibshi\Desktop\data incubator\interview\images sq\img" + i_str + 'sq.jpg'
        misc.imsave(out_img_file, img)

# make heat maps

sd_blur = 50.00
big_factor = 1.663
sd_blur_big = sd_blur * big_factor

for i in range(b_img,e_img):
    sum_heat = np.zeros((ysize,xsize), dtype=np.float64)
    sum_heat_big = np.zeros((ysize_big,xsize_big), dtype=np.float64)
    i_str = str(i)
    img_file = img_dir + "\img" + i_str + '.jpg'
    img = misc.imread(img_file,mode = 'L')
    for j in range(1,n_part):
        heat = np.zeros((ysize,xsize), dtype=np.float64)
        heat_big = np.zeros((ysize_big,xsize_big), dtype=np.float64)

        if (i <> big_id0 and i <> big_id1):
            if (pick_x[j,i] <> 0 or pick_y[j,i] <> 0):
                x = np.float64(pick_x[j,i])
                y = ysize - np.float64(pick_y[j,i])
                for k in range(0,xsize -1):
                    for l in range(0,ysize -1):
						dist = math.sqrt( (x-np.float64(k))**2 + (y-np.float64(l))**2)
						heat[l,k] = gauss(dist,sd_blur)
                sum_heat = sum_heat + heat

        else:
            if (i == big_id0):
                big_index = 0 
            else:
                big_index = 1
            if (pick_big_x[j,big_index] <> 0 or pick_big_y[j,big_index] <> 0):
                x = np.float64(pick_x[j,i])
                y = ysize - np.float64(pick_y[j,i])
                for k in range(0,xsize -1):
                    for l in range(0,ysize -1):
						dist = math.sqrt( (x-np.float64(k))**2 + (y-np.float64(l))**2)
						heat_big[l,k] = gauss(dist,sd_blur)
                sum_heat_big = sum_heat_big + heat_big
        
    if (i <> big_id0 and i <> big_id1):
        maxvalue = np.amax(sum_heat)
        sum_heat = sum_heat/maxvalue
        image_heat = np.uint8(sum_heat*255.00)
        heatmap_file = 'C:\Users\ibshi\Desktop\data incubator\interview\images sq\img' + i_str + 'heat.jpg'
        misc.imsave(heatmap_file, image_heat)
        img = np.uint8(np.float64(img*sum_heat))
        heatmapped_file = 'C:\Users\ibshi\Desktop\data incubator\interview\images sq\img' + i_str + 'heatmapped.jpg'
        misc.imsave(heatmapped_file, img)
    else:
        maxvalue = np.amax(sum_heat_big)
        sum_heat_big = sum_heat_big/maxvalue
        image_heat_big = np.uint8(sum_heat_big*255.00)
        heatmap_file = 'C:\Users\ibshi\Desktop\data incubator\interview\images sq\img' + i_str + 'heat.jpg'
        misc.imsave(heatmap_file, image_heat)
        img = np.uint8(np.float64(img*sum_heat_big))
        heatmapped_file = 'C:\Users\ibshi\Desktop\data incubator\interview\images sq\img' + i_str + 'heatmapped.jpg'
        misc.imsave(heatmapped_file, img)
            
           
                        

        
                
              
