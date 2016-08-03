function add_square,img,sq_size,x,y,xmax,ymax

	colour_sq = 255
	begx = round(x - ((sq_size-1)/2))
	begy = round(y - ((sq_size-1)/2))
	if (begx lt 0) then begx = 0
	if (begy lt 0) then begy = 0

	endx = round(x + ((sq_size-1)/2))
	endy = round(y + ((sq_size-1)/2))
	if (endx gt xmax) then endx = xmax
	if (endy gt ymax) then endy = ymax


for i = begx,endx do begin
	for j = begy,endy do begin
		img(i,j) = colour_sq
	endfor
endfor

return,img
end


;----------------------


function gauss, distance, sd
xvalue = distance/sd
yvalue = exp(-1.000*xvalue^2/2.000)/sqrt(2.000*!DPI)
return, yvalue
end



;----------------------



pro heat_maps_v2

in_str			= strarr(1)
temp 			= in_str(0)

xsize = 1280
ysize = 1024

xsize_big = 2202
ysize_big = 1645

big_id0 = 28
big_id1 = 65

n_part = 319
n_img = 150

pick_x = intarr(n_part+1,n_img+1)
pick_x(*,*) = 0

pick_y = intarr(n_part+1,n_img+1)
pick_y(*,*) = 0

pick_big_x = intarr(n_part+1,2)
pick_big_x(*,*) = 0

pick_big_y = intarr(n_part+1,2)
pick_big_y(*,*) = 0

heat = dblarr(xsize,ysize)
heat_big = dblarr(xsize_big,ysize_big)

sum_heat = dblarr(xsize,ysize)
sum_heat(*,*) = 0.000

sum_heat_big = dblarr(xsize_big,ysize_big)
sum_heat_big(*,*) = 0.000

image = bytarr(xsize,ysize)
image_big = bytarr(xsize_big,ysize_big)

image_heat = bytarr(xsize,ysize)
image_heat_big = bytarr(xsize_big,ysize_big)

cd,'R:\attention_modelling\ss373\ageing\eleanor\heat maps'

;     partic  image   x       y
;     319     150     751     411
;0123456789012345678901234567890123456789

openr,1,'heat map idl.prn'

ct = 0
while (EOF(1) eq 0) do begin
ct = ct+1
	readf,1,temp
	temp2 = strtrim(strmid(temp,5,3),2)
	part = fix(temp2)
	temp2 = strtrim(strmid(temp,13,3),2)
	img_no = fix(temp2)
	temp2 = strtrim(strmid(temp,21,3),2)
	x = fix(temp2)
	temp2 = strtrim(strmid(temp,29,3),2)
	y = fix(temp2)
	if (img_no ne big_id0 and img_no ne big_id1) then begin
		if ((x eq 0 and y eq 0) or pick_x(part,img_no) ne 0 or pick_y(part,img_no) ne 0) then begin
			; do nothing
		endif else begin
			pick_x(part,img_no) = x
			pick_y(part,img_no) = y
		endelse
	endif else begin
		if (img_no eq big_id0) then big_index = 0 else big_index = 1
		if ((x eq 0 and y eq 0) or (pick_big_x(part,big_index) ne 0 or pick_big_y(part,big_index) ne 0)) then begin
			; do nothing
		endif else begin
			pick_big_x(part,big_index) = x
			pick_big_y(part,big_index) = y
		endelse
	endelse

;if (ct eq 200) then begin
;	print,temp
;	print,part,img_no,x,y
;	ct = 0
;endif

;if (ct le 25) then begin
;	print,temp
;	print,part,img_no,x,y
;endif

endwhile

close,1

;stop

;		i=1
;		i_str = strtrim(string(i),2)
;		img_file = 'R:\attention_modelling\ss373\ageing\eleanor\heat maps\images\img' + i_str + '.jpg'

;		read_jpeg,img_file,image,/GRAYSCALE
;		print,img_file
;		tv,image
;		write_jpeg,'test.jpg',image

;		read_jpeg,img_file,a,/GRAYSCALE
;		print,img_file
;		tv,a
;		write_jpeg,'test.jpg',a



; add squares

do_squares = 0

if (do_squares) then begin

squaresize = 11

for i = 1, n_img do begin
	i_str = strtrim(string(i),2)
	img_file = 'R:\attention_modelling\ss373\ageing\eleanor\heat maps\images\img' + i_str + '.jpg'
	if (i ne big_id0 and i ne big_id1) then read_jpeg,img_file,image,/GRAYSCALE else read_jpeg,img_file,image_big,/GRAYSCALE
	for j = 1,n_part do begin
		if (i ne big_id0 and i ne big_id1) then begin
			if (pick_x(j,i) ne 0 or pick_y(j,i) ne 0) then begin
				image = add_square(image,squaresize,pick_x(j,i),pick_y(j,i),xsize-1,ysize-1)
			endif
		endif else begin
			if (i eq big_id0) then big_index = 0 else big_index = 1
				if (pick_big_x(j,big_index) ne 0 or pick_big_y(j,big_index) ne 0) then begin
					image_big = add_square(image_big,squaresize,pick_big_x(j,big_index),pick_big_y(j,big_index),xsize_big-1,ysize_big-1)
				endif
		endelse
	endfor
	out_img_file = 'R:\attention_modelling\ss373\ageing\eleanor\heat maps\images sq\img' + i_str + 'sq.jpg'
	write_jpeg,out_img_file,image
endfor

endif

; make heat maps

start_i = 28

sd_blur = 50.00
big_factor = 1.663
sd_blur_big = sd_blur * big_factor

for i = start_i, n_img do begin
	sum_heat(*,*) = 0.000
	sum_heat_big(*,*) = 0.000
	i_str = strtrim(string(i),2)
	img_file = 'R:\attention_modelling\ss373\ageing\eleanor\heat maps\images\img' + i_str + '.jpg'
	if (i ne big_id0 and i ne big_id1) then read_jpeg,img_file,image,/GRAYSCALE else read_jpeg,img_file,image_big,/GRAYSCALE
	for j = 1,n_part do begin
		heat(*,*) = 0.000
		heat_big(*,*) = 0.000
		if (i ne big_id0 and i ne big_id1) then begin
			if (pick_x(j,i) ne 0 or pick_y(j,i) ne 0) then begin
				x = double(pick_x(j,i))
				y = double(pick_y(j,i))
				for k = 0,xsize -1 do begin
					for l = 0,ysize -1 do begin
						dist = sqrt( (x-double(k))^2 + (y-double(l))^2)
						heat(k,l) = gauss(dist,sd_blur)
					endfor
				endfor
				sum_heat(*,*) = sum_heat(*,*) + heat(*,*)
			endif
		endif else begin
			if (i eq big_id0) then big_index = 0 else big_index = 1
			if (pick_big_x(j,big_index) ne 0 or pick_big_y(j,big_index) ne 0) then begin
				x = double(pick_big_x(j,big_index))
				y = double(pick_big_y(j,big_index))
				for k = 0,xsize_big -1 do begin
					for l = 0,ysize_big -1 do begin
						dist = sqrt( (x-double(k))^2 + (y-double(l))^2)
						heat_big(k,l) = gauss(dist,sd_blur_big)
					endfor
				endfor
				sum_heat_big(*,*) = sum_heat_big(*,*) + heat_big(*,*)
			endif
		endelse
	endfor

	if (i ne big_id0 and i ne big_id1) then begin
		; normalise max to 1
		maxvalue = max(sum_heat)
		sum_heat = sum_heat/maxvalue
		heat_file = 'R:\attention_modelling\ss373\ageing\eleanor\heat maps\images heat maps\img' + i_str + 'heat.dat'
		openw,1,heat_file
		writeu,1,sum_heat
		close,1
		image_heat(*,*) = byte(sum_heat(*,*)*255.00)
		heatmap_file = 'R:\attention_modelling\ss373\ageing\eleanor\heat maps\images heat maps\img' + i_str + 'heat.jpg'
		write_jpeg,heatmap_file,image_heat
		for k = 0,xsize -1 do begin
			for l = 0,ysize -1 do begin
				image(k,l) = byte(double(image(k,l))*sum_heat(k,l))
			endfor
		endfor
		heatmapped_file = 'R:\attention_modelling\ss373\ageing\eleanor\heat maps\images heatmapped\img' + i_str + 'heatmapped.jpg'
		write_jpeg,heatmapped_file,image
	endif else begin
		; normalise max to 1
		maxvalue = max(sum_heat_big)
		sum_heat_big = sum_heat_big/maxvalue
		heat_file = 'R:\attention_modelling\ss373\ageing\eleanor\heat maps\images heat maps\img' + i_str + 'heat.dat'
		openw,1,heat_file
		writeu,1,sum_heat_big
		close,1
		image_heat_big(*,*) = byte(sum_heat_big(*,*)*255.00)
		heatmap_file = 'R:\attention_modelling\ss373\ageing\eleanor\heat maps\images heat maps\img' + i_str + 'heat.jpg'
		write_jpeg,heatmap_file,image_heat_big
		for k = 0,xsize_big -1 do begin
			for l = 0,ysize_big -1 do begin
				image_big(k,l) = byte(double(image_big(k,l))*sum_heat_big(k,l))
			endfor
		endfor
		heatmapped_file = 'R:\attention_modelling\ss373\ageing\eleanor\heat maps\images heatmapped\img' + i_str + 'heatmapped.jpg'
		write_jpeg,heatmapped_file,image_big
	endelse

endfor
close,/all

stop
end
