pro table_driving,ss,dp_young,dp_old,w_young,w_old,w_out

;s = 2

;prelims

priorD  = 0.50
priorT  = 0.50

num_iter = 50000.00

start_dp = 0.00
end_dp = 4.00
step_dp = 0.01
num_dp = round((end_dp-start_dp)/step_dp)+1

start_crit = -4.00
end_crit = 4.00
step_crit = 0.01
num_crit = round((end_crit-start_crit)/step_crit)+1

fa_out = dblarr(num_dp,num_crit)
hit_out = dblarr(num_dp,num_crit)

temp_fa = dblarr(num_crit)
temp_hit = dblarr(num_crit)

fa_ct = lon64arr(num_crit)
hit_ct = lon64arr(num_crit)

crit_value = dblarr(num_crit)

for i = 0,num_crit-1 do begin
crit_value(i) = start_crit + double(i)*step_crit
endfor

print_counter = 20
half_loc = round(double(ss)/2.00)

; begin

for dp = 0,num_dp-1  do begin

if (dp mod print_counter eq 0) then begin
openw,1,'counter.txt'
printf,1,dp
close,1
endif

dp_value = start_dp + double(dp)*step_dp
fa_ct(*)  = 0
hit_ct(*) = 0

for iter = 0.00,num_iter-1.00 do begin

orig_values = randomn(seed,ss)

;add dp_values for distractors (in this case, distractors are -dp_value and dp-value, half and half in absent trials, one is replaced in present trials. target mean = 0)

for i = 0,half_loc-1 do begin
orig_values(i) = orig_values(i) + dp_value
endfor

for i = half_loc,ss-1 do begin
orig_values(i) = orig_values(i) - dp_value
endfor

	;signal present

test_values = orig_values

; choose one random location to be target (mean = 0)
choose = round(ss*randomu(seed)-0.500)
if (choose lt half_loc) then test_values(choose) = test_values(choose) - dp_value else test_values(choose) = test_values(choose) + dp_value


;p(T)

pT = 0
	for i = 0, ss - 1 do begin ; go through each location, calculate prob of target at that location and prob of no target at other locations
		for j = 0, num_combs - 1 do begin ;go through each combination of distractor locations
			temp2 = 1.000
			for k = 0, ss - 1 do begin ; calculate prob for each location k and multiply together
				if (k eq i) then begin ; prob of target at location i.
					very_temp = test_values(k)
					temp = (1.000/sqrt(2.000*!DPI))*exp(-1.000*very_temp^2/2.0000)
				endif else begin ; prob of distractor for combination j, location k
					very_temp = test_values(k) - dp_value*stim_matrix(j,k)
					temp = (1.000/sqrt(2.000*!DPI))*exp(-1.000*very_temp^2/2.0000)
				endelse
				temp2 = temp2*temp
			endfor ; k
		pT = pT + temp2
		endfor ; j
	endfor ; i
pT = priorT*pT
; weight each location equally as the prior prob of each location having the target is the same in this task
weight = 1.000/double(ss)
pT = weight*pT

;p(D)
pD = 0
		for j = 0, num_combs - 1 do begin ;go through each combination of distractor locations
			temp2 = 1.000
			for k = 0, ss - 1 do begin ; calculate prob for each location k and multiply together
				very_temp = test_values(k) - dp_value*stim_matrix(j,k)
				temp = (1.000/sqrt(2.000*!DPI))*exp(-1.000*very_temp^2/2.0000)
				temp2 = temp2*temp
			endfor ; k
		pD = pd + temp2
		endfor ; j
pD = priorD*pD

;

l_yes = pT
l_no = pD

;YYY

		;test crit
test_value = l_yes/l_no
test_value = alog(test_value)
for i = 0,num_crit-1 do begin
if (test_value ge crit_value(i)) then hit_ct(i) = hit_ct(i) + 1
endfor

	;signal absent

test_values(*) = orig_values(*)

	;same as above for signal present

;p(T)

pT = 0
	for i = 0, ss - 1 do begin ; go through each location, calculate prob of target at that location and prob of no target at other locations
		for j = 0, num_combs - 1 do begin ;go through each combination of distractor locations
			temp2 = 1.000
			for k = 0, ss - 1 do begin ; calculate prob for each location k and multiply together
				if (k eq i) then begin ; prob of target at location i.
					very_temp = test_values(k)
					temp = (1.000/sqrt(2.000*!DPI))*exp(-1.000*very_temp^2/2.0000)
				endif else begin ; prob of distractor for combination j, location k
					very_temp = test_values(k) - dp_value*stim_matrix(j,k)
					temp = (1.000/sqrt(2.000*!DPI))*exp(-1.000*very_temp^2/2.0000)
				endelse
				temp2 = temp2*temp
			endfor ; k
		pT = pT + temp2
		endfor ; j
	endfor ; i
; weight each location equally as the prior prob of each location having the target is the same in this task
weight = 1.000/double(ss)
pT = weight*pT
pT = priorT*pT

;p(D)
pD = 0
		for j = 0, num_combs - 1 do begin ;go through each combination of distractor locations
			temp2 = 1.000
			for k = 0, ss - 1 do begin ; calculate prob for each location k and multiply together
				very_temp = test_values(k) - dp_value*stim_matrix(j,k)
				temp = (1.000/sqrt(2.000*!DPI))*exp(-1.000*very_temp^2/2.0000)
				temp2 = temp2*temp
			endfor ; k
		pD = pd + temp2
		endfor ; j
pD = priorD*pD

;

l_yes = pT
l_no = pD

;YYY

		;test crit
test_value = l_yes/l_no
test_value = alog(test_value)
for i = 0,num_crit-1 do begin
if (test_value ge crit_value(i)) then fa_ct(i) = fa_ct(i) + 1
endfor

endfor ; iter

for i = 0,num_crit - 1 do begin

temp_hit(*) = double(hit_ct(*))/double(num_iter)
temp_fa(*)  = double(fa_ct(*))/double(num_iter)

hit_out(dp,*) = temp_hit(*)
fa_out(dp,*)  = temp_fa(*)
endfor

endfor ; dp

ss_str = strtrim(string(ss),2)
file_out = 'table_2distractors_'+ss_str+'.dat'
openw,1,file_out
writeu,1,hit_out
writeu,1,fa_out
close,1

end
