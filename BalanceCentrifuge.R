#READ THIS FIRST!
#You can run this by downloading R (https://www.r-project.org/).
#Copy everything you see here to a text editor. 
#Edit the variables immediately below, but do not change the rest. 
#Select all the text in the file (ctrl+A or command+A), 
#and paste it all into R were you see the ">" sign.
#It will write a PNG file to your computer in the location you specify

#Assumes centrifuges with an even number of slots (6, 12, 20, 24, or 30 slots)

#EDIT BELOW: These are useful paramters you may want/need to change.
local_path_to_save_file="/Users/dlundberg/Documents/centrifuge_diagram.png"   #Edit the path to save the file and the file name
slots_in_centrifuge=30  #default = 30, assumes standard centrifuges and even number of slots (6, 12, 20, 24, or 30 slots)
rows_in_picture=5       #default = 5,   make sure rows*columns >= slots in centrifuge. Otherwise error.
columns_in_picture=6    #default = 6
slot_circle_size=15    #default = 15
text_size=20 		 #default = 20
size_slot_outline=5    #default = 5


#Do not edit anything below this sentence.







#############################################################
png(local_path_to_save_file, height = rows_in_picture*1200, width = columns_in_picture*1200, units = "px", res = 100)

par(mfrow=c(rows_in_picture,columns_in_picture))

#plot the first case, 1 tube, which is impossible
plot(0, 0, pch="n", xlim=c(-1.2, 1.2), ylim=c(-1.2, 1.2), xaxt="n", yaxt="n", xlab="", ylab="", type="n")
text(0, 0, 1, cex=text_size)
points(0, 0, pch=4, cex=200,  lwd = 5)


#fill in slots
for(tubes in 2:(slots_in_centrifuge-2)){


	#make a plot and put the number of tubes in the middle of the plot
	plot(0, 0, pch="n", xlim=c(-1.2, 1.2), ylim=c(-1.2, 1.2), xaxt="n", yaxt="n", xlab="", ylab="", type="n")
	text(0, 0, tubes, cex=text_size)
	
	
	
	#if divisible by 2, and number of slots in centrifuge also divisible by 2, always split into a top and bottom set and plot
	if(tubes%%2 == 0 & slots_in_centrifuge%%2 == 0){
		#First make a circle with empty slots
		for(slot in 1:slots_in_centrifuge){
			a=slot*pi/(slots_in_centrifuge/2)
			points(x = 0 + 1 * cos(a), y = 0 + 1 * sin(a), cex=slot_circle_size, lwd=size_slot_outline)
		}
		#now fill the appropriate slots
		top=1:(tubes/2)
		bottom=((slots_in_centrifuge/2)+1):((slots_in_centrifuge/2)+(tubes/2))
		c(top, bottom)->filled		
		for(f in filled){
			a=f*pi/(slots_in_centrifuge/2)
			points(x = 0 + 1 * cos(a), y = 0 + 1 * sin(a), pch=16, cex=slot_circle_size, col="blue")
		}
	}
	
	
	
	#if only divisible by 3, and the centrifuge has a number of slots divisible by 3, split into thirds.
	if(tubes%%3 == 0 & tubes%%2 > 0 & slots_in_centrifuge%%3 == 0){
		#First make a circle with empty slots
		for(slot in 1:slots_in_centrifuge){
			a=slot*pi/(slots_in_centrifuge/2)
			points(x = 0 + 1 * cos(a), y = 0 + 1 * sin(a), cex=slot_circle_size, lwd=size_slot_outline)
		}
		#now fill the appropriate slots
		batch1=1:(tubes/3)
		batch2=((1*slots_in_centrifuge/3)+1):((1*slots_in_centrifuge/3)+(tubes/3))
		batch3=((2*slots_in_centrifuge/3)+1):((2*slots_in_centrifuge/3)+(tubes/3))
		c(batch1, batch2, batch3)->filled
		for(f in filled){
			a=f*pi/(slots_in_centrifuge/2)
			points(x = 0 + 1 * cos(a), y = 0 + 1 * sin(a), pch=16, cex=slot_circle_size, col="red")
		}
	}
	
	
	
	#For the rest, divide first by 3, then fill in remainder by 2. (as long as total slots divisible by 2 and 3)
	if(tubes%%3 > 0 & tubes%%2 > 0 & slots_in_centrifuge%%3 == 0 & slots_in_centrifuge%%2 == 0){
		
		#First make a circle with empty slots
		for(slot in 1:slots_in_centrifuge){
			a=slot*pi/(slots_in_centrifuge/2)
			points(x = 0 + 1 * cos(a), y = 0 + 1 * sin(a), cex=slot_circle_size, lwd=size_slot_outline)
		}
		#now fill the appropriate slots
		#first fill a group of 3
		batch1=1
		batch2=((1*slots_in_centrifuge/3)+1)
		batch3=((2*slots_in_centrifuge/3)+1)
		c(batch1, batch2, batch3)->filled3
		for(f in filled3){
			a=f*pi/(slots_in_centrifuge/2)
			points(x = 0 + 1 * cos(a), y = 0 + 1 * sin(a), pch=16, cex=slot_circle_size, col="red")
		}
		tubes-3 -> tubes
	
		#now fill the remaining by two, but ignore those which are "filled"
		pos=2
		while(tubes>0){
			top=pos
			bottom=pos+(slots_in_centrifuge/2)
			c(top, bottom)->filled2
			if(sum(match(filled2, filled3, nomatch=0))==0){
				for(f in filled2){
					a=f*pi/(slots_in_centrifuge/2)
					points(x = 0 + 1 * cos(a), y = 0 + 1 * sin(a), pch=16, cex=slot_circle_size, col="blue")
				}
			tubes=tubes-2
			}
		pos+1->pos
		}
	}
	
	
	#if it's not possible on the centrifuge, put an X.  
	#if the number of tubes is only cleanly divisible by 3, or is not cleanly divisible 
	#by 3 or 2, and if the centrifuge
	# doesn't have slots cleanly divisible by 3, it is impossible
	if(tubes%%3>=0 & tubes%%2 > 0 & slots_in_centrifuge%%3 > 0){
		points(0, 0, pch=4, cex=200,  lwd = 5)
	}

}

#plot the second to last case as an X, which is also impossible
plot(0, 0, pch="n", xlim=c(-1.2, 1.2), ylim=c(-1.2, 1.2), xaxt="n", yaxt="n", xlab="", ylab="", type="n")
text(0, 0,slots_in_centrifuge-1, cex=text_size)
points(0, 0, pch=4, cex=200,  lwd = 5)

#plot the last case, in which all are filled
plot(0, 0, pch="n", xlim=c(-1.2, 1.2), ylim=c(-1.2, 1.2), xaxt="n", yaxt="n", xlab="", ylab="", type="n")
	for(slot in 1:slots_in_centrifuge){
		a=slot*pi/(slots_in_centrifuge/2)
		points(x = 0 + 1 * cos(a), y = 0 + 1 * sin(a), cex=slot_circle_size, lwd=size_slot_outline)
	}
1:slots_in_centrifuge->filled
	for(f in filled){
		a=f*pi/(slots_in_centrifuge/2)
		points(x = 0 + 1 * cos(a), y = 0 + 1 * sin(a), pch=16, cex=slot_circle_size, col="blue")
	}
text(0, 0, slots_in_centrifuge, cex=text_size)
title("Derek S Lundberg, Twitter @DerekSeveri", cex.main=4)

if(rows_in_picture*columns_in_picture<slots_in_centrifuge){
	print("ERROR: not enough rows and columns to show every possibility")
	par(mfrow=c(1,1))
	plot(0, 0, pch="n", xlim=c(-1.2, 1.2), ylim=c(-1.2, 1.2), xaxt="n", yaxt="n", xlab="", ylab="", type="n")
	text(0, 0, "ERROR: not enough rows and columns to show every possibility", cex=4)
}
if(slots_in_centrifuge%%2>0){
	print("ERROR: odd number of slots in centrifuge")
	par(mfrow=c(1,1))
	plot(0, 0, pch="n", xlim=c(-1.2, 1.2), ylim=c(-1.2, 1.2), xaxt="n", yaxt="n", xlab="", ylab="", type="n")
	text(0, 0, "ERROR: odd number of slots in centrifuge", cex=4)
}

dev.off()

