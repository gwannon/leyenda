 player1:
 %01101100
 %01101100
 %00100100
 %00100100
 %00011000
 %00011000
 %11011011
 %11111111
 %00011000
 %00100100
 %01111110
 %01000010
end

 score = 0

 COLUBK = $04
 COLUPF = $C0

 dim nodown = a
 dim noup = b
 dim noleft = c
 dim noright = d
 dim room = e
 dim haslance = f
 dim hasshield = g
 dim hascoin = h
 dim randnumber = i
 dim coinvalue = j
 dim compass = k
 dim dead = l

 haslance = 0
 hasshield = 0
 hascoin = 0
 coinvalue = 1
 dead = 0

 room = 0
 nodown = 0
 noup = 0
 noleft = 0
 noright = 0

 player0x = 24
 player0y = 76

 player1x = 118
 player1y = 28

 missile0height = 8
 missile0x = 83
 missile0y = 48

 missile1height = 1
 missile1x = 120
 missile1y = 20	

 ballheight = 4
 CTRLPF = $21
 ballx = 0
 bally = 0

mainloop

 if room = 0 then gosub room1 : gosub moverderecha 
 const screenheight=84

 NUSIZ1 = $10

 COLUP0 = $86
 COLUP1 = $4A 
 if coinvalue = 5 then COLUP1 = $0A 
 if coinvalue = 32 then COLUP1 = $1E 

 AUDV0 = 0

 if collision(player1,player0) && hasshield = 1 then gosub boing : hasshield = 0 : ballx = 0 : bally = 0  
 if collision(player1,player0) && hasshield = 0 then drawscreen : goto mainloop

 if joy0left && !joy0right && !joy0up && !joy0down && noleft = 0 then gosub moverizquierda
 if !joy0left && joy0right && !joy0up && !joy0down && noright = 0 then gosub moverderecha
 if !joy0left && !joy0right && joy0up && !joy0down && noup = 0 then gosub moverarriba
 if !joy0left && !joy0right && !joy0up && joy0down && nodown = 0 then gosub moverabajo
 if joy0fire && haslance = 1 then haslance = 2

 gosub moverenemigo

 if haslance = 2 && !collision(playfield,missile0) then gosub moverlanza
 if haslance = 2 && collision(playfield,missile0) && compass = 2 then haslance = 3 : missile0x = missile0x - 0  
 if haslance = 2 && collision(playfield,missile0) then haslance = 3 
 if haslance = 2 && collision(player1,missile0) then haslance = 3 

 if compass = 1 && haslance = 3 then NUSIZ0 = $00 : missile0height = 8
 if compass = 2 && haslance = 3 then NUSIZ0 = $30 : missile0height = 0
 if compass = 3 && haslance = 3 then NUSIZ0 = $00 : missile0height = 8
 if compass = 4 && haslance = 3 then NUSIZ0 = $30 : missile0height = 0

 if collision(missile0,player0) && haslance = 0 then haslance = 1 : NUSIZ0 = $00 : missile0height = 8 
 if collision(missile0,player0) && haslance = 3 then haslance = 1 : NUSIZ0 = $00 : missile0height = 8 
 if collision(ball,player0) && hasshield = 0 then hasshield = 1
 if collision(missile1,player0) && hascoin = 0 then hascoin = 1 : score = score + coinvalue : gosub colocarmoneda 

 if room = 1 && haslance = 0 then missile0x = 83 : missile0y = 48
 if room <> 1 && haslance = 0 then missile0x = 0 : missile0y = 0

 if room = 2 && hasshield = 0 then ballx = 83 : bally = 45
 if room <> 2 && hasshield = 0 then ballx = 0 : bally = 0

 if room = 1 && player0x > 145 then gosub room2	: player0x = 22 
 if room = 2 && player0x < 5 then gosub room1 : player0x = 140 	

 if room = 1 && player0y < 10 then gosub room3 : player0y = 80 	
 if room = 3 && player0y > 85 then gosub room1 : player0y = 10 	

 if room = 3 && player0x > 145 then gosub room4	: player0x = 22 
 if room = 4 && player0x < 5 then gosub room3 : player0x = 140 	
 
 if room = 2 && player0y < 5 then gosub room4 : player0y = 80 	
 if room = 4 && player0y > 85 then gosub room2 : player0y = 10 	

 if room = 3 && player0y < 10 then gosub room5 : player0y = 80 	
 if room = 5 && player0y > 85 then gosub room3 : player0y = 10 	

 if room = 4 && player0x > 145 then gosub room6	: player0x = 22 
 if room = 6 && player0x < 5 then gosub room4 : player0x = 140 	

 if room = 6 && player0x > 145 then gosub room7	: player0x = 22 
 if room = 7 && player0x < 5 then gosub room6 : player0x = 140 	

 if room = 7 && player0x > 145 then gosub room9	: player0x = 22 
 if room = 9 && player0x < 5 then gosub room7 : player0x = 140 	
 
 if room = 8 && player0y < 5 then gosub room7 : player0y = 80 	
 if room = 7 && player0y > 85 then gosub room8 : player0y = 10 

 drawscreen
 goto mainloop

moverizquierda
 player0:
 %01101100
 %00100100
 %00100100
 %00011000
 %11111111
 %10011001
 %00100100
 %00111100
 %00110110
end

 if haslance = 1 then compass = 4 
 if collision(playfield,player0) then player0x = player0x + 1 : noright = 0 : noleft = 1 : noup = 0 : nodown = 0 else player0x = player0x - 1 : noright = 0 : noleft = 0 : noup = 0 : nodown = 0
 if haslance = 1 then missile0x = player0x : missile0y = player0y - 2
 if hasshield = 1 then ballx = player0x + 7 : bally = player0y - 3
 return

moverderecha
 player0:
 %00110110
 %00100100
 %00100100
 %00011000
 %11111111
 %10011001
 %00100100
 %00111100
 %01101100
end
 
 if haslance = 1 then compass = 2
 if collision(playfield,player0) then player0x = player0x - 1 : noright = 1 : noleft = 0 : noup = 0 : nodown = 0 else player0x = player0x + 1: noright = 0 : noleft = 0 : noup = 0 : nodown = 0
 if haslance = 1 then missile0x = player0x + 9 : missile0y = player0y - 2
 if hasshield = 1 then ballx = player0x - 1  : bally = player0y - 3
 return

moverarriba
 player0:
 %01100110
 %00100100
 %00100100
 %00011000
 %11111111
 %10011001
 %00100100
 %00111100
 %00100100
end

 if haslance = 1 then compass = 1
 if collision(playfield,player0) then player0y = player0y + 1 : noright = 0 : noleft = 0 : noup = 1 : nodown = 0 else player0y = player0y - 1: noright = 0 : noleft = 0 : noup = 0 : nodown = 0
 if haslance = 1 then missile0x = player0x : missile0y = player0y - 2
 if hasshield = 1 then ballx = player0x + 7 : bally = player0y - 3
 return

moverabajo
 player0:
 %01100110
 %00100100
 %00100100
 %10011001
 %11111111
 %00011000
 %00100100
 %00111100
 %00100100
end

 if haslance = 1 then compass = 3
 if collision(playfield,player0) then player0y = player0y - 1 : noright = 0 : noleft = 0 : noup = 0 : nodown = 1 else player0y = player0y + 1: noright = 0 : noleft = 0 : noup = 0 : nodown = 0
 if haslance = 1 then missile0x = player0x : missile0y = player0y - 2
 if hasshield = 1 then ballx = player0x + 7 : bally = player0y - 3
 return

boing
 if compass = 1 then player0x = player0y - 20 
 if compass = 2 then player0x = player0x - 20 
 if compass = 3 then player0x = player0y + 20 
 if compass = 4 then player0x = player0x + 20 
 return

colocarmoneda
 if hascoin = 1 then missile1x = 0 : missile1y = 0 : player1x = 0 : player1y = 0

 randnumber = rand
 if hascoin = 0 && randnumber <= 153 then coinvalue = 1 
 if hascoin = 0 && randnumber > 153 && randnumber <= 204 then coinvalue = 5 
 if hascoin = 0 && randnumber > 204 && randnumber <= 255 then coinvalue = 32

 randnumber = rand
 if hascoin = 0 && randnumber <= 51 then player1x = 28 : player1y = 22
 if hascoin = 0 && randnumber > 51 && randnumber <= 102 then player1x = 118 : player1y = 22
 if hascoin = 0 && randnumber > 102 && randnumber <= 153 then player1x = 28 : player1y = 77
 if hascoin = 0 && randnumber > 153 && randnumber <= 204 then player1x = 118 : player1y = 77
 if hascoin = 0 && randnumber > 204 && randnumber <= 255 then player1x = 85 : player1y = 50
 return

moverlanza
 if compass = 1 then NUSIZ0 = $00 : missile0height = 8 : missile0y = missile0y - 2
 if compass = 2 then NUSIZ0 = $30 : missile0height = 0 : missile0x = missile0x + 2
 if compass = 3 then NUSIZ0 = $00 : missile0height = 8 : missile0y = missile0y + 2
 if compass = 4 then NUSIZ0 = $30 : missile0height = 0 : missile0x = missile0x - 2
 return

moverenemigo
 if player1y = 22 && player1x < 118 then player1x = player1x + 1 
 if player1y = 77 && player1x > 28 then player1x = player1x - 1
 if player1x = 118 && player1y < 77 then player1y = player1y + 1
 if player1x = 28 && player1y > 22 then player1y = player1y - 1
 missile1x = player1x + 4
 missile1y = player1y - 4
 return

room1
 room = 1
 hascoin = 0
 COLUPF = $C0
 playfield:
 XXXXXXXXXXXXX.......XXXXXXXXXXXX
 X..............................X
 X..............................X
 X............XXXXXXX...........X
 X..................X............
 X..................X............
 X..................X............
 X............XXXXXXX...........X
 X..............................X
 X..............................X
 XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
end
 if haslance = 1 then gosub colocarmoneda else hascoin = 1 : gosub colocarmoneda
 if haslance = 3 then haslance = 0 : NUSIZ0 = $00 : missile0height = 8 

 return

room2
 room = 2
 hascoin = 0
 COLUPF = $C0
 playfield:
 XXXXXXXXXXXXX.......XXXXXXXXXXXX
 X..............................X
 X..............................X
 X............XXXXXXX...........X
 .............X.................X
 .............X.................X
 .............X.................X
 X............XXXXXXX...........X
 X..............................X
 X..............................X
 XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
end
 if hasshield = 1 then gosub colocarmoneda else hascoin = 1 : gosub colocarmoneda
 if haslance = 3 then haslance = 0 : NUSIZ0 = $00 : missile0height = 8 
 return

room3
 room = 3
 hascoin = 0
 COLUPF = $C0
 playfield:
 X......XXXXXXXXX.......XXXXXXXXX  
 X..............................X
 X..............................X
 X............XXXXXXX...........X
 X...............................
 X...............................
 X...............................
 X............XXXXXXX...........X
 X..............................X
 X..............................X
 XXXXXXXXXXXXX.......XXXXXXXXXXXX
end
 gosub colocarmoneda
 if haslance = 3 then haslance = 0 : NUSIZ0 = $00 : missile0height = 8 
 return

room4
 room = 4
 hascoin = 0
 COLUPF = $C0
 playfield:
 XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
 X..............................X
 X..............................X
 X...........X.......X..........X
 ............X.......X...........
 ............X.......X...........
 ............X.......X...........
 X...........X.......X..........X
 X..............................X
 X..............................X
 XXXXXXXXXXXXX.......XXXXXXXXXXXX 
end
 gosub colocarmoneda
 if haslance = 3 then haslance = 0 : NUSIZ0 = $00 : missile0height = 8 
 return

room5
 room = 5
 hascoin = 0
 COLUPF = $40
 playfield:
 XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
 X..............................X
 X..............................X
 X..............................X
 X......XXXXXXXXXXXXXXXXXXXXXXXXX
 X......X.......................X
 X......X.......................X
 X......X.......................X
 X......X.......................X
 X......X.......................X
 X......XXXXXXXXX.......XXXXXXXXX 
end
 gosub colocarmoneda
 if haslance = 3 then haslance = 0 : NUSIZ0 = $00 : missile0height = 8 
 return

room6
 room = 6
 hascoin = 1
 COLUPF = $A0
 playfield:
 XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
 XXXXXXXXXXXXXXXXXXXXXXXX........
 XXXXXXXXXXXXXXXXXXXXXXXX........
 XXXXXXXXXXXXXXXXXXXXXXXX........
 .........XXXXXXXXXXXXXXX.......X
 .........XXXXXXXXXXXXXXX.......X
 .........XXXXXXXXXXXXXXX.......X
 X..............................X
 X..............................X
 X..............................X
 XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX 
end
 gosub colocarmoneda
 if haslance = 3 then haslance = 0 : NUSIZ0 = $00 : missile0height = 8 
 return

room7
 room = 7
 hascoin = 1
 COLUPF = $A0
 playfield:
 XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
 ....................XXXXXXXXXXXX
 ....................XXXXXXXXXXXX
 ....................XXXXXXXXXXXX
 XXXXXXXXXXXXX..................X
 XXXXXXXXXXXXX..................X
 XXXXXXXXXXXXX..................X
 XXXXXXXXXXXXX.......XXXXXXXXXXXX
 XXXXXXXXXXXXX.......X...........
 XXXXXXXXXXXXX.......X...........
 XXXXXXXXXXXXX.......X..........X 
end
 gosub colocarmoneda
 if haslance = 3 then haslance = 0 : NUSIZ0 = $00 : missile0height = 8 
 return

room8
 room = 8
 hascoin = 0
 COLUPF = $A0
 playfield:
 XXXXXXXXXXXXX.......XX.........X
 X...................XX.........X
 X...................XX.........X
 X...................XX.........X
 X...................XX.........X
 X...................XX.........X
 X.......XXXXXXXXXXXXXX.........X
 X..............................X
 X..............................X
 X..............................X
 XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX 
end
 gosub colocarmoneda
 if haslance = 3 then haslance = 0 : NUSIZ0 = $00 : missile0height = 8 
 return

room9
 room = 9
 hascoin = 0
 COLUPF = $A0
 playfield:
 XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
 X..............................X
 X..............................X
 X............XXXXXXX...........X
 X............X.....X...........X
 X............X.....X...........X
 X............X.....X...........X
 X............XXXXXXX...........X
 ...............................X
 ...............................X
 XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
end
 gosub colocarmoneda
 if haslance = 3 then haslance = 0 : NUSIZ0 = $00 : missile0height = 8 
 return
