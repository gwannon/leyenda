 playfield:
 XXXXXXXXXXXX........XXXXXXXXXXXX
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

 player1:
 %0110
 %1111
 %1111
 %0110
end

 COLUBK = 2
 COLUPF = 240
 scorecolor = 52
 score = 0

 dim nodown = a
 dim noup = b
 dim noleft = c
 dim noright = d
 dim room = e
 dim haslance = f
 dim hasshield = g
 dim hascoin = h

 haslance = 0
 hasshield = 0
 hascoin = 0

 room = 1
 nodown = 0
 noup = 0
 noleft = 0
 noright = 0

 player0x = 21
 player0y = 80

 missile0height = 8
 missile0x = 83
 missile0y = 45

 player1x = 120
 player1y = 20

 ballheight = 4
 CTRLPF = $21
 ballx = 0
 bally = 0

mainloop
 const screenheight=84

 AUDV0 = 0
 COLUP0 = 4

 if joy0left && !joy0right && !joy0up && !joy0down && noleft = 0 then gosub moverizquierda
 if !joy0left && joy0right && !joy0up && !joy0down && noright = 0 then gosub moverderecha
 if !joy0left && !joy0right && joy0up && !joy0down && noup = 0 then gosub moverarriba
 if !joy0left && !joy0right && !joy0up && joy0down && nodown = 0 then gosub moverabajo

 if collision(missile0,player0) && haslance = 0 then haslance = 1 
 if collision(ball,player0) && hasshield = 0 then hasshield = 1 
 if collision(player1,player0) && hascoin = 0 then hascoin = 1 : score = score + 10 

 if room = 1 && player0x > 145 then gosub room2	: player0x = 22
 if room = 2 && player0x < 5 then gosub room1 : player0x = 140	

 if room = 1 && player0y < 10 then gosub room3 : player0y = 80	
 if room = 3 && player0y > 85 then gosub room1 : player0y = 10	

 if room = 3 && player0x > 145 then gosub room4	: player0x = 22
 if room = 4 && player0x < 5 then gosub room3 : player0x = 140	
 
 if room = 2 && player0y < 5 then gosub room4 : player0y = 80	
 if room = 4 && player0y > 85 then gosub room2 : player0y = 10	

 if room = 1 && haslance = 0 then missile0x = 83 : missile0y = 45
 if room <> 1 && haslance = 0 then missile0x = 0 : missile0y = 0

 if room = 2 && hasshield = 0 then ballx = 83 : bally = 45
 if room <> 2 && hasshield = 0 then ballx = 0 : bally = 0

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
 
 if collision(playfield,player0) then player0y = player0y - 1 : noright = 0 : noleft = 0 : noup = 0 : nodown = 1 else player0y = player0y + 1: noright = 0 : noleft = 0 : noup = 0 : nodown = 0
 if haslance = 1 then missile0x = player0x : missile0y = player0y - 2
 if hasshield = 1 then ballx = player0x + 7 : bally = player0y - 3
 return

room1
 room = 1
 hascoin = 0
 pfclear
 COLUBK = 2
 COLUPF = 240
 playfield:
 XXXXXXXXXXXX........XXXXXXXXXXXX
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
 drawscreen
 return

room2
 room = 2
 hascoin = 0
 pfclear
 COLUBK = 2
 COLUPF = 240
 playfield:
 XXXXXXXXXXXX........XXXXXXXXXXXX
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
 drawscreen
 return

room3
 room = 3
 hascoin = 0
 pfclear
 COLUBK = 2
 COLUPF = 240
 playfield:
 XXXXXXXXXXXXX.......XXXXXXXXXXXX
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
 drawscreen
 return

room4
 room = 4
 hascoin = 0
 pfclear
 COLUBK = 2
 COLUPF = 240
 playfield:
 XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
 X..............................X
 X..............................X
 X............X.....X...........X
 .............X.....X............
 .............X.....X............
 .............X.....X............
 X............X.....X...........X
 X..............................X
 X..............................X
 XXXXXXXXXXXX........XXXXXXXXXXXX 
end
 drawscreen
 return

room5
 room = 5
 hascoin = 0
 pfclear
 COLUBK = 2
 COLUPF = 64
 playfield:
 XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
 X..............................X
 X..............................X
 X..............................X
 X..............................X
 X..............................X
 X..............................X
 X..............................X
 X..............................X
 X..............................X
 XXXXXXXXXXXX........XXXXXXXXXXXX 
end
 drawscreen
 return

room6
 room = 6
 hascoin = 0
 pfclear
 COLUBK = 2
 COLUPF = 112
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
 drawscreen
 return

room7
 room = 7
 hascoin = 0
 pfclear
 COLUBK = 2
 COLUPF = 112
 playfield:
 XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
 ....................XXXXXXXXXXXX
 ....................XXXXXXXXXXXX
 ....................XXXXXXXXXXXX
 XXXXXXXXXXXX....................
 XXXXXXXXXXXX....................
 XXXXXXXXXXXX....................
 XXXXXXXXXXXX........XXXXXXXXXXXX
 XXXXXXXXXXXX........XXXXXXXXXXXX
 XXXXXXXXXXXX........XXXXXXXXXXXX
 XXXXXXXXXXXX........XXXXXXXXXXXX 
end
 drawscreen
 return

room8
 room = 8
 hascoin = 0
 pfclear
 COLUBK = 2
 COLUPF = 112
 playfield:
 XXXXXXXXXXXX........XXXXXXXXXXXX
 X..............................X
 X..............................X
 X..............................X
 X..............................X
 X..............................X
 X..............................X
 X..............................X
 X..............................X
 X..............................X
 XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX 
end
 drawscreen
 return

room9
 room = 9
 hascoin = 0
 pfclear
 COLUBK = 2
 COLUPF = 112
 playfield:
 XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
 X..............................X
 X..............................X
 X............XXXXXXX...........X
 .............XXXXXXX...........X
 .............XXXXXXX...........X
 .............XXXXXXX...........X
 X............XXXXXXX...........X
 X..............................X
 X..............................X
 XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
end
 drawscreen
 return

