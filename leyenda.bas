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
 %00100100
 %00100100
 %00100100
 %10011001
 %11111111
 %10011001
 %10100100
 %10111100
 %10110110

end

 COLUPF = 176
 scorecolor = 52
 score = 0



 dim nodown = a
 dim noup = b
 dim noleft = c
 dim noright = d
 dim room = e

 room = 1
 nodown = 0
 noup = 0
 noleft = 0
 noright = 0

 player0x = 21
 player0y = 80

mainloop
 const screenheight=84

 AUDV0 = 0
 COLUP0 = 4

 if joy0left && !joy0right && !joy0up && !joy0down && noleft = 0 then gosub moverizquierda
 if !joy0left && joy0right && !joy0up && !joy0down && noright = 0 then gosub moverderecha
 if !joy0left && !joy0right && joy0up && !joy0down && noup = 0 then gosub moverarriba
 if !joy0left && !joy0right && !joy0up && joy0down && nodown = 0 then gosub moverabajo

 if room = 1 && player0x > 145 then gosub room2	: player0x = 22
 if room = 2 && player0x < 10 then gosub room1 : player0x = 140	
 
 drawscreen
 goto mainloop

moverizquierda
 player0:
 %00100100
 %00100100
 %00100100
 %10011001
 %11111111
 %10011001
 %10100100
 %10111100
 %10110110
end
 
 if collision(playfield,player0) then player0x = player0x + 1 : noright = 0 : noleft = 1 : noup = 0 : nodown = 0 else player0x = player0x - 1 : noright = 0 : noleft = 0 : noup = 0 : nodown = 0
 return

moverderecha
 player0:
 %00100100
 %00100100
 %00100100
 %10011001
 %11111111
 %10011001
 %00100101
 %00111101
 %01101101
end
 
 if collision(playfield,player0) then player0x = player0x - 1 : noright = 1 : noleft = 0 : noup = 0 : nodown = 0 else player0x = player0x + 1: noright = 0 : noleft = 0 : noup = 0 : nodown = 0
 return

moverarriba
 player0:
 %00100100
 %00100100
 %00100100
 %10011001
 %11111111
 %10011001
 %00100101
 %00111101
 %00100101
end

 if collision(playfield,player0) then player0y = player0y + 1 : noright = 0 : noleft = 0 : noup = 1 : nodown = 0 else player0y = player0y - 1: noright = 0 : noleft = 0 : noup = 0 : nodown = 0
 return

moverabajo
 player0:
 %00100100
 %00100101
 %00100101
 %10011001
 %11111111
 %10011001
 %00100100
 %00111100
 %00100100
end
 
 if collision(playfield,player0) then player0y = player0y - 1 : noright = 0 : noleft = 0 : noup = 0 : nodown = 1 else player0y = player0y + 1: noright = 0 : noleft = 0 : noup = 0 : nodown = 0
 return

room1
 room = 1
 pfclear
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
 pfclear
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
 pfclear
 playfield:
 XXXXXXXXXXXXXXXXXXXXXXXXxxXXXXXX
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
 pfclear
 playfield:
 XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
 X..............................X
 X..............................X
 X............X.....X...........X
 .............X.....X...........X
 .............X.....X...........X
 .............X.....X...........X
 X............X.....X...........X
 X..............................X
 X..............................X
 XXXXXXXXXXXX........XXXXXXXXXXXX 
end
 drawscreen
 return

