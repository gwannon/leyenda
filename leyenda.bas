 rem Leyenda

 playfield:
 XXXXXXXXXXXX........XXXXXXXXXXXX
 X..............................X
 X..............................X
 X..............................X
 X............XXXXXXX............
 X............XXXXXXX............
 X............XXXXXXX............
 X..............................X
 X..............................X
 X..............................X
 XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
end

 player0:
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

 nodown = 0
 noup = 0
 noleft = 0
 noright = 0
 
 player0x = 21
 player0y = 80

mainloop

 AUDV0 = 0
 COLUP0 = 4
 COLUP1 = 132

 rem if !collision(playfield,player0) then nodown = 0 : noup = 0 : noleft = 0 : noright = 0
 if joy0left && noleft = 0 then gosub moverizquierda
 if joy0right && noright = 0 then gosub moverderecha
 if joy0up && noup = 0 then gosub moverarriba
 if joy0down && nodown = 0 then gosub moverabajo

 drawscreen
 goto mainloop

moverizquierda
 player0:
 %00100100
 %00100100
 %10011001
 %11111111
 %10011001
 %10100100
 %10111100
 %10110110
end
 player0x = player0x - 1
 if collision(playfield,player0) then noleft = 1 : noright = 0 : player0x = player0x + 2 else noleft = 0 : noright = 0 
 return

moverderecha
 player0:
 %00100100
 %00100100
 %10011001
 %11111111
 %10011001
 %00100101
 %00111101
 %01101101
end
 player0x = player0x + 1
 if collision(playfield,player0) then noleft = 0 : noright = 1 : player0x = player0x - 2 else noleft = 0 : noright = 0 
 return

moverarriba
 player0:
 %00100100
 %00100100
 %10011001
 %11111111
 %10011001
 %00100101
 %00111101
 %00100101
end
 player0y = player0y - 1
 if collision(playfield,player0) then nodown = 0 : noup = 1 : player0y = player0y + 2 else nodown = 0 : noup = 0
 return

moverabajo
 player0:
 %00100101
 %00100101
 %10011001
 %11111111
 %10011001
 %00100100
 %00111100
 %00100100
end
 player0y = player0y + 1
 if collision(playfield,player0) then nodown = 1 : noup = 0 : player0y = player0y - 2 else nodown = 0 : noup = 0
 return


