import MBot

main =  do
  d <- openMBot
  sendCommand d $ setRGB 1 0   0 0
  sendCommand d $ setRGB 2 0 0 0
  closeMBot d
