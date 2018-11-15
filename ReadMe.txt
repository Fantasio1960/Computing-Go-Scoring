' how to use this module:
' the routine "ScoreCompute" must be called with the following parameters:
' - NN1 = goban's dimension - 1 (usually 18)
' - japanese = true when territory scoring, = false when area scoring
' - komi = komi's value
' - hc = number of handicap stones (0 = no handicap)
' - whitemoves = number of white moves played
' - blackmoves = number of black moves played
' also, the module exposes the integer matrix "position" (18 x 18) that must be filled in advance according to the following rules:
' 0 = empty intersection; 1 = black stone; -1 = white stone; it may be filled manually or by means of an SGF file
' the matrix may be used by the main program in order to show the position whose score is computed and so on
' the module returns an integer value according to the following rules:
' 0 = jigo (draw)
' +n = black wins by n
' -n = white wins by n
' 1000 = black wins by resignation (= black is leading, and dame are > NN1*NN1/6 (usually 54)
' -1000 = white wins by resignation (= white is leading, and dame are > NN1*NN1/6 (usually 54)
' if compiled with the global variable GUI the module has GUI capabilities:
' it calls an external routine called "draw", that can draw something over an image of the goban, with the following parameters:
' - "element" (0 = stone; 1 = mark on a stone; 2 = point of territory; 3 = point of forced connection; 4 = stone dead, thus virtually removed )
' - coordinates on the goban (x, y, usually between 0 and 18)
' - color of "element"
' it calls an external routine called "debug", that displays (usually on a textBox) some informations about the strings
' (that happens if the main program calls the public routine "stringAnalysis" specifying the number of the string plus the parameter "true":
' the number of the string is kept in the public matrix ID(), indexed by means of the goban's coordinates, i.e. ID(9,9) contains the number
' of the string including the stone (if there is one) on the Tengen point)
' it calls an external routine called "score", that displays (usually on a textBox) extended informationss regarding the score of the game

The file dyer.rar contains 623 scored games I tested the module on. They have been collected by David Dyer in 2007, and may also be found at: http://www.real-me.net/ddyer/go/scored-games.zip
