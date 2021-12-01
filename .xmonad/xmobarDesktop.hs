Config { font = "xft:Bitstream Vera Sans Mono:size=12:antialias=true"
       , bgColor = "#242424"
       , fgColor = "grey"
       , position = TopW L 94
       , commands = [ Run Cpu ["-t", "<total>%", "-L","5","-H","40","--normal","green","--high","red"] 15
                    , Run Memory ["-t", "<usedratio>%"] 30
                    , Run Date "%a %b %_d - %l:%M" "date" 30
                    , Run StdinReader
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "%StdinReader% }{ %date% | CPU: %cpu% | MEM: %memory%"
       , lowerOnStart = False
}
