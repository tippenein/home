Config { font = "xft:Bitstream Vera Sans Mono:size=12:antialias=true"
       , bgColor = "#242422"
       , fgColor = "grey"
       , position = TopW L 86
       , commands = [ Run Cpu ["-t", "<total>%", "-L","5","-H","40","--normal","green","--high","red"] 15
                    , Run Memory ["-t", "<usedratio>%"] 30
                    , Run Date "%a %b %_d - %l:%M" "date" 30
                    , Run StdinReader
                    , Run Com "/bin/bash" ["-c", "~/.config/xmonad/get-volume"] "myvolume" 1
                    ]
       , sepChar = "%"
       , alignSep = "}{"
       , template = "%StdinReader% }{ %date% | Vol: <fc=#00CDCD>%myvolume%</fc> | CPU: %cpu% | MEM: %memory%"
       , lowerOnStart = False
}
