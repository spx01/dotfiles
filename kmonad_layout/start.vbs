Dim WinScriptHost
Set WinScriptHost = CreateObject("WScript.shell")
WinScriptHost.Run Chr(34) & "C:\Users\spx\AppData\Roaming\local\bin\kmonad.exe" & Chr(34) & " " & Chr(34) & "C:\misc\kmonad_layout\main.kbd" & Chr(34), 0, false
Set WinScriptHost = Nothing
