While debugging your DLL, it may be easier for you
to run Tecplot directly from you DLL project.
To do this from Developer Studio 5:

1. Select Project/Settings.
2. Click the Debug tab.
3. Set "Executable for debug sessions" file to "tecplot.exe" (include the full path if necessary).
4. Set "Working Directory" to "Debug".
5. Set "Program Arguments" to "-loadaddon LOADTXT"
