Sheet music with playback using HTML Canvas. The project is extremely unfinished right now.

### Demonstration
 * Project [demo](http://cdn.rawgit.com/nickgeoca/music-browserside/master/proj.html). Click through buttons, then in canvas, and then on a note. If canvas does not show up, try refreshing page.
 * Currently xml data is copied into the html based code.

### Useful Commands
 * ./copy_docker.py | xclip -selection c
 * sudo docker run -it- -p 80:80 agocorona/tryhplay

### Compile/Run HTML code
1. git clone
2. Start docker image and then, using browser, click execute/edit any of the examples
3. run this to copy code to clipboard (combines code from different files): ./copy_docker.py | xclip -selection c
4. paste clipboard into the browser (ctrl-v) and compile

### Browser Side Files
Creates html from Haskell (Haste lib). Use this command to combine files: ./copy_docker.py | xclip -selection c
 * docker_code.hs
   * Single file using docker. The Haste compiler was not working at the time.
   * Docker image: sudo docker run -it- -p 80:80 agocorona/tryhplay
 * Graphic.hs : Has graphic library for HTML canvas
 * ListTree.hs: Has library for ListTree data structure. Explained in file.
 * Music.hs: This defines haskell type which is used by HTML canvas. 

### Server Side Files
Parses MusicXML to create Music type used by HTML canvas
 * Main.hs : xml file (MusicXML)                       -> haskell type (intermediate MusicXML type)
 * MXml.hs : haskell type (intermediate MusicXML type) -> haskell type (Music type used by HTML Canvas)
 * Music.hs: This defines haskell type which is used by HTML canvas. 

### Goals
Short term project goal was to parse demo-score.xml and create a canvas that looked like demo-score.png.
Long term project goal was to parse MusicXML and pass [test cases](http://lilypond.org/doc/v2.19/input/regression/musicxml/collated-files#test-cases)

### Similar projects
 * (music-suite)[http://music-suite.github.io/docs/ref/] is a haskell based music language. It can plug into lilypond for graphical display.
 * (alphaTab)[http://www.alphatab.net/] ((github)[https://github.com/CoderLine/alphaTab]) is a music notation rendering library. It can play in browser with HTML5/Flash
