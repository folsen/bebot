bebot
=====

This using a simple tree AI to figure out the best moves in Bejeweled and uses java.awt.Robot for image analysis.

Installation
------------

lein deps

Usage
-----

After you've set everything up you should open up a repl with lein repl from the root folder.

Bring up the game window and start a game (the closer the game window is to the upper right corner the better).
Now type `(calibrate)` in the repl.

When it's calibrated successfully (if it doesn't calibrate then you probably need to change the color scheme in the code) you only have to run:

    (run true false)

to start a game. 
The first parameter says it will count down from 5. The second parameter is wether it should look ahead a move or not.

Check out http://folsen.github.com/bebot/ for more information

License
-------

The MIT License

Copyright (C) 2010 Fredrik <fredrik@dekompile.com>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
