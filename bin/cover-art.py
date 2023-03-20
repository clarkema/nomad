#! /usr/bin/env python

# pip install python-mpd2

import os
import tempfile
import sys
import signal
from mpd import MPDClient

def signal_handler(_signum, _frame):
    sys.exit(0)

def main():
    mpd = MPDClient()
    mpd.connect("localhost", 6600)
    old_art = None

    with tempfile.NamedTemporaryFile() as temp_file:
        def display():
            nonlocal old_art
            if old_art is not None:
                os.system("kitty +kitten icat --clear " + temp_file.name)
        signal.signal(signal.SIGWINCH, lambda _sig, _frame : display())

        while True:
            art = mpd.readpicture(mpd.currentsong()['file'])
            if 'binary' in art:
                art_bin = art['binary']

                if art_bin != old_art:
                    with open(temp_file.name, "wb") as fh:
                        fh.write(art_bin)
                    old_art = art_bin
                    display()
            else:
                os.system("kitty +kitten icat --clear")
                old_art = None

            mpd.idle("player")

if __name__ == '__main__':
    signal.signal(signal.SIGINT, signal_handler)

    if os.environ['TERM'] == "xterm-kitty":
        main()
    else:
        sys.exit("cover-art.py only works in kitty")
