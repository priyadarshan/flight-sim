#!/bin/sh

sbcl --eval "(progn (format t \"LOADING FLIGHT-SIM AND DEPENDENCIES...~%\") (asdf:operate 'asdf:load-op 'flight-sim))" --eval "(progn (format t \"RUNNING FLIGHT-SIM:MAIN_LOOP...~%\") (flight-sim:main-loop))" --eval "(quit)"

