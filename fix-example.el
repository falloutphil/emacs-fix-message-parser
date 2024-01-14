(module-load "/home/phil/git/quickfix_emacs/emacs-fix-parser.so")

(message "Parsed FIX message: %s" (parse-fix-message "8=FIX.4.49=14835=D34=108049=TESTBUY152=20180920-18:14:19.50856=TESTSELL111=63673064027889863415=USD21=238=700040=154=155=MSFT60=20180920-18:14:19.49210=092"))


(message "Parsed FIX message: %s" (parse-fix-message "8=FIX.4.49=20535=D34=112249=CLIENT1252=20240115-09:30:15.00056=BROKER9911=18273829121=138=30040=244=50.554=155=AAPL59=060=20240115-09:30:15.000100=N10=228"))


(message "Parsed FIX message: %s" (parse-fix-message "8=FIX.4.49=13135=D34=112249=CLIENT1252=20240115-09:30:15.00056=BROKER9911=18273829121=138=300040=254=155=MSFT60=20240115-09:30:15.00010=253"))