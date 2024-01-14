g++ -std=c++11 -fPIC -c emacs-fix-parser.cpp -o emacs-fix-parser.o && g++ -shared -o emacs-fix-parser.so emacs-fix-parser.o -lquickfix

