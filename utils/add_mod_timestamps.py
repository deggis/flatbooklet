import sys
import os.path
import time

if __name__ == '__main__':
    if len(sys.argv) == 2:
        fn = sys.argv[1]
        timeformat = "@%Y-%m-%dT%H:%M:%S"
        t = time.localtime(os.path.getmtime(fn))
        file = open(fn,'a')
        print "Modify fn=%s" % fn
        file.write("\n\nautomod: %s" % time.strftime(timeformat, t))
        file.close()
