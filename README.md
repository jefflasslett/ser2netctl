.h1 ser2netctl

A control program for the se2net serial to TCPIP daemon.

```
ser2netctl add|remove|stop|start|update|show|restart|shutdown [options]
  -p <port>                  --port=<port>                   The TCP/IP port number of the port of interest
  -d <tty device>            --tty=<tty device>              The character device file of the serial device
  -t <timeout seconds>       --timeout=<timeout seconds>     The number of seconds of inactivity before ser2net will close a tty
  -m <off|raw|rawlp|telnet>  --mode=<off|raw|rawlp|telnet>   The mode or protocol used to talk to the serial device
  -b <baud rate>             --baud=<baud rate>              The baud rate that the serial device should talk at
  -w <7|8>                   --databits=<7|8>                The number of bits per character for the serial device
  -s <1|2>                   --stopbits=<1|2>                The number of stop bits per character for the serial device
  -P <odd|even|none>         --parity=<odd|even|none>        The parity setting for the serial device
  -x                         --swfc                          Use software flow control on serial device [default: no flow control]
  -h                         --hwfc                          Use hardware flow control on serial device [default: no flow control]
  -r                         --force-daemon-restart          If present, causes ser2netctl to be restarted
  -n                         --no-config-reload              If present, prevents the modified ser2net.conf from being reloaded by the daemon
  -i <pid>                   --pid=<pid>                     Use this to specify the PID of the ser2net daemon
  -f <PID file name>         --pid-file=<PID file name>      Use this to specify the name of the ser2net daemon's PID file [Default: /var/run/ser2net.pid]
  -c <conf file name>        --config-file=<conf file name>  Use this to specify an alternative config file
  -v                         --version                       Print version information
  -h                         --help                          Print this usage information
```

