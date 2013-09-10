# ser2netctl

A control program for the `ser2net` serial to TCPIP daemon.

What's the point of this?  You may well ask.  What it does can probably be achieved with judicious use of `sed` and regular daemon 
management commands.

I was asked to provide a means to easily alter `ser2net` config, and optionally reload the config and/or restart `ser2net`.  A shell script
would probably have done the job, but I saw my opportunity to write some __haskell__ code and I took it.

`ser2netctl` provides a way to add new ports for `ser2net` to manage, remove ports, update the config of existing ports, show all managed 
ports, restart `ser2net` ,and shutdown `ser2net.`  At present the bulk of the command line options apply to the add and update commands.
They provide the means to specify many aspects of a port's configuration.


## Usage

    ser2netctl add --port=<port> [options]
    ser2netctl remove --port=<port>
    ser2netctl update --port=<port> [options]
    ser2netctl show
    ser2netctl restart
    ser2netctl shutdown

    Options:
    
      -p <port>                  --port=<port>                   The TCP/IP port number of the port of interest
      -d <tty device>            --tty=<tty device>              The character device file of the serial device
      -t <timeout seconds>       --timeout=<timeout seconds>     The number of seconds of inactivity before ser2net will close a tty
      -m <off|raw|rawlp|telnet>  --mode=<off|raw|rawlp|telnet>   The mode or protocol used to talk to the serial device
      -b <baud rate>             --baud=<baud rate>              The baud rate that the serial device should talk at
      -w <7|8>                   --databits=<7|8>                The number of bits per character for the serial device
      -s <1|2>                   --stopbits=<1|2>                The number of stop bits per character for the serial device
      -P <odd|even|none>         --parity=<odd|even|none>        The parity setting for the serial device
      -x                         --swfc                          Use software flow control on serial device [default: no flow control]
      -H                         --hwfc                          Use hardware flow control on serial device [default: no flow control]
      -r                         --force-daemon-restart          If present, causes ser2net daemon to be restarted
      -C                         --clear-absent-tty-config       Clear any tty options not specified on command line [Default: absent tty options preserved]
      -n                         --no-config-reload              If present, prevents the modified ser2net.conf from being reloaded by the daemon
      -i <pid>                   --pid=<pid>                     Use this to specify the PID of the ser2net daemon
      -f <PID file name>         --pid-file=<PID file name>      Use this to specify the name of the ser2net daemon's PID file [Default: /var/run/ser2net.pid]
      -c <conf file name>        --config-file=<conf file name>  Use this to specify an alternative config file
      -v                         --version                       Print version information
      -h                         --help                          Print this usage information


## Options

<dl>
  <dt><strong><code>-p &lt;num&gt; --port=&lt;num&gt;</code></strong></dt>
  <dd>The TCP/IP port number through which <code>ser2net</code> will marshal access to a given serial tty device.
    <p>
      This option must be provided with the <code>add</code>, <code>remove</code>, and <code>update</code> commands.
    </p>
  </dd>

  <dt><strong><code>-d &lt;tty device&gt;, --tty=&lt;tty device&gt;</code></strong></dt>
  <dd>The character device file of the serial device
    <p>
      <code>/dev/ttyS0</code> for example.  This is the full path to the serial TTY device that is to be mapped to a TCP/IP port by <code>ser2net</code>.
    </p>
  </dd>

  <dt><strong><code>-t &lt;timeout seconds&gt;, --timeout=&lt;timeout seconds&gt;</code></strong></dt>
  <dd>The number of seconds of inactivity before <code>ser2net</code> will close a tty
  </dd>

  <dt><strong><code>-m &lt;off|raw|rawlp|telnet&gt;, --mode=&lt;off|raw|rawlp|telnet&gt;</code></strong></dt>
  <dd>The mode or protocol used to talk to the serial device
    <p>
      For more on these modes, please read the <code>ser2net</code> man page.
    </p>
    <p>
      Valid modes are <code>off</code>, <code>raw</code>, <code>rawlp</code>, and <code>telnet</code>.
    </p>
  </dd>

  <dt><strong><code>-b &lt;baud rate&gt;, --baud=&lt;baud rate&gt;</code></strong></dt>
  <dd>The baud rate that the serial device should talk at
  </dd>

  <dt><strong><code>-w &lt;7|8&gt;, --databits=&lt;7|8&gt;</code></strong></dt>
  <dd>The number of bits per character for the serial device
  </dd>

  <dt><strong><code>-s &lt;1|2&gt;, --stopbits=&lt;1|2&gt;</code></strong></dt>
  <dd>The number of stop bits per character for the serial device
  </dd>

  <dt><strong><code>-P &lt;odd|even|none&gt;, --parity=&lt;odd|even|none&gt;</code></strong></dt>
  <dd>The parity setting for the serial device
  </dd>

  <dt><strong><code>-x, --swfc</code></strong></dt>
  <dd>Use software flow control on serial device [default: no flow control]
  </dd>

  <dt><strong><code>-H, --hwfc</code></strong></dt>
  <dd>Use hardware flow control on serial device [default: no flow control]
  </dd>

  <dt><strong><code>-C, --clear-absent-tty-config</code></strong></dt>
  <dd>Clear any tty options not specified on command line [Default: absent tty options preserved]
    <p>
    Use this with the <code>update</code> command.  For a given port, it will cause any option not specified on the command line to be
    cleared from the port's configuration.  Without this option, TTY parameters of a port's configuration that are not mentioned on 
    the command line are left in place.
    </p>
  </dd>

  <dt><strong><code>-n, --no-config-reload</code></strong></dt>
  <dd>If present, prevents the modified ser2net.conf from being reloaded by the daemon
    <p>
    <code>ser2netctl</code> will not try to send <code>SIGHUP</code> to the daemon to reload its config.
    </p>
  </dd>

  <dt><strong><code>-r, --force-daemon-restart</code></strong></dt>
  <dd>If present, causes <code>ser2net</code> daemon to be restarted
  </dd>

  <dt><strong><code>-i &lt;pid&gt;, --pid=&lt;pid&gt;</code></strong></dt>
  <dd>Use this to specify the PID of the ser2net daemon
    <p>
    This is one way to tell <code>ser2netctl</code> the process id of the <code>ser2net</code> daemon.  <code>ser2netctl</code>
    uses the process id to send SIGHUP to the daemon to get it to reload its config file.
    </p>
  </dd>

  <dt><strong><code>-f &lt;PID file name&gt;, --pid-file=&lt;PID file name&gt;</code></strong></dt>
  <dd>Use this to specify the name of the <code>ser2net</code> daemon's PID file [Default: <code>/var/run/ser2net.pid</code>]
    <p>
    This is another way to tell <code>ser2netctl</code> the process id of the <code>ser2net</code> daemon.
    </p>
  </dd>

  <dt><strong><code>-c &lt;conf file name&gt;, --config-file=&lt;conf file name&gt;</code></strong></dt>
  <dd>Use this to specify an alternative config file
  </dd>

  <dt><strong><code>-v, --version</code></strong></dt>
  <dd>Print version information
  </dd>

  <dt><strong><code>-h, --help</code></strong></dt>
  <dd>Print this usage information
  </dd>
</dl>


## Compiling

A working GHC is required.  It is probably easiest to install the haskell platform if you don't already have it.

These modules are required: `base >=4.5`, `safe`, `unix`, `regex-pcre`, `strict`, `split`, `template-haskell`

To compile:

```
$ runhaskell Setup.hs configure
$ runhaskell Setup.hs build
```

The `ser2netctl` binary will be in `dist/build/ser2netctl/`

OR

```
$ cabal install
```


## Further Work

* Interact with the running ser2net daemon via its control port.  Particularly, disable active connections.
* Manipulate more configuration items such as banners, log files.


