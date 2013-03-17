# A Simple TCP Server Programming Exercise

## The Task

Write a simple TCP server that listens on some port and calculates the
MD5 sum of the data received, returning it as a hex-encoded string.

## My Solution

I used Riak Core (http://basho.com/where-to-start-with-riak-core) to 
create a distributed MD5 Server. Using Erlang supervisors I keep
20 TCP sockets open waiting for requests from clients.

### Running a Singleton

```bash
$ make rel
$ ./rel/md5server/bin/md5server start
```

And we can verify if it is correct by comparing it with a command-line
md5 utility:

```bash
$ SERV_OUT=$(echo -n 'Hello, MD5 server!' | nc localhost 7000)
$ echo $SERV_OUT
ba6f4fdbbfad6e8613f4f9459db93c7e
$ CLI_OUT=$(echo -n 'Hello, MD5 server!' | md5)
$ echo $CLI_OUT
ba6f4fdbbfad6e8613f4f9459db93c7e
$ [ $SERV_OUT = $CLI_OUT ] && echo "Great, our MD5 server seems to be working"
Great, our MD5 server seems to be working
```

### Running Multiple Nodes with Load Balancer
Credits: http://vas.io/blog/2012/07/15/erlang-real-time-server-part-3-putting-them-together/

First start up the cluster
```bash
$ make devrel
$ ./dev/dev1/bin/md5server start
$ ./dev/dev2/bin/md5server start
$ ./dev/dev3/bin/md5server start
$ ./dev/dev2/bin/md5server-admin join md5server1@127.0.0.1
$ ./dev/dev3/bin/md5server-admin join md5server1@127.0.0.1
$ ./dev/dev1/bin/md5server-admin  member_status
```
This starts up 3 nodes listening on port 7000, 7001 and 7002.
Next setup a single point of failure...I mean load balancer. 
I am using HAProxy. To Install:

```bash
$ wget http://haproxy.1wt.eu/download/1.4/src/haproxy-1.4.20.tar.gz

.....

$ tar -xzf haproxy-1.4.20.tar.gz

.....

$ cd haproxy-1.4.20
$ make TARGET=linux26
$ sudo make install
```

Configuration:
```
global
  # specify the maximum connections across the board
  maxconn 2048
  # enable debug output
  debug

# now set the default settings for each sub-section
defaults
  # stick with http traffic
  mode http
  # set the number of times HAProxy should attempt to
  # connect to the target
  retries 3
  # specify the number of connections per front and
  # back end
  maxconn 1024
  # specify some timeouts (all in milliseconds)
  timeout connect 5000

########### MD5 Server Configuration ###################

frontend diaserver
  mode tcp

  # bind to default DIAMETER port 3868
  bind 127.0.0.1:3868

  # Default to the riak cluster configuration
  default_backend md5_cluster

  # timeouts
  timeout client 1200000

# Here is the magic bit which load balances across
# our instances of riak_core which are clustered
# together
backend md5_cluster
  mode tcp
  balance roundrobin
  # timeouts
  timeout server 1200000
  timeout connect 3000

  server md51 127.0.0.1:7000 check
  server md52 127.0.0.1:7001 check
  server md53 127.0.0.1:7002 check
```

Then run HAProxy

```bash
$ sudo haproxy -f dev.haproxy.config -d
```
And test:

```bash
$ echo -n 'Hello, MD5 server!' | nc localhost 3868
```

## Evaluation Criteria

Firstly, the code has to be correct. But working code is only the
first step in any program.

Some other criteria can be:

 * Reliability. What happens if things fail?
 * Simplicity. See "Reliability" above.
 * Performance. What happens if we put your server under load?

It is basically up to you to implement a solution that shows off your
skill level and what you believe is good code.
