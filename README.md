# A Simple TCP Server Programming Exercise

## The Task

Write a simple TCP server that listens on some port and calculates the
MD5 sum of the data received, returning it as a hex-encoded string.

Assuming your server is listening on port 7000, we can test it using
the netcat command-line utility:

```bash
$ echo -n 'Hello, MD5 server!' | nc localhost 7000
ba6f4fdbbfad6e8613f4f9459db93c7e
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

## Evaluation Criteria

Firstly, the code has to be correct. But working code is only the
first step in any program.

Some other criteria can be:

 * Reliability. What happens if things fail?
 * Simplicity. See "Reliability" above.
 * Performance. What happens if we put your server under load?

It is basically up to you to implement a solution that shows off your
skill level and what you believe is good code.
