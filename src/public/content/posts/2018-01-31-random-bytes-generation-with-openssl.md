Sometimes you need to generate random bytes with high entropy - perhaps for a password, a secret key, or testing purposes. OpenSSL provides a simple command for this task: `rand`.

<!--more-->

The basic syntax is straightforward:

``` bash
$ openssl rand [options] num
```

The `rand` command outputs `num` pseudo-random bytes. However, since raw bytes often contain unprintable characters, it's better to use either `-hex` or `-base64` encoding to ensure the output is a valid, printable string suitable for passwords and keys.

``` bash
$ openssl rand 16
fӤ?v ???^@?y??

$ openssl rand -hex 16
b4ef65a47a327727bf4ad77d8d3352b2

$ openssl rand -base64 16
o1DqThmx1DWGoPAidi6DKQ==
```

One thing to note: when you use `-hex` or `-base64` encoding, the output string is longer than the `num` parameter you specified. This is because encoding increases the data size. If you need a specific output length, you can truncate the result using the `cut` command.

``` bash
$ openssl rand -hex 16 | cut -c1-16
ce8ad63b50cbe611

$ openssl rand -base64 16 | cut -c1-16
4HIcmt4vTcmchbHU
```

# Further reading

- [Rand command manual](https://wiki.openssl.org/index.php/Manual:Rand(1)) – Official OpenSSL documentation for the rand command
- [OpenSSL random numbers](https://wiki.openssl.org/index.php/Random_Numbers) – Understanding random number generation in OpenSSL
- [Password Haystack](https://www.grc.com/haystack.htm) – Analysing password strength and entropy
